(* InterMediate Language, or Intermediate ML *)
open Spotlib.Spot
open Asttypes
open Typedtree
open Tools

module M = Michelson
open M.Type
open M.Opcode

type pat = 
  { loc : Location.t
  ; id : Ident.t
  ; typ : M.Type.t
  }

type t = { loc : Location.t ; desc : desc ; typ : M.Type.t }
and desc =
  | Const of M.Opcode.constant
  | Nil of M.Type.t
  | Cons of t * t
  | Left of M.Type.t * t
  | Right of M.Type.t * t
  | Unit
  | Var of Ident.t * M.Type.t
  | Tuple of t * t
  | Assert of t
  | AssertFalse
  | Fun of M.Type.t * M.Type.t * pat * t * (Ident.t * M.Type.t) list (* freevars *)
  | IfThenElse of t * t * t
  | App of t * t list
  | Prim of string * M.Opcode.t list * t list
  | Let of pat * t * t
  | Switch_or of t * pat * t * pat * t
  | Switch_cons of t * t * pat * t

let rec pp ppf = 
  let p = Format.pp_print_string ppf in
  let f fmt = Format.fprintf ppf fmt in
  fun t -> match t.desc with
  | Const c -> M.Opcode.pp_constant ppf c
  | Nil ty -> f "([] : %a)" M.Type.pp ty
  | Cons (t1, t2) -> f "(%a :: %a)" pp t1 pp t2
  | Left (ty, t) -> f "Left (%a) (%a)" M.Type.pp ty pp t
  | Right (ty, t) -> f "Right (%a) (%a)" M.Type.pp ty pp t
  | Unit -> p "()"
  | Var (id, _) -> f "%s" (Ident.name id)
  | Tuple (t1, t2) -> f "(%a, %a)" pp t1 pp t2
  | Assert t -> f "assert (%a)" pp t
  | AssertFalse -> p "assert false"
  | Fun (_ty1, _ty2, pat, body, _fvars) ->
      f "@[<2>(fun %s ->@ %a@ : %a)@]"
        (Ident.name pat.id) pp body M.Type.pp t.typ
  | IfThenElse (t1, t2, t3) -> 
      f "(if %a @[then %a@ else %a@])"
        pp t1 pp t2 pp t3
  | App (t1, ts) -> 
      f "(%a %a)" pp t1 Format.(list " " (fun ppf t -> fprintf ppf "(%a)" pp t)) ts
  | Prim (n, _ops, ts) ->
      f "(%s %a)" 
        n
        Format.(list " " (fun ppf t -> fprintf ppf "(%a)" pp t)) ts
  | Let (p, t1, t2) ->
      f "@[<2>(let %s =@ %a in@ %a)@]"
        (Ident.name p.id) pp t1 pp t2
  | Switch_or (t, p1, t1, p2, t2) ->
      f "@[<2>(match %a with@ | Left %s -> %a@ | Right %s -> %a)@]"
        pp t
        (Ident.name p1.id) pp t1 
        (Ident.name p2.id) pp t2
  | Switch_cons (t, t1, p2, t2) ->
      f "@[<2>(match %a with@ | [] -> %a@ | Right %s -> %a)@]"
        pp t
        pp t1 
        (Ident.name p2.id) pp t2

module IdTys = Set.Make(struct type t = Ident.t * M.Type.t let compare (id1,_) (id2,_) = compare id1 id2 end)

let rec freevars t = 
  let open IdTys in
  match t.desc with
  | Const _
  | Nil _ 
  | Unit -> empty
  | Cons (t1,t2) 
  | Tuple (t1,t2)
         -> union (freevars t1) (freevars t2)
  | Left (_,t) 
  | Right (_,t)
  | Assert t -> freevars t
  | AssertFalse -> empty
  | Var (id,ty) -> singleton (id,ty)
  | IfThenElse (t1,t2,t3) -> union (freevars t1) (union (freevars t2) (freevars t3))
  | App (t,ts) ->
      List.fold_left (fun acc t -> union acc (freevars t)) empty (t::ts)
  | Prim (_,_,ts) ->
      List.fold_left (fun acc t -> union acc (freevars t)) empty ts
  | Fun (_,_,pat,t,_fvs) -> 
      diff (freevars t) (singleton (pat.id, pat.typ))
  | Let (pat, t1, t2) ->
      diff (union (freevars t1) (freevars t2)) (singleton (pat.id, pat.typ))
  | Switch_or (t, p1, t1, p2, t2) ->
      union (freevars t)
        (union 
           (remove (p1.id, p1.typ) (freevars t1))
           (remove (p2.id, p2.typ) (freevars t2)))
  | Switch_cons (t, t1, p2, t2) ->
      union (freevars t)
        (union 
           (freevars t1)
           (remove (p2.id, p2.typ) (freevars t2)))

type type_expr_error =
  | Type_variable of Types.type_expr
  | Unsupported_type of Types.type_expr

let rec type_expr tenv ty = 
  let open Result.Infix in
  let ty = Ctype.expand_head tenv ty in
  match ty.desc with
  | Tvar _ -> Error (Type_variable ty)
  | Tarrow (Nolabel, f, t, _) -> 
      type_expr tenv f >>= fun f ->
      type_expr tenv t >>= fun t -> Ok (TyLambda (f, t, { closure_desc= CLList [] }))
  | Ttuple [t1; t2] -> 
      type_expr tenv t1 >>= fun t1 ->
      type_expr tenv t2 >>= fun t2 -> Ok (TyPair (t1, t2))
  | Tconstr (p, [], _) when p = Predef.path_bool -> Ok TyBool
  | Tconstr (p, [t], _) when p = Predef.path_list -> 
      type_expr tenv t >>= fun t ->Ok (TyList t)
  | Tconstr (p, [t1; t2], _) when (match Path.is_scaml p with Some ("sum", _) -> true | _ -> false) ->
      type_expr tenv t1 >>= fun t1 ->
      type_expr tenv t2 >>= fun t2 -> Ok (TyOr (t1, t2))
  | Tconstr (p, [], _) when (match Path.is_scaml p with Some ("operation", _) -> true | _ -> false) ->
      Ok TyOperation
  | Tconstr (p, [], _) when p = Predef.path_unit -> Ok TyUnit
  | Tconstr (p, [], _) when p = Predef.path_string -> Ok TyString
  | Tconstr (p, [], _) ->
      begin match Path.is_scaml p with
        | Some ("int", _) -> Ok TyInt
        | Some ("nat", _) -> Ok TyNat
        | _ -> Error (Unsupported_type ty)
      end
  | _ -> Error (Unsupported_type ty)

let type_expr ~loc tenv ty = 
  match type_expr tenv ty with
  | Ok x -> x
  | Error (Type_variable ty') -> 
      errorf ~loc "This expression has type %a, which has too generic type %a for SCaml."
        Printtyp.type_expr ty
        Printtyp.type_expr ty'
  | Error (Unsupported_type ty') ->
      errorf ~loc "This expression has type %a, which has unsupported type %a in SCaml"
        Printtyp.type_expr ty
        Printtyp.type_expr ty'

let pattern { pat_desc; pat_loc=loc; pat_type; pat_env } = 
  match pat_desc with
  | Tpat_var (id, {loc}) ->
      let ty = type_expr ~loc pat_env pat_type in
      [{ loc; id; typ= ty }]

  | Tpat_alias ({pat_desc = Tpat_any; pat_loc=_}, id, _) -> 
      (* We transform (_ as x) in x if _ and x have the same location.
         The compiler transforms (x:t) into (_ as x : t).
         This avoids transforming a warning 27 into a 26.
       *)
      let ty = type_expr ~loc pat_env pat_type in
      [{ loc; id; typ= ty }]

  | Tpat_any         -> unsupported ~loc "any pattern"
  | Tpat_alias _     -> unsupported ~loc "alias pattern"
  | Tpat_constant _  -> unsupported ~loc "constant pattern"
  | Tpat_tuple _     -> unsupported ~loc "tuple pattern"
  | Tpat_construct _ -> unsupported ~loc "variant pattern"
  | Tpat_variant _   -> unsupported ~loc "polymorphic variant pattern"
  | Tpat_record _    -> unsupported ~loc "record pattern"
  | Tpat_array _     -> unsupported ~loc "array pattern"
  | Tpat_or _        -> unsupported ~loc "or pattern"
  | Tpat_lazy _      -> unsupported ~loc "lazy pattern"

let rec construct ~loc env exp_env exp_type {Types.cstr_name} args =
  let make typ desc = { loc; typ; desc } in
  match (Ctype.expand_head exp_env exp_type).Types.desc with
  (* bool *)
  | Tconstr (p, [], _) when p = Predef.path_bool ->
      make TyBool (match cstr_name with
          | "true" -> Const M.Opcode.True
          | "false" -> Const M.Opcode.False
          | s -> internal_error ~loc "strange bool constructor %s" s)

  (* list *)
  | Tconstr (p, [ty], _) when p = Predef.path_list ->
      begin match cstr_name with
          | "[]" -> 
              let ty = type_expr ~loc exp_env ty in
              make (TyList ty) (Nil ty)
          | "::" ->
              begin match args with
                | [e1; e2] ->
                    let e1 = expression env e1 in
                    let e2 = expression env e2 in
                    let typ = unify (TyList e1.typ) e2.typ in
                    make typ @@ Cons (e1, e2)
                | _ -> internal_error ~loc "strange cons"
              end
          | s -> internal_error ~loc "strange list constructor %s" s
      end

  (* sum *)
  | Tconstr (p, [_; _], _) when (match Path.is_scaml p with Some ("sum", _) -> true | _ -> false) ->
      let typ = type_expr ~loc exp_env exp_type in
      let ty1, ty2 = match typ with
        | TyOr (ty1, ty2) -> ty1, ty2
        | _ -> assert false
      in
      let arg = match args with [arg] -> arg | _ -> internal_error ~loc "strange sum arguments" in
      begin match cstr_name with
      | "Left" -> 
          let e = expression env arg in
          ignore (unify e.typ ty1);
          make typ @@ Left (ty2, e)
      | "Right" ->
          let e = expression env arg in
          ignore (unify e.typ ty2);
          make typ @@ Right (ty1, e)
      | s -> internal_error ~loc "strange sum constructor %s" s
      end

  | Tconstr (p, _, _) when p = Predef.path_unit -> 
      make TyUnit Unit

  | Tconstr (p, [], _) when Path.is_scaml_dot "int" p ->
      make TyInt begin
        let arg = match args with
            | [arg] -> arg
            | _ -> internal_error ~loc "strange Int arguments"
          in
          match arg.exp_desc with
            | Texp_constant (Const_int n) -> Const (Int n)
            | _ -> errorf ~loc "Int can only take an integer constant"
        end

  | Tconstr (p, [], _) when Path.is_scaml_dot "nat" p ->
      make TyNat begin 
        let arg = match args with
          | [arg] -> arg
          | _ -> internal_error ~loc "strange Nat arguments"
        in
        match arg.exp_desc with
          | Texp_constant (Const_int n) -> 
              if n < 0 then 
                errorf ~loc "Nat can only take a positive integer constant";
              Const (Nat n)
          | _ -> errorf ~loc "Nat can only take an integer constant"
      end

  | Tconstr (p, _, _) -> failwith (Path.xname p)
  | _ -> prerr_endline cstr_name; assert false

and expression env { exp_desc; exp_loc=loc; exp_type; exp_env; exp_extra=_; exp_attributes=_ } =
  (* wildly ignores extra *)
  (* if exp_extra <> [] then unsupported ~loc "expression extra"; *)
  let typ = type_expr ~loc exp_env exp_type in
  let make desc = { loc; typ; desc } in
  match exp_desc with
  | Texp_ident (Path.Pident id, {loc=_}, _vd) -> 
      begin match List.assoc_opt id env with
        | None -> assert false
        | Some ty ->
            ignore @@ unify typ ty;
            make @@ Var (id, typ)
      end
  | Texp_ident (p, {loc}, _vd) ->
      unsupported ~loc "complex path %s" (Path.xname p)
  | Texp_constant (Const_string (s, None)) -> 
      make @@ Const (String s)
  | Texp_constant _ -> unsupported ~loc "constant"
  | Texp_tuple [e1; e2] ->
      let e1 = expression env e1 in
      let e2 = expression env e2 in
      ignore @@ unify (TyPair (e1.typ, e2.typ)) typ;
      make @@ Tuple (e1, e2)
  | Texp_tuple _ -> unsupported ~loc "tuple with more than 2 elems"
  | Texp_construct ({loc}, c, args) -> 
      construct ~loc env exp_env exp_type c args
  | Texp_assert e ->
      begin match e.exp_desc with
      | Texp_construct (_, {cstr_name="false"}, []) ->
          (* assert false has type 'a *)
          make AssertFalse
      | _ -> 
          make @@ Assert (expression env e)
      end

  | Texp_apply (_, []) -> assert false
  | Texp_apply (f, args) -> 
      let args = List.map (function
          | (Nolabel, Some e) -> expression env e
          | _ -> unsupported ~loc "labeled arguments") args
      in
      let name = match f with
        | { exp_desc= Texp_ident (p, _, _) } ->
            begin match Path.is_scaml p with
            | None -> None
            | Some (s, _) -> Some s
            end
        | _ -> None
      in
      (* only some fixed combinations *)
      begin match name with
        | None -> 
            let f = expression env f in
            let ftype = List.fold_right (fun arg ty -> TyLambda(arg.typ, ty, { closure_desc= CLList [] })) args typ in
            ignore (unify f.typ ftype);
            make @@ App (f, args)

        | Some n -> make @@ primitive ~loc:f.exp_loc n args
      end

  | Texp_function { arg_label= (Labelled _ | Optional _) } ->
      unsupported ~loc "labeled arguments"

  | Texp_function { arg_label= Nolabel; param=_; cases; partial } ->
      if partial = Partial then errorf ~loc "Pattern match is partial";
      let { c_lhs ; c_guard ; c_rhs} = begin match cases with [] -> assert false | [c] -> c | _ -> unsupported ~loc "multi case" end in
      (* Format.eprintf "DEBUG: texp_function %s@." (Ident.name param); (* param is "param" if the pattern is not a simple variable *) *)
      begin match c_guard with Some e -> unsupported ~loc:e.exp_loc "guard" | None -> () end;
      let vs = pattern c_lhs in
      begin match vs with
        | [v] ->
            let ty1 = type_expr ~loc:c_lhs.pat_loc c_lhs.pat_env c_lhs.pat_type in
            let ty2 = type_expr ~loc:c_rhs.exp_loc c_rhs.exp_env c_rhs.exp_type in
            ignore (unify v.typ ty1);
            let env = (v.id,v.typ)::env in
            let e = expression env c_rhs in
            let s = IdTys.(elements @@ remove (v.id, v.typ) (freevars e)) in
            let clinfo = { closure_desc= CLList s } in
            ignore @@ unify typ @@ TyLambda (ty1, ty2, clinfo);
            ignore @@ unify ty2 e.typ;
            make @@ Fun (ty1, ty2, v, e, s)
        | _ -> assert false
      end

  | Texp_ifthenelse (cond, then_, Some else_) -> 
      let econd = expression env cond in
      let ethen = expression env then_ in
      let eelse = expression env else_ in
      ignore (unify ethen.typ eelse.typ);
      ignore (unify typ ethen.typ);
      { loc; typ; desc = IfThenElse (econd, ethen, eelse) }

  | Texp_match (_, _, e::_, _) -> 
      unsupported ~loc:e.c_lhs.pat_loc "exception pattern"
      
  | Texp_match (_ , _, _, Partial) -> 
      unsupported ~loc "non exhaustive pattern match"
      
  | Texp_match (e , cases, [], Total) -> 
      make @@ compile_match ~loc env e cases
      
  | _ -> unsupported ~loc "this type of expression"

and primitive ~loc n args =
  (* They need type unifications! *)
  let arity, ops = match n with
    | "fst"        -> 1, [CAR]
    | "snd"        -> 1, [CDR]
    | "compare"    -> 2, [COMPARE]
    | "="          -> 2, [COMPARE; EQ]
    | "<"          -> 2, [COMPARE; LT]
    | ">"          -> 2, [COMPARE; GT]
    | "<="         -> 2, [COMPARE; LE]
    | ">="         -> 2, [COMPARE; GE]
    | ("+" | "+^") -> 2, [ADD]
    | "-"          -> 2, [SUB]
    | "&&"         -> 2, [AND]
    | _ -> errorf ~loc "Unknown primitive SCaml.%s" n
  in
  if arity > List.length args then
    unsupported ~loc "partial application of primitive (here SCaml.%s)" n;
  Prim (n, ops, args)

and compile_match ~loc:loc0 env e cases =
  let loc = e.exp_loc in
  let ty = type_expr ~loc e.exp_env e.exp_type in
  let compile_case case = 
    match case.c_guard with
    | Some e -> unsupported ~loc:e.exp_loc "guard"
    | None -> 
        match case.c_lhs.pat_desc with
        | Tpat_construct (_, { cstr_name }, xs) -> cstr_name, xs, case.c_rhs
        | _ -> unsupported ~loc:case.c_lhs.pat_loc "non variant pattern"
  in
  let cases = 
    List.sort (fun (n1,_,_) (n2,_,_) -> compare n1 n2) (List.map compile_case cases)
  in
  match ty, cases with
  | TyOr (_ty1, _ty2), [("Left",[l],le); ("Right",[r],re)] ->
      let get_var p = match pattern p with [v] -> v | _ -> assert false in
      let lv = get_var l in
      let rv = get_var r in
      Switch_or (expression env e,
                 lv, expression ((lv.id, lv.typ)::env) le,
                 rv, expression ((rv.id, rv.typ)::env) re)
  | TyOr _, _ -> internal_error ~loc:loc0 "sum pattern match"
  | TyList _ty1, [("::",[l],le); ("[]",[],re)] ->
      let get_var p = match pattern p with [v] -> v | _ -> assert false in
      let lv = get_var l in
      Switch_cons (expression env e,
                   expression env re,
                   lv, expression ((lv.id, lv.typ)::env) le)
  | _, _ -> unsupported ~loc:loc0 "pattern match other than SCaml.sum"

let value_binding env { vb_pat; vb_expr; vb_attributes=_; vb_loc=_loc } = 
  (* currently we only handle very simple sole variable pattern *)
  match pattern vb_pat with
  | [v] ->
      let e = expression env vb_expr in
      ignore @@ unify v.typ e.typ;
      v, e
  | _ -> assert false

(* The condition of the entry point is a bit too strict.
   Currently: the last sitem must be an entry point.
   Better: the last value binding must be an entry point.,
*)
let structure_item env { str_desc; str_loc=loc } =
  match str_desc with
  | Tstr_eval _ -> unsupported ~loc "toplevel evaluation"
  | Tstr_primitive _ -> unsupported ~loc "primitive declaration"
  | Tstr_type _ -> env, []
  | Tstr_typext _ -> unsupported ~loc "type extension"
  | Tstr_exception _ -> unsupported ~loc "exception declaration"
  | Tstr_module _ | Tstr_recmodule _ -> unsupported ~loc "module declaration"
  | Tstr_class _ -> unsupported ~loc "class declaration"
  | Tstr_class_type _ -> unsupported ~loc "class type declaration"
  | Tstr_include _ -> unsupported ~loc "include"
  | Tstr_modtype _ -> unsupported ~loc "module type declaration"

  | Tstr_value (Recursive, _vbs) -> unsupported ~loc "recursive definitions"

  | Tstr_value (Nonrecursive, vbs) ->
      let env, rev_vbs = 
        List.fold_left (fun (env, rev_vbs) vb ->
            let v,b = value_binding env vb in
            (v.id,v.typ)::env, (v,b)::rev_vbs) (env, []) vbs
      in
      env, List.rev rev_vbs

  | Tstr_open _open_description -> env, []

  | Tstr_attribute _ -> 
      (* simply ignores it for now *)
      env, []

let structure env { str_items= sitems } =
  let _env, rev_vbss =
    List.fold_left (fun (env, rev_vbss) sitem ->
        let env, vbs = structure_item env sitem in
        env, vbs :: rev_vbss) (env, []) sitems 
  in
  List.fold_right (fun (v,b) x ->
      { loc=Location.none; typ= TyUnit; desc= Let (v, b, x) })
    (List.flatten (List.rev rev_vbss))
    { loc=Location.none; typ= TyUnit; desc= Unit }

let fix_entrypoint_type sourcefile str =
  let unify ~loc env ty ty' =
    let open Ctype in
    try unify env ty ty' with
    | Unify trace ->
        raise(Typecore.Error(loc, env, Pattern_type_clash(trace)))
    | Tags(l1,l2) ->
        raise(Typetexp.Error(loc, env, Typetexp.Variant_tags (l1, l2)))
  in
  let entry_point { vb_pat; vb_expr=_; vb_attributes=_; vb_loc=loc } = 
    let tenv = vb_pat.pat_env in
    let ty = vb_pat.pat_type in
    let ty_parameter, ty = 
      try
        Ctype.filter_arrow tenv ty Nolabel 
      with
      | Ctype.Unify _ -> errorf ~loc "Entry point must have 2 arguments"
    in
    let ty_storage, _res_ty = 
      try
        Ctype.filter_arrow tenv ty Nolabel
      with
      | Ctype.Unify _ -> errorf ~loc "Entry point must have 2 arguments"
    in
    let ty_operations = 
      let path = 
        Env.lookup_type ~loc 
          (Longident.(Ldot (Lident "SCaml", "operations"))) tenv
      in
      Ctype.newconstr path []
    in
    let ty_fun = 
      Ctype.newty (Tarrow (Nolabel, ty_parameter,
                           Ctype.newty (Tarrow (Nolabel, ty_storage,
                                                Ctype.newty (Ttuple [ty_operations; ty_storage ]), Cok)), Cok)) in
    unify ~loc tenv vb_pat.pat_type ty_fun;
    let ty_parameter = type_expr ~loc tenv ty_parameter in
    let ty_storage = type_expr ~loc tenv ty_storage in
    (ty_parameter, ty_storage)
  in

  (* The condition of the entry point is a bit too strict.
     Currently: the last sitem must be an entry point.
     Better: the last value binding must be an entry point.,
  *)
  let structure_item { str_desc; str_loc=loc } =
    let must_be_entry_point () =
      errorf ~loc "SCaml needs an entry point at the end of module"
    in
    match str_desc with
    | Tstr_value (Recursive, _vbs) -> unsupported ~loc "recursive definitions"
    | Tstr_value (Nonrecursive, vbs) -> 
        begin match List.last vbs with
        | None -> 
            errorf ~loc
              "SCaml needs an entry point at the end of module"
        | Some vb ->
            entry_point vb
        end
    | _ -> must_be_entry_point ()
  in
  let structure sourcefile { str_items= sitems } =
    match List.last sitems with
    | None -> 
        errorf ~loc:(Location.in_file sourcefile)
          "SCaml needs an entry point at the end of module"
    | Some sitem -> structure_item sitem
  in
  structure sourcefile str


