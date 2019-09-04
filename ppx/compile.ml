open Spotlib.Spot
open Asttypes
open Typedtree

module List = struct
  include List

  let rec mark_last = function
    | [] -> []
    | [x] -> [x,true]
    | x::xs -> (x,false)::mark_last xs
end

module Longident = struct
  include Longident

  let rec to_string = function
    | Lident s -> s
    | Ldot (lid, s) -> to_string lid ^ "." ^ s
    | Lapply (t1,t2) -> to_string t1 ^ "(" ^ to_string t2 ^ ")"
end

module Ident = struct
  include Ident

  let is_stdlib i = name i = "Stdlib" && persistent i
  let is_scaml i = name i = "SCaml" && persistent i

  let dummy = Ident.create "_"
end

module Path = struct
  include Path

  let rec xname = function
    | Pident id -> Ident.unique_name id
    | Pdot (t, s, n) -> Printf.sprintf "%s.%s/%d" (xname t) s n
    | Papply (t1,t2) -> xname t1 ^ "(" ^ xname t2 ^ ")"
                        
  let is_stdlib = function
    | Pdot (Pident id, s, n) when Ident.is_stdlib id -> Some (s, n)
    | _ -> None
      
  let is_scaml = function
    | Pdot (Pident id, s, n) when Ident.is_scaml id -> Some (s, n)
    | _ -> None
end

module MicType = struct
  type t = 
    | TyString
    | TyNat
    | TyInt
    | TyBytes
    | TyBool
    | TyUnit
    | TyList of t
    | TyPair of t * t
    | TyOption of t
    | TyOr of t * t
    | TySet of t (* comparable *)
    | TyMap of t (* comparable *) * t
    | TyBigMap of t (* comparable *) * t

    | TyMutez
    | TyKeyHash
    | TyTimestamp
    | TyAddress
                  
    | TyKey
    | TySignature
    | TyOperation
    | TyContract of t
    | TyLambda of t * t

  let rec pp ppf =
    let open Format in
    let p = pp_print_string ppf in
    let f fmt = fprintf ppf fmt in
    function
    | TyString -> p "string"
    | TyNat -> p "nat"
    | TyInt -> p "int"
    | TyBytes -> p "bytes"
    | TyBool -> p "bool"
    | TyUnit -> p "unit"
    | TyList t -> f "list (%a)" pp t
    | TyPair (t1, t2) -> f "pair (%a) (%a)" pp t1 pp t2
    | TyOption t -> f "option (%a)" pp t
    | TyOr (t1, t2) -> f "or (%a) (%a)" pp t1 pp t2
    | TySet t -> f "set (%a)" pp t
    | TyMap (t1, t2) -> f "map (%a) (%a)" pp t1 pp t2
    | TyBigMap (t1, t2) -> f "bigmap (%a) (%a)" pp t1 pp t2

    | TyMutez -> p "mutez"
    | TyKeyHash -> p "key_hash"
    | TyTimestamp -> p "timestamp"
    | TyAddress -> p "address"
                  
    | TyKey -> p "key"
    | TySignature -> p "signature"
    | TyOperation -> p "operation"
    | TyContract t -> f "contract (%a)" pp t
    | TyLambda (t1, t2) -> f "lambda (%a) (%a)" pp t1 pp t2
                  
end
  
module Opcode = struct
  type constant = 
    | True | False
    | Unit

  type t = 
    | DUP
    | DIP of t list
    | DROP
    | SWAP
    | STRING of string
    | PAIR
    | ASSERT
    | CAR | CDR
    | LEFT of MicType.t
    | RIGHT of MicType.t
    | LAMBDA of MicType.t * MicType.t * t list
    | PUSH of MicType.t * constant
    | NIL of MicType.t
    | CONS
    (* | COMMENT of string *)
    
  let pp_constant ppf =
    let open Format in
    let p = pp_print_string ppf in
    function
    | True -> p "True"
    | False -> p "False"
    | Unit -> p "Unit"

  let rec pp ppf =
    let open Format in
    let p = pp_print_string ppf in
    let f fmt = fprintf ppf fmt in
    function
    | DUP -> p "DUP"
    | DIP code -> f "DIP @[<2>{ %a }@]" (Format.list " ;@ " pp) code 
    | SWAP -> p "SWAP"
    | STRING s -> f "%S" s
    | PAIR -> p "PAIR"
    | PUSH (ty, const) -> f "PUSH (%a) (%a)" MicType.pp ty pp_constant const
    | ASSERT -> p "ASSERT"
    | CAR -> p "CAR"
    | CDR -> p "CDR"
    | LEFT ty -> f "LEFT (%a)" MicType.pp ty
    | RIGHT ty -> f "RIGHT (%a)" MicType.pp ty
    | LAMBDA (ty1, ty2, code) -> f "@[<2>LAMBDA (%a) (%a)@ @[<2>{ %a }@]@]" MicType.pp ty1 MicType.pp ty2 (Format.list " ;@ " pp) code
    | CONS -> p "CONS"
    | NIL ty -> f "NIL (%a)" MicType.pp ty
    | DROP -> p "DROP"
end

module Michelson = struct
  type t = { parameter : MicType.t ; storage : MicType.t ; code : Opcode.t list }
           
  let pp ppf { parameter ; storage ; code } =
    Format.fprintf ppf "@[<2>{ parameter (%a) ;@ storage (%a) ;@ code @[<2>{ %a }@] }@]"
      MicType.pp parameter 
      MicType.pp storage
      (Format.list ";@ " Opcode.pp ) code
end

open Opcode

let errorf = Location.raise_errorf
let not_support ~loc fmt = Printf.ksprintf (fun s -> errorf ~loc "SCaml does not support %s" s) fmt
let internal_error ~loc fmt = Printf.ksprintf (fun s -> errorf ~loc "SCaml internal error: %s" s) fmt

let constant ~loc = function
  | Const_string (s, None) -> [STRING s]
  | Const_string (_, _) -> not_support ~loc "quoted string"
  | Const_int _ -> not_support ~loc "int"
  | Const_char _ -> not_support ~loc "char"
  | Const_float _ -> not_support ~loc "float"
  | Const_int32 _ -> not_support ~loc "int32"
  | Const_int64 _ -> not_support ~loc "int64"
  | Const_nativeint _ -> not_support ~loc "nativeint"

let find_env id env = 
  let rec aux n = function
    | [] -> None
    | id'::env when id = id' -> Some n
    | _::env -> aux (n+1) env
  in
  aux 0 env


type type_expr_error =
  | Type_variable of Types.type_expr
  | Unsupported_type of Types.type_expr

let rec type_expr env ty = 
  let open MicType in
  let open Result.Infix in
  let ty = Ctype.expand_head env ty in
  match ty.desc with
  | Tvar _ -> Error (Type_variable ty)
  | Tarrow (Nolabel, f, t, _) -> 
      type_expr env f >>= fun f ->
      type_expr env t >>= fun t -> Ok (TyLambda (f, t))
  | Ttuple [t1; t2] -> 
      type_expr env t1 >>= fun t1 ->
      type_expr env t2 >>= fun t2 -> Ok (TyPair (t1, t2))
  | Tconstr (p, [], _) when p = Predef.path_bool -> Ok TyBool
  | Tconstr (p, [t], _) when p = Predef.path_list -> 
      type_expr env t >>= fun t ->Ok (TyList t)
  | Tconstr (p, [t1; t2], _) when (match Path.is_scaml p with Some ("sum", _) -> true | _ -> false) ->
      type_expr env t1 >>= fun t1 ->
      type_expr env t2 >>= fun t2 -> Ok (TyOr (t1, t2))
  | Tconstr (p, [], _) when (match Path.is_scaml p with Some ("operation", _) -> true | _ -> false) ->
      Ok TyOperation
  | Tconstr (p, [], _) when p = Predef.path_unit -> Ok TyUnit
  | _ -> Error (Unsupported_type ty)

(*

  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr         (* for copying *)
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * Longident.t list * type_expr list
*)

let type_expr ~loc env ty = 
  match type_expr env ty with
  | Ok x -> x
  | Error (Type_variable ty') -> 
      errorf ~loc "This expression has type %a, which has too generic type %a"
        Printtyp.type_expr ty
        Printtyp.type_expr ty'
  | Error (Unsupported_type ty') ->
      errorf ~loc "This expression has type %a, which has unsupported type %a"
        Printtyp.type_expr ty
        Printtyp.type_expr ty'
  
let pattern { pat_desc; pat_loc=loc } = match pat_desc with
  | Tpat_var (id, {loc; }) -> [id]
  | Tpat_any -> not_support ~loc "any pattern"

  (* We transform (_ as x) in x if _ and x have the same location.
     The compiler transforms (x:t) into (_ as x : t).
     This avoids transforming a warning 27 into a 26.
   *)
  | Tpat_alias ({pat_desc = Tpat_any; pat_loc=_}, id, _name) -> [id]

  | Tpat_alias _ -> not_support ~loc "alias pattern"
  | Tpat_constant _ -> not_support ~loc "constant pattern"
  | Tpat_tuple _ -> not_support ~loc "tuple pattern"
  | Tpat_construct _ -> not_support ~loc "variant pattern"
  | Tpat_variant _ -> not_support ~loc "polymorphic variant pattern"
  | Tpat_record _ -> not_support ~loc "record pattern"
  | Tpat_array _ -> not_support ~loc "array pattern"
  | Tpat_or _ -> not_support ~loc "or pattern"
  | Tpat_lazy _ -> not_support ~loc "lazy pattern"

let rec construct ~loc env tenv exp_type {Types.cstr_name; cstr_res} args =
  match (Ctype.expand_head tenv exp_type).Types.desc with

  (* bool *)
  | Tconstr (p, [], _) when p = Predef.path_bool ->
      let ty = type_expr ~loc tenv exp_type in
      begin match cstr_name with
        | "true" -> [PUSH (ty, True)]
        | "false" -> [PUSH (ty, False)]
        | s -> internal_error ~loc "strange bool constructor %s" s
      end

  (* list *)
  | Tconstr (p, [ty], _) when p = Predef.path_list ->
      begin match cstr_name with
        | "[]" -> 
            let ty = type_expr ~loc tenv ty in
            [NIL ty]
        | "::" ->
            begin match args with
              | [e1; e2] ->
                  let o2 = expression env e2 in
                  let o1 = expression (Ident.dummy :: env) e1 in
                  o2 @ o1 @ [CONS]
              | _ -> internal_error ~loc "strange cons"
            end
        | s -> internal_error ~loc "strange list constructor %s" s
      end

  (* sum *)
  | Tconstr (p, [_; _], _) when (match Path.is_scaml p with Some ("sum", _) -> true | _ -> false) ->
      let ty1, ty2 = 
        match 
          (Ctype.repr exp_type).desc 
        with
        | Tconstr (_, [ty1; ty2], _) -> (ty1, ty2)
        | _ -> assert false
      in
      let arg = match args with [arg] -> arg | _ -> internal_error ~loc "strange sum arguments" in
      begin match cstr_name with
        | "Left" -> 
            let ty2 = type_expr ~loc tenv ty2 in
            let o = expression env arg in o @ [LEFT ty2]
        | "Right" ->
            let ty1 = type_expr ~loc tenv ty1 in
            let o = expression env arg in o @ [RIGHT ty1]
        | s -> internal_error ~loc "strange sum constructor %s" s
      end
      
  | Tconstr (p, _, _) when p = Predef.path_unit -> [PUSH (MicType.TyUnit, Unit)]
      
  | Tconstr (p, _, _) -> failwith (Path.xname p)
  | _ -> prerr_endline cstr_name; assert false

and expression env { exp_desc; exp_loc=loc; exp_type; exp_env; exp_extra; exp_attributes=_ } =
  let open Result.Infix in
  (* wildly ignores extra *)
  (* if exp_extra <> [] then not_support ~loc "expression extra"; *)
  match exp_desc with
  | Texp_ident (Path.Pident id, {loc}, _vd) ->
      (* y1, x, y3, ... => x, y1, x, y3
         DIP { DUP }; SWAP => x, y1, x, y3

         y1, y2, x, y4, ... => x, y1, y2, x, y3
         DIP { DIP { DUP };  SWAP }; SWAP

         y1, y2, .., yn, x, yn+2, ... => x, y1, y2, .., yn, x, x, yn+2, ..
         DIP { ... } ; SWAP
               DIP { ... } ; SWAP
                    DIP DUP
      *)
      begin match find_env id env with
        | None -> internal_error ~loc "variable not found: %s in [ %s ]" 
                    (Ident.unique_name id)
                    (String.concat "; " (List.map (fun id -> Ident.unique_name id) env))
        | Some n -> 
            let rec f = function
              | 0 -> [ DUP ]
              | n -> 
                  assert (n > 0);
                  [ DIP (f (n-1)); SWAP ]
            in
            f n
      end
  | Texp_ident (p, {loc}, _vd) ->
      not_support ~loc "complex path %s" (Path.xname p)
  | Texp_constant const -> constant ~loc const

  | Texp_tuple [e1; e2] ->
      let o2 = expression env e2 in
      let o1 = expression (Ident.dummy :: env) e1 in
      o2 @ o1 @ [PAIR]
      
  | Texp_tuple _ -> not_support ~loc "tuple with more than 2 elems"

  | Texp_construct ({loc; txt}, c, args) -> construct ~loc env exp_env exp_type c args

  | Texp_assert e ->
      let o = expression env e in
      o @ [ ASSERT ]

  (* Nolabel *)

  | Texp_apply (f, []) -> assert false
  | Texp_apply (f, args) -> 
      let args = List.map (function
          | (Nolabel, Some e) -> e
          | _ -> not_support ~loc "labeled arguments") args
      in
      let stdlib = match f with
        | { exp_desc= Texp_ident (p, _, _) } ->
            begin match Path.is_stdlib p with
            | None -> None
            | Some (s, _) -> Some s
            end
        | _ -> None
      in
      begin match stdlib, args with
        | Some "fst", [arg] ->
            let o = expression env arg in
            o @ [CAR]
        | Some "snd", [arg] ->
            let o = expression env arg in
            o @ [CDR]
        | _ -> not_support ~loc "application"
      end

  | Texp_function { arg_label= (Labelled _ | Optional _) } ->
      not_support ~loc "labeled arguments"

  | Texp_function { arg_label= Nolabel; param; cases; partial } ->
      if partial = Partial then errorf ~loc "Pattern match is partial";
      let { c_lhs ; c_guard ; c_rhs} = begin match cases with [] -> assert false | [c] -> c | _ -> not_support ~loc "multi case" end in
      (* Format.eprintf "DEBUG: texp_function %s@." (Ident.name param); (* param is "param" if the pattern is not a simple variable *) *)
      begin match c_guard with Some e -> not_support ~loc:e.exp_loc "guard" | None -> () end;
      let vs = pattern c_lhs in
      let ty1 = type_expr ~loc:c_lhs.pat_loc c_lhs.pat_env c_lhs.pat_type in
      let ty2 = type_expr ~loc:c_rhs.exp_loc c_rhs.exp_env c_rhs.exp_type in
      let code = expression (vs@env) c_rhs (* XXX order of vs ? rev? *) in
      [LAMBDA (ty1, ty2, code @ [ DIP [ DROP ] ] )]

  | _ -> not_support ~loc "this type of expression"

(*
  | Texp_let of rec_flag * value_binding list * expression
        (** let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
            let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Texp_function of { arg_label : arg_label; param : Ident.t;
      cases : case list; partial : partial; }
        (** [Pexp_fun] and [Pexp_function] both translate to [Texp_function].
            See {!Parsetree} for more details.

            [param] is the identifier that is to be used to name the
            parameter of the function.

            partial =
              [Partial] if the pattern match is partial
              [Total] otherwise.
         *)
  | Texp_apply of expression * (arg_label * expression option) list
        (** E0 ~l1:E1 ... ~ln:En

            The expression can be None if the expression is abstracted over
            this argument. It currently appears when a label is applied.

            For example:
            let f x ~y = x + y in
            f ~y:3

            The resulting typedtree for the application is:
            Texp_apply (Texp_ident "f/1037",
                        [(Nolabel, None);
                         (Labelled "y", Some (Texp_constant Const_int 3))
                        ])
         *)
  | Texp_match of expression * case list * case list * partial
        (** match E0 with
            | P1 -> E1
            | P2 -> E2
            | exception P3 -> E3

            [Texp_match (E0, [(P1, E1); (P2, E2)], [(P3, E3)], _)]
         *)
  | Texp_try of expression * case list
        (** try E with P1 -> E1 | ... | PN -> EN *)
  | Texp_tuple of expression list
        (** (E1, ..., EN) *)
  | Texp_construct of
      Longident.t loc * constructor_description * expression list
        (** C                []
            C E              [E]
            C (E1, ..., En)  [E1;...;En]
         *)
  | Texp_variant of label * expression option
  | Texp_record of {
      fields : ( Types.label_description * record_label_definition ) array;
      representation : Types.record_representation;
      extended_expression : expression option;
    }
        (** { l1=P1; ...; ln=Pn }           (extended_expression = None)
            { E0 with l1=P1; ...; ln=Pn }   (extended_expression = Some E0)

            Invariant: n > 0

            If the type is { l1: t1; l2: t2 }, the expression
            { E0 with t2=P2 } is represented as
            Texp_record
              { fields = [| l1, Kept t1; l2 Override P2 |]; representation;
                extended_expression = Some E0 }
        *)
  | Texp_field of expression * Longident.t loc * label_description
  | Texp_setfield of
      expression * Longident.t loc * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * Parsetree.pattern * expression * expression * direction_flag *
        expression
  | Texp_send of expression * meth * expression option
  | Texp_new of Path.t * Longident.t loc * Types.class_declaration
  | Texp_instvar of Path.t * Path.t * string loc
  | Texp_setinstvar of Path.t * Path.t * string loc * expression
  | Texp_override of Path.t * (Path.t * string loc * expression) list
  | Texp_letmodule of Ident.t * string loc * module_expr * expression
  | Texp_letexception of extension_constructor * expression
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
  | Texp_unreachable
  | Texp_extension_constructor of Longident.t loc * Path.t
*)

let value_binding env { vb_pat; vb_expr; vb_attributes=_; vb_loc=loc } = 
  (* currently we only handle very simple sole variable pattern *)
  match pattern vb_pat with
  | [v] ->
      let os = expression env vb_expr in
      v :: env, (* COMMENT (Ident.name v) :: *) os
  | _ -> assert false

let unify ~loc env ty ty' =
  try
    Ctype.unify env ty ty'
  with
    Ctype.Unify trace ->
      raise(Typecore.Error(loc, env, Pattern_type_clash(trace)))
  | Ctype.Tags(l1,l2) ->
      raise(Typetexp.Error(loc, env, Typetexp.Variant_tags (l1, l2)))

(* XXX what happens a function is used both for an entry point and a normal function ? *)
let entry_point env ({ vb_pat; vb_expr; vb_attributes=_; vb_loc=loc } as vb) = 
  let tenv = vb_pat.pat_env in
  let ty = vb_pat.pat_type in
  let ty_parameter, ty = 
    try
      Ctype.filter_arrow tenv ty Nolabel 
    with
    | Ctype.Unify _ -> errorf ~loc "Entry point must have 2 arguments"
  in
  let ty_storage, res_ty = 
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
  let env, os = value_binding env vb in
  let os =
    (* parameters exist at the end of the stack *)
    let rec f = function
      (*      | (COMMENT _  as c) :: os -> c :: f os *)
      | [ LAMBDA (_, _, [ LAMBDA (_, _, code); DIP [ DROP ] ]) ] ->
(*
          Format.eprintf "Got %a@." (Format.list " ;@ " Opcode.pp) os;
*)
          let rec f = function
            | [ DIP [ DROP ]] -> []
            | [] -> assert false
            | x::xs -> x :: f xs
          in
          f code
      | _ -> assert false
    in
    f os
  in
  let ty_parameter = type_expr ~loc tenv ty_parameter in
  let ty_storage = type_expr ~loc tenv ty_storage in
  (ty_parameter, ty_storage, os)

let type_ _loc _env _rf decls =
  List.iter (fun decl ->
      let loc = decl.typ_loc in
      match decl.typ_kind, decl.typ_manifest with
      | Ttype_abstract, Some _ -> ()
      | Ttype_abstract, None -> not_support ~loc "abstract type declaration"
      | Ttype_variant _, _ -> not_support ~loc "varinat type declaration"
      | Ttype_record _, _ -> not_support ~loc "record type declaration"
      | Ttype_open, _ ->  not_support ~loc "open type declaration") decls

(* The condition of the entry point is a bit too strict.
   Currently: the last sitem must be an entry point.
   Better: the last value binding must be an entry point.,
*)
let structure_item ~is_entry_point env { str_desc; str_loc=loc; str_env } =
  let must_be_entry_point () =
    errorf ~loc "SCaml needs an entry point at the end of module"
  in
  match str_desc with
  | Tstr_eval _ -> not_support ~loc "toplevel evaluation"
  | Tstr_primitive _ -> not_support ~loc "primitive declaration"
  | Tstr_type _ when is_entry_point -> must_be_entry_point ()
  | Tstr_type (rf, decls) -> 
      type_ loc env rf decls; 
      env, []
  | Tstr_typext _ -> not_support ~loc "type extension"
  | Tstr_exception _ -> not_support ~loc "exception declaration"
  | Tstr_module _ | Tstr_recmodule _ -> not_support ~loc "module declaration"
  | Tstr_class _ -> not_support ~loc "class declaration"
  | Tstr_class_type _ -> not_support ~loc "class type declaration"
  | Tstr_include _ -> not_support ~loc "include"
  | Tstr_modtype _ -> not_support ~loc "module type declaration"
                       
  | Tstr_value (Recursive, _vbs) -> not_support ~loc "recursive definitions"

  | Tstr_value (Nonrecursive, vbs) -> 
      let vbs = List.mark_last vbs in
      let env, rev_res = 
        List.fold_left (fun (env, rev_res) (vb, is_last) -> 
            if is_entry_point && is_last then begin
              env (* it has an addtional element at the top*), (`EntryPoint (entry_point env vb)) ::rev_res
            end else 
              let env, res = value_binding env vb in
              env, (`Value res)::rev_res) (env, []) vbs 
      in
      env, List.rev rev_res
        
  | Tstr_open _ when is_entry_point -> must_be_entry_point ()
  | Tstr_open _open_description -> env, []

  | Tstr_attribute _ when is_entry_point -> must_be_entry_point ()
  | Tstr_attribute _ -> 
      (* simply ignores it for now *)
      env, []

let structure ~with_entry_point env { str_items= sitems } =
  let sitems = List.mark_last sitems in
  let env, rev_res = 
    List.fold_left (fun (env, rev_res) (sitem, is_last) -> 
        let env, res =
          structure_item ~is_entry_point:(is_last && with_entry_point) env sitem 
        in
        env, res :: rev_res) (env, []) sitems 
  in
  env, List.flatten @@ List.rev rev_res
  
let implementation sourcefile outputprefix modulename (str, _coercion) =
  Format.eprintf "sourcefile=%s outputprefix=%s modulename=%s@." sourcefile outputprefix modulename;
  let env, xs = structure ~with_entry_point:true [] str in
  let vals, entry = 
    List.partition_map (function
        | `Value x -> `Left x
        | `EntryPoint y -> `Right y) xs
  in
  match entry with
  | [] -> errorf ~loc:(Location.in_file sourcefile) "SCaml needs an entry point";
  | _::_::_ -> internal_error ~loc:(Location.in_file sourcefile) "Multiple entry points?"
  | [(parameter, storage, entry)] ->
      let code = [ DIP (List.concat vals) ] @ [ DUP ; CDR ; DIP [ CAR ] ] @ entry @ 
                 [ DIP (List.init (List.length env + 2) (fun _ -> DROP) ) ] in
      let m = { Michelson.parameter;
                storage;
                code } in
      Format.eprintf "%a@." Michelson.pp m;
      let oc = open_out (outputprefix ^ ".tz") in
      let ppf = Format.of_out_channel oc in
      Format.fprintf ppf "%a@." Michelson.pp m;
      close_out oc
