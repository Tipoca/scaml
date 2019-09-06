open Spotlib.Spot
open Asttypes
open Typedtree
open Tools

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
    | TyLambda of t * t * closure_info

  and closure_info = { mutable closure_desc : closure_desc }
  and closure_desc = 
    | CLList of (Ident.t * t) list
    | CLLink of closure_info

  let rec repr_closure_info ({ closure_desc } as i) = 
    match closure_desc with
    | CLList _ -> i
    | CLLink cl -> repr_closure_info cl
    
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
    | TyLambda (t1, t2, cli) -> 
        f "@[<2>lambda (%a%a) (%a)@]" 
          pp t1 
          (fun ppf cli ->
             match (repr_closure_info cli).closure_desc with
             | CLLink _ -> assert false
             | CLList [] -> ()
             | CLList xs -> 
                 f " /* %a */"
                   Format.(list ";@ " (fun ppf (id, ty) ->
                       fprintf ppf "%s : %a" (Ident.name id) pp ty)) xs) cli
          pp t2

  let rec merge env1 env2 =
    let ids = List.map fst env1 @ List.map fst env2 in
    let ids = List.sort_uniq compare ids in
    List.map (fun id ->
        (id,
         match List.assoc_opt id env1, List.assoc_opt id env2 with
         | None, None -> assert false
         | Some ty, None -> ty
         | None, Some ty -> ty
         | Some ty1, Some ty2 -> unify ty1 ty2)) ids

  and unify t1 t2 = 
    if t1 == t2 then t1
    else match t1, t2 with
      | TyList t1, TyList t2 -> TyList (unify t1 t2)
      | TyPair (t11, t12), TyPair (t21, t22) ->
          TyPair (unify t11 t21, unify t12 t22)
      | TyOption t1, TyOption t2 -> TyOption (unify t1 t2)
      | TyOr (t11, t12), TyOr (t21, t22) ->
          TyOr (unify t11 t21, unify t21 t22)
      | TySet t1, TySet t2 -> TySet (unify t1 t2)
      | TyMap (t11, t12), TyMap (t21, t22) ->
          TyMap (unify t11 t21, unify t21 t22)
      | TyBigMap (t11, t12), TyBigMap (t21, t22) ->
          TyBigMap (unify t11 t21, unify t21 t22)
      | TyContract t1, TyContract t2 ->  TyContract (unify t1 t2)
      | TyLambda (t11, t12, cli1), TyLambda (t21, t22, cli2) ->
          let cli1 = repr_closure_info cli1 in
          let cli2 = repr_closure_info cli2 in
          let get_list x = match x.closure_desc with CLList xs -> xs | _ -> assert false in
          let env = merge (get_list cli1) (get_list cli2) in
          let newcli = { closure_desc= CLList env } in
          cli1.closure_desc <- CLLink newcli;
          cli2.closure_desc <- CLLink newcli;
          TyLambda (unify t11 t21, unify t12 t22, cli1)
      | _ -> 
          Format.eprintf "Unifying %a and %a!!@." pp t1 pp t2;
          assert false

end

open MicType
  
module Opcode = struct
  type constant = 
    | True | False
    | Unit
    | Int of int
    | Nat of int
    | String of string

  type t = 
    | DUP
    | DIP of t list
    | DROP
    | SWAP
    | PAIR
    | ASSERT
    | CAR | CDR
    | LEFT of MicType.t
    | RIGHT of MicType.t
    | LAMBDA of MicType.t * MicType.t * t list
    | PUSH of MicType.t * constant
    | NIL of MicType.t
    | CONS
    | NONE of MicType.t
    | SOME
    | COMPARE
    | EQ | LT | LE | GT | GE
    | IF of t list * t list
    | ADD
    | SUB
    | AND
    | EXEC
    | IF_SOME of t list * t list
    | FAIL
    | COMMENT of string * t list
    
  let pp_constant ppf =
    let open Format in
    let p = pp_print_string ppf in
    let f fmt = fprintf ppf fmt in
    function
    | True -> p "True"
    | False -> p "False"
    | Unit -> p "Unit"
    | Int n -> f "%d" n
    | Nat n -> f "%d" n
    | String s -> f "%S" s

  let rec pp ppf =
    let open Format in
    let p = pp_print_string ppf in
    let f fmt = fprintf ppf fmt in
    function
    | DUP -> p "DUP"
    | DIP code -> f "DIP @[<2>{ %a }@]" (Format.list " ;@ " pp) code 
    | SWAP -> p "SWAP"
    | PAIR -> p "PAIR"
    | PUSH (ty, const) -> f "PUSH (%a) %a" MicType.pp ty pp_constant const
    | ASSERT -> p "ASSERT"
    | CAR -> p "CAR"
    | CDR -> p "CDR"
    | LEFT ty -> f "LEFT (%a)" MicType.pp ty
    | RIGHT ty -> f "RIGHT (%a)" MicType.pp ty
    | LAMBDA (ty1, ty2, code) -> f "@[<2>LAMBDA @[(%a) (%a)@ @[<2>{ %a }@]@]@]" MicType.pp ty1 MicType.pp ty2 (Format.list " ;@ " pp) code
    | CONS -> p "CONS"
    | NIL ty -> f "NIL (%a)" MicType.pp ty
    | SOME -> p "SOME"
    | NONE ty -> f "NONE (%a)" MicType.pp ty
    | DROP -> p "DROP"
    | COMPARE -> p "COMPARE"
    | EQ -> p "EQ"
    | LT -> p "LT"
    | LE -> p "LE"
    | GT -> p "GT"
    | GE -> p "GE"
    | IF (t,e) -> f "IF @[<0>{ @[%a@] }@ { @[%a@] }@]" 
                    (Format.list " ;@ " pp) t
                    (Format.list " ;@ " pp) e

    | IF_SOME (t,e) -> f "IF_SOME @[<0>{ @[%a@] }@ { @[%a@] }@]" 
                         (Format.list " ;@ " pp) t
                         (Format.list " ;@ " pp) e
    | ADD -> p "ADD"
    | SUB -> p "SUB"
    | AND -> p "AND"
    | EXEC -> p "EXEC"
    | FAIL -> p "FAIL"
    | COMMENT (s, ts) ->
        f "{ @[/* %s */@ @[%a@]@] }" s (Format.list " ;@ " pp) ts
          
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
  | Const_string (s, None) -> [PUSH (TyString, String s)]
  | Const_string (_, _) -> not_support ~loc "quoted string"
  | Const_int _ -> not_support ~loc "int"
  | Const_char _ -> not_support ~loc "char"
  | Const_float _ -> not_support ~loc "float"
  | Const_int32 _ -> not_support ~loc "int32"
  | Const_int64 _ -> not_support ~loc "int64"
  | Const_nativeint _ -> not_support ~loc "nativeint"

module MEnv : sig
  type t = (Ident.t * MicType.t) list
  val find : Ident.t -> t -> (int * MicType.t) option
  val add : (Ident.t * MicType.t) -> t -> t
  val add_list : (Ident.t * MicType.t) list -> t -> t
  val empty : t
  val pp : Format.formatter -> t -> unit
  val of_list : (Ident.t * MicType.t) list -> t
  val length : t -> int
  val singleton : (Ident.t * MicType.t) -> t
end = struct
  type t = (Ident.t * MicType.t) list

  let singleton x = [x]

  let find id env = 
    let rec aux n = function
      | [] -> None
      | (id',ty)::env when id = id' -> Some (n,ty)
      | _::env -> aux (n+1) env
    in
    aux 0 env

  let add x xs = x :: xs
  let add_list = (@)

  let empty = []

  let pp ppf t =
    Format.fprintf ppf "@[<2>[ %a ]@]"
      (Format.list ";@ "
         (fun ppf (id, ty) ->
            Format.fprintf ppf 
              "%s : %a"
              (Ident.unique_name id)
              MicType.pp ty)) t

  let of_list xs = xs
    
  let length = List.length
end

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

type pat = 
  { ploc : Location.t
  ; id : Ident.t
  ; typ : MicType.t
  }

let pattern { pat_desc; pat_loc=loc; pat_type; pat_env } = match pat_desc with
  | Tpat_var (id, {loc}) ->
      let ty = type_expr ~loc pat_env pat_type in
      { ploc = loc; id; typ= ty }

  | Tpat_alias ({pat_desc = Tpat_any; pat_loc=_}, id, _) -> 
      (* We transform (_ as x) in x if _ and x have the same location.
         The compiler transforms (x:t) into (_ as x : t).
         This avoids transforming a warning 27 into a 26.
       *)
      let ty = type_expr ~loc pat_env pat_type in
      { ploc = loc; id; typ= ty }

  | Tpat_any -> not_support ~loc "any pattern"

  | Tpat_alias _ -> not_support ~loc "alias pattern"
  | Tpat_constant _ -> not_support ~loc "constant pattern"
  | Tpat_tuple _ -> not_support ~loc "tuple pattern"
  | Tpat_construct _ -> not_support ~loc "variant pattern"
  | Tpat_variant _ -> not_support ~loc "polymorphic variant pattern"
  | Tpat_record _ -> not_support ~loc "record pattern"
  | Tpat_array _ -> not_support ~loc "array pattern"
  | Tpat_or _ -> not_support ~loc "or pattern"
  | Tpat_lazy _ -> not_support ~loc "lazy pattern"

(*
let rec construct ~loc env xenv tenv exp_type {Types.cstr_name; cstr_res} args =
  match (Ctype.expand_head tenv exp_type).Types.desc with

  (* bool *)
  | Tconstr (p, [], _) when p = Predef.path_bool ->
      let ty = type_expr ~loc tenv exp_type in
      begin match cstr_name with
        | "true" -> [PUSH (ty, True)], xenv
        | "false" -> [PUSH (ty, False)], xenv
        | s -> internal_error ~loc "strange bool constructor %s" s
      end

  (* list *)
  | Tconstr (p, [ty], _) when p = Predef.path_list ->
      begin match cstr_name with
        | "[]" -> 
            let ty = type_expr ~loc tenv ty in
            [NIL ty], xenv
        | "::" ->
            begin match args with
              | [e1; e2] ->
                  let o2, xenv = expression env xenv e2 in
                  let o1, xenv = expression (MEnv.add (Ident.dummy, TyUnit) env) xenv e1 in
                  (o2 @ o1 @ [CONS]), xenv
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
            let o, xenv = expression env xenv arg in
            (o @ [LEFT ty2]), xenv
        | "Right" ->
            let ty1 = type_expr ~loc tenv ty1 in
            let o, xenv = expression env xenv arg in
            (o @ [RIGHT ty1]), xenv
        | s -> internal_error ~loc "strange sum constructor %s" s
      end
      
  | Tconstr (p, _, _) when p = Predef.path_unit -> 
      [PUSH (TyUnit, Unit)], xenv
      
  | Tconstr (p, [], _) when Path.is_scaml_dot "int" p ->
      let arg = match args with
        | [arg] -> arg
        | _ -> internal_error ~loc "strange Int arguments"
      in
      begin match arg.exp_desc with
        | Texp_constant (Const_int n) -> 
            [PUSH (TyInt, Opcode.Int n)], xenv
        | _ -> errorf ~loc "Int can only take an integer constant"
      end

  | Tconstr (p, [], _) when Path.is_scaml_dot "nat" p ->
      let arg = match args with
        | [arg] -> arg
        | _ -> internal_error ~loc "strange Nat arguments"
      in
      begin match arg.exp_desc with
        | Texp_constant (Const_int n) -> 
            if n < 0 then 
              errorf ~loc "Nat can only take a positive integer constant";
            [PUSH (TyNat, Opcode.Nat n)], xenv 
        | _ -> errorf ~loc "Nat can only take an integer constant"
      end

  | Tconstr (p, _, _) -> failwith (Path.xname p)
  | _ -> prerr_endline cstr_name; assert false

and expression ?(lambda=true) env xenv { exp_desc; exp_loc=loc; exp_type; exp_env; exp_extra; exp_attributes=_ } =
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
      begin match MEnv.find id (env @ xenv) with
        | None -> 
(*
            (* variable in a closure, referring out of the body *)
            internal_error ~loc "variable not found: %s in %s" 
              (Ident.unique_name id)
              (Format.sprintf "%a" MEnv.pp env)
*)
            let n = List.length env + List.length xenv in
            let xenv = xenv @ [id, type_expr ~loc exp_env exp_type] in (* XXX *)
            let rec f = function
              | 0 -> [ DUP ]
              | n -> 
                  assert (n > 0);
                  [ DIP (f (n-1)); SWAP ]
            in
            f n, xenv
            
        | Some (n, _ty) -> 
            let rec f = function
              | 0 -> [ DUP ]
              | n -> 
                  assert (n > 0);
                  [ DIP (f (n-1)); SWAP ]
            in
            f n, xenv
      end
  | Texp_ident (p, {loc}, _vd) ->
      not_support ~loc "complex path %s" (Path.xname p)
  | Texp_constant const -> constant ~loc const, xenv

  | Texp_tuple [e1; e2] ->
      let o2, xenv = expression env xenv e2 in
      let o1, xenv = expression (MEnv.add (Ident.dummy, TyUnit) env) xenv e1 in
      (o2 @ o1 @ [PAIR]), xenv
      
  | Texp_tuple _ -> not_support ~loc "tuple with more than 2 elems"

  | Texp_construct ({loc; txt}, c, args) -> 
      construct ~loc env xenv exp_env exp_type c args

  | Texp_assert e ->
      let o, xenv = expression env xenv e in
      (o @ [ ASSERT ; PUSH (TyUnit, Unit)]), xenv

  (* Nolabel *)

  | Texp_apply (f, []) -> assert false
  | Texp_apply (f, args) -> 
      let args = List.map (function
          | (Nolabel, Some e) -> e
          | _ -> not_support ~loc "labeled arguments") args
      in
      let name = match f with
        | { exp_desc= Texp_ident (p, _, _) } ->
            begin match Path.is_scaml p with
            | None -> prerr_endline (Path.xname p); None
            | Some (s, _) -> Some s
            end
        | _ -> None
      in
      (* only some fixed combinations *)
      begin match name with
        | None -> application env xenv f args
        | Some n -> primitive ~loc:f.exp_loc env xenv n args
      end

  | Texp_function { arg_label= (Labelled _ | Optional _) } ->
      not_support ~loc "labeled arguments"

  | Texp_function { arg_label= Nolabel; param; cases; partial } ->
      if partial = Partial then errorf ~loc "Pattern match is partial";
      let { c_lhs ; c_guard ; c_rhs} = begin match cases with [] -> assert false | [c] -> c | _ -> not_support ~loc "multi case" end in
      (* Format.eprintf "DEBUG: texp_function %s@." (Ident.name param); (* param is "param" if the pattern is not a simple variable *) *)
      begin match c_guard with Some e -> not_support ~loc:e.exp_loc "guard" | None -> () end;
      let v = pattern c_lhs in
      let ty1 = type_expr ~loc:c_lhs.pat_loc c_lhs.pat_env c_lhs.pat_type in
      let ty2 = type_expr ~loc:c_rhs.exp_loc c_rhs.exp_env c_rhs.exp_type in
      if lambda then begin
        (* LAMBDA is not the real lambda! *)
        let code, xenv = expression (MEnv.singleton (v.id, v.typ)) [] (* xxx (vs@env) *) c_rhs in
        Format.eprintf "Closure conversion: freevars: %a@." MEnv.pp xenv;
        begin match xenv with
          | [] -> [LAMBDA (ty1, ty2, code @ [ DIP [ DROP ] ] )], xenv
          | [(_,aty1)] ->
              [LAMBDA (TyPair (ty1, aty1), ty2,
                       [ DUP; CAR; DIP [ CDR ] ]
                       @ code @ [ DIP [ DROP ; DROP ] ] )], xenv
          | _ -> assert false
        end
      end else begin
        expression ~lambda:false (MEnv.add (v.id, v.typ) env) xenv c_rhs
      end
      
  | Texp_ifthenelse (cond, then_, Some else_) -> 
      let o, xenv = expression env xenv cond in
      let othen, xenv = expression env xenv then_ in
      let oelse, xenv = expression env xenv else_ in
      (o @ [IF (othen, oelse)]), xenv

  | _ -> not_support ~loc "this type of expression"

and application env xenv f args =
  let os, xenv = expression env xenv f in
  let env = MEnv.add (Ident.dummy, TyUnit) env in
  let rec aux (acc, xenv) = function
    | [] -> acc, xenv
    | arg::args ->
        let oa, xenv = expression env xenv arg in
        aux (acc @ oa @ [ EXEC ], xenv) args
  in
  aux (os, xenv) args 

and primitive ~loc env xenv n args =
  let _, xenv, os =
    List.fold_right (fun a (env, xenv, os) ->
        let os', xenv = expression env xenv a in
        MEnv.add (Ident.dummy, TyUnit) env, xenv, os @ os') args (env, xenv, [])
  in
  (os @ match n with
    | "fst" -> [CAR]
    | "snd" -> [CDR]
    | "compare" -> [COMPARE]
    | "=" -> [COMPARE; EQ]
    | "<" -> [COMPARE; LT]
    | ">" -> [COMPARE; GT]
    | "<=" -> [COMPARE; LE]
    | ">=" -> [COMPARE; GE]
    | ("+" | "+^") -> [ADD]
    | "-" -> [SUB]
    | "&&" -> [AND]
    | _ -> errorf ~loc "Unknown primitive SCaml.%s" n),
  xenv
*)

module IdTys = Set.Make(struct type t = Ident.t * MicType.t let compare (id1,_) (id2,_) = compare id1 id2 end)

module X = struct

  type pat = 
    { ploc : Location.t
    ; id : Ident.t
    ; typ : MicType.t
    }

  let pattern { pat_desc; pat_loc=loc; pat_type; pat_env } = 
    match pat_desc with
    | Tpat_var (id, {loc}) ->
        let ty = type_expr ~loc pat_env pat_type in
        [{ ploc= loc; id; typ= ty }]

    | Tpat_alias ({pat_desc = Tpat_any; pat_loc=_}, id, _) -> 
        (* We transform (_ as x) in x if _ and x have the same location.
           The compiler transforms (x:t) into (_ as x : t).
           This avoids transforming a warning 27 into a 26.
         *)
        let ty = type_expr ~loc pat_env pat_type in
        [{ ploc= loc; id; typ= ty }]
  
    | Tpat_any -> not_support ~loc "any pattern"
  
    | Tpat_alias _ -> not_support ~loc "alias pattern"
    | Tpat_constant _ -> not_support ~loc "constant pattern"
    | Tpat_tuple _ -> not_support ~loc "tuple pattern"
    | Tpat_construct _ -> not_support ~loc "variant pattern"
    | Tpat_variant _ -> not_support ~loc "polymorphic variant pattern"
    | Tpat_record _ -> not_support ~loc "record pattern"
    | Tpat_array _ -> not_support ~loc "array pattern"
    | Tpat_or _ -> not_support ~loc "or pattern"
    | Tpat_lazy _ -> not_support ~loc "lazy pattern"

  type t = { loc : Location.t ; desc : desc ; typ : MicType.t }
  and desc =
    | Const of Opcode.constant
    | Nil of MicType.t
    | Cons of t * t
    | Left of MicType.t * t
    | Right of MicType.t * t
    | Unit
    | Var of Ident.t * MicType.t
    | Tuple of t * t
    | Assert of t
    | Fun of MicType.t * MicType.t * pat * t * (Ident.t * MicType.t) list (* freevars *)
    | IfThenElse of t * t * t
    | App of t * t list
    | Prim of string * Opcode.t list * t list
    | Let of pat * t * t

  let rec pp ppf = 
    let p = Format.pp_print_string ppf in
    let f fmt = Format.fprintf ppf fmt in
    fun t -> match t.desc with
    | Const c -> Opcode.pp_constant ppf c
    | Nil ty -> f "([] : %a)" MicType.pp ty
    | Cons (t1, t2) -> f "(%a :: %a)" pp t1 pp t2
    | Left (ty, t) -> f "Left (%a) (%a)" MicType.pp ty pp t
    | Right (ty, t) -> f "Right (%a) (%a)" MicType.pp ty pp t
    | Unit -> p "()"
    | Var (id, _) -> f "%s" (Ident.name id)
    | Tuple (t1, t2) -> f "(%a, %a)" pp t1 pp t2
    | Assert t -> f "assert (%a)" pp t
    | Fun (ty1, ty2, pat, body, _fvars) ->
        f "@[<2>(fun %s ->@ %a@ : %a)@]"
          (Ident.name pat.id) pp body MicType.pp t.typ
    | IfThenElse (t1, t2, t3) -> 
        f "(if %a @[then %a@ else %a@])"
          pp t1 pp t2 pp t3
    | App (t1, ts) -> 
        f "(%a %a)" pp t1 Format.(list " " (fun ppf t -> fprintf ppf "(%a)" pp t)) ts
    | Prim (n, ops, ts) ->
        f "(%s %a)" 
          n
          Format.(list " " (fun ppf t -> fprintf ppf "(%a)" pp t)) ts
    | Let (p, t1, t2) ->
        f "@[<2>(let %s =@ %a in@ %a)@]"
          (Ident.name p.id) pp t1 pp t2
    
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
    | Assert t
            -> freevars t
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

  let rec construct ~loc env exp_env exp_type {Types.cstr_name; cstr_res} args =
    let make typ desc = { loc; typ; desc } in
    match (Ctype.expand_head exp_env exp_type).Types.desc with
    (* bool *)
    | Tconstr (p, [], _) when p = Predef.path_bool ->
        make TyBool (match cstr_name with
            | "true" -> Const Opcode.True
            | "false" -> Const Opcode.False
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

  and expression env { exp_desc; exp_loc=loc; exp_type; exp_env; exp_extra; exp_attributes=_ } =
    let open Result.Infix in
    (* wildly ignores extra *)
    (* if exp_extra <> [] then not_support ~loc "expression extra"; *)
    let typ = type_expr ~loc exp_env exp_type in
    let make desc = { loc; typ; desc } in
    match exp_desc with
    | Texp_ident (Path.Pident id, {loc}, _vd) -> 
        begin match List.assoc_opt id env with
          | None -> assert false
          | Some ty ->
              ignore @@ unify typ ty;
              make @@ Var (id, typ)
        end
    | Texp_ident (p, {loc}, _vd) ->
        not_support ~loc "complex path %s" (Path.xname p)
    | Texp_constant (Const_string (s, None)) -> 
        make @@ Const (String s)
    | Texp_constant _ -> not_support ~loc "constant"
    | Texp_tuple [e1; e2] ->
        let e1 = expression env e1 in
        let e2 = expression env e2 in
        ignore @@ unify (TyPair (e1.typ, e2.typ)) typ;
        make @@ Tuple (e1, e2)
    | Texp_tuple _ -> not_support ~loc "tuple with more than 2 elems"
    | Texp_construct ({loc; txt}, c, args) -> 
        construct ~loc env exp_env exp_type c args
    | Texp_assert e ->
        make @@ Assert (expression env e)

    (* Nolabel *)

    | Texp_apply (f, []) -> assert false
    | Texp_apply (f, args) -> 
        let args = List.map (function
            | (Nolabel, Some e) -> expression env e
            | _ -> not_support ~loc "labeled arguments") args
        in
        let name = match f with
          | { exp_desc= Texp_ident (p, _, _) } ->
              begin match Path.is_scaml p with
              | None -> prerr_endline (Path.xname p); None
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
        not_support ~loc "labeled arguments"

    | Texp_function { arg_label= Nolabel; param; cases; partial } ->
        if partial = Partial then errorf ~loc "Pattern match is partial";
        let { c_lhs ; c_guard ; c_rhs} = begin match cases with [] -> assert false | [c] -> c | _ -> not_support ~loc "multi case" end in
        (* Format.eprintf "DEBUG: texp_function %s@." (Ident.name param); (* param is "param" if the pattern is not a simple variable *) *)
        begin match c_guard with Some e -> not_support ~loc:e.exp_loc "guard" | None -> () end;
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

    | _ -> not_support ~loc "this type of expression"

  and primitive ~loc n args =
    (* They need type unifications! *)
    let ops = match n with
      | "fst" -> [CAR]
      | "snd" -> [CDR]
      | "compare" -> [COMPARE]
      | "=" -> [COMPARE; EQ]
      | "<" -> [COMPARE; LT]
      | ">" -> [COMPARE; GT]
      | "<=" -> [COMPARE; LE]
      | ">=" -> [COMPARE; GE]
      | ("+" | "+^") -> [ADD]
      | "-" -> [SUB]
      | "&&" -> [AND]
      | _ -> errorf ~loc "Unknown primitive SCaml.%s" n
    in
    Prim (n, ops, args)

  let value_binding env { vb_pat; vb_expr; vb_attributes=_; vb_loc=loc } = 
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
  let structure_item env { str_desc; str_loc=loc; str_env } =
    match str_desc with
    | Tstr_eval _ -> not_support ~loc "toplevel evaluation"
    | Tstr_primitive _ -> not_support ~loc "primitive declaration"
    | Tstr_type _ -> env, []
    | Tstr_typext _ -> not_support ~loc "type extension"
    | Tstr_exception _ -> not_support ~loc "exception declaration"
    | Tstr_module _ | Tstr_recmodule _ -> not_support ~loc "module declaration"
    | Tstr_class _ -> not_support ~loc "class declaration"
    | Tstr_class_type _ -> not_support ~loc "class type declaration"
    | Tstr_include _ -> not_support ~loc "include"
    | Tstr_modtype _ -> not_support ~loc "module type declaration"
  
    | Tstr_value (Recursive, _vbs) -> not_support ~loc "recursive definitions"
  
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
    let env, rev_vbss =
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
    let entry_point { vb_pat; vb_expr; vb_attributes=_; vb_loc=loc } = 
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
      let ty_parameter = type_expr ~loc tenv ty_parameter in
      let ty_storage = type_expr ~loc tenv ty_storage in
      (ty_parameter, ty_storage)
    in
    
    (* The condition of the entry point is a bit too strict.
       Currently: the last sitem must be an entry point.
       Better: the last value binding must be an entry point.,
    *)
    let structure_item { str_desc; str_loc=loc; str_env } =
      let must_be_entry_point () =
        errorf ~loc "SCaml needs an entry point at the end of module"
      in
      match str_desc with
      | Tstr_value (Recursive, _vbs) -> not_support ~loc "recursive definitions"
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
          
end

module Y = struct

  let closure_env_type xtys =
    match List.rev xtys with
    | [] -> assert false
    | (_,tylast)::xs ->
        List.fold_left (fun acc (_,ty) ->
            TyPair (TyOption ty, acc)) (TyOption tylast) xs
  
  let rec closure_type = function
    | (TyString
      | TyNat
      | TyInt
      | TyBytes
      | TyBool
      | TyUnit
      | TyMutez
      | TyKeyHash
      | TyTimestamp
      | TyAddress
      | TyKey
      | TySignature
      | TyOperation as t) -> t
    | TyList t -> TyList (closure_type t)
    | TyPair (t1,t2) -> TyPair (closure_type t1, closure_type t2)
    | TyOption t -> TyOption (closure_type t)
    | TyOr (t1,t2) -> TyOr (closure_type t1, closure_type t2)
    | TySet t -> TySet (closure_type t)
    | TyMap (t1,t2) -> TyMap (closure_type t1, closure_type t2)
    | TyBigMap (t1,t2) -> TyBigMap (closure_type t1, closure_type t2)
    | TyContract t -> TyContract (closure_type t)
    | (TyLambda (t1, t2, closure_info) as t) ->
        match (repr_closure_info closure_info).closure_desc with
        | CLLink _ -> assert false
        | CLList [] -> t
        | CLList xtys -> 
            let env_type = 
              closure_type @@ closure_env_type xtys
            in
            TyPair (TyLambda ( TyPair(closure_type t1, env_type),
                               closure_type t2,
                               { closure_desc = CLList [] } ),
                    env_type)

  let rec repr_closure_info ({ closure_desc } as i) = 
    match closure_desc with
    | CLList _ -> i
    | CLLink cl -> repr_closure_info cl


  let compile_var ~loc env id = match MEnv.find id env with
    | None -> 
        internal_error ~loc "variable not found: %s in %s" 
          (Ident.unique_name id)
          (Format.sprintf "%a" MEnv.pp env)
    | Some (n,typ) ->
        let rec f = function
          | 0 -> [ DUP ]
          | n -> 
              assert (n > 0);
              [ DIP (f (n-1)); SWAP ]
        in
        f n

  let rec compile env t = 
    let loc = t.X.loc in
    match t.X.desc with
    | Const op -> [ PUSH (t.typ, op) ]
    | Nil ty -> [ NIL (closure_type ty) ]
    | Cons (t1, t2) -> 
        let os2 = compile env t2 in
        let os1 = compile ((Ident.dummy, t2.typ)::env) t1 in
        os2 @ os1 @ [ CONS ]
    | Left (ty, t) ->
        let os = compile env t in
        os @ [ LEFT (closure_type ty) ]
    | Right (ty, t) -> 
        let os = compile env t in
        os @ [ RIGHT (closure_type ty) ]
    | Unit -> [ PUSH (TyUnit, Unit) ]

    | Var (id, _) -> compile_var ~loc env id

    | Tuple (t1, t2) ->
        let os2 = compile env t2 in
        let os1 = compile ((Ident.dummy, t2.typ)::env) t1 in
        os2 @ os1 @ [ PAIR ]
    | Assert t ->
        let os = compile env t in
        os @ [ ASSERT; PUSH (TyUnit, Unit) ]
    | IfThenElse (t1, t2, t3) ->
        let oif = compile env t1 in
        let othen = compile env t2 in
        let oelse = compile env t3 in
        oif @ [IF (othen, oelse)]
    | Prim (_, ops, ts) ->
        (* Prim (ops, [t1; t2])
           t2 ; t1; ops
        *)
        (snd @@ List.fold_right (fun t (env, os) ->
            let os' = compile env t in
            let env' = (Ident.dummy, TyUnit (* dummy *)) :: env in
            env', os @ os') ts (env, [])) @ ops
    | Let (pat, t1, t2) ->
        let os1 = compile env t1 in
        let os2 = compile ((pat.id, pat.typ)::env) t2 in
        COMMENT (Ident.name pat.id, os1) :: os2
    | Fun (_ty1, _ty2, p, body, fvars) ->
        begin match t.typ with
          | TyLambda (ty1, ty2, cli) ->
              begin match (repr_closure_info cli).closure_desc with
                | CLLink _ -> assert false
                | CLList [] ->
                    let env = (p.id,p.typ)::env in
                    let o = compile env body in
                    let clean = [ COMMENT ("lambda clean up", [DIP [ DROP ] ]) ] in
                    [ LAMBDA (closure_type ty1, closure_type ty2, o @ clean) ]
                | CLList xtys -> 
                    (* (x1:xty1),(x2:xty2),...,(xn:xtyn)
                       => (ty1 * (xty1 option * (xty2 option * ... * xtyn option)))
                    *)
                    let lambda =
                      let ity = 
                        match List.rev xtys with
                        | [] -> assert false
                        | (_,tylast)::xs -> TyPair (ty1, closure_env_type xtys)
                      in
                      (* This conversion is not required,
                         if we have a way to access variables deep
                         inside the tuple:

                         (a,(x1,(x2,..xn))) :: s
                         => (x1,(x2,..xn)) :: a :: s
                         => (x2,..xn) :: x1 :: a :: s
                         => xn :: .. x2 :: x1 :: a :: s

                         repeat List.length xs times.

                         if xi is None, it must be removed.
                         if xi is Some vi, it must be replaced by vi
                      *)
                      (* XXX we should use LOOP?  Maybe not since the types are different *)

                      let init_ops = [ DUP; DIP [ CAR ]; CDR ] in (* to get a *)
                      (* inside LAMBDA, the env is empty *)
                      let env = [(p.id,p.typ)] in
                      let rec f ops env = function
                        | [] -> assert false
                        | [(x,ty)] -> 
                            ops @ [ IF_SOME ([], [ FAIL ]) ],
                            if List.mem_assoc x xtys then (x,ty)::env else env
                        | (x,ty)::xtys ->
                            let ops = 
                              ops @
                              [ DUP ; DIP [ CAR; IF_SOME ([], [ FAIL (* to avoid the stack type differences *)]) ] ; CDR ]
                            in
                            let env = 
                              if List.mem_assoc x xtys then (x,ty)::env else env
                            in
                            f ops env xtys
                      in
                      let ops, env = f init_ops env xtys in
                      let o = compile env body in
                      let clean = COMMENT ( "lambda clean up", [DIP (List.map (fun _ -> DROP) env)]) in
                      LAMBDA (closure_type ity, closure_type ty2, ops @ o @ [clean])
                    in

                    (* (v1,(v2,...vn)) 

                       vn :: S         by get_var

                       vn-1 :: vn :: S by get_var
                       (vn-1,vn) :: S  by pair

                       vn-2 :: (vn-1,vn) :: S by get_var
                       (vn-2, (vn-1,vn) ) :: S by pair

                       (v1,(v2,...vn)) :: S by pair
                    *)
                    let bindings = 
                      let compile_var_or_default env x ty = 
                        if List.mem_assoc x fvars then
                          compile_var ~loc env x @ [ SOME ]
                        else
                          [ NONE (closure_type ty) ]
                      in
                      let rec f env = function
                        | [] -> assert false
                        | [(x,ty)] -> 
                            compile_var_or_default env x ty
                        | (x,ty)::xtys ->
                            let os = compile_var ~loc env x @ [ PAIR ] in
                            let env = (Ident.dummy,TyUnit)::env in
                            let os' = f env xtys in
                            os @ os'
                      in
                      f env xtys
                    in
                    bindings @ [lambda; PAIR]
              end
          | _ -> assert false
        end
    | App (t, []) -> compile env t
    | App (f, [arg]) ->
        let ofun = compile env f in
        ofun @ application ((Ident.dummy,TyUnit)::env) f.typ arg
    | App (f, args) ->
        let ofun = compile env f in
        let env = (Ident.dummy,TyUnit)::env in
        fst @@ List.fold_left (fun (ofun, ftyp) arg ->
            let ofun = ofun @ application env ftyp arg in
            let ftyp = match ftyp with
              | TyLambda (_, ty2, _) -> ty2
              | _ -> assert false
            in
          (ofun, ftyp)) (ofun, f.typ) args

  and application env funty arg =
    begin match funty with
      | TyLambda (ty1, ty2, cinfo) ->
          begin match (repr_closure_info cinfo).closure_desc with
            | CLLink _ -> assert false
            | CLList [] ->
                (* it is not a closure *)
                let oarg = compile env arg in
                oarg @ [ EXEC ]
            | CLList xs -> 
                (* it is a closure 
                   arg :: (lambda, env) :: s  
                   arg :: env :: lambda :: s  <-  DIP [ DUP; CDR; DIP [ CAR ] ]
                   (arg,env) :: lambda ::s    <- PAIR
                   EXEC!
                *)
                let oarg = compile env arg in
                oarg @ [ DIP [ DUP; CDR; DIP [ CAR ] ]; PAIR ; EXEC ]
          end
      | _ -> assert false
    end

  let split_entry_point t =
    let rec f st t = match t.X.desc with
      | X.Let (p, t1, { X.desc= Unit }) ->
          (List.rev st, (p, t1))
      | Let (p, t1, t2) ->
          f ((p,t1)::st) t2
      | _ -> assert false
    in
    f [] t
          
  let compile_structure t =
    let defs, entry_point = split_entry_point t in
    let ops, env = 
      List.fold_left (fun (ops, env) (p,t) ->
          let os1 = compile env t in
          ops @ [ COMMENT (Ident.name p.X.id, os1) ], 
          ((p.X.id,p.typ)::env)) ([], []) defs
    in
    (* (parameter, storage) :: []
       -> parameter :: storage :: values
    *)
    let (p,t) = entry_point in
    
    (* replace fun by let *)
    let rec get_abst t = match t.X.desc with
      | X.Fun (_, _, p, t, _) -> p, t
      | Let (p, t1, t2) -> 
          let p', t2 = get_abst t2 in
          p', { t with desc= Let (p, t1, t2) }
      | _ -> assert false
    in
    let p1, t = get_abst t in
    let p2, t = get_abst t in
    let env = ((p2.id,p2.typ)::(p1.id,p1.typ)::env) in
    let os = compile env t in
    [ COMMENT ("defs", [DIP ops]) 
    ; COMMENT ("entry point init", [DUP ; CDR; DIP [CAR]])
    ; COMMENT ("entry point", os )
    ; COMMENT ("final clean up",
               [ DIP (List.init (List.length env) (fun _ -> DROP)) ]) ]
    
end
  

let implementation sourcefile outputprefix modulename (str, _coercion) =
  Format.eprintf "sourcefile=%s outputprefix=%s modulename=%s@." sourcefile outputprefix modulename;
  let parameter, storage = X.fix_entrypoint_type sourcefile str in
  let t = X.structure [] str in

  let oc = open_out (outputprefix ^ ".scaml") in
  let ppf = Format.of_out_channel oc in
  Format.fprintf ppf "%a@." X.pp t;
  close_out oc;

  let code = Y.compile_structure t in
  let m = { Michelson.parameter;
            storage;
            code } in

  let oc = open_out (outputprefix ^ ".tz") in
  let ppf = Format.of_out_channel oc in
  Format.eprintf "@[<2>%a@]@." Michelson.pp m;
  Format.fprintf ppf "@[<2>%a@]@." Michelson.pp m;
  close_out oc
