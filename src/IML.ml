(* InterMediate Language, or Intermediate ML *)
open Spotlib.Spot
open Asttypes
open Typedtree
open Tools

module M = Michelson
open M.Type
module C = M.Constant

let repr_desc ty = (Ctype.repr ty).desc

let create_ident =
  let cntr = ref 0 in
  (* XXX Variables start with __ must be rejected to avoid name crashes *)
  fun n -> incr cntr; Ident.create & "__" ^ n ^ "_" ^ string_of_int !cntr

let contract_self_id = create_ident "self"
  
type ('desc, 'attrs) with_loc_and_type =
  { desc  : 'desc
  ; loc   : Location.t
  ; typ   : M.Type.t
  ; attrs : 'attrs
  }

module Constructor = struct
  type t = Unit | Left | Right | Some | None | Cons | Nil | Bool of bool | Pair | Constant of Michelson.Constant.t
  
  let to_string = function
    | Pair -> "(,)"
    | Left -> "Left"
    | Right -> "Right"
    | Some -> "Some"
    | None -> "None"
    | Cons -> "(::)"
    | Nil -> "[]"
    | Unit -> "()"
    | Bool true -> "true"
    | Bool false -> "false"
    | Constant c -> Format.sprintf "%a" C.pp c
end

module Cnstr = Constructor

module IdTys = Set.Make(struct type t = Ident.t * M.Type.t let compare (id1,_) (id2,_) = compare id1 id2 end)

module Pat = struct
  type desc =
    | Var of Ident.t
    | Constr of Constructor.t * t list
    | Wild
    | Alias of t * Ident.t * Location.t (* location of ident *)
    | Or of t * t 

  and t = (desc, unit) with_loc_and_type

  let rec pp ppf pat = 
    let open Format in
    match pat.desc with
    | Var i -> fprintf ppf "%s" (Ident.name i)
    | Constr (c, []) -> fprintf ppf "%s" (Constructor.to_string c)
    | Constr (c, ps) -> fprintf ppf "@[%s (%a)@]" (Constructor.to_string c) (Format.list ",@ " pp) ps
    | Wild -> string ppf "_"
    | Alias (p, id, _) -> fprintf ppf "(@[%a as %s@])" pp p (Ident.name id)
    | Or (p1,p2) -> fprintf ppf "(@[%a@ | %a@])" pp p1 pp p2
        
  type var = (Ident.t, unit) with_loc_and_type
  
  let pp_var ppf var = pp ppf { var with desc= Var var.desc }

  let rec vars p =
    let open IdTys in
    match p.desc with
    | Var id -> singleton (id, p.typ)
    | Constr (_, []) -> empty
    | Constr (_, p::ps) -> List.fold_left union (vars p) (List.map vars ps)
    | Wild -> empty
    | Alias (p, id, _) -> add (id, p.typ) & vars p
    | Or (p1, p2) -> union (vars p1) (vars p2)
end

module P = Pat

module Attr = struct
  type t = 
    | Comment of string
  
  type ts = t list
  
  let add a t = { t with attrs= a :: t.attrs }
  let adds attrs t = { t with attrs= attrs @ t.attrs }
end

type t = (desc, Attr.ts) with_loc_and_type

and desc =
  | Const of M.Opcode.constant
  | Nil
  | Cons of t * t
  | IML_None
  | IML_Some of t
  | Left of t
  | Right of t
  | Unit
  | Var of Ident.t
  | Pair of t * t
  | Assert of t
  | AssertFalse
  | Fun of P.var * t
  | IfThenElse of t * t * t
  | App of t * t list
  | Prim of string * (M.Opcode.t list -> M.Opcode.t list) * t list
  | Let of P.var * t * t
  | Switch_or of t * P.var * t * P.var * t
  | Switch_cons of t * P.var * P.var * t * t
  | Switch_none of t * t * P.var * t
  | Contract_create_raw of string

let pp ppf = 
  let p = Format.pp_print_string ppf in
  let f fmt = Format.fprintf ppf fmt in
  let rec pp _ppf t =
    match t.attrs with
    | [] -> f "(%a : %a)" desc t M.Type.pp t.typ
    | _ -> f "(%a <%a> : %a)" desc t attr t.attrs M.Type.pp t.typ

  and attr _ppf attrs = 
    List.iter (function
        | Attr.Comment s -> f "/* %s */" s) attrs
                         
  and desc _ppf t = match t.desc with
    | Const c -> f "%a" C.pp c
    | Nil -> f "[]"
    | Cons (t1, t2) -> f "%a :: %a" pp t1 pp t2
    | IML_None -> f "None"
    | IML_Some t -> f "Some %a" pp t
    | Left t -> f "Left %a" pp t
    | Right t -> f "Right %a" pp t
    | Unit -> p "()"
    | Var id -> f "%s" (Ident.name id)
    | Pair (t1, t2) -> f "@[%a,@ %a@]" pp t1 pp t2
    | Assert t -> f "assert %a" pp t
    | AssertFalse -> p "assert false"
    | Fun (pat, body) ->
        f "@[<2>fun %a ->@ %a@]"
          P.pp_var pat pp body
    | IfThenElse (t1, t2, t3) -> 
        f "@[if %a@ then %a@ else %a@]"
          pp t1 pp t2 pp t3
    | App (t1, ts) -> 
        f "%a %a" pp t1 Format.(list " " (fun ppf t -> fprintf ppf "%a" pp t)) ts
    | Prim (n, _ops, ts) ->
        f "%s %a" 
          n
          Format.(list " " (fun ppf t -> fprintf ppf "%a" pp t)) ts
    | Let (p, t1, t2) ->
        f "@[@[<2>let %a@ = %a@]@ in@ %a@]"
          P.pp_var p pp t1 pp t2
    | Switch_or (t, p1, t1, p2, t2) ->
        f "@[<2>match %a with@ | Left %a -> %a@ | Right %a -> %a@]"
          pp t
          P.pp_var p1 pp t1 
          P.pp_var p2 pp t2
    | Switch_cons (t, p1, p2, t1, t2) ->
        f "@[<2>match %a with@ | %a::%a -> %a@ | [] -> %a@]"
          pp t
          P.pp_var p1
          P.pp_var p2
          pp t1 
          pp t2
    | Switch_none (t, t1, p2, t2) ->
        f "@[<2>match %a with@ | None -> %a@ | Some %a -> %a@]"
          pp t
          pp t1 
          P.pp_var p2 pp t2
    | Contract_create_raw s ->
        f "@[<2>Contract.create_raw@ %S@]" s
  in
  pp ppf

let almost_constant t = 
  match t.desc with
  | Nil ->
     (* We cannot PUSH (list operation) {}. If we do, we get:
        "operation type forbidden in parameter, storage and constants" *)
      begin match t.typ.desc with
        | TyList { desc= TyOperation } -> None
        | _ -> Some (C.List [])
      end
  | Const c -> Some c
  | IML_None -> Some (C.Option None)
  | Unit -> Some C.Unit
  | _ -> None

let make_constant t = 
  match
    let (>>=) = Option.bind in
    match t.desc with
    | Const c -> Some c
    | Nil | IML_None | Unit -> 
        None (* They have special opcodes to push.  Let's keep as they are. *)
    | Cons (t1, t2) ->
        begin almost_constant t1 >>= fun c1 ->
          almost_constant t2 >>= function 
          | C.List c2 -> Some (C.List (c1 :: c2))
          | _ -> assert false
        end
    | IML_Some t -> almost_constant t >>= fun c -> Some (C.Option (Some c))
    | Pair (t1, t2) -> 
        almost_constant t1 >>= fun c1 ->
        almost_constant t2 >>= fun c2 ->
        Some (C.Pair (c1, c2))
    | _ -> None
  with
  | None -> t
  | Some c -> { t with desc= Const c }

let rec get_constant t = 
  let (>>=) = Option.bind in
  match t.desc with
  | Unit -> Some C.Unit
  | Const c -> Some c
  | IML_None -> Some (C.Option None)
  | IML_Some t -> get_constant t >>= fun c -> Some (C.Option (Some c))
  | Left t -> get_constant t >>= fun t -> Some (C.Left t)
  | Right t -> get_constant t >>= fun t -> Some (C.Right t)
  | Pair (t1, t2) -> get_constant t1 >>= fun t1 -> get_constant t2 >>= fun t2 -> Some (C.Pair (t1, t2))
  | Nil -> Some (C.List [])
  | Cons (t1, t2) -> get_constant t1 >>= fun t1 -> get_constant t2 >>= fun t2 ->
      begin match t2 with
        | C.List t2 -> Some (C.List (t1::t2))
        | _ -> assert false
      end
  | _ -> None

let mke typ desc = { typ; desc; loc= Location.none; attrs= [] }

let mkfst e =
  let ty = match e.typ.desc with
    | TyPair (ty, _) -> ty
    | _ -> assert false
  in
  let prim = snd (List.assoc "fst" Primitives.primitives) (tyLambda (e.typ, ty)) in
  mke ty (Prim ("fst", prim, [e]))

let mksnd e =
  let ty = match e.typ.desc with
    | TyPair (_, ty) -> ty
    | _ -> 
        Format.eprintf "mksnd %a !?@." M.Type.pp e.typ;
        assert false
  in
  let prim = snd (List.assoc "snd" Primitives.primitives) (tyLambda (e.typ, ty)) in
  mke ty (Prim ("snd", prim, [e]))

let mkleft ty e = mke (tyOr (e.typ, ty)) (Left e)
let mkright ty e = mke (tyOr (ty, e.typ)) (Right e)

let mkeq e1 e2 =
  let prim = snd (List.assoc "=" Primitives.primitives) 
             & tyLambda (e1.typ, tyLambda (e2.typ, tyBool)) in
  mke tyBool (Prim ("=", prim, [e1; e2]))

let mkpair e1 e2 = mke (tyPair (e1.typ, e2.typ)) (Pair (e1, e2))

let mkppair p1 p2 = 
  { loc=Location.none; desc= P.Constr (Cnstr.Pair, [p1; p2]); typ= tyPair (p1.typ, p2.typ); attrs= () }


let mkint n = mke tyInt (Const (M.Constant.Int (Z.of_int n)))

let mkpint n = 
  { desc= P.Constr (Cnstr.Constant (Michelson.Constant.Int (Z.of_int n)), []);
    typ= tyInt;
    loc= Location.none;
    attrs= () }

let mkpleft ty p = 
  { desc= P.Constr (Cnstr.Left, [p]);
    typ= tyOr (p.typ, ty);
    loc= Location.none;
    attrs= () }

let mkpright ty p = 
  { desc= P.Constr (Cnstr.Right, [p]);
    typ= tyOr (ty, p.typ);
    loc= Location.none;
    attrs= () }

let mkfun pvar e = mke (tyLambda (pvar.typ, e.typ)) & Fun (pvar, e)
let mkcons h t = mke t.typ (Cons (h, t))
let mksome t = mke (tyOption t.typ) (IML_Some t)
let mkunit () = mke tyUnit Unit
let mkassert t = mke tyUnit & Assert t
let mklet p t1 t2 = mke t2.typ & Let (p, t1, t2)
let mkvar (id, typ) = mke typ & Var id

let rec freevars t = 
  let open IdTys in
  let psingleton p = singleton (p.desc, p.typ) in
  match t.desc with
  | Const _ | Nil | IML_None | Unit | Contract_create_raw _ -> empty
  | Cons (t1,t2) | Pair (t1,t2) -> union (freevars t1) (freevars t2)
  | Left t | Right t | IML_Some t | Assert t -> freevars t
  | AssertFalse -> empty
  | Var id -> singleton (id, t.typ)
  | IfThenElse (t1,t2,t3) -> union (freevars t1) (union (freevars t2) (freevars t3))
  | App (t,ts) ->
      List.fold_left (fun acc t -> union acc (freevars t)) empty (t::ts)
  | Prim (_,_,ts) ->
      List.fold_left (fun acc t -> union acc (freevars t)) empty ts
  | Fun (pat,t) -> 
      diff (freevars t) (psingleton pat)
  | Let (pat, t1, t2) ->
      diff (union (freevars t1) (freevars t2)) (psingleton pat)
  | Switch_or (t, p1, t1, p2, t2) ->
      union (freevars t)
        (union 
           (diff (freevars t1) (psingleton p1))
           (diff (freevars t2) (psingleton p2)))
  | Switch_cons (t, p1, p2, t1, t2) ->
      union (freevars t)
        (union 
           (diff (diff (freevars t1) (psingleton p1)) (psingleton p2))
           (freevars t2))
  | Switch_none (t, t1, p2, t2) ->
      union (freevars t)
        (union 
           (freevars t1)
           (diff (freevars t2) (psingleton p2)))

type type_expr_error =
  | Type_variable of Types.type_expr
  | Unsupported_type of Types.type_expr
  | Unsupported_data_type of Path.t

let pp_type_expr_error ppf = function
  | Type_variable ty -> 
      Format.fprintf ppf "type variable %a is not supported in SCaml." Printtyp.type_expr ty
  | Unsupported_type ty ->
      Format.fprintf ppf "type %a is not supported in SCaml." Printtyp.type_expr ty
  | Unsupported_data_type p ->
      Format.fprintf ppf "data type %s is not supported in SCaml." (Path.name p)

let encode_by branch xs =
  Binplace.fold 
      ~leaf:(fun x -> x) 
      ~branch
  & Binplace.place xs
      
let rec type_expr tyenv ty = 
  let open Result.Infix in
  let ty = Ctype.expand_head tyenv ty in
  match ty.desc with
  | Tvar _ -> Error (Type_variable ty)
  | Tarrow (Nolabel, f, t, _) -> 
      type_expr tyenv f >>= fun f ->
      type_expr tyenv t >>= fun t -> 
      Ok (tyLambda (f, t))
  | Ttuple [t1; t2] -> 
      type_expr tyenv t1 >>= fun t1 ->
      type_expr tyenv t2 >>= fun t2 -> Ok (tyPair (t1, t2))
  | Ttuple tys -> 
      Result.mapM (type_expr tyenv) tys 
      >>| encode_by (fun ty1 ty2 -> tyPair (ty1, ty2))
  | Tconstr (p, [], _) when p = Predef.path_bool -> Ok (tyBool)
  | Tconstr (p, [t], _) when p = Predef.path_list -> 
      type_expr tyenv t >>= fun t -> Ok (tyList t)
  | Tconstr (p, [t], _) when p = Predef.path_option -> 
      type_expr tyenv t >>= fun t -> Ok (tyOption t)
  | Tconstr (p, [], _) when p = Predef.path_unit -> Ok (tyUnit)
  | Tconstr (p, [], _) when p = Predef.path_string -> Ok (tyString)
  | Tconstr (p, [], _) when p = Predef.path_bytes -> Ok (tyBytes)
  | Tconstr (p, tys, _) ->
      let rec f res = function
        | [] -> Ok (List.rev res)
        | ty::tys ->
            type_expr tyenv ty >>= fun ty -> 
            f (ty::res) tys
      in
      f [] tys >>= fun tys ->
      begin match Path.is_scaml p, tys with
        | Some "sum", [t1; t2] -> Ok (tyOr (t1, t2))
        | Some "int", [] -> Ok (tyInt)
        | Some "nat", [] -> Ok (tyNat)
        | Some "tz",  [] -> Ok (tyMutez)
        | Some "set"         , [ty]       -> Ok (tySet ty)
        | Some "map"         , [ty1; ty2] -> Ok (tyMap (ty1, ty2))
        | Some "big_map"     , [ty1; ty2] -> Ok (tyBigMap (ty1, ty2))
        | Some "operation"   , []         -> Ok (tyOperation)
        | Some "contract"    , [ty]       -> Ok (tyContract ty)
        | Some "timestamp"   , []         -> Ok (tyTimestamp)
        | Some "address"     , []         -> Ok (tyAddress)
        | Some "key"         , []         -> Ok (tyKey)
        | Some "signature"   , []         -> Ok (tySignature)
        | Some "key_hash"    , []         -> Ok (tyKeyHash)
        | Some "bytes"       , []         -> Ok (tyBytes)
        | Some "chain_id"    , []         -> Ok (tyChainID)
        | Some _, _ -> Error (Unsupported_data_type p)
        | None, _ -> 
            match Env.find_type_descrs p tyenv with
            | [], [] -> Error (Unsupported_data_type p) (* abstract XXX *)
            | [], labels ->
                let tys = List.map (fun label ->
                    let _, ty_arg, ty_res = 
                      Ctype.instance_label false (* XXX I do not know what it is *)
                        label
                    in
                    Ctype.unify tyenv ty ty_res; (* XXX should succeed *)
                    ty_arg) labels
                in
                Result.mapM (type_expr tyenv) tys 
                >>| encode_by (fun ty1 ty2 -> tyPair (ty1, ty2))

            | constrs, [] ->
                let consts, non_consts =
                  List.partition (fun constr -> constr.Types.cstr_arity = 0) constrs
                in
                (* XXX use cstr_consts and cstr_nonconsts *)
                let non_consts =
                  match non_consts with
                  | [] -> Ok None
                  | _ ->
                      let tys_list = 
                        List.map (fun constr ->
                            let ty_args, ty_res = Ctype.instance_constructor constr in
                            Ctype.unify tyenv ty ty_res; (* XXX should succeed *)
                            ty_args
                          ) non_consts
                      in
                      Result.mapM (Result.mapM (type_expr tyenv)) tys_list >>| fun tys_list ->
                      let ty_list = List.map (encode_by (fun ty1 ty2 -> tyPair (ty1, ty2))) tys_list in
                      Some (encode_by (fun ty1 ty2 -> tyOr (ty1, ty2)) ty_list)
                in
                non_consts >>| fun non_consts ->
                begin match consts, non_consts with
                 | [], None -> assert false
                 | [], Some t -> t
                 | _::_, None -> tyInt
                 | _::_, Some t -> tyOr (tyInt, t)
                end

            | _ -> assert false (* impossible *)
            | exception _ -> Error (Unsupported_data_type p)
      end
  | Tpoly (ty, []) -> type_expr tyenv ty
  | _ -> Error (Unsupported_type ty)

(* Literals *)

let parse_timestamp s = match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> 
      let t' = Ptime.truncate ~frac_s:0 t in
      if not (Ptime.equal t t') then Error "Subsecond is not allowed in timestamps"
      else 
        let posix = Ptime.to_float_s t in
        if posix < 0. then Error "Timestamp before Epoch is not allowed"
        else Ok (C.Timestamp (Z.of_float posix))
  | Error (`RFC3339 (_, e)) -> 
      Error (Format.sprintf "%a" Ptime.pp_rfc3339_error e)

let parse_bytes s =
  try
    ignore & Hex.to_string (`Hex s); Ok (C.Bytes s)
  with
  | _ -> Error "Bytes must take hex representation of bytes"

let constructions_by_string =
  [ ("signature" , ("signature", "Signature", tySignature, 
                    fun x -> Ok (C.String x)));
    ("key_hash"  , ("key_hash", "Key_hash", tyKeyHash, 
                    fun x -> Ok (C.String x)));
    ("key"       , ("key", "Key", tyKey, 
                    fun x -> Ok (C.String x)));
    ("address"   , ("address", "Address", tyAddress, 
                    fun x -> Ok (C.String x)));
    ("timestamp" , ("timestamp", "Timestamp", tyTimestamp, 
                    parse_timestamp));
    ("bytes"     , ("bytes", "Bytes", tyBytes, 
                    parse_bytes));
    ("chain_id"  , ("chain_id", "Chain_id", tyChainID,
                    fun x -> Ok (C.String x)))
  ]

let pattern_simple { pat_desc; pat_loc=loc; pat_type= mltyp; pat_env= tyenv } = 
  let typ = 
    Result.at_Error (fun e -> errorf ~loc "This pattern has type %a.  It contains %a" Printtyp.type_expr mltyp pp_type_expr_error e) 
    & type_expr tyenv mltyp 
  in
  let mk loc id typ = { loc; desc=id; typ; attrs= () } in
  let mk_dummy loc typ = mk loc Ident.dummy typ in
  match pat_desc with
  | Tpat_var (id, {loc}) -> [mk loc id typ]
  | Tpat_alias ({ pat_desc = Tpat_any; pat_loc=_ }, id, _) -> 
      (* We transform (_ as x) to x if _ and x have the same location.
         The compiler transforms (x:t) into (_ as x : t).
         This avoids transforming a warning 27 into a 26.
       *)
      [mk loc id typ]
  | Tpat_any         -> [mk_dummy loc typ]
  | Tpat_construct ({loc}, _, []) when typ.desc = TyUnit -> [mk_dummy loc typ]
  | Tpat_construct _ -> unsupported ~loc "variant pattern"
  | Tpat_alias _     -> unsupported ~loc "alias pattern"
  | Tpat_constant _  -> unsupported ~loc "constant pattern"
  | Tpat_tuple _     -> unsupported ~loc "tuple pattern"
  | Tpat_variant _   -> unsupported ~loc "polymorphic variant pattern"
  | Tpat_record _    -> unsupported ~loc "record pattern"
  | Tpat_array _     -> unsupported ~loc "array pattern"
  | Tpat_or _        -> unsupported ~loc "or pattern"
  | Tpat_lazy _      -> unsupported ~loc "lazy pattern"

let rec patternx { pat_desc; pat_loc=loc; pat_type= mltyp; pat_env= tyenv } = 
  let typ = 
    Result.at_Error (fun e -> errorf ~loc "This pattern has type %a.  It contains %a" Printtyp.type_expr mltyp pp_type_expr_error e) 
    & type_expr tyenv mltyp 
  in
  let mk desc = { loc; desc; typ; attrs= () } in
  match pat_desc with
  | Tpat_array _     -> unsupported ~loc "array pattern"
  | Tpat_lazy _      -> unsupported ~loc "lazy pattern"
  | Tpat_variant _   -> unsupported ~loc "polymorphic variant pattern"

  | Tpat_var (id, _) -> mk (P.Var id)

  | Tpat_alias ({pat_desc = Tpat_any; pat_loc=_}, id, _) -> 
      (* We transform (_ as x) in x if _ and x have the same location.
         The compiler transforms (x:t) into (_ as x : t).
         This avoids transforming a warning 27 into a 26.
       *)
      mk (P.Var id)

  | Tpat_any         -> mk P.Wild

  | Tpat_alias (p, id, {loc}) -> mk (P.Alias (patternx p, id, loc))

  | Tpat_tuple [p1; p2] -> mk (P.Constr (Cnstr.Pair, [patternx p1; patternx p2]))

  | Tpat_tuple ps -> encode_by mkppair & List.map patternx ps

  | Tpat_constant (Const_string (s, None))  -> 
      mk (P.Constr (Cnstr.Constant (C.String s), []))
                                                 
  | Tpat_constant (Const_string (_, Some _))  -> unsupported ~loc "quoted string"

  | Tpat_constant _ -> unsupported ~loc "constant pattern of type %s"
                         (Format.sprintf "%a" Printtyp.type_scheme mltyp)

  | Tpat_or (p1, p2, None)   -> mk & P.Or (patternx p1, patternx p2)

  | Tpat_or (_, _, _)        -> unsupported ~loc "or pattern with row"

  | Tpat_construct (_, cdesc, ps) ->
      
      (* XXX should check the module path *)
      begin match cdesc.cstr_name, typ.desc, ps with
        | "()", TyUnit, [] -> mk (P.Constr (Cnstr.Unit, []))
        | "Left", TyOr _, [p] -> mk (P.Constr (Cnstr.Left, [patternx p]))
        | "Right", TyOr _, [p] -> mk (P.Constr (Cnstr.Right, [patternx p]))
        | "Some", TyOption _, [p] -> mk (P.Constr (Cnstr.Some, [patternx p]))
        | "None", TyOption _, [] -> mk (P.Constr (Cnstr.None, []))
        | "::", TyList _, [p1; p2] -> mk (P.Constr (Cnstr.Cons, [patternx p1; patternx p2]))
        | "[]", TyList _, [] -> mk (P.Constr (Cnstr.Nil, []))
        | "true", TyBool, [] -> mk (P.Constr (Cnstr.Bool true, []))
        | "false", TyBool, [] -> mk (P.Constr (Cnstr.Bool false, []))
        | "Int", TyInt, [{pat_desc= Tpat_constant (Const_int n)}] ->
            mk & P.Constr (Cnstr.Constant (C.Int (Z.of_int n)), [])
        | "Int", TyInt, [_] -> errorf ~loc "Int can only take an integer constant"
        | "Nat", TyNat, [{pat_desc= Tpat_constant (Const_int n)}] ->
            if n < 0 then 
              errorf ~loc "Nat can only take a positive integer constant";
            mk & P.Constr (Cnstr.Constant (C.Nat (Z.of_int n)), [])
        | "Nat", TyNat, [_] -> errorf ~loc "Nat can only take an integer constant"
        | _, TyMutez, [_] -> errorf ~loc "tz constant cannot be used as a pattern"
        | "Key_hash", TyKeyHash, [{pat_desc= Tpat_constant (Const_string (s, None))}] ->
            mk & P.Constr (Cnstr.Constant (C.String s), [])
        | "Key_hash", TyKeyHash, [{pat_desc= Tpat_constant (Const_string (_, Some _))}] -> unsupported ~loc "quoted string"
        | "Key_hash", TyKeyHash, [_] -> unsupported ~loc "Key_hash can only take a string constant"


        | "Address", TyAddress, [{pat_desc= Tpat_constant (Const_string (s, None))}] ->
            mk & P.Constr (Cnstr.Constant (C.String s), [])
        | "Address", TyAddress, [{pat_desc= Tpat_constant (Const_string (_, Some _))}] -> unsupported ~loc "quoted string"
        | "Address", TyAddress, [_] -> unsupported ~loc "Address can only take a string constant"


        | "Timestamp", TyTimestamp, [{pat_desc= Tpat_constant (Const_string (s, None))}] ->
            begin match parse_timestamp s with
              | Error _e -> errorf ~loc "straonge arguments for Timestamp" 
              | Ok t -> mk & P.Constr (Cnstr.Constant t, [])
            end
        | "Timestamp", TyTimestamp, [{pat_desc= Tpat_constant (Const_string (_, Some _))}] -> unsupported ~loc "quoted string"
        | "Timestamp", TyTimestamp, [_] -> unsupported ~loc "Timestamp can only take a string constant"

        | "Bytes", TyBytes, [{pat_desc= Tpat_constant (Const_string (s, None))}] ->
            begin match parse_bytes s with
              | Error _e -> errorf ~loc "straonge arguments for Bytes" 
              | Ok t -> mk & P.Constr (Cnstr.Constant t, [])
            end
        | "Bytes", TyBytes, [{pat_desc= Tpat_constant (Const_string (_, Some _))}] -> unsupported ~loc "quoted string"
        | "Bytes", TyBytes, [_] -> unsupported ~loc "Bytes can only take a string constant"
                                     
        | _, _, _ ->
            
            begin match repr_desc mltyp with
              | Tconstr (p, _, _) ->
                  begin match Env.find_type_descrs p tyenv with
                    | [], [] -> 
                        errorf ~loc "Abstract data type %s is not supported in SCaml" (Path.name p)
                    | [], _::_ -> assert false (* record cannot come here *)
                    | _::_, _::_ -> assert false
                    | constrs, [] -> 
                        let consts, non_consts =
                          List.partition (fun constr -> constr.Types.cstr_arity = 0) constrs
                        in
                        let non_consts_ty =
                          match non_consts with
                          | [] -> None
                          | _ ->
                              let tys_list = 
                                List.map (fun constr ->
                                    let ty_args, ty_res = Ctype.instance_constructor constr in
                                    Ctype.unify tyenv mltyp ty_res; (* XXX should succeed *)
                                    ty_args
                                  ) non_consts
                              in
                              let tys_list = List.map (List.map (fun ty -> from_Ok & type_expr tyenv ty)) tys_list in
                              let ty_list = List.map (encode_by (fun ty1 ty2 -> tyPair (ty1, ty2))) tys_list in
                              Some (encode_by (fun ty1 ty2 -> tyOr (ty1, ty2)) ty_list)
                        in
                        let rec find_constr i = function
                          | [] -> assert false
                          | c::_ when c.Types.cstr_name = cdesc.cstr_name -> i
                          | _::consts -> find_constr (i+1) consts
                        in
                        match cdesc.cstr_arity, consts, non_consts_ty with
                        | _, [], None -> assert false
                        | 0, [], _ -> assert false
                        | 0, _, None  -> mkpint & find_constr 0 consts
                        | 0, _, Some ty -> mkpleft ty & mkpint & find_constr 0 consts
                        | _, _, None -> assert false
                        | _, _, Some ty ->
                            let i = find_constr 0 non_consts in
                            let sides = Binplace.path i (List.length non_consts) in
                            let arg = encode_by mkppair & List.map patternx ps in
                            let rec f ty sides = match ty.M.Type.desc, sides with
                              | _, [] -> arg
                              | TyOr (ty1, ty2), Binplace.Left::sides -> mkpleft ty2 (f ty1 sides)
                              | TyOr (ty1, ty2), Right::sides -> mkpright ty1 (f ty2 sides)
                              | _ -> assert false
                            in
                            match consts with
                            | [] -> f ty sides
                            | _ -> mkpright tyInt & f ty sides
                  end
            | _ ->  unsupported ~loc "pattern %s" cdesc.cstr_name
            end
      end

  | Tpat_record (pfields, _) ->
      (* fields are sorted, but omitted labels are not in it *)
      match repr_desc mltyp with
      | Tconstr (p, _, _) ->
          begin match Env.find_type_descrs p tyenv with
            | [], labels ->
                let labels = 
                  List.map (fun label ->
                      let _, arg, res = 
                        Ctype.instance_label false (* XXX ? *) label 
                      in
                      Ctype.unify tyenv res mltyp;
                      label.lbl_name, arg
                    ) labels
                in
                let rec f labels pfields = match labels, pfields with
                  | (n, _)::labels, (_, plabel, p)::pfields when n = plabel.Types.lbl_name ->
                      patternx p :: f labels pfields
                  | (n, typ)::labels, _ ->
                      let typ = 
                        Result.at_Error (fun e -> 
                            errorf ~loc "This pattern has a field %s with type %a.  %a" 
                              n 
                              Printtyp.type_expr typ
                              pp_type_expr_error e) 
                        & type_expr tyenv typ 
                      in
                      { loc; desc= P.Wild; typ; attrs= () } :: f labels pfields
                  | [], [] -> []
                  | [], _ -> assert false
                in
                encode_by mkppair & f labels pfields

            | _, _ -> assert false
          end
      | _ -> assert false

let attr_has_entry_point = 
  List.find_map_opt (function
      | ({ txt = "entry"; loc }, Parsetree.PStr []) -> Some loc
      | _ -> None)
  
module Pmatch = struct
  open Spotlib.Spot
  open Tools
  module Type = Michelson.Type
  
  let mkp typ desc = { typ; desc; loc= Location.none; attrs= () }
  
  (* rows <-> columns *)
  let transpose : 'p list list -> 'p list list = fun rows ->
    if rows = [] then []
    else 
      let ncolumns = List.length & List.hd rows in
      List.init ncolumns (fun i ->
          List.map (fun row ->
              assert (List.length row = ncolumns);
              List.nth row i) rows)
  
  type id_ty = Ident.t * Type.t
  
  type case = 
    { pats : P.t list
    ; guard : int option
    ; action : int 
    ; bindings : (Ident.t * id_ty) list
    }
  
  type matrix = case list
  
  type tree = 
    | Fail
    | Leaf of (Ident.t * id_ty) list * int
    | Switch of id_ty * (Constructor.t * id_ty list * tree) list * tree option (* default *)
    | Guard of (Ident.t * id_ty) list (* binder *)
               * int (* guard *)
               * int (* case *)
               * tree (* otherwise *)
                  
  let rec pp_tree ppf =
    let f fmt = Format.fprintf ppf fmt in
    function
    | Fail -> f "Fail"
    | Leaf (binders, n) -> 
        f "Leaf %a %d" 
          (Format.list ",@," (fun ppf (v,(v',_)) ->
               Format.fprintf ppf "%s=%s"
                 (Ident.name v)
                 (Ident.name v'))) binders
          n
    | Switch (v, xs, None) ->
        f "@[<2>Switch %s@ [ @[%a@] ]@]"
          (Ident.name & fst v)
          (Format.list ";@ " (fun ppf -> 
               let f fmt = Format.fprintf ppf fmt in
               let pvs _ppf vs = 
                 f "%s" & String.concat "," & List.map (fun (x,_) -> Ident.name x) vs
               in
               fun (c, vs, t) ->
                 f "%s %a (%a)" (Constructor.to_string c) pvs vs pp_tree t
             )) xs
    | Switch (v, xs, Some d) ->
        f "@[<2>Switch %s@ [ @[%a@] default %a]@]"
          (Ident.name & fst v)
          (Format.list ";@ " (fun ppf -> 
               let f fmt = Format.fprintf ppf fmt in
               let pvs _ppf vs = 
                 f "%s" & String.concat "," & List.map (fun (x,_) -> Ident.name x) vs
               in
               fun (c, vs, t) ->
                 f "%s %a (%a)" (Constructor.to_string c) pvs vs pp_tree t
             )) xs
          pp_tree d
    | Guard (binders, guard, case, otherwise) ->
        f "@[<2>Guard (%a) guard%d case%d [%a]@]" 
          (Format.list ",@," (fun ppf (v,(v',_)) ->
               Format.fprintf ppf "%s=%s"
                 (Ident.name v)
                 (Ident.name v'))) binders
          guard
          case
          pp_tree otherwise
  
  (* specialize on Left and Right *)
  let rec specialize o c (matrix : matrix) : matrix =
    List.fold_right (fun ({ pats; bindings } as case) st ->
        match pats with
        | [] -> assert false
        | pat::pats ->
            let rec f pat = match c, pat.desc with
              | _, P.Alias (pat, i, _loc) -> 
                  let cases = f pat in
                  List.map (fun case -> { case with bindings = (i,o) :: case.bindings }) cases
              | c, P.Or (p1, p2) ->
                  specialize o c [{ case with pats= p1::pats }]
                  @ specialize o c [{ case with pats= p2::pats }]
              | c, P.Constr (c', ps) ->
                  if c = c' then [{ case with pats= ps @ pats }] else []
  
              (* For wild, we need to build another wild with arg type. 
                 XXX Currently we must code for each.  Ugh.
              *)
              | Cnstr.Pair, P.Wild -> 
                  let ty1, ty2 = match pat.typ.desc with
                    | TyPair (ty1, ty2) -> ty1, ty2
                    | _ -> assert false
                  in
                  [{ case with pats= mkp ty1 P.Wild :: mkp ty2 P.Wild :: pats }]
  
              | Left, P.Wild -> 
                  let typl = match pat.typ.desc with
                    | TyOr (typl, _typr) -> typl
                    | _ -> assert false
                  in
                  [{ case with pats= mkp typl P.Wild :: pats }]
  
              | Right, P.Wild -> 
                  let typr = match pat.typ.desc with
                    | TyOr (_typl, typr) -> typr
                    | _ -> assert false
                  in
                  [{ case with pats= mkp typr P.Wild :: pats }]
  
              | Some, P.Wild -> 
                  let typ = match pat.typ.desc with
                    | TyOption typ -> typ
                    | _ -> assert false
                  in
                  [{ case with pats= mkp typ P.Wild :: pats }]
  
  
              | Cons, P.Wild -> 
                  let typ = match pat.typ.desc with
                    | TyList typ -> typ
                    | _ -> assert false
                  in
                  [{ case with pats= mkp typ P.Wild :: mkp pat.typ P.Wild :: pats }]
  
              | (None | Nil | Unit | Bool _ | Constant _), P.Wild -> 
                  [{ case with pats }]
  
              | (_ , P.Var v) -> [{ case with pats; bindings= (v, o) :: bindings }]
            in
            let cases = f pat in
            cases @ st
      ) matrix []
  
  let pp_matrix ppf matrix =
    let open Format in
    fprintf ppf "matrix:@.";
    List.iter (function
        | { pats; guard= None; action= i } ->
            eprintf "| %a -> %d@."
              (list ", " P.pp) pats
              i
        | { pats; guard= Some g; action= i } ->
            eprintf "| %a when %d -> %d@."
              (list ", " P.pp) pats
              g
              i
      ) matrix
  
  let pp_osmatrix ppf (os, matrix) =
    let open Format in
    fprintf ppf "match %a with@." (list ", " (fun ppf (id,_) ->
        fprintf ppf "%s" & Ident.name id)) os;
    List.iter (function
        | { pats; guard= None; action= i } ->
            eprintf "| %a -> %d@."
              (list ", " P.pp) pats
              i
        | { pats; guard= Some g; action= i } ->
            eprintf "| %a when %d -> %d@."
              (list ", " P.pp) pats
              g
              i
      ) matrix
  
  let specialize o c matrix =
    Format.eprintf "specializing... %a@." pp_matrix matrix;
    let matrix = specialize o c matrix in
    Format.eprintf "specialized... %a@." pp_matrix matrix;
    matrix
  
  let rec default o (matrix : matrix) : matrix =
    List.fold_right (fun ({ pats } as case) st ->
        match pats with
        | [] -> assert false
        | pat::pats ->
            let rec f pat = match pat.desc with
              | P.Constr (_, _) -> st
              | P.Wild -> { case with pats } :: st
              | P.Var v -> { case with pats ; bindings= (v, o) :: case.bindings } :: st
              | P.Alias (pat, _id, _loc) -> f pat
              | P.Or (p1, p2) ->
                  default o [{ case with pats= p1::pats }]
                  @ default o [{ case with pats= p2::pats}]
                  @ st
            in
            f pat
      ) matrix []
  
  let swap i os (matrix : matrix) : _ * matrix =
    let rec f rev_st i xs = match i, xs with
      | 0, x::xs -> x::List.rev rev_st@xs
      | _, [] -> assert false
      | i, x::xs -> f (x::rev_st) (i-1) xs
    in
    f [] i os,
    List.map (fun ({ pats } as case) -> { case with pats= f [] i pats }) matrix
  
  let rec cc os matrix = 
    Format.eprintf "compile: %a" pp_osmatrix (os, matrix);
    match matrix with
    | [] -> Fail
    | { pats=ps; guard= g; action= a; bindings }::_ ->
        if List.for_all (fun p -> 
            let rec f p = match p.desc with 
              | P.Alias (p, _, _) -> f p
              | P.Wild | P.Var _ -> true 
              | P.Constr _ -> false
              | P.Or _ -> false
            in
            f p ) ps 
        then 
          let bindings = List.fold_right2 (fun v p st -> 
              match p.desc with
              | P.Wild -> st
              | P.Var v' -> (v',v)::st
              | P.Alias _ | P.Constr _ | P.Or _ -> assert false) os ps bindings
          in
          match g with
          | None -> Leaf (bindings, a)
          | Some g -> 
              prerr_endline "guard";
              Guard (bindings, g, a, cc os & List.tl matrix)
        else 
          (* find column i where at least one pattern which is not a wildcard *)
          let columns = transpose & List.map (fun case -> case.pats) matrix in
          let icolumns = List.mapi (fun i c -> (i,c)) columns in
          let i, column = 
            match 
              List.find_all (fun (_i,c) ->
                  List.exists (fun p -> 
                      let rec f p = match p.desc with
                        | P.Alias (p, _, _) -> f p
                        | P.Wild | P.Var _ -> false
                        | P.Constr _ -> true
                        | P.Or (p1, p2) -> f p1 || f p2
                      in
                      f p
                    ) c) icolumns
            with
            | [] -> assert false
            | (i,c)::_ -> i,c (* blindly select the first *)
          in
          (* algo 3 (a) *)
          let algo os column =
  
            let constructors = 
              List.sort_uniq compare
              & List.fold_left (fun st p -> 
                  let rec f p = match p.desc with
                    | P.Alias (p, _, _) -> f p
                    | P.Constr (c, _) -> [c]
                    | P.Wild | P.Var _ -> []
                    | P.Or (p1, p2) -> f p1 @ f p2
                  in
                  f p @ st
                ) [] column
            in
            (* for Left and Right, true and false, 
               think constructors are always full *)
            let constructors = 
              match constructors with
              | [Left] | [Right] -> [Cnstr.Left; Right]
              | [Some] | [None] -> [Some; None]
              | [Cons] | [Nil] -> [Cons; Nil]
              | [(Bool _)] -> [(Bool true) ; (Bool false)]
              | _ -> constructors
            in
            (* XXX weak. this depends on the above code *)
            let is_signature = 
              match constructors with
              | [Left; Right]
              | [Some; None]
              | [Cons; Nil]
              | [Pair]
              | [(Bool true) ; (Bool false)] -> true
              | _ -> false
            in
            assert (constructors <> []);
  
            let ivty = List.hd os in
            Switch (ivty,
                    (let _, vty = ivty in
                    List.map (fun c ->
                        let os = List.tl os in
                        let vs = 
                          match c with
                          | Cnstr.Left -> 
                              let ty = match vty.desc with
                                | TyOr (ty, _) -> ty
                                | _ -> assert false
                              in
                              [ create_ident "l", ty ]
                          | Right -> 
                              let ty = match vty.desc with
                                | TyOr (_, ty) -> ty
                                | _ -> assert false
                              in
                              [ create_ident "r", ty ]
                          | Pair -> 
                              let ty1,ty2 = match vty.desc with
                                | TyPair (ty1, ty2) -> ty1, ty2
                                | _ -> assert false
                              in
                              [ create_ident "l", ty1 ; 
                                create_ident "r", ty2 ]
                          | Cons -> 
                              let ty = match vty.desc with
                                | TyList ty -> ty
                                | _ -> assert false
                              in
                              [ create_ident "hd", ty 
                              ; create_ident "tl", vty
                              ]
                          | Some -> 
                              let ty = match vty.desc with
                                | TyOption ty -> ty
                                | _ -> assert false
                              in
                              [ create_ident "x", ty ]
  
                          | Nil | None | Bool _ | Constant _ (* int/nat/tz *)
                          | Unit -> []
  
                        in
                        c, 
                        vs, 
                        (prerr_endline ("specialize on " ^ Constructor.to_string c);
                        cc (vs @ os) (specialize ivty c matrix))
                       ) constructors),
  
                    if is_signature then None
                    else Some (prerr_endline "default"; cc (List.tl os) (default ivty matrix))
                   )
            in
            if i = 0 then algo os column
            else begin
              let o', matrix' = swap i os matrix in
              cc o' matrix' (* xxx inefficient *)
            end
  
  let build aty acts guards t = 
    let rec f = function
      | Fail -> assert false (* ? *)
      | Leaf (binders, i) -> 
          List.fold_right (fun (v,(v',ty)) st ->
              mklet (mkp ty v) (mkvar (v',ty)) st) 
            binders (List.nth acts i)
      | Guard (binders, guard, case, otherwise) ->
          List.fold_right (fun (v,(v',ty)) st ->
              mklet (mkp ty v) (mkvar (v',ty)) st)
            binders
          & mke aty & IfThenElse (List.nth guards guard, 
                                  List.nth acts case, 
                                  f otherwise)
      | Switch (_, [], _) -> assert false
      | Switch (v, [Pair, [v1,ty1; v2,ty2], t], None) ->
          let t = f t in
          mklet (mkp ty1 v1) (mkfst & mkvar v)
          & mklet (mkp ty2 v2) (mksnd & mkvar v) t
      | Switch (_, [Unit, [], t], None) -> f t
      | Switch (v, ( [ Left, [vl,tyl], tl
                     ; Right, [vr,tyr], tr ]
                   | [ Right, [vr,tyr], tr 
                     ; Left, [vl,tyl], tl ] ), None) ->
          let tl = f tl in
          let tr = f tr in
          mke aty & Switch_or (mkvar v, 
                               mkp tyl vl, tl,
                               mkp tyr vr, tr)
  
      | Switch (v, ( [(Bool true), [], tt ;
                      (Bool false), [], tf]
                   | [(Bool false), [], tf ;
                      (Bool true), [], tt] ), None) ->
          let tt = f tt in
          let tf = f tf in
          mke aty & IfThenElse (mkvar v, tt, tf)
  
      | Switch (v, ( [Some, [vs,tys], ts;
                      None, [], tn]
                   | [None, [], tn; 
                      Some, [vs,tys], ts]), None) ->
          let ts = f ts in
          let tn = f tn in
          mke aty & Switch_none (mkvar v, tn, mkp tys vs, ts) 
  
      | Switch (v, ( [Cons, [v1,ty1; v2,ty2], tc;
                      Nil, [], tn]
                   | [Nil, [], tn; 
                      Cons, [v1,ty1; v2,ty2], tc]), None) ->
          let tc = f tc in
          let tn = f tn in
          mke aty & Switch_cons (mkvar v, 
                                 mkp ty1 v1, mkp ty2 v2, tc, tn)
  
      | Switch (_v, _cases, None) -> assert false
  
      | Switch (v, cases, Some d) ->
          (* all cases must be about constants with infinite members *)
          if not & List.for_all (function (Cnstr.Constant _, [], _) -> true
                                        | (Constant _, _, _) -> assert false
                                        | (c, _, _) -> 
                                            prerr_endline (Constructor.to_string c);
                                            false) cases
          then assert false;
          List.fold_right (fun case telse ->
              match case with 
              | (Cnstr.Constant c, [], t) ->
                  let t = f t in
                  mke aty & IfThenElse (mkeq (mkvar v) 
                                          (mke (snd v) & Const c),
                                        t, telse)
              | _ -> assert false) cases  & f d
    in
    f t
          
  let compile e (cases : (P.t * t option * t) list) =
  
    (* actions as functions *)
    let acts = 
      List.mapi (fun i (pat, _g, action) -> 
          let v = create_ident (Printf.sprintf "case%d" i) in
          let vars = IdTys.elements & P.vars pat in
          match vars with
          | [] ->
              (* if [vars = []], we need a [fun () ->].
                 Think about the case of [| _ -> assert false].
              *)
              let pvar = mkp Type.tyUnit & create_ident "unit" in
              let f = mkfun pvar action in 
              (v, 
               f,
               mke action.typ (App (mkvar (v, f.typ), [mkunit ()]))) 
          | _ -> 
              let f = List.fold_right (fun (v,ty) st ->
                  mkfun (mkp ty v) st) vars action
              in
              let e = 
                mke action.typ (App (mkvar (v, f.typ), List.map mkvar vars)) 
              in
              (v, f, e)) cases
    in
  
    let cases, guards =
      let cases, guards, _ = 
        List.fold_left (fun (cases, guards, i) case ->
            match case with
            | p, None, e -> (p, None, e)::cases, guards, i
            | p, Some g, e -> (p, Some i, e)::cases, g::guards, i+1)
          ([], [], 0) cases
      in
      List.rev cases, List.rev guards
    in
      
    let v = create_ident "v" in
  
    let typ = (match List.hd cases with (_,_,e) -> e).typ in
  
    (* let casei = fun ... in let v = e in ... *)
    let make x = 
      let match_ = mklet (mkp e.typ v) e x in
      List.fold_right (fun (v, f, _e) st ->
          mklet (mkp f.typ v) f st) acts match_
    in
  
    let matrix : matrix = 
      List.mapi (fun i (pat, g, _) -> 
          { pats=[pat]; guard= g; action= i; bindings= [] }) cases 
    in
  
    let res = cc [(v,e.typ)] matrix in
    Format.eprintf "pmatch debug: %a@." pp_tree res;
  
    let e = build typ (List.map (fun (_,_,e) -> e) acts) guards res in
    Format.eprintf "pmatch debug: %a@." pp e;
    make e
end

let structure str final =

  let rec construct ~loc tyenv exp_type ({Types.cstr_name} as cdesc) args =
    let typ = 
      Result.at_Error (fun e ->
          errorf ~loc "This has type %a.  %a" Printtyp.type_expr exp_type pp_type_expr_error e)
      & type_expr tyenv exp_type 
    in
    let make typ desc = { loc; typ; desc; attrs= [] } in
    match (Ctype.expand_head tyenv exp_type).Types.desc, typ.desc with
    (* bool *)
    | Tconstr (p, [], _), _ when p = Predef.path_bool ->
        make tyBool (match cstr_name with
            | "true" -> Const (C.Bool true)
            | "false" -> Const (C.Bool false)
            | s -> internal_error ~loc "strange bool constructor %s" s)
  
    (* list *)
    | Tconstr (p, [_], _), TyList ty when p = Predef.path_list ->
        begin match cstr_name with
          | "[]" -> make (tyList ty) Nil
          | "::" ->
              begin match args with
                | [e1; e2] ->
                    let e1 = expression e1 in
                    let e2 = expression e2 in
                    (* tyList e1.typ = e2.typ *)
                    make_constant & mkcons e1 e2
                | _ -> internal_error ~loc "strange cons"
              end
          | s -> internal_error ~loc "strange list constructor %s" s
        end
  
    (* option *)
    | Tconstr (p, [_], _), TyOption ty when p = Predef.path_option ->
        begin match cstr_name with
          | "None" -> make (tyOption ty) IML_None
          | "Some" ->
              begin match args with
                | [e1] ->
                    let e1 = expression e1 in
                    make_constant & mksome e1
                | _ -> internal_error ~loc "strange cons"
              end
          | s -> internal_error ~loc "strange list constructor %s" s
        end
  
    (* sum *)
    | Tconstr (p, [_; _], _), TyOr _ when (match Path.is_scaml p with Some "sum" -> true | _ -> false) ->
        let arg = match args with [arg] -> arg | _ -> internal_error ~loc "strange sum arguments" in
        begin match cstr_name with
        | "Left" -> 
            let e = expression arg in
            (* e.typ = ty1 *)
            make_constant & make typ & Left e
        | "Right" ->
            let e = expression arg in
            (* e.typ = ty2 *)
            make_constant & make typ & Right e
        | s -> internal_error ~loc "strange sum constructor %s" s
        end

    | Tconstr (p, _, _), TyUnit when p = Predef.path_unit -> 
        make tyUnit Unit
  
    | Tconstr (p, [], _), TyInt when Path.is_scaml_dot "int" p ->
        make tyInt begin
          let arg = match args with
              | [arg] -> arg
              | _ -> internal_error ~loc "strange Int arguments"
            in
            match arg.exp_desc with
              | Texp_constant (Const_int n) -> Const (Int (Z.of_int n))
              | _ -> errorf ~loc "Int can only take an integer constant"
          end
  
    | Tconstr (p, [], _), TyNat when Path.is_scaml_dot "nat" p ->
        make tyNat begin 
          let arg = match args with
            | [arg] -> arg
            | _ -> internal_error ~loc "strange Nat arguments"
          in
          match arg.exp_desc with
            | Texp_constant (Const_int n) -> 
                if n < 0 then 
                  errorf ~loc "Nat can only take a positive integer constant";
                Const (Nat (Z.of_int n))
            | _ -> errorf ~loc "Nat can only take an integer constant"
        end
  
    | Tconstr (p, [], _), TyMutez when Path.is_scaml_dot "tz" p ->
        make tyMutez begin 
          let arg = match args with
            | [arg] -> arg
            | _ -> internal_error ~loc "strange Tz arguments"
          in
          match arg.exp_desc with
            | Texp_constant (Const_float f) -> 
                begin try
                  let pos = String.index f '.' in
                  let dec = String.sub f 0 pos in
                  let sub = String.sub f (pos+1) (String.length f - pos - 1) in
                  let all_dec s =
                    for i = 0 to String.length s - 1 do
                      match String.unsafe_get s i with
                      | '0'..'9' -> ()
                      | _ -> errorf ~loc "%s: Tz can only take simple decimal floats" f
                    done
                  in
                  all_dec dec;
                  all_dec sub;
                  let sub = 
                    if String.length sub > 6 then 
                      errorf ~loc "%s: the smallest expressive franction of tz is micro" f;
                    
                    sub ^ String.init (6 - String.length sub) (fun _ -> '0')
                  in
                  Const (Nat (Z.of_string (dec ^ sub)))
                with
                | _ -> errorf ~loc "%s: Tz can only take simple decimal floats" f
              end
            | _ -> errorf ~loc "Nat can only take an integer constant"
        end
  
    (* set *)
    | Tconstr (p, [_], _), TySet _ when (match Path.is_scaml p with Some "set" -> true | _ -> false) ->
        (* XXX comparable type check *)
        if cstr_name <> "Set" then internal_error ~loc "strange set constructor";
        begin match args with 
          | [arg] -> 
              let e = expression arg in
              (* tyList ty = e.typ *)
              begin match get_constant e with
              | Some (List xs) -> 
                  (* XXX Uniqueness and sorting? *)
                  { e with typ; desc= Const (Set xs) }
              | None -> errorf ~loc:e.loc "Elements of Set must be constants"
              | Some _ -> assert false
              end
          | _ -> internal_error ~loc "strange set arguments"
        end
  
    (* map *)
    | Tconstr (p, [_; _], _), TyMap _ when (match Path.is_scaml p with Some "map" -> true | _ -> false) ->
        (* XXX comparable type check *)
        if cstr_name <> "Map" then internal_error ~loc "strange map constructor";
        begin match args with 
          | [arg] -> 
              let e = expression arg in
              (* tyList (tyPair (ty1, ty2)) = e.typ *)
              begin match get_constant e with
              | Some (List xs) -> 
                  (* XXX Uniqueness and sorting? *)
                  let xs = List.map (function
                      | C.Pair (c1,c2) -> (c1,c2)
                      | _ -> assert false) xs in
                  { e with typ; desc= Const (Map xs) }
              | Some _ -> assert false
              | None -> errorf ~loc:e.loc "Elements of Map must be constants"
              end
          | _ -> internal_error ~loc "strange map arguments"
        end

    (* C "string" style constants *)
    | Tconstr (p, [], _), _ when Path.is_scaml p <> None ->
        begin match Path.is_scaml p with
          | None -> assert false
          | Some n ->
              match List.assoc_opt n constructions_by_string with
              | None -> 
                  unsupported ~loc "constants of data type %s" (Path.name p)
              | Some (tyname, cname, typ, parse) ->
                  if cstr_name <> cname then internal_error ~loc "strange constructor for %s" tyname;
                  match args with 
                  | [] | _::_::_ ->
                      internal_error ~loc "strange arguments for %s" cname
                  | [arg] -> 
                      let e = expression arg in
                      match e.desc with
                      | Const (String s) -> 
                          begin match parse s with
                            | Ok v -> { e with typ; desc= Const v }
                            | Error s -> errorf ~loc "Parse error of %s string: %s" tyname s
                          end
                      | _ -> errorf ~loc "%s only takes a string literal" cname
        end

    | Tconstr (p, [], _), _ when Path.is_scaml p = None ->
        begin match Env.find_type_descrs p tyenv with
          | [], [] -> 
              errorf ~loc "Abstract data type %s is not supported in SCaml" (Path.name p)
          | [], _::_ -> assert false (* record cannot come here *)
          | _::_, _::_ -> assert false

          | constrs, [] -> 
              let consts, non_consts =
                List.partition (fun constr -> constr.Types.cstr_arity = 0) constrs
              in
              let non_consts_ty =
                match non_consts with
                | [] -> None
                | _ ->
                    let tys_list = 
                      List.map (fun constr ->
                          let ty_args, ty_res = Ctype.instance_constructor constr in
                          Ctype.unify tyenv exp_type ty_res; (* XXX should succeed *)
                          List.map (fun ty -> 
                              Result.at_Error (fun e ->
                                  errorf ~loc "This expression has type %a.  One of its constructors %s has type %a.  %a" 
                                    Printtyp.type_expr exp_type 
                                    constr.cstr_name
                                    Printtyp.type_expr ty
                                    pp_type_expr_error e)
                              & type_expr tyenv exp_type) ty_args) non_consts
                    in
                    let ty_list = List.map (encode_by (fun ty1 ty2 -> tyPair (ty1, ty2))) tys_list in
                    Some (encode_by (fun ty1 ty2 -> tyOr (ty1, ty2)) ty_list)
              in
              let rec find_constr i = function
                | [] -> assert false
                | c::_ when c.Types.cstr_name = cstr_name -> i
                | _::consts -> find_constr (i+1) consts
              in
              match cdesc.cstr_arity, consts, non_consts_ty with
              | _, [], None -> assert false
              | 0, [], _ -> assert false
              | 0, _, None  -> mkint & find_constr 0 consts
              | 0, _, Some ty -> mkleft ty & mkint & find_constr 0 consts
              | _, _, None -> assert false
              | _, _, Some ty ->
                  let i = find_constr 0 non_consts in
                  let sides = Binplace.path i (List.length non_consts) in
                  let arg = encode_by mkpair & List.map expression args in
                  let rec f ty sides = match ty.M.Type.desc, sides with
                    | _, [] -> arg
                    | TyOr (ty1, ty2), Binplace.Left::sides -> mkleft ty2 (f ty1 sides)
                    | TyOr (ty1, ty2), Right::sides -> mkright ty1 (f ty2 sides)
                    | _ -> assert false
                  in
                  match consts with
                  | [] -> f ty sides
                  | _ -> mkright tyInt & f ty sides
        end

    | _ -> prerr_endline ("Constructor compilation failure: " ^ cstr_name); assert false
  
  and expression { exp_desc; exp_loc=loc; exp_type= mltyp; exp_env= tyenv; exp_extra=_; exp_attributes } =
    (* wildly ignores extra *)
    (* if exp_extra <> [] then unsupported ~loc "expression extra"; *)
    begin match attr_has_entry_point exp_attributes with
      | None -> ()
      | Some loc ->
          errorf ~loc "entry declaration is only allowed for the toplevel definitions";
    end;
    let typ = Result.at_Error (fun e ->
        errorf ~loc "This expression has type %a.  %a" Printtyp.type_expr mltyp pp_type_expr_error e)
        & type_expr tyenv mltyp 
    in
    let mk desc = { loc; typ; desc; attrs= [] } in
    match exp_desc with
    | Texp_ident (Path.Pident id, {loc=_}, _vd) -> mk & Var id
    | Texp_ident (p, {loc}, _vd) ->
        begin match Path.is_scaml p with
          | Some "Contract.create_raw" ->
              (* SCaml.Contract.create_raw must be always applied with a string literal.
                 If we see it here, it is not.
              *)
              errorf ~loc "Contract.create_raw must be immediately applied with a string literal"
          | Some n -> mk & primitive ~loc typ n []
          | None -> unsupported ~loc "complex path %s" (Path.xname p)
        end
    | Texp_constant (Const_string (s, None)) -> 
        mk & Const (String s)
    | Texp_constant (Const_string (_, Some _)) -> 
        unsupported ~loc "quoted string"
    | Texp_constant _ -> unsupported ~loc "constant"
    | Texp_tuple [e1; e2] ->
        let e1 = expression e1 in
        let e2 = expression e2 in
        (* tyPair (e1.typ, e2.typ) = typ *) 
        make_constant & mk & Pair (e1, e2)
    | Texp_tuple es ->
        Binplace.fold
          ~leaf:(fun c -> c)
          ~branch:(fun c1 c2 ->
              make_constant & mk & Pair (c1, c2))
          & Binplace.place 
          & List.map expression es
          
    | Texp_construct ({loc}, c, args) -> 
        construct ~loc tyenv mltyp c args
  
    | Texp_assert e ->
        begin match e.exp_desc with
        | Texp_construct (_, {cstr_name="false"}, []) ->
            (* assert false has type 'a *)
            mk AssertFalse
        | _ -> mkassert & expression e
        end
  
    | Texp_let (Recursive, _, _) -> unsupported ~loc "recursion"
    | Texp_let (Nonrecursive, vbs, e) ->
        if not Flags.(!flags.iml_pattern_match) then begin
          (* simple let x = e in e' *)
          let rev_vbs =
            List.fold_left (fun rev_vbs vb -> 
                let _, v, e = value_binding vb in
                (v, e) :: rev_vbs) [] vbs
          in
          let e = expression e in
          List.fold_left (fun e (v,def) ->
              { loc; (* XXX inaccurate *)
                typ= e.typ;
                desc= Let (v, def, e);
                attrs= [] } ) e rev_vbs
        end else begin
          (* let p = e and p' = e' in e''
             =>
             let x = e in match x with p ->
             let x' = e' in match x' with p' -> e''
          *)
          List.fold_right (fun vb e' ->
              let { vb_pat; vb_expr } = vb in
              let vb_expr = expression vb_expr in
              let typ = vb_expr.typ in
              let i = create_ident "x" in
              let x = { desc= i; typ; loc= Location.none; attrs= () } in
              let ex = { desc= Var i; typ; loc= Location.none; attrs= [] } in
              { desc= Let (x, vb_expr, 
                           Pmatch.compile ex [(patternx vb_pat, None, e')])
              ; typ= e'.typ
              ; loc
              ; attrs= [] 
              }
            ) vbs & expression e
        end
  
    | Texp_apply (_, []) -> assert false

    | Texp_apply ({ exp_desc= Texp_ident (p, _, _vd) ; exp_loc= loc_create_raw },
                  (Nolabel, Some { exp_desc= Texp_constant(Const_string (s, _)) }) 
                  :: args ) when Path.is_scaml p = Some "Contract.create_raw" ->
        (* Contract.create_raw <string literal> ... *)
        let args = List.map (function
            | (Nolabel, Some (e: Typedtree.expression)) -> expression e
            | _ -> unsupported ~loc "labeled arguments") args
        in
        let retty = tyPair (tyOperation, tyAddress) in
        let fty =
          List.fold_right (fun arg ty -> tyLambda (arg.typ, ty)) args retty 
        in
        mk & App (
          { desc= Contract_create_raw s;
            loc= loc_create_raw;
            typ= fty;
            attrs= [] },
          args )

    | Texp_apply (f, args) -> 
        let args = List.map (function
            | (Nolabel, Some (e: Typedtree.expression)) -> expression e
            | _ -> unsupported ~loc "labeled arguments") args
        in
        let name = match f with
          | { exp_desc= Texp_ident (p, _, _) } -> Path.is_scaml p
          | _ -> None
        in
        (* only some fixed combinations *)
        begin match name with
          | None -> mk & App (expression f, args)
          | Some n -> 
              let _fty' = 
                List.fold_right (fun arg ty -> tyLambda(arg.typ, ty)) args typ 
              in
              let fty = Result.at_Error (fun e ->
                  errorf ~loc:f.exp_loc "This primitive has type %a.  %a"
                    Printtyp.type_expr f.exp_type
                    pp_type_expr_error e)
                  & type_expr f.exp_env f.exp_type 
              in
              (* fty = fty' *)
              mk & primitive ~loc:f.exp_loc fty n args
        end
  
    | Texp_function { arg_label= (Labelled _ | Optional _) } ->
        unsupported ~loc "labeled arguments"
  
    | Texp_function { arg_label= Nolabel; param=_; cases; partial } ->
        if partial = Partial then errorf ~loc "Pattern match is partial";
        let i = create_ident "x" in
        let targ, _tret = match typ.desc with
          | TyLambda (targ, tret) -> targ, tret
          | _ -> assert false
        in
        let var = { desc= i; typ= targ; loc= Location.none; attrs= () } in
        let evar = { desc= Var i; typ= targ; loc= Location.none; attrs= [] } in
        if not Flags.(!flags.iml_pattern_match) then begin
          mkfun var & switch ~loc evar cases
        end else begin
          (* function case1 | .. | casen
             =>
             fun x -> match x with case1 | .. | casen 
          *)
          let compile_case case = 
            let guard = Option.fmap expression case.c_guard in
            (patternx case.c_lhs, guard, expression case.c_rhs)
          in
          let t = Pmatch.compile evar & List.map compile_case cases in
          mkfun var t
        end
  
    | Texp_ifthenelse (cond, then_, Some else_) -> 
        let econd = expression cond in
        let ethen = expression then_ in
        let eelse = expression else_ in
        (* ignore (unify ethen.typ eelse.typ);
           ignore (unify typ ethen.typ); *)
        mk & IfThenElse (econd, ethen, eelse)
  
    | Texp_match (_, _, e::_, _) -> 
        unsupported ~loc:e.c_lhs.pat_loc "exception pattern"
        
    | Texp_match (_ , _, _, Partial) -> 
        unsupported ~loc "non exhaustive pattern match"
        
    | Texp_match (e , cases, [], Total) -> 
        let e = expression e in
        if not Flags.(!flags.iml_pattern_match) then begin
          switch ~loc e cases
        end else begin
          let compile_case case = 
            let guard = Option.fmap expression case.c_guard in
            (patternx case.c_lhs, guard, expression case.c_rhs)
          in
          Pmatch.compile e (List.map compile_case cases)
        end
    | Texp_ifthenelse (_, _, None) ->
        unsupported ~loc "if-then without else"
    | Texp_try _ -> unsupported ~loc "try-with"
    | Texp_variant _ -> unsupported ~loc "polymorphic variant"

    | Texp_record { fields; extended_expression= None; _ } -> 
        (* I believe fields are already sorted *)
        let es = 
          List.map (fun (_ldesc, ldef) -> match ldef with
              | Overridden (_, e) -> expression e
              | Kept _ -> assert false) & Array.to_list fields
        in
        Binplace.fold
          ~leaf:(fun c -> c)
          ~branch:(fun c1 c2 ->
              make_constant & mkpair c1 c2)
        & Binplace.place es

    | Texp_record { fields; extended_expression= Some e; } ->
        (* optimal code, I hope *)
        (* I believe fields are already sorted *)
        let es = 
          List.map (fun (_ldesc, ldef) -> match ldef with
              | Overridden (_, e) -> Some (expression e)
              | Kept _ -> None) & Array.to_list fields
        in
        let tree = Binplace.place es in
        let rec simplify = function
          | Binplace.Leaf x as t -> t, x = None
          | Branch (t1, t2) -> 
              let t1, b1 = simplify t1 in
              let t2, b2 = simplify t2 in
              if b1 && b2 then Leaf None, true
              else Branch (t1, t2), false
        in
        let rec f e = function
          | Binplace.Leaf None -> e (* no override *)
          | Leaf (Some e) -> e
          | Branch (t1, t2) ->
              let e1 = f (mkfst e) t1 in
              let e2 = f (mksnd e) t2 in
              make_constant & mkpair e1 e2
        in
        f (expression e) & fst & simplify tree

    | Texp_field (e, _, label) ->
        let pos = label.lbl_pos in
        let nfields = Array.length label.lbl_all in
        Format.eprintf "field %d %s of %d @." pos label.lbl_name nfields;
        let e = expression e in
        let rec f e = function
          | [] -> e
          | Binplace.Left :: dirs -> f (mkfst e) dirs
          | Binplace.Right :: dirs -> f (mksnd e) dirs
        in
        f e & Binplace.path pos nfields
    | Texp_setfield _ -> unsupported ~loc "record field set"
    | Texp_array _ -> unsupported ~loc "array"
    | Texp_sequence _ -> unsupported ~loc "sequence"
    | Texp_while _ -> unsupported ~loc "while-do-done"
    | Texp_for _ -> unsupported ~loc "for-do-done"
    | Texp_send _ -> unsupported ~loc "method call"
    | Texp_new _ -> unsupported ~loc "new"
    | Texp_instvar _ -> unsupported ~loc "class instance variable"
    | Texp_setinstvar _ -> unsupported ~loc "class instance variable set"
    | Texp_override _ -> unsupported ~loc "override"
    | Texp_letmodule _ -> unsupported ~loc "let-module"
    | Texp_letexception _ -> unsupported ~loc "let-exception"
    | Texp_lazy _ -> unsupported ~loc "lazy"
    | Texp_object _ -> unsupported ~loc "object"
    | Texp_pack _ -> unsupported ~loc "first class module"
    | Texp_extension_constructor _ -> unsupported ~loc "open variant"
    | Texp_unreachable -> unsupported ~loc "this type of expression"

  and primitive ~loc fty n args =
    match n with
    | "Contract.self" ->
        (* SELF cannot go into LAMBDA.  We have let contract_id = self in .. *)
        assert (args = []);
        Var contract_self_id
    | _ -> 
        match List.assoc_opt n Primitives.primitives with
        | None -> errorf ~loc "Unknown primitive SCaml.%s" n
        | Some (arity, conv) ->
            if arity > List.length args then
              unsupported ~loc "partial application of primitive (here SCaml.%s)" n;
            let args, left = List.split_at arity args in
            match left with
            | [] -> 
                (* Bit tricky.  fty will be unified and its closure info will be
                   modified.  The changes will be fixed when [conv fty] is used
                   in compile.ml
                *)
                Prim (n, conv fty, args)
            | _ -> 
                let typ = 
                  let rec f ty = function
                    | [] -> ty
                    | _arg::args ->
                        match ty.M.Type.desc with
                        | TyLambda (_,ty2) -> f ty2 args
                        | _ -> assert false
                  in
                  f fty args
                in
                App ({ loc; (* XXX inaccurate *)
                       typ;
                       desc= Prim (n, conv fty, args);
                       attrs= [] }, left)
  
  and switch ~loc:loc0 e cases =
    let ty = e.typ in
    let compile_case case = match case.c_guard with
      | Some e -> unsupported ~loc:e.exp_loc "guard"
      | None -> 
          match case.c_lhs.pat_desc with
          | Tpat_construct (_, { cstr_name }, xs) -> cstr_name, xs, expression case.c_rhs
          | _ -> unsupported ~loc:case.c_lhs.pat_loc "non variant pattern"
    in
    let cases = 
      List.sort (fun (n1,_,_) (n2,_,_) -> compare n1 n2) (List.map compile_case cases)
    in
    let mk desc = { desc; loc=loc0; typ= (let _, _, e = List.hd cases in e.typ); attrs= [] } in
    match ty.desc, cases with
    | TyOr (_ty1, _ty2), [("Left",[l],le); ("Right",[r],re)] ->
        let get_var p = match pattern_simple p with [v] -> v | _ -> assert false in
        let lv = get_var l in
        let rv = get_var r in
        mk & Switch_or (e,
                        lv, le,
                        rv, re)
    | TyOr _, _ -> internal_error ~loc:loc0 "sum pattern match"
    | TyList _ty1, [("::",[l1;l2],le); ("[]",[],re)] ->
        let get_var p = match pattern_simple p with [v] -> v | _ -> assert false in
        let lv1 = get_var l1 in
        let lv2 = get_var l2 in
        mk & Switch_cons (e,
                          lv1, lv2, le,
                          re)
    | TyOption _ty1, [("None",[],le); ("Some",[r],re)] ->
        let get_var p = match pattern_simple p with [v] -> v | _ -> assert false in
        let rv = get_var r in
        mk & Switch_none (e,
                          le,
                          rv, re)
    | _, _ -> unsupported ~loc:loc0 "pattern match other than SCaml.sum, list, and option"

  and value_binding { vb_pat; vb_expr; vb_attributes=_; vb_loc=_loc } = 
    (* currently we only handle very simple sole variable pattern *)
    match pattern_simple vb_pat with
    | [v] ->
        let e = expression vb_expr in
        (* ignore & unify v.typ e.typ; *)
        vb_pat.pat_type, v, e
    | _ -> assert false
  
  (* The condition of the entry point is a bit too strict.
     Currently: the last sitem must be an entry point.
     Better: the last value binding must be an entry point.,
  *)
  and structure_item { str_desc; str_loc=loc } =
    match str_desc with
    | Tstr_eval _ -> unsupported ~loc "toplevel evaluation"
    | Tstr_primitive _ -> unsupported ~loc "primitive declaration"
    | Tstr_typext _ -> unsupported ~loc "type extension"
    | Tstr_exception _ -> unsupported ~loc "exception declaration"
    | Tstr_module _ | Tstr_recmodule _ -> unsupported ~loc "module declaration"
    | Tstr_class _ -> unsupported ~loc "class declaration"
    | Tstr_class_type _ -> unsupported ~loc "class type declaration"
    | Tstr_include _ -> unsupported ~loc "include"
    | Tstr_modtype _ -> unsupported ~loc "module type declaration"
  
    | Tstr_value (Recursive, _vbs) -> unsupported ~loc "recursive definitions"
  
    | Tstr_value (Nonrecursive, vbs) ->
        let rev_vbs = 
          List.fold_left (fun rev_vbs vb ->
              let pat_typ,v,b = value_binding vb in
              (pat_typ,v,b)::rev_vbs) [] vbs
        in
        List.rev rev_vbs
  
    | Tstr_open _open_description -> []
  
    | Tstr_type _ -> []

    | Tstr_attribute _ -> 
        (* simply ignores it for now *)
        []
  
  and structure { str_items= sitems } =
    let rev_vbss =
      List.fold_left (fun rev_vbss sitem ->
          let vbs = structure_item sitem in
          vbs :: rev_vbss) [] sitems 
    in
    (* This is if the entry point is alone and at the end *)
    let vbs = List.flatten & List.rev rev_vbss in
    List.fold_right (fun (_,v,b) x ->
        { loc=Location.none; typ= tyUnit; desc= Let (v, b, x); attrs= [] })
      vbs
      final
  in
  structure str

 

  
(* parameter and storage types *)

let toplevel_value_bindings str =
  let structure_item st { str_desc; _ } =
  match str_desc with
    | Tstr_value (Nonrecursive, vbs) ->
        List.rev_append vbs st
    | _ -> st
  in
  let structure { str_items= sitems } =
    List.rev & List.fold_left (fun st sitem ->
        structure_item st sitem) [] sitems
  in
  structure str

let get_explicit_entries vbs =
  List.filter (fun vb -> attr_has_entry_point vb.vb_attributes <> None) vbs

let get_entries vbs =
  match get_explicit_entries vbs with
  | [] -> 
      begin match List.last vbs with
        | None -> []
        | Some vb -> [vb]
      end
  | ents -> ents
    
let type_check_entry templ vb =
  let unify ty ty' =
    let open Ctype in
    let env = vb.vb_pat.pat_env in
    let loc = vb.vb_pat.pat_loc in
    try unify env ty ty' with
    | Unify trace ->
        raise(Typecore.Error(loc, env, Pattern_type_clash(trace)))
    | Tags(l1,l2) ->
        raise(Typetexp.Error(loc, env, Typetexp.Variant_tags (l1, l2)))
    | e -> 
        prerr_endline "unify raised something unfamiliar"; raise e
  in
  unify templ vb.vb_pat.pat_type

let type_check_entries tyenv vbs =
  let ty_storage = Ctype.newvar () in
  let ty_operations = 
    let path =
      try
        Env.lookup_type (*~loc: *)
          (Longident.(Ldot (Lident "SCaml", "operations"))) tyenv
      with
      | Not_found -> internal_error ~loc:Location.none "Type SCaml.operations is not defined.  Something wrong in your SCaml installation."
    in
    Ctype.newconstr path []
  in
  let mk_entry_type () =
    let ty_parameter = Ctype.newvar () in
    let ty_fun = 
      Ctype.newty (Tarrow (Nolabel, ty_parameter,
                           Ctype.newty (Tarrow (Nolabel, ty_storage,
                                                Ctype.newty (Ttuple [ty_operations; ty_storage ]), Cok)), Cok))
    in
    (ty_parameter, ty_fun)
  in
  List.map (fun vb ->
      let ty_param, templ = mk_entry_type () in
      type_check_entry templ vb;
      (ty_param, vb)
    ) vbs, 
  ty_storage

let unite_entries pvbs =
  (* simple balanced binary tree *)
  let rec split vbs = match vbs with
    | [] -> assert false                                                       
    | [ty_param, vb] -> `Leaf (ty_param, vb)
    | vbs ->                    
        let len = List.length vbs in
        let len' = len / 2 in
        let rec take rev_st n xs = match n, xs with
          | 0, _ 
          | _, [] -> List.rev rev_st, xs
          | n, x::xs -> take (x::rev_st) (n-1) xs
        in
        let vbs_l, vbs_r = take [] len' vbs in
        let node_l = split vbs_l in
        let node_r = split vbs_r in
        `Node (node_l, node_r)
  in
  split pvbs

let global_parameter_type tyenv node =
  let path =
    Env.lookup_type (*~loc: *)
      (Longident.(Ldot (Lident "SCaml", "sum"))) tyenv
  in
  let rec f = function
    | `Leaf (param_ty, _) -> param_ty
    | `Node (n1, n2) ->
        let ty1 = f n1 in
        let ty2 = f n2 in
        Ctype.newconstr path [ty1; ty2]
  in
  f node


let check_self ty_self str =
  let selfs = ref [] in
  let record_self e = selfs := e :: !selfs in
  let module X = TypedtreeIter.MakeIterator(struct
      include TypedtreeIter.DefaultIteratorArgument
      let enter_expression e = match e.exp_desc with
        | Texp_ident (p, {loc=_}, _vd) ->
            begin match Path.is_scaml p with
              | Some "Contract.self" -> record_self e
              | _ -> ()
            end
        | _ -> ()
    end)
  in
  X.iter_structure str;

  (* This tries to instantiate Contract.self's type to ty_parameter contract,
     but this does not fully work since we have polymorphism:
       let self = Contract.self

     Only a good way is to add a type constraint around Contract.self
     as (Contract.self : ty_parameter contract) and then retype it,
     which makes the compilation path more complex...
  *)

  List.iter (fun self ->
    let unify ty ty' =
      let open Ctype in
      let tyenv = self.exp_env in
      let loc = self.exp_loc in
      try unify tyenv ty ty' with
      | Unify trace ->
          raise(Typecore.Error(loc, tyenv, Expr_type_clash(trace, None)))
      | Tags(l1,l2) ->
          raise(Typetexp.Error(loc, tyenv, Typetexp.Variant_tags (l1, l2)))
      | e -> 
          prerr_endline "unify raised something unfamiliar"; raise e
    in
    match (Ctype.expand_head self.exp_env self.exp_type).Types.desc with
    | Tconstr (p, [t], _) when Path.is_scaml p =  Some "contract" ->
        if t.level = Btype.generic_level then
          errorf ~loc:self.exp_loc "Contract.self cannot have a generic type, but it has type %a.  Please use a type constraint to instantiate its type." Printtyp.type_scheme self.exp_type;
        unify self.exp_type ty_self
    | _ -> assert false
    ) !selfs


let compile_global_entry ty_storage ty_return node =

  let id_storage = Ident.create "storage" in
  let mk desc typ = { desc; typ; loc= Location.none; attrs= [] } in
  let mk_var id typ =  mk (Var id) typ in
  let mk_pat desc typ = { desc; typ; loc= Location.none; attrs= () } in

  let pat_storage = mk_pat id_storage ty_storage in
  let e_storage = mk_var id_storage ty_storage in

  let rec f param_id node = match node with
    | `Leaf (_,vb) ->
        (* XXX top entry has poor pattern expressivity *)
        let id, var = match pattern_simple vb.vb_pat with
          | [p] -> p.desc, mk_var p.desc p.typ
          | _ -> assert false
        in
        let param_type = match var.typ with
          | { desc= TyLambda (t1, _); _ } -> t1
          | _ -> assert false
        in
        let param_var = mk_var param_id param_type in
        Attr.add (Attr.Comment ("entry " ^ Ident.name id))
        & mk (App (var, [param_var; e_storage])) ty_return,
        param_type

    | `Node (n1, n2) ->
        let id_l = Ident.create "l" in
        let id_r = Ident.create "r" in
        let e_l, param_typ_l = f id_l n1 in
        let e_r, param_typ_r = f id_r n2 in
        let pat_l = mk_pat id_l param_typ_l  in
        let pat_r = mk_pat id_r param_typ_r  in
        let param_typ = tyOr (param_typ_l, param_typ_r) in
        let param_var = mk_var param_id param_typ in
        mk (Switch_or (param_var, pat_l, e_l, pat_r, e_r)) ty_return,
        param_typ
  in
  let param_id = Ident.create "global_param" in
  let e, param_typ = f param_id node in
  let param_pat = mk_pat param_id param_typ in
  let f1 = mkfun pat_storage e in
  mkfun param_pat f1

module VMap = Map.Make(struct type t = Ident.t let compare = compare end)

let count_variables t = 
  let incr v st = match VMap.find_opt v st with
    | None -> VMap.add v 1 st
    | Some n -> VMap.add v (n+1) st
  in
  let rec f t st = match t.desc with
    | Var id -> incr id st

    | Contract_create_raw _
    | Const _ | Nil | IML_None | Unit | AssertFalse -> st

    | IML_Some t | Left t | Right t | Assert t
    | Fun (_, t) -> f t st

    | Let (_, t1, t2) | Cons (t1, t2) | Pair (t1, t2) -> f t1 & f t2 st

    | IfThenElse (t1, t2, t3) 
    | Switch_or (t1, _, t2, _, t3)
    | Switch_cons (t1, _, _, t2, t3)
    | Switch_none (t1, t2, _, t3) -> f t1 & f t2 & f t3 st

    | App (t, ts) -> List.fold_right f (t::ts) st
    | Prim (_, _, ts) -> List.fold_right f ts st
  in
  f t VMap.empty

(* t2[t1/id] *)
let subst id t1 t2 =
  let rec f t = 
    let mk desc = { t with desc } in
    match t.desc with
    | Var id' when id = id' -> 
        Attr.adds t.attrs t1 (* t1 never contains id *)
    | Var _ | Const _ | Nil | IML_None | Unit | AssertFalse 
    | Contract_create_raw _ -> t

    | IML_Some t -> mk & IML_Some (f t)
    | Left t -> mk & Left (f t)
    | Right t -> mk & Right (f t)
    | Assert t -> mk & Assert (f t)
    | Fun (pat, t) -> mk & Fun (pat, f t)
    | Let (p, t1, t2) -> mk & Let (p, f t1, f t2)
    | Cons (t1, t2) -> mk & Cons (f t1, f t2)
    | Pair (t1, t2) -> mk & Pair (f t1, f t2)
    | IfThenElse (t1, t2, t3) -> mk & IfThenElse (f t1, f t2, f t3)
    | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (f t1, p1, f t2, p2, f t3)
    | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (f t1, p1, p2, f t2, f t3)
    | Switch_none (t1, t2, p, t3) -> mk & Switch_none (f t1, f t2, p, f t3)
    | App (t, ts) -> mk & App (f t, List.map f ts)
    | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
  in
  f t2

(* 
   (fun x -> e1) e2  =>  let x = e2 in e1 
   let x = e2 in e1  =>  e1[e2/x]  when x appears only once in e1
   
   Free variables are counted for each let (and fun).  Very inefficient.
*)
let optimize t = 
  let rec f t = 
    let attrs = ref (Some t.attrs) in
    let add_attrs t' = 
      match !attrs with
      | Some as_ ->
          attrs := None;
          { t' with attrs= as_ @ t'.attrs } 
      | None -> assert false
    in
    let mk desc = add_attrs & { t with desc; attrs= [] } in
    let res = match t.desc with
      | App (t, []) -> add_attrs & f t 
      | App (u, t::ts) -> 
          let t = f t in
          let ts = List.map f ts in
          begin match f u with
          | {desc= Fun (pat, body); typ= {desc= TyLambda (_, ty2)} } ->
              f & mk & App ({ desc= Let (pat, t, body);
                              loc= t.loc; (* incorrect *)
                              typ= ty2;
                              attrs= [] },
                            ts)
          | u -> mk & App (u, t::ts)
          end
      | Let (p, t1, t2) -> 
          let vmap = count_variables t2 in
          begin match VMap.find_opt p.desc vmap with
            | None -> add_attrs & f t2
            | Some 1 when p.desc <> contract_self_id -> 
                (* contract_self_id must not be inlined into LAMBDAs *)
                (* XXX This is adhoc *)
                add_attrs 
                & f & subst p.desc (Attr.add (Attr.Comment ("= " ^ Ident.name p.desc)) t1) t2
            | _ -> mk & Let (p, f t1, f t2)
          end
      | Var _ | Const _ | Nil | IML_None | Unit | AssertFalse | Contract_create_raw _ -> 
          add_attrs { t with attrs= [] }
      | IML_Some t -> mk & IML_Some (f t)
      | Left t -> mk & Left (f t)
      | Right t -> mk & Right (f t)
      | Assert t -> mk & Assert (f t)
      | Fun (c, t) -> mk & Fun (c, f t)
      | Cons (t1, t2) -> mk & Cons (f t1, f t2)
      | Pair (t1, t2) -> mk & Pair (f t1, f t2)
      | IfThenElse (t1, t2, t3) -> mk & IfThenElse (f t1, f t2, f t3)
      | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (f t1, p1, f t2, p2, f t3)
      | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (f t1, p1, p2, f t2, f t3)
      | Switch_none (t1, t2, p, t3) -> mk & Switch_none (f t1, f t2, p, f t3)
      | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
    in
    begin match !attrs with
      | Some _ -> assert false
      | None -> ()
    end;
    res
  in
  f t

let add_self self_typ t =
  (* let __contract_id = SELF in t *)
  (* This variable must not be inlined *)
  mklet 
    { desc= contract_self_id; typ= self_typ; loc= Location.none; attrs= () }
    (mke self_typ & Prim ("Contract.self", (fun os -> M.Opcode.SELF :: os), []))
    t

let implementation sourcefile str = 
  let attrs = Attribute.get_scaml_toplevel_attributes str in
  List.iter (fun ({txt=k},v) ->
      Format.eprintf "attr %s=%s@."
        (String.concat "." & Longident.flatten k)
        (match v with
         | `Bool b -> Printf.sprintf "%b" b
         | `Constant c -> 
             match c with
             | Parsetree.Pconst_integer (s, None)
             | Pconst_float (s, None) -> s
             | Pconst_integer (s, Some c)
             | Pconst_float (s, Some c) ->  s ^ String.make 1 c
             | Pconst_char c -> Printf.sprintf "%c" c (*XXX *)
             | Pconst_string (s, None) -> Printf.sprintf "%S" s
             | Pconst_string (s, Some t) -> Printf.sprintf "{%s|%s|%s}" t s t
        )
    ) attrs;
  Flags.flags := List.fold_left Flags.eval !Flags.flags attrs;
  let vbs = toplevel_value_bindings str in
  match get_entries vbs with
  | [] -> 
      errorf ~loc:(Location.in_file sourcefile)
        "SCaml needs at least one value definition for an entry point"
  | vbs ->
      let tyenv = str.str_final_env in
      let pvbs, ty_storage = type_check_entries str.str_final_env vbs in
      let node = unite_entries pvbs in

      (* self *)
      let ty_param = global_parameter_type tyenv node in
      let self_type = 
        let path =
          Env.lookup_type (*~loc: *)
            (Longident.(Ldot (Lident "SCaml", "contract"))) tyenv
        in
        Ctype.newconstr path [ty_param]
      in
      check_self self_type str;

      let ty_operations = 
        let ty_operations = 
          let path =
            Env.lookup_type (*~loc: *)
              (Longident.(Ldot (Lident "SCaml", "operations"))) str.str_final_env
          in
          Ctype.newconstr path []
        in
        Result.at_Error (fun e ->
            errorf ~loc:(Location.in_file sourcefile) 
              "SCaml.operations failed to be converted to Michelson type: %a"
              pp_type_expr_error e)
          & type_expr tyenv ty_operations
      in

      let ty_storage = 
        Result.at_Error (fun e ->
            errorf ~loc:(Location.in_file sourcefile) "Contract has storage type %a.  %a"
              Printtyp.type_expr ty_storage
              pp_type_expr_error e)
          & type_expr tyenv ty_storage 
      in
      let ty_return = tyPair (ty_operations, ty_storage) in
      let final = compile_global_entry ty_storage ty_return node in
      let ty_param = 
        Result.at_Error (fun e ->
            errorf ~loc:(Location.in_file sourcefile) "Contract has parameter type %a.  %a"
              Printtyp.type_expr ty_param
              pp_type_expr_error e)
          & type_expr tyenv ty_param 
      in
      let ty_param = 
        match node with
        | `Leaf _ (* sole entry point *) -> ty_param
        | `Node _ ->
            let open M.Type in
            let rec add_annot ty node = match ty.desc, node with
              | _, `Leaf (_,vb) -> 
                  (* XXX dup *)
                  let id = match pattern_simple vb.vb_pat with
                    | [p] -> p.desc
                    | _ -> assert false
                  in
                  let fix_name s = match s with
                    | "" -> assert false
                    | _ ->
                        if String.unsafe_get s (String.length s - 1) = '_' then
                          String.sub s 0 (String.length s - 1)
                        else
                          s
                  in
                  { ty with attrs= [ "@" ^ fix_name (Ident.name id) ] }
              | TyOr (ty1, ty2), `Node (n1, n2) ->
                  let ty1 = add_annot ty1 n1 in
                  let ty2 = add_annot ty2 n2 in
                  { ty with desc= TyOr (ty1, ty2) }
              | _, `Node _ -> 
                  Format.eprintf "Entry point type: %a@." M.Type.pp ty;
                  assert false
            in
            add_annot ty_param node
      in
      ty_param, 
      ty_storage, 
      add_self (tyContract ty_param) & structure str final

let save path t = 
  let oc = open_out path in
  let ppf = Format.of_out_channel oc in
  Format.fprintf ppf "%a@." pp t;
  close_out oc
