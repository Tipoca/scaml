(* InterMediate Language, or Intermediate ML *)
open Spotlib.Spot
open Asttypes
open Typedtree
open Tools

module M = Michelson
open M.Type
module C = M.Constant

let repr_desc ty = (Ctype.repr ty).desc

let dummy_loc = 
  { Location.loc_start= Lexing.dummy_pos; loc_end= Lexing.dummy_pos; loc_ghost= true }   

type ('desc, 'attr) with_loc_and_type =
  { desc : 'desc
  ; loc  : Location.t
  ; typ  : M.Type.t
  ; attr : 'attr
  }

type var = Ident.t

type constr = CLeft | CRight | CSome | CNone | CCons | CNil | CUnit | CBool of bool | CPair | CConstant of Michelson.Constant.t

let string_of_constr = function
  | CPair -> "(,)"
  | CLeft -> "Left"
  | CRight -> "Right"
  | CSome -> "Some"
  | CNone -> "None"
  | CCons -> "(::)"
  | CNil -> "[]"
  | CUnit -> "()"
  | CBool true -> "true"
  | CBool false -> "false"
  | CConstant c -> Format.sprintf "%a" C.pp c

module IdTys = Set.Make(struct type t = Ident.t * M.Type.t let compare (id1,_) (id2,_) = compare id1 id2 end)
  

module Pat = struct
  type desc =
    | Var of var
    | Constr of constr * t list
    | Wild
    | Alias of t * Ident.t * Location.t (* location of ident *)
    | Or of t * t 

  and t = (desc, unit) with_loc_and_type

  let rec pp ppf pat = 
    let open Format in
    match pat.desc with
    | Var i -> fprintf ppf "%s" (Ident.name i)
    | Constr (c, []) -> fprintf ppf "%s" (string_of_constr c)
    | Constr (c, ps) -> fprintf ppf "@[%s (%a)@]" (string_of_constr c) (Format.list ",@ " pp) ps
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

type attr = 
  | Comment of string

type attrs = attr list

let add_attr a t = { t with attr= a :: t.attr }

type t = (desc, attrs) with_loc_and_type

and desc =
  | Const of M.Opcode.constant
  | Nil of M.Type.t
  | Cons of t * t
  | IML_None of M.Type.t
  | IML_Some of t
  | Left of M.Type.t * t
  | Right of M.Type.t * t
  | Unit
  | Var of Ident.t * M.Type.t
  | Pair of t * t
  | Assert of t
  | AssertFalse
  | Fun of M.Type.t * M.Type.t * P.var * t
  | IfThenElse of t * t * t
  | App of t * t list
  | Prim of string * (M.Opcode.t list -> M.Opcode.t list) * t list
  | Let of P.var * t * t
  | Switch_or of t * P.var * t * P.var * t
  | Switch_cons of t * P.var * P.var * t * t
  | Switch_none of t * t * P.var * t
  | Match of t * (P.t * t option * t) list (* XXX should be removed from t *)

let almost_constant t = 
  match t.desc with
  | Const c -> Some c
  | Nil ty when ty.desc <> TyOperation -> 
     (* We cannot PUSH (list operation) {}. We get
        "operation type forbidden in parameter, storage and constants" *)
      Some (C.List [])
  | IML_None _ -> Some (C.Option None)
  | Unit -> Some C.Unit
  | _ -> None

let make_constant t = 
  match
    let (>>=) = Option.bind in
    match t.desc with
    | Const c -> Some c
    | Nil _ | IML_None _ | Unit -> None (* let's keep it *)
    | Cons (t1, t2) ->
        begin almost_constant t1 >>= fun c1 ->
          almost_constant t2 >>= function 
          |C.List c2 -> Some (C.List (c1 :: c2))
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
  let open Result.Infix in
  match t.desc with
  | Unit -> Ok C.Unit
  | Const c -> Ok c
  | IML_None _ -> Ok (C.Option None)
  | IML_Some t -> get_constant t >>= fun c -> Ok (C.Option (Some c))
  | Left (_, t) -> get_constant t >>= fun t -> Ok (C.Left t)
  | Right (_, t) -> get_constant t >>= fun t -> Ok (C.Right t)
  | Pair (t1, t2) -> get_constant t1 >>= fun t1 -> get_constant t2 >>= fun t2 -> Ok (C.Pair (t1, t2))
  | Nil _ -> Ok (C.List [])
  | Cons (t1, t2) -> get_constant t1 >>= fun t1 -> get_constant t2 >>= fun t2 ->
      begin match t2 with
        | C.List t2 -> Ok (C.List (t1::t2))
        | _ -> assert false
      end
  | _ -> Error t.loc

let pp ppf = 
  let p = Format.pp_print_string ppf in
  let f fmt = Format.fprintf ppf fmt in
  let rec pp _ppf t =
    f "%a %a" desc t attr t.attr

  and attr _ppf attrs = 
    List.iter (function
        | Comment s -> f "/* %s */" s) attrs
                         
  and desc _ppf t = match t.desc with
    | Const c -> f "(%a : %a)" C.pp c M.Type.pp t.typ
    | Nil ty -> f "([] : %a)" M.Type.pp ty
    | Cons (t1, t2) -> f "(%a :: %a)" pp t1 pp t2
    | IML_None ty -> f "(None : %a)" M.Type.pp ty
    | IML_Some t -> f "(Some %a)" pp t
    | Left (ty, t) -> f "Left (%a) (%a)" M.Type.pp ty pp t
    | Right (ty, t) -> f "Right (%a) (%a)" M.Type.pp ty pp t
    | Unit -> p "()"
    | Var (id, _) -> f "%s" (Ident.name id)
    | Pair (t1, t2) -> f "(%a, %a)" pp t1 pp t2
    | Assert t -> f "assert (%a)" pp t
    | AssertFalse -> p "assert false"
    | Fun (_ty1, _ty2, pat, body) ->
        f "@[<2>(fun (%a) ->@ %a@ : %a)@]"
          P.pp_var pat pp body M.Type.pp t.typ
    | IfThenElse (t1, t2, t3) -> 
        f "(@[if %a@ then %a@ else %a@])"
          pp t1 pp t2 pp t3
    | App (t1, ts) -> 
        f "(%a %a)" pp t1 Format.(list " " (fun ppf t -> fprintf ppf "(%a)" pp t)) ts
    | Prim (n, _ops, ts) ->
        f "(%s %a)" 
          n
          Format.(list " " (fun ppf t -> fprintf ppf "(%a)" pp t)) ts
    | Let (p, t1, t2) ->
        f "@[<2>(let %a =@ %a in@ %a)@]"
          P.pp_var p pp t1 pp t2
    | Switch_or (t, p1, t1, p2, t2) ->
        f "@[<2>(match %a with@ | Left %a -> %a@ | Right %a -> %a)@]"
          pp t
          P.pp_var p1 pp t1 
          P.pp_var p2 pp t2
    | Switch_cons (t, p1, p2, t1, t2) ->
        f "@[<2>(match %a with@ | %a::%a -> %a@ | [] -> %a)@]"
          pp t
          P.pp_var p1
          P.pp_var p2
          pp t1 
          pp t2
    | Switch_none (t, t1, p2, t2) ->
        f "@[<2>(match %a with@ | None -> %a@ | Some (%a) -> %a)@]"
          pp t
          pp t1 
          P.pp_var p2 pp t2
    | Match (t, cases) ->
        f "@[<2>(match %a with@ @[%a@])@]"
          pp t
          (Format.list "@ | "
             (fun ppf (p, guard, e) -> 
                match guard with
                | None -> Format.fprintf ppf "@[<2>%a ->@ %a@]"
                            P.pp p
                            pp e
                | Some g -> Format.fprintf ppf "@[<2>%a when %a ->@ %a@]"
                            P.pp p
                            pp g
                            pp e
             )) cases
  in
  pp ppf

let mke typ desc = { typ; desc; loc= dummy_loc; attr= [] }

let mkfst e =
  let ty = match e.typ.desc with
    | TyPair (ty, _) -> ty
    | _ -> assert false
  in
  let prim = snd (List.assoc "fst" Primitives.primitives) e.typ (* XXX wrong. this should be e.typ -> ty *) in
  mke ty (Prim ("fst", prim, [e]))

let mksnd e =
  let ty = match e.typ.desc with
    | TyPair (_, ty) -> ty
    | _ -> assert false
  in
  let prim = snd (List.assoc "snd" Primitives.primitives) e.typ (* XXX wrong. this should be e.typ -> ty *) in
  mke ty (Prim ("snd", prim, [e]))

let mkleft ty e = mke (tyOr (e.typ, ty)) (Left (ty, e))
let mkright ty e = mke (tyOr (ty, e.typ)) (Right (ty, e))

let mkeq e1 e2 =
  let prim = snd (List.assoc "=" Primitives.primitives) 
             & tyLambda (e1.typ, tyLambda (e2.typ, tyBool)) in
  mke tyBool (Prim ("=", prim, [e1; e2]))

let mkpair e1 e2 = mke (tyPair (e1.typ, e2.typ)) (Pair (e1, e2))

let mkppair p1 p2 = 
  { loc=dummy_loc; desc= P.Constr (CPair, [p1; p2]); typ= tyPair (p1.typ, p2.typ); attr= () }


let mkint n = mke tyInt (Const (M.Constant.Int (Z.of_int n)))

let mkpint n = 
  { desc= P.Constr (CConstant (Michelson.Constant.Int (Z.of_int n)), []);
    typ= tyInt;
    loc= dummy_loc;
    attr= () }

let mkpleft ty p = 
  { desc= P.Constr (CLeft, [p]);
    typ= tyOr (p.typ, ty);
    loc= dummy_loc;
    attr= () }

let mkpright ty p = 
  { desc= P.Constr (CRight, [p]);
    typ= tyOr (ty, p.typ);
    loc= dummy_loc;
    attr= () }

let rec freevars t = 
  let open IdTys in
  let psingleton p = singleton (p.desc, p.typ) in
  match t.desc with
  | Const _ | Nil _  | IML_None _ | Unit -> empty
  | Cons (t1,t2) | Pair (t1,t2) -> union (freevars t1) (freevars t2)
  | Left (_,t) | Right (_,t) | IML_Some t | Assert t -> freevars t
  | AssertFalse -> empty
  | Var (id,ty) -> singleton (id,ty)
  | IfThenElse (t1,t2,t3) -> union (freevars t1) (union (freevars t2) (freevars t3))
  | App (t,ts) ->
      List.fold_left (fun acc t -> union acc (freevars t)) empty (t::ts)
  | Prim (_,_,ts) ->
      List.fold_left (fun acc t -> union acc (freevars t)) empty ts
  | Fun (_,_,pat,t) -> 
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
  | Match (t, cases) ->
      union (freevars t)
        (List.fold_left (fun acc (p, g, t) ->
             union acc 
               (diff (union 
                        (freevars t) 
                        (match g with 
                         | None -> empty
                         | Some g -> freevars g)) (P.vars p)))
            empty cases)

type type_expr_error =
  | Type_variable of Types.type_expr
  | Unsupported_type of Types.type_expr
  | Unsupported_data_type of Path.t

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
        | Some "Set.t"       , [ty]       -> Ok (tySet ty)
        | Some "Map.t"       , [ty1; ty2] -> Ok (tyMap (ty1, ty2))
        | Some "BigMap.t"    , [ty1; ty2] -> Ok (tyBigMap (ty1, ty2))
        | Some "Operation.t" , []         -> Ok (tyOperation)
        | Some "Contract.t"  , [ty]       -> Ok (tyContract ty)
        | Some "Timestamp.t" , []         -> Ok (tyTimestamp)
        | Some "Address.t"   , []         -> Ok (tyAddress)
        | Some "Key.t"       , []         -> Ok (tyKey)
        | Some "Signature.t" , []         -> Ok (tySignature)
        | Some "Key_hash.t"  , []         -> Ok (tyKeyHash)
        | Some "Bytes.t"     , []         -> Ok (tyBytes)
        | Some "Chain_id.t"  , []         -> Ok (tyChainID)
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

let type_expr ~loc ?(this="This") tyenv ty = match type_expr tyenv ty with
  | Ok x -> x
  | Error (Type_variable ty') -> 
      errorf ~loc "%s has type %a, which has too generic type %a for SCaml."
        this
        Printtyp.type_expr ty
        Printtyp.type_expr ty'
  | Error (Unsupported_type ty') ->
      errorf ~loc "%s has type %a, which has unsupported type %a in SCaml"
        this
        Printtyp.type_expr ty
        Printtyp.type_expr ty'
  | Error (Unsupported_data_type p) ->
      errorf ~loc "%s has type %a, which has unsupported data type %s in SCaml"
        this
        Printtyp.type_expr ty
        (Path.name p)

(* Literals *)

let parse_timestamp s =
  match Ptime.of_rfc3339 s with
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
  [ ("Signature.t" , ("signature", "Signature", tySignature, 
                      fun x -> Ok (C.String x)));
    ("Key_hash.t"  , ("key_hash", "Key_hash", tyKeyHash, 
                      fun x -> Ok (C.String x)));
    ("Key.t"       , ("key", "Key", tyKey, 
                      fun x -> Ok (C.String x)));
    ("Address.t"   , ("address", "Address", tyAddress, 
                      fun x -> Ok (C.String x)));
    ("Timestamp.t" , ("timestamp", "Timestamp", tyTimestamp, 
                      parse_timestamp));
    ("Bytes.t"     , ("bytes", "Bytes", tyBytes, 
                      parse_bytes));
    ("Chain_id.t"  , ("chain_id", "Chain_id", tyChainID,
                      fun x -> Ok (C.String x)))
  ]

(* This is old function which must be removed once
   [let] and [function] use [patternx] *)
let pattern { pat_desc; pat_loc=loc; pat_type= mltyp; pat_env= tyenv } = 
  let typ = type_expr ~loc tyenv mltyp in
  let mk loc id typ = { loc; desc=id; typ; attr= () } in
  let mk_dummy loc typ = { loc; desc=Ident.dummy; typ; attr= () } in
  match pat_desc with
  | Tpat_var (id, {loc}) -> [mk loc id typ]

  | Tpat_alias ({pat_desc = Tpat_any; pat_loc=_}, id, _) -> 
      (* We transform (_ as x) in x if _ and x have the same location.
         The compiler transforms (x:t) into (_ as x : t).
         This avoids transforming a warning 27 into a 26.
       *)
      [mk loc id typ]

  | Tpat_any         -> [mk_dummy loc typ]

  | Tpat_alias _     -> unsupported ~loc "alias pattern"
  | Tpat_constant _  -> unsupported ~loc "constant pattern"
  | Tpat_tuple _     -> unsupported ~loc "tuple pattern"

  | Tpat_construct ({loc}, _, []) when typ.desc = TyUnit -> [mk_dummy loc typ]

  | Tpat_construct _ -> unsupported ~loc "variant pattern"
  | Tpat_variant _   -> unsupported ~loc "polymorphic variant pattern"
  | Tpat_record _    -> unsupported ~loc "record pattern"
  | Tpat_array _     -> unsupported ~loc "array pattern"
  | Tpat_or _        -> unsupported ~loc "or pattern"
  | Tpat_lazy _      -> unsupported ~loc "lazy pattern"

let rec patternx { pat_desc; pat_loc=loc; pat_type= mltyp; pat_env= tyenv } = 
  let typ = type_expr ~loc tyenv mltyp in
  let mk desc = { loc; desc; typ; attr= () } in
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

  | Tpat_tuple [p1; p2] -> mk (P.Constr (CPair, [patternx p1; patternx p2]))

  | Tpat_tuple ps -> encode_by mkppair & List.map patternx ps

  | Tpat_construct (_, _, []) when typ.desc = TyUnit -> mk P.Wild

  | Tpat_constant (Const_string (s, None))  -> 
      mk (P.Constr (CConstant (C.String s), []))
                                                 
  | Tpat_constant (Const_string (_, Some _))  -> unsupported ~loc "quoted string"

  | Tpat_constant _ -> unsupported ~loc "constant pattern of type %s"
                         (Format.sprintf "%a" Printtyp.type_scheme mltyp)

  | Tpat_or (p1, p2, None)   -> mk & P.Or (patternx p1, patternx p2)

  | Tpat_or (_, _, _)        -> unsupported ~loc "or pattern with row"

  | Tpat_construct (_, cdesc, ps) ->
      
      (* XXX should check the module path *)
      begin match cdesc.cstr_name, typ.desc, ps with
        | "Left", TyOr _, [p] -> mk (P.Constr (CLeft, [patternx p]))
        | "Right", TyOr _, [p] -> mk (P.Constr (CRight, [patternx p]))
        | "Some", TyOption _, [p] -> mk (P.Constr (CSome, [patternx p]))
        | "None", TyOption _, [] -> mk (P.Constr (CNone, []))
        | "::", TyList _, [p1; p2] -> mk (P.Constr (CCons, [patternx p1; patternx p2]))
        | "[]", TyList _, [] -> mk (P.Constr (CNil, []))
        | "true", TyBool, [] -> mk (P.Constr (CBool true, []))
        | "false", TyBool, [] -> mk (P.Constr (CBool false, []))
        | "Int", TyInt, [{pat_desc= Tpat_constant (Const_int n)}] ->
            mk & P.Constr (CConstant (C.Int (Z.of_int n)), [])
        | "Int", TyInt, [_] -> errorf ~loc "Int can only take an integer constant"
        | "Nat", TyNat, [{pat_desc= Tpat_constant (Const_int n)}] ->
            if n < 0 then 
              errorf ~loc "Nat can only take a positive integer constant";
            mk & P.Constr (CConstant (C.Nat (Z.of_int n)), [])
        | "Nat", TyNat, [_] -> errorf ~loc "Nat can only take an integer constant"
        | _, TyMutez, [_] -> errorf ~loc "tz constant cannot be used as a pattern"
        | "Key_hash", TyKeyHash, [{pat_desc= Tpat_constant (Const_string (s, None))}] ->
            mk & P.Constr (CConstant (C.String s), [])
        | "Key_hash", TyKeyHash, [{pat_desc= Tpat_constant (Const_string (_, Some _))}] -> unsupported ~loc "quoted string"
        | "Key_hash", TyKeyHash, [_] -> unsupported ~loc "Key_hash can only take a string constant"


        | "Address", TyAddress, [{pat_desc= Tpat_constant (Const_string (s, None))}] ->
            mk & P.Constr (CConstant (C.String s), [])
        | "Address", TyAddress, [{pat_desc= Tpat_constant (Const_string (_, Some _))}] -> unsupported ~loc "quoted string"
        | "Address", TyAddress, [_] -> unsupported ~loc "Address can only take a string constant"


        | "Timestamp", TyTimestamp, [{pat_desc= Tpat_constant (Const_string (s, None))}] ->
            begin match parse_timestamp s with
              | Error _e -> errorf ~loc "straonge arguments for Timestamp" 
              | Ok t -> mk & P.Constr (CConstant t, [])
            end
        | "Timestamp", TyTimestamp, [{pat_desc= Tpat_constant (Const_string (_, Some _))}] -> unsupported ~loc "quoted string"
        | "Timestamp", TyTimestamp, [_] -> unsupported ~loc "Timestamp can only take a string constant"

        | "Bytes", TyBytes, [{pat_desc= Tpat_constant (Const_string (s, None))}] ->
            begin match parse_bytes s with
              | Error _e -> errorf ~loc "straonge arguments for Bytes" 
              | Ok t -> mk & P.Constr (CConstant t, [])
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
                              let tys_list = List.map (List.map (type_expr ~loc tyenv)) tys_list in
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
                  | (_, typ)::labels, _ ->
                      let typ = type_expr ~loc:dummy_loc tyenv typ in
                      { loc; desc= P.Wild; typ; attr= () } :: f labels pfields
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
  
let structure env str final =

  let rec construct ~loc env tyenv exp_type ({Types.cstr_name} as cdesc) args =
    let make typ desc = { loc; typ; desc; attr= [] } in
    match (Ctype.expand_head tyenv exp_type).Types.desc with
    (* bool *)
    | Tconstr (p, [], _) when p = Predef.path_bool ->
        make tyBool (match cstr_name with
            | "true" -> Const (C.Bool true)
            | "false" -> Const (C.Bool false)
            | s -> internal_error ~loc "strange bool constructor %s" s)
  
    (* list *)
    | Tconstr (p, [ty], _) when p = Predef.path_list ->
        begin match cstr_name with
          | "[]" -> 
              let ty = type_expr ~loc tyenv ty in
              make (tyList ty) (Nil ty)
          | "::" ->
              begin match args with
                | [e1; e2] ->
                    let e1 = expression env e1 in
                    let e2 = expression env e2 in
                    (* tyList e1.typ = e2.typ *)
                    make_constant & make e2.typ & Cons (e1, e2)
                | _ -> internal_error ~loc "strange cons"
              end
          | s -> internal_error ~loc "strange list constructor %s" s
        end
  
    (* option *)
    | Tconstr (p, [ty], _) when p = Predef.path_option ->
        begin match cstr_name with
          | "None" -> 
              let ty = type_expr ~loc tyenv ty in
              make (tyOption ty) (IML_None ty)
          | "Some" ->
              begin match args with
                | [e1] ->
                    let e1 = expression env e1 in
                    make_constant & make (tyOption e1.typ) & IML_Some e1
                | _ -> internal_error ~loc "strange cons"
              end
          | s -> internal_error ~loc "strange list constructor %s" s
        end
  
    (* sum *)
    | Tconstr (p, [_; _], _) when (match Path.is_scaml p with Some "sum" -> true | _ -> false) ->
        let typ = type_expr ~loc tyenv exp_type in
        let ty1, ty2 = match typ.desc with
          | TyOr (ty1, ty2) -> ty1, ty2
          | _ -> assert false
        in
        let arg = match args with [arg] -> arg | _ -> internal_error ~loc "strange sum arguments" in
        begin match cstr_name with
        | "Left" -> 
            let e = expression env arg in
            (* e.typ = ty1 *)
            make_constant & make typ & Left (ty2, e)
        | "Right" ->
            let e = expression env arg in
            (* e.typ = ty2 *)
            make_constant & make typ & Right (ty1, e)
        | s -> internal_error ~loc "strange sum constructor %s" s
        end

    | Tconstr (p, _, _) when p = Predef.path_unit -> 
        make (tyUnit) Unit
  
    | Tconstr (p, [], _) when Path.is_scaml_dot "int" p ->
        make (tyInt) begin
          let arg = match args with
              | [arg] -> arg
              | _ -> internal_error ~loc "strange Int arguments"
            in
            match arg.exp_desc with
              | Texp_constant (Const_int n) -> Const (Int (Z.of_int n))
              | _ -> errorf ~loc "Int can only take an integer constant"
          end
  
    | Tconstr (p, [], _) when Path.is_scaml_dot "nat" p ->
        make (tyNat) begin 
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
  
    | Tconstr (p, [], _) when Path.is_scaml_dot "tz" p ->
        make (tyMutez) begin 
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
    | Tconstr (p, [_], _) when (match Path.is_scaml p with Some "Set.t" -> true | _ -> false) ->
        let typ = type_expr ~loc tyenv exp_type in
        let _ty = match typ.desc with
          | TySet ty-> ty
          | _ -> assert false
        in
        (* XXX comparable type check *)
        if cstr_name <> "Set" then internal_error ~loc "strange set constructor";
        begin match args with 
          | [arg] -> 
              let e = expression env arg in
              (* tyList ty = e.typ *)
              begin match get_constant e with
              | Ok (List xs) -> 
                  (* XXX Uniqueness and sorting? *)
                  { e with typ; desc= Const (Set xs) }
              | Error loc -> errorf ~loc "Elements of Set must be constants"
              | Ok _ -> assert false
              end
          | _ -> internal_error ~loc "strange set arguments"
        end
  
    (* map *)
    | Tconstr (p, [_; _], _) when (match Path.is_scaml p with Some "Map.t" -> true | _ -> false) ->
        let typ = type_expr ~loc tyenv exp_type in
        let _ty1, _ty2 = match typ.desc with
          | TyMap (ty1, ty2) -> ty1, ty2
          | _ -> assert false
        in
        (* XXX comparable type check *)
        if cstr_name <> "Map" then internal_error ~loc "strange map constructor";
        begin match args with 
          | [arg] -> 
              let e = expression env arg in
              (* tyList (tyPair (ty1, ty2)) = e.typ *)
              begin match get_constant e with
              | Ok (List xs) -> 
                  (* XXX Uniqueness and sorting? *)
                  let xs = List.map (function
                      | C.Pair (c1,c2) -> (c1,c2)
                      | _ -> assert false) xs in
                  { e with typ; desc= Const (Map xs) }
              | Ok _ -> assert false
              | Error loc -> errorf ~loc "Elements of Map must be constants"
              end
          | _ -> internal_error ~loc "strange map arguments"
        end

    (* C "string" style constants *)
    | Tconstr (p, [], _) when (Path.is_scaml p <> None) ->
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
                      let e = expression env arg in
                      match e.desc with
                      | Const (String s) -> 
                          begin match parse s with
                            | Ok v -> { e with typ; desc= Const v }
                            | Error s -> errorf ~loc "Parse error of %s string: %s" tyname s
                          end
                      | _ -> errorf ~loc "%s only takes a string literal" cname
        end

    | Tconstr (p, [], _) when Path.is_scaml p = None ->
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
                          ty_args
                        ) non_consts
                    in
                    let tys_list = List.map (List.map (type_expr ~loc tyenv)) tys_list in
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
                  let arg = encode_by mkpair & List.map (expression env) args in
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
  
  and expression env { exp_desc; exp_loc=loc; exp_type= mltyp; exp_env= tyenv; exp_extra=_; exp_attributes } =
    (* wildly ignores extra *)
    (* if exp_extra <> [] then unsupported ~loc "expression extra"; *)
    begin match attr_has_entry_point exp_attributes with
      | None -> ()
      | Some loc ->
          errorf ~loc "entry declaration is only allowed for the toplevel definitions";
    end;
    let typ = type_expr ~loc tyenv mltyp in
    let mk desc = { loc; typ; desc; attr= [] } in
    match exp_desc with
    | Texp_ident (Path.Pident id, {loc=_}, _vd) -> 
        begin match List.assoc_opt id env with
          | None -> assert false
          | Some _ty ->
              (* typ = ty *)
              mk & Var (id, typ)
        end
    | Texp_ident (p, {loc}, _vd) ->
        begin match Path.is_scaml p with
          | Some n -> mk & primitive ~loc typ n []
          | None -> unsupported ~loc "complex path %s" (Path.xname p)
        end
    | Texp_constant (Const_string (s, None)) -> 
        mk & Const (String s)
    | Texp_constant (Const_string (_, Some _)) -> 
        unsupported ~loc "quoted string"
    | Texp_constant _ -> unsupported ~loc "constant"
    | Texp_tuple [e1; e2] ->
        let e1 = expression env e1 in
        let e2 = expression env e2 in
        (* tyPair (e1.typ, e2.typ) = typ *) 
        make_constant & mk & Pair (e1, e2)
    | Texp_tuple es ->
        Binplace.fold
          ~leaf:(fun c -> c)
          ~branch:(fun c1 c2 ->
              make_constant & mk & Pair (c1, c2))
          & Binplace.place 
          & List.map (expression env) es
          
    | Texp_construct ({loc}, c, args) -> 
        construct ~loc env tyenv mltyp c args
  
    | Texp_assert e ->
        begin match e.exp_desc with
        | Texp_construct (_, {cstr_name="false"}, []) ->
            (* assert false has type 'a *)
            mk AssertFalse
        | _ -> 
            mk & Assert (expression env e)
        end
  
    | Texp_let (Recursive, _, _) -> unsupported ~loc "recursion"
    | Texp_let (Nonrecursive, vbs, e) ->
        let rev_vbs, env =
          List.fold_left (fun (rev_vbs, env) vb -> 
              let _, v, e = value_binding env vb in
              (v, e) :: rev_vbs, ((v.desc,v.typ)::env)) ([], env) vbs
        in
        let e = expression env e in
        List.fold_left (fun e (v,def) ->
            { loc; (* XXX inaccurate *)
              typ= e.typ;
              desc= Let (v, def, e);
              attr= [] } ) e rev_vbs
  
    | Texp_apply (_, []) -> assert false
    | Texp_apply (f, args) -> 
        let args = List.map (function
            | (Nolabel, Some (e: Typedtree.expression)) -> expression env e
            | _ -> unsupported ~loc "labeled arguments") args
        in
        let _fty' = 
          List.fold_right (fun arg ty -> tyLambda(arg.typ, ty)) args typ 
        in
        let fty = type_expr ~loc:f.exp_loc f.exp_env f.exp_type in
        (* fty = fty' *)
  
        let name = match f with
          | { exp_desc= Texp_ident (p, _, _) } -> Path.is_scaml p
          | _ -> None
        in
        (* only some fixed combinations *)
        begin match name with
          | None -> 
              let f = expression env f in
              (* f.typ = fty *)
              mk & App (f, args)
          | Some n -> mk & primitive ~loc:f.exp_loc fty n args
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
              (* v.typ = ty1 *)
              let env = (v.desc,v.typ)::env in
              let e = expression env c_rhs in
(* freevars can be changed because of optimizatios
              let s = IdTys.(elements & remove (v.desc, v.typ) (freevars e)) in
*)
(*
              typ = tyLambda (ty1, ty2)
              ty2 = e.typ
*)
              mk & Fun (ty1, ty2, v, e)
          | _ -> assert false
        end
  
    | Texp_ifthenelse (cond, then_, Some else_) -> 
        let econd = expression env cond in
        let ethen = expression env then_ in
        let eelse = expression env else_ in
        (* ignore (unify ethen.typ eelse.typ);
           ignore (unify typ ethen.typ); *)
        mk & IfThenElse (econd, ethen, eelse)
  
    | Texp_match (_, _, e::_, _) -> 
        unsupported ~loc:e.c_lhs.pat_loc "exception pattern"
        
    | Texp_match (_ , _, _, Partial) -> 
        unsupported ~loc "non exhaustive pattern match"
        
    | Texp_match (e , cases, [], Total) -> 
        (* mk & compile_match ~loc env e cases *)
        (* Format.eprintf "match-e %a@." Printtyp.type_scheme e.exp_type; *)
        mk & compile_match env e cases
        
    | Texp_ifthenelse (_, _, None) ->
        unsupported ~loc "if-then without else"
    | Texp_try _ -> unsupported ~loc "try-with"
    | Texp_variant _ -> unsupported ~loc "polymorphic variant"

    | Texp_record { fields; extended_expression= None; _ } -> 
        (* I believe fields are already sorted *)
        let es = 
          List.map (fun (_ldesc, ldef) -> match ldef with
              | Overridden (_, e) -> expression env e
              | Kept _ -> assert false) & Array.to_list fields
        in
        Binplace.fold
          ~leaf:(fun c -> c)
          ~branch:(fun c1 c2 ->
              make_constant & mk & Pair (c1, c2))
        & Binplace.place es

    | Texp_record { fields; extended_expression= Some e; } ->
        (* optimal code, I hope *)
        (* I believe fields are already sorted *)
        let es = 
          List.map (fun (_ldesc, ldef) -> match ldef with
              | Overridden (_, e) -> Some (expression env e)
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
        f (expression env e) & fst & simplify tree

    | Texp_field (e, _, label) ->
        let pos = label.lbl_pos in
        let nfields = Array.length label.lbl_all in
        let e = expression env e in
        let rec f = function
          | [] -> e
          | Binplace.Left :: dirs -> mkfst & f dirs (* XXX mkfst, mnsnd should be somewhere else *)
          | Binplace.Right :: dirs -> mksnd & f dirs
        in
        f & Binplace.path pos nfields
        
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
                   attr= [] }, left)
  
  and compile_match env e cases =
    let compile_case env case = 
      let p = patternx case.c_lhs in
      let pvars = IdTys.elements & P.vars p in
      let guard = Option.fmap (expression (pvars@env)) case.c_guard in
      (patternx case.c_lhs, guard, expression (pvars@env) case.c_rhs)
    in
    Match (expression env e, List.map (compile_case env) cases)
  
  and value_binding env { vb_pat; vb_expr; vb_attributes=_; vb_loc=_loc } = 
    (* currently we only handle very simple sole variable pattern *)
    match pattern vb_pat with
    | [v] ->
        let e = expression env vb_expr in
        (* ignore & unify v.typ e.typ; *)
        vb_pat.pat_type, v, e
    | _ -> assert false
  
  (* The condition of the entry point is a bit too strict.
     Currently: the last sitem must be an entry point.
     Better: the last value binding must be an entry point.,
  *)
  and structure_item env { str_desc; str_loc=loc } =
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
              let pat_typ,v,b = value_binding env vb in
              (v.desc,v.typ)::env, (pat_typ,v,b)::rev_vbs) (env, []) vbs
        in
        env, List.rev rev_vbs
  
    | Tstr_open _open_description -> env, []
  
    | Tstr_attribute _ -> 
        (* simply ignores it for now *)
        env, []
  
  and structure env { str_items= sitems } =
    let _env, rev_vbss =
      List.fold_left (fun (env, rev_vbss) sitem ->
          let env, vbs = structure_item env sitem in
          env, vbs :: rev_vbss) (env, []) sitems 
    in
    (* This is if the entry point is alone and at the end *)
    let vbs = List.flatten & List.rev rev_vbss in
    List.fold_right (fun (_,v,b) x ->
        { loc=Location.none; typ= tyUnit; desc= Let (v, b, x); attr= [] })
      vbs
      final
  in
  structure env str

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
      Env.lookup_type (*~loc: *)
        (Longident.(Ldot (Lident "SCaml", "operations"))) tyenv
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
    | Tconstr (p, [t], _) when Path.is_scaml p =  Some "Contract.t" ->
        if t.level = Btype.generic_level then
          errorf ~loc:self.exp_loc "Contract.self cannot have a generic type, but it has type %a.  Please use a type constraint to instantiate its type." Printtyp.type_scheme self.exp_type;
        unify self.exp_type ty_self
    | _ -> assert false
    ) !selfs


let compile_global_entry ty_storage ty_return node =

  let id_storage = Ident.create "storage" in
  let mk desc typ = { desc; typ; loc= dummy_loc; attr= [] } in
  let mk_var id typ =  mk (Var (id, typ)) typ in
  let mk_pat desc typ = { desc; typ; loc= dummy_loc; attr= () } in

  let pat_storage = mk_pat id_storage ty_storage in
  let e_storage = mk_var id_storage ty_storage in

  let rec f param_id node = match node with
    | `Leaf (_,vb) ->
        let id, var = match pattern vb.vb_pat with
          | [p] -> p.desc, mk_var p.desc p.typ
          | _ -> assert false
        in
        let param_type = match var.typ with
          | { desc= TyLambda (t1, _); _ } -> t1
          | _ -> assert false
        in
        let param_var = mk_var param_id param_type in
(*
        ignore & unify var.typ (tyLambda (param_type, 
                                 tyLambda (ty_storage, ty_return, { closure_desc= CLEmpty }),
                                 { closure_desc= CLEmpty }));
*)
        add_attr (Comment ("entry " ^ Ident.name id))
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
  let f1 = 
    mk (Fun (ty_storage, ty_return, pat_storage, e))
      (tyLambda (ty_storage, ty_return))
  in
  mk (Fun (param_typ, ty_return, param_pat, f1))
    (tyLambda (param_typ, f1.typ))

module VMap = Map.Make(struct type t = Ident.t let compare = compare end)

let add_attrs attrs t = { t with attr= attrs @ t.attr }

let count_variables t = 
  let incr v st = match VMap.find_opt v st with
    | None -> VMap.add v 1 st
    | Some n -> VMap.add v (n+1) st
  in
  let rec f t st = match t.desc with
    | Var (id, _) -> incr id st

    | Const _ | Nil _ | IML_None _ | Unit | AssertFalse -> st

    | IML_Some t | Left (_, t) | Right (_, t) | Assert t
    | Fun (_, _, _, t) -> f t st

    | Let (_, t1, t2) | Cons (t1, t2) | Pair (t1, t2) -> f t1 & f t2 st

    | IfThenElse (t1, t2, t3) 
    | Switch_or (t1, _, t2, _, t3)
    | Switch_cons (t1, _, _, t2, t3)
    | Switch_none (t1, t2, _, t3) -> f t1 & f t2 & f t3 st

    | App (t, ts) -> List.fold_right f (t::ts) st
    | Prim (_, _, ts) -> List.fold_right f ts st
    | Match (t, cases) -> 
        f t & List.fold_left (fun st (_,g,t) ->
            match g with
            | None -> f t st
            | Some g -> f t & f g st
          ) st cases
  in
  f t VMap.empty

(* t2[t1/id] *)
let subst id t1 t2 =
  let rec f t = 
    let mk desc = { t with desc } in
    match t.desc with
    | Var (id', _) when id = id' -> 
        add_attrs t.attr t1 (* t1 never contains id *)
    | Var _ | Const _ | Nil _ | IML_None _ | Unit | AssertFalse -> t

    | IML_Some t -> mk & IML_Some (f t)
    | Left (a, t) -> mk & Left (a, f t)
    | Right (a, t) -> mk & Right (a, f t)
    | Assert t -> mk & Assert (f t)
    | Fun (a, b, pat, t) -> mk & Fun (a, b, pat, f t)
    | Let (p, t1, t2) -> mk & Let (p, f t1, f t2)
    | Cons (t1, t2) -> mk & Cons (f t1, f t2)
    | Pair (t1, t2) -> mk & Pair (f t1, f t2)
    | IfThenElse (t1, t2, t3) -> mk & IfThenElse (f t1, f t2, f t3)
    | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (f t1, p1, f t2, p2, f t3)
    | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (f t1, p1, p2, f t2, f t3)
    | Switch_none (t1, t2, p, t3) -> mk & Switch_none (f t1, f t2, p, f t3)
    | App (t, ts) -> mk & App (f t, List.map f ts)
    | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
    | Match (t, cases) ->
        mk & Match (f t, 
                    List.map (fun (p,g,t) -> 
                        (p, 
                         Option.fmap f g,
                         f t)) cases)
  in
  f t2

(* 
   (fun x -> e1) e2  =>  let x = e2 in e1 
   let x = e2 in e1  =>  e1[e2/x]  when x appears only once in e1
   
   Free variables are counted for each let (and fun).  Very inefficient.
*)
let optimize t = 
  let rec f t = 
    let attrs = ref (Some t.attr) in
    let add_attrs t' = 
      match !attrs with
      | Some as_ ->
          attrs := None;
          { t' with attr= as_ @ t'.attr } 
      | None -> assert false
    in
    let mk desc = add_attrs & { t with desc; attr= [] } in
    let res = match t.desc with
      | App (t, []) -> add_attrs & f t 
      | App (u, t::ts) -> 
          let t = f t in
          let ts = List.map f ts in
          begin match f u with
          | {desc= Fun (_ty1, ty2, pat, body)} ->
              f & mk & App ({ desc= Let (pat, t, body);
                              loc= t.loc; (* incorrect *)
                              typ= ty2;
                              attr= [] },
                            ts)
          | u -> mk & App (u, t::ts)
          end
      | Let (p, t1, t2) -> 
          let vmap = count_variables t2 in
          begin match VMap.find_opt p.desc vmap with
            | None -> add_attrs & f t2
            | Some 1 -> 
                add_attrs 
                & f & subst p.desc (add_attr (Comment ("= " ^ Ident.name p.desc)) t1) t2
            | _ -> mk & Let (p, f t1, f t2)
          end
      | Var _ | Const _ | Nil _ | IML_None _ | Unit | AssertFalse -> 
          add_attrs { t with attr= [] }
      | IML_Some t -> mk & IML_Some (f t)
      | Left (a, t) -> mk & Left (a, f t)
      | Right (a, t) -> mk & Right (a, f t)
      | Assert t -> mk & Assert (f t)
      | Fun (a, b, c, t) -> mk & Fun (a, b, c, f t)
      | Cons (t1, t2) -> mk & Cons (f t1, f t2)
      | Pair (t1, t2) -> mk & Pair (f t1, f t2)
      | IfThenElse (t1, t2, t3) -> mk & IfThenElse (f t1, f t2, f t3)
      | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (f t1, p1, f t2, p2, f t3)
      | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (f t1, p1, p2, f t2, f t3)
      | Switch_none (t1, t2, p, t3) -> mk & Switch_none (f t1, f t2, p, f t3)
      | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
      | Match (t, cases) -> mk & Match (f t, List.map (fun (p,g,t) -> 
          p, 
          Option.fmap f g,
          f t) cases)
    in
    begin match !attrs with
      | Some _ -> assert false
      | None -> ()
    end;
    res
  in
  f t

let implementation sourcefile str = 
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
        type_expr ~loc:(Location.in_file sourcefile) (* XXX *) tyenv ty_operations
      in

      let ty_storage = type_expr ~loc:(Location.in_file sourcefile) ~this:"Contract storage" tyenv ty_storage in
      let ty_return = tyPair (ty_operations, ty_storage) in
      let final = compile_global_entry ty_storage ty_return node in
      let ty_param = type_expr ~loc:(Location.in_file sourcefile) ~this:"Contract global parameter" tyenv ty_param in
      let ty_param = 
        match node with
        | `Leaf _ (* sole entry point *) -> ty_param
        | `Node _ ->
            let open M.Type in
            let rec add_annot ty node = match ty.desc, node with
              | _, `Leaf (_,vb) -> 
                  (* XXX dup *)
                  let id = match pattern vb.vb_pat with
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
      ty_param, ty_storage, structure [] str final

let save path t = 
  let oc = open_out path in
  let ppf = Format.of_out_channel oc in
  Format.fprintf ppf "%a@." pp t;
  close_out oc
