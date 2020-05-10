(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Spotlib.Spot
open Tools

module M = Michelson
open M.Type
open M.Opcode

module type Config = sig 
  val allow_big_map : bool 
end

module Env : sig
  type t = (Ident.t * M.Type.t) list
  val find : Ident.t -> t -> (int * M.Type.t) option
  val pp : Format.formatter -> t -> unit
end = struct
  type t = (Ident.t * M.Type.t) list

  let find id env = 
    let rec aux n = function
      | [] -> None
      | (id',ty)::_ when id = id' -> Some (n,ty)
      | _::env -> aux (n+1) env
    in
    aux 0 env

  let pp ppf t =
    Format.fprintf ppf "@[<2>[ %a ]@]"
      (Format.list ";@ "
         (fun ppf (id, ty) ->
            Format.fprintf ppf 
              "%s : %a"
              (Ident.unique_name id)
              M.Type.pp ty)) t
end

(* Copy a value of the identifier from the deep of the stack to its top. *)
let var ~loc env id = match Env.find id env with
  | None -> 
      internal_error ~loc "Variable not found: %s in %s" 
        (Ident.unique_name id)
        (Format.sprintf "%a" Env.pp env)
  | Some (0,_typ) ->
      [ COMMENT( "var " ^ Ident.unique_name id, [ DUP ]) ] 
  | Some (n,_typ) ->
      [ COMMENT( "var " ^ Ident.unique_name id, [ DIG n; DUP; DUG (n+1) ]) ] 

(* Field annotation cannot appear at the first level of type tree.
   The followings are rejectred:
   
   PUSH (nat %nat) 1
   RIGHT (string %Vote) 
   
   XXX should be fixed at the creation of these types.
*)
let clean_field_annot typ =
  let fix attrs = 
    List.filter (fun s ->
        match s with
        | "" -> false
        | s when s.[0] = '%' -> false
        | _ -> true) attrs 
  in
  (* remove field annotations
     * not under Pair
     * not under Or
  *)
  let rec f parent_can_have_annotations typ = 
    let attrs = 
      if parent_can_have_annotations then typ.attrs
      else fix typ.attrs
    in
    let desc = match typ.desc with
      | TyPair (t1, t2) -> TyPair (f true t1, f true t2)
      | TyOr (t1, t2) -> TyOr (f true t1, f true t2)
      | TyString
      | TyNat
      | TyInt
      | TyBytes
      | TyBool
      | TyUnit
      | TyMutez
      | TyKeyHash
      | TyTimestamp
      | TyAddress
      | TyChainID
      | TyKey
      | TySignature
      | TyOperation -> typ.desc
      | TyList t -> TyList (f false t)
      | TyOption t -> TyOption (f false t)
      | TySet t -> TySet (f false t)
      | TyMap (t1, t2) -> TyMap (f false t1, f false t2)
      | TyBigMap (t1, t2) -> TyBigMap (f false t1, f false t2)
      | TyContract t -> TyContract (f false t)
      | TyLambda (t1, t2) -> TyLambda (f false t1, f false t2)
    in
    { desc; attrs }
  in
  f false typ

module Make(Config : Config) = struct
  let rec compile env t = match constant t with
    | None -> compile' env t 
    | Some c -> 
        [ PUSH (clean_field_annot t.IML.typ, c) ]
      
  and compile' env t = 
    let os = desc env t in 
    let comments = 
      List.filter_map (function
          | IML.Attr.Comment s when !Flags.flags.scaml_debug -> Some s
          | _ -> None
        ) t.IML.attrs
    in
    match comments with
    | [] -> os
    | _ -> [COMMENT (String.concat ", " comments, os)]
        
  and desc env t =
    let loc = t.IML.loc in
    match t.IML.desc with
    | IML.Set _ -> errorf_constant ~loc "Set elements must be constants"
    | Map _ -> errorf_constant ~loc "Map bindings must be constants"
    | BigMap _ -> errorf_constant ~loc "BigMap bindings must be constants"
    | Const c -> 
        [ PUSH (clean_field_annot t.typ, c) ]
    | Nil -> 
        let ty = match t.typ.desc with
          | TyList ty -> ty
          | _ -> assert false
        in
        [ NIL (clean_field_annot ty) ]
    | Cons (t1, t2) -> 
        let os2 = compile env t2 in
        let os1 = compile ((Ident.dummy, t2.typ)::env) t1 in
        os2 @ os1 @ [ CONS ]
    | IML_None -> 
        let ty = match t.IML.typ.desc with
          | TyOption ty -> ty
          | _ -> assert false
        in
        [ NONE (clean_field_annot ty) ]
    | IML_Some t1 -> 
        let os1 = compile env t1 in
        os1 @ [ SOME ]
    | Left t' ->
        let ty = match t.typ.desc with
          | TyOr (_, ty) -> ty
          | _ -> assert false
        in
        let os = compile env t' in
        os @ [ LEFT (clean_field_annot ty) ]
    | Right t' -> 
        let ty = match t.typ.desc with
          | TyOr (ty, _) -> ty
          | _ -> assert false
        in
        let os = compile env t' in
        os @ [ RIGHT (clean_field_annot ty) ]
    | Unit -> [ UNIT ]
  
    | Var id -> var ~loc env id
  
    | Pair (t1, t2) ->
        let os2 = compile env t2 in
        let os1 = compile ((Ident.dummy, t2.typ)::env) t1 in
        os2 @ os1 @ [ PAIR ]
    | Seq (t1, t2) ->
        let os1 = compile env t1 in
        let os2 = compile env t2 in
        os1 @ [ DROP 1 ] @ os2 (* erm... not sure about FAIL *)
    | Assert t ->
        let os = compile env t in
        os @ [ ASSERT; UNIT ]
    | AssertFalse -> [ UNIT ; FAILWITH ]
    | IfThenElse (t1, t2, Some t3) ->
        let oif = compile env t1 in
        let othen = compile env t2 in
        let oelse = compile env t3 in
        oif @ [IF (othen, oelse)]
    | IfThenElse (t1, t2, None) ->
        let oif = compile env t1 in
        let othen = compile env t2 in
        if t2.typ.desc = TyUnit then
          oif @ [IF (othen, [UNIT])]
        else
          (* I think this is never called *)
          let othen = compile env t2 in
          oif @ [IF (othen @ [DROP 1; UNIT], [UNIT])]
    | Prim (n, conv, ts) ->
        (* Prim (ops, [t1; t2])
           t2 ; t1; ops
        *)
        let _, pre = 
          List.fold_right (fun t (env, os) ->
              let os' = compile env t in
              let env' = (Ident.dummy, tyUnit (* dummy *)) :: env in
              env', os @ os') ts (env, [])
        in
        [COMMENT (n, conv pre)]
    | Let (pat, t1, t2) ->
        let os1 = compile env t1 in
        let os2 = compile ((pat.desc, pat.typ)::env) t2 in
        COMMENT (Ident.unique_name pat.desc, os1) :: os2 
        @ [COMMENT ("clean " ^ Ident.unique_name pat.desc, [DIP (1, [DROP 1])])]
    | Switch_or (t, p1, t1, p2, t2) ->
        let os = compile env t in
        let os1 = compile ((p1.desc,p1.typ)::env) t1 in
        let os2 = compile ((p2.desc,p2.typ)::env) t2 in
        (* XXX if the return types are the same, we can unify DIP { DROP } into one *)
        os @ [IF_LEFT (os1 @ [DIP (1, [DROP 1])], 
                       os2 @ [DIP (1, [DROP 1])])]
    | Switch_cons (t, p1, p2, t1, t2) ->
        let os = compile env t in
        let os1 = compile ((p1.desc,p1.typ)::(p2.desc,p2.typ)::env) t1 in
        let os2 = compile env t2 in
        os @ [IF_CONS (os1 @ [DIP (1, [DROP 2])], os2)]
    | Switch_none (t, t1, p2, t2) ->
        let os = compile env t in
        let os1 = compile env t1 in
        let os2 = compile ((p2.desc,p2.typ)::env) t2 in
        os @ [IF_NONE (os1, os2 @ [DIP (1, [DROP 1])])]
    | Fun (p, body) ->
        let fvars = IML.IdTys.elements & IML.freevars t in
        (*
        Format.eprintf "fvars: @[%a@] env: @[%a@]@." 
          (Format.list ";@ " (fun ppf (id,ty) ->
               Format.fprintf ppf "%s:%a" (Ident.unique_name id) M.Type.pp ty)) fvars
          (Format.list ";@ " (fun ppf (id,ty) ->
               Format.fprintf ppf "%s:%a" (Ident.unique_name id) M.Type.pp ty)) env;
        *)
        (* XXX
           
           APPLY stores the values of freevars into a closure.
           If the values are not serializable, it fails.
           For example, contracts are not serializable
  
           operation, contract, and big_map cannot be APPLY'ed
        *)
        begin match t.typ.desc with
          | TyLambda (ty1, ty2) ->
              begin match fvars with
                | [] ->
                    let env = [p.desc,p.typ] in
                    let o = compile env body in
                    let clean = [ COMMENT ("lambda clean up", [DIP (1, [ DROP 1 ]) ]) ] in
                    [ LAMBDA (clean_field_annot ty1, clean_field_annot ty2, o @ clean) ]
                | _ -> 
                    (* fvars: x1:ty1 :: x2:ty2 :: .. :: xn:tyn 
  
                       inside lambda:  p :: x1:ty1 :: x2:ty2 :: .. :: xn:tyn
  
                       lambda's parameter:  tyn * (ty(n-1) * ( .. (ty1 * p.typ) .. ))
                    *)
                    let lambda =
                      let env = (p.desc,p.typ) :: fvars in
                      let ity = 
                        (* (tyn * (ty(n-1) * .. * (ty1 * p.typ) .. )) *)
                        List.fold_left (fun st (_x,ty) ->
                            tyPair (ty,st)) p.typ fvars
                      in
                      (* 
                         -   (xn, (xn-1, .., (x1, p1) ..))  :: 0
  
                         DUP (xn, (xn-1, .., (x1, p1) ..)) :: (xn, (xn-1, .., (x1, p) ..)) :: 0
                         DIP { CAR }  (vn, (vn-1, .., (v1, p1) ..)) :: vn :: 0
                         CDR   (vn-1, .., (v1, p1) ..) :: vn :: 0
  
                         DUP
                         DIP { CAR }
                         CDR
  
                         ..  p1 :: x1 :: .. :: xn :: 0
                      *)
                      let extractor = 
                        List.rev_map (fun (v,_ty) ->
                            COMMENT ("fvar " ^ Ident.unique_name v, [ DUP ; DIP (1, [ CAR ]); CDR ])) fvars
                      in
                      let len = List.length fvars in
                      let clean = [ COMMENT ("lambda clean up", [DIP (1, [ DROP (len + 1) ]) ]) ] in
                      LAMBDA (clean_field_annot ity, clean_field_annot ty2, extractor @ compile env body @ clean)
                    in
                    let partial_apply =
                      (* Apply fvars from xn to x1 *)
                      List.fold_left (fun st (x,_ty) ->
                          let o = var ~loc ((Ident.dummy,tyUnit (* for lambda *))::env) x in
                          COMMENT ("partial app " ^ Ident.unique_name x, o @ [APPLY]) :: st) [] fvars
                    in
                    lambda :: partial_apply
              end
          | _ -> assert false
        end
    | App (t, []) -> compile env t
    | App (f, args) ->
        let ofun = compile env f in
        let env = (Ident.dummy, tyUnit)::env in
        List.fold_left (fun ofun arg ->
            let oarg = compile env arg in
            ofun @ oarg @ [ EXEC ]) ofun args
    | Contract_create (s, sloc, e1, e2, e3) -> (* XXX *)
        match s with
        | Tz_file path -> 
            let s = 
              match File.to_string path with
              | Ok s -> s
              | Error (`Exn exn) ->
                  errorf_contract ~loc:sloc "Loading of %s failed: %s"
                    path (Printexc.to_string exn) 
            in
            let nodes =
              let open Tezos_micheline in
              let open Micheline_parser in
              let open Tezos_error_monad in
              match tokenize s with
              | tkns, [] ->
                  begin match parse_toplevel ~check:false tkns with
                  | nodes, [] ->
                      List.map 
                        (Micheline.map_node 
                           (fun _ -> { Micheline_printer.comment= None })
                           (fun x -> x))
                        nodes
                  | _nodes, es ->
                      errorf_contract ~loc:sloc "Michelson parse error: %a"
                        Error_monad.pp_print_error es
                  end
              | _tkns, es ->
                  errorf_contract ~loc:sloc "Michelson parse error: %a"
                    Error_monad.pp_print_error es
            in
            List.fold_left (fun os arg ->
                let oarg = compile env arg in
                oarg @ os) [] [e1; e2; e3]
            @ [CREATE_CONTRACT (Raw nodes) ; PAIR]
        | Tz_code s ->
            let nodes =
              let open Tezos_micheline in
              let open Micheline_parser in
              let open Tezos_error_monad in
              match tokenize s with
              | tkns, [] ->
                  begin match parse_toplevel ~check:false tkns with
                  | nodes, [] ->
                      List.map 
                        (Micheline.map_node 
                           (fun _ -> { Micheline_printer.comment= None })
                           (fun x -> x))
                        nodes
                  | _nodes, es ->
                      errorf_contract ~loc:sloc "Michelson parse error: %a"
                        Error_monad.pp_print_error es
                  end
              | _tkns, es ->
                  errorf_contract ~loc:sloc "Michelson parse error: %a"
                    Error_monad.pp_print_error es
            in
            List.fold_left (fun os arg ->
                let oarg = compile env arg in
                oarg @ os) [] [e1; e2; e3]
            @ [CREATE_CONTRACT (Raw nodes) ; PAIR]
  
  and constant t =
    (* try to compile an expressions to a constant, rather than opcodes *)
    (* XXX This is incredibly inefficient, which tries to compile 
       expressions as constants so many times.
    *)
    let rec f t = 
      let module C = M.Constant in
      let (>>=) = Option.bind in
      match t.IML.desc with
      | IML.Unit -> Some C.Unit
      | Const c -> Some c
      | IML_None -> Some (C.Option None)
      | IML_Some t -> f t >>= fun c -> Some (C.Option (Some c))
      | Left t -> f t >>= fun t -> Some (C.Left t)
      | Right t -> f t >>= fun t -> Some (C.Right t)
      | Pair (t1, t2) -> f t1 >>= fun t1 -> f t2 >>= fun t2 -> Some (C.Pair (t1, t2))
      | Nil -> 
         (* We cannot PUSH (list operation) {}. If we do, we get:
            "operation type forbidden in parameter, storage and constants" *)
          begin match t.typ.desc with
            | TyList { desc= TyOperation } -> None
            | _ -> Some (C.List [])
          end
      | Cons (t1, t2) -> f t1 >>= fun t1 -> f t2 >>= fun t2 ->
          begin match t2 with
            | C.List t2 -> Some (C.List (t1::t2))
            | _ -> assert false
          end
      | Fun _ when IML.IdTys.is_empty (IML.freevars t) ->
          begin try 
              match compile' [] t with
              | [LAMBDA (_, _, os)] -> Some (C.Code (fst (clean_failwith os)))
              | _ -> assert false (* impossible *)
            with _ -> None 
          end
      | Set ts ->
          Some (C.Set
                  (List.map (fun t -> 
                      match constant t with
                      | Some c -> c
                      | None -> errorf_constant ~loc:t.loc "Set expects constant elements") ts))
      | Map kvs ->
          let kvs =
            List.map (fun (k,v) -> 
                match constant k with
                | None -> errorf_constant ~loc:k.loc "Map expects constant bindings"
                | Some k ->
                    match constant v with
                    | None -> errorf_constant ~loc:v.loc "Map expects constant bindings"
                    | Some v -> (k,v)) kvs
          in
          (* XXX do not use OCaml's comparison *)
          let kvs = List.sort (fun (k1,_) (k2,_) -> compare k1 k2) kvs in
          let rec check_uniq = function
            | [] | [_] -> ()
            | (c1,_)::(c2,_)::_ when c1 = c2 -> (* XXX OCaml's compare *)
                errorf_constant ~loc:t.loc "Map literal contains duplicated key %a" C.pp c1 
            | _::xs -> check_uniq xs
          in
          check_uniq kvs;
          Some (C.Map kvs)
      | BigMap _ when not Config.allow_big_map ->
          errorf_big_map ~loc:t.loc "BigMap constant is not allowed"
      | BigMap kvs ->
          let kvs =
            List.map (fun (k,v) -> 
                match constant k with
                | None -> errorf_constant ~loc:k.loc "BigMap expects constant bindings"
                | Some k ->
                    match constant v with
                    | None -> errorf_constant ~loc:v.loc "BigMap expects constant bindings"
                    | Some v -> (k,v)) kvs
          in
          (* XXX do not use OCaml's comparison *)
          let kvs = List.sort (fun (k1,_) (k2,_) -> compare k1 k2) kvs in
          let rec check_uniq = function
            | [] | [_] -> ()
            | (c1,_)::(c2,_)::_ when c1 = c2 -> (* XXX OCaml's compare *)
                errorf_constant ~loc:t.loc "BigMap literal contains duplicated key %a" C.pp c1 
            | _::xs -> check_uniq xs
          in
          check_uniq kvs;
          Some (C.Map kvs)
      | _ -> None
      in
      f t
  
  let split_entry_point t =
    let rec f st t = match t.IML.desc with
      | IML.Let (p, t1, t2) -> f ((p,t1)::st) t2
      | _ -> (List.rev st, t)
    in
    f [] t
  
  let structure t =
    let defs, t = split_entry_point t in
    let ops, env = 
      List.fold_left (fun (ops, env) (p,t) ->
          let os1 = compile env t in
          ops @ [ COMMENT ("let " ^ Ident.unique_name p.IML.desc, os1) ], 
          ((p.desc, p.typ)::env)) ([], []) defs
    in
    (* (parameter, storage) :: []
       -> parameter :: storage :: values
    *)
  
    (* replace fun by let *)
    let rec get_abst t = match t.IML.desc with
      | IML.Fun (p, t) -> p, t
      | Let (p, t1, t2) -> 
          let p', t2 = get_abst t2 in
          p', { t with desc= Let (p, t1, t2) }
      | _ -> assert false
    in
    let p1, t = get_abst t in
    let p2, t = get_abst t in
    let env = ((p2.desc,p2.typ)::(p1.desc,p1.typ)::env) in
    let os = compile env t in
    [ COMMENT ("top defs", if ops = [] then [] else [DIP (1, ops)]) 
    ; COMMENT ("entry point init", [DUP ; CDR; DIP (1, [CAR])])
    ; COMMENT ("entry point code", os )
    ; COMMENT ("final clean up", [ DIP (1, [ DROP (List.length env) ]) ])]
    |> clean_failwith
    |> fst
    |> dip_1_drop_n_compaction
end
