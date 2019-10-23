open Spotlib.Spot
open Tools

module M = Michelson
open M.Type
open M.Opcode

module MEnv : sig
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

(* The Michelson type of the environment part of a closure:
   (ty1 option * (ty2 option * (ty(n-1) option * tyn option)..))
*)
let closure_env_type xtys =
  match List.rev xtys with
  | [] -> assert false
  | (_,tylast)::xs ->
      List.fold_left (fun acc (_,ty) ->
          tyPair (tyOption ty, acc)) (tyOption tylast) xs

(*
(* Convert closure type represented in TyLambda (ty1, ty2, cinfo)
   to the real type in Michelson: 
     (TyLambda (ty1, ty2, empty), closure_env_type (env of cinfo))
*)
let rec closure_type t = match t.desc with
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
    | TyOperation) -> t
  | TyList t -> tyList (closure_type t)
  | TyPair (t1,t2) -> tyPair (closure_type t1, closure_type t2)
  | TyOption t -> tyOption (closure_type t)
  | TyOr (t1,t2) -> tyOr (closure_type t1, closure_type t2)
  | TySet t -> tySet (closure_type t)
  | TyMap (t1,t2) -> tyMap (closure_type t1, closure_type t2)
  | TyBigMap (t1,t2) -> tyBigMap (closure_type t1, closure_type t2)
  | TyContract t -> tyContract (closure_type t)
  | TyLambda (t1, t2) ->
      match (repr_closure_info closure_info).closure_desc with
      | CLLink _ -> assert false
      | CLEmpty -> assert false
      | CLList [] -> t
      | CLList xtys -> 
          let env_type = closure_type @@ closure_env_type xtys in
          tyPair (tyLambda ( tyPair(closure_type t1, env_type),
                                       closure_type t2,
                                       { closure_desc = CLList [] } ),
                       env_type)
*)

(* Copy a value of the identifier from the deep of the stack to its top. *)
let compile_var ~loc env id = match MEnv.find id env with
  | None -> 
      internal_error ~loc "variable not found: %s in %s" 
        (Ident.unique_name id)
        (Format.sprintf "%a" MEnv.pp env)
  | Some (n,_typ) ->
      let rec f = function
        | 0 -> [ DUP ]
        | n -> 
            assert (n > 0);
            [ DIP (1, f (n-1)); SWAP ]
      in
      [ COMMENT( "var " ^ Ident.name id, f n ) ]

let rec compile env t = 
  let loc = t.IML.loc in
  match t.IML.desc with
  | IML.Const op -> [ PUSH (t.typ, op) ]
  | Nil ty -> [ NIL ty ]
  | Cons (t1, t2) -> 
      let os2 = compile env t2 in
      let os1 = compile ((Ident.dummy, t2.typ)::env) t1 in
      os2 @ os1 @ [ CONS ]
  | IML_None ty -> [ NONE ty ]
  | IML_Some t1 -> 
      let os1 = compile env t1 in
      os1 @ [ SOME ]
  | Left (ty, t) ->
      let os = compile env t in
      os @ [ LEFT ty ]
  | Right (ty, t) -> 
      let os = compile env t in
      os @ [ RIGHT ty ]
  | Unit -> [ UNIT ]

  | Var (id, _) -> compile_var ~loc env id

  | Tuple (t1, t2) ->
      let os2 = compile env t2 in
      let os1 = compile ((Ident.dummy, t2.typ)::env) t1 in
      os2 @ os1 @ [ PAIR ]
  | Assert t ->
      let os = compile env t in
      os @ [ ASSERT; PUSH (tyUnit, Unit) ]
  | AssertFalse -> [ UNIT ; FAILWITH ]
  | IfThenElse (t1, t2, t3) ->
      let oif = compile env t1 in
      let othen = compile env t2 in
      let oelse = compile env t3 in
      oif @ [IF (othen, oelse)]
  | Prim (_n, conv, ts) ->
      (* Prim (ops, [t1; t2])
         t2 ; t1; ops
      *)
      let _, pre = 
        List.fold_right (fun t (env, os) ->
            let os' = compile env t in
            let env' = (Ident.dummy, tyUnit (* dummy *)) :: env in
            env', os @ os') ts (env, [])
      in
      conv pre
  | Let (pat, t1, t2) ->
      let os1 = compile env t1 in
      let os2 = compile ((pat.desc, pat.typ)::env) t2 in
      COMMENT (Ident.name pat.desc, os1) :: os2 
      @ [COMMENT ("clean " ^ Ident.name pat.desc, [DIP (1, [DROP 1])])]
  | Switch_or (t, p1, t1, p2, t2) ->
      let os = compile env t in
      let os1 = compile ((p1.desc,p1.typ)::env) t1 in
      let os2 = compile ((p2.desc,p2.typ)::env) t2 in
      os @ [IF_LEFT (os1 @ [DIP (1, [DROP 1])], os2 @ [DIP (1, [DROP 1])])]
  | Switch_cons (t, p1, p2, t1, t2) ->
      let os = compile env t in
      let os1 = compile ((p1.desc,p1.typ)::(p2.desc,p2.typ)::env) t1 in
      let os2 = compile env t2 in
      os @ [IF_CONS (os1 @ [DIP (1, [DROP 1; DROP 1])], os2)]
  | Switch_none (t, t1, p2, t2) ->
      let os = compile env t in
      let os1 = compile env t1 in
      let os2 = compile ((p2.desc,p2.typ)::env) t2 in
      os @ [IF_NONE (os1, os2 @ [DIP (1, [DROP 1])])]
  | Fun (_ty1, _ty2, p, body, fvars) ->
      (*
      Format.eprintf "fvars: @[%a@] env: @[%a@]@." 
        (Format.list ";@ " (fun ppf (id,ty) ->
             Format.fprintf ppf "%s:%a" (Ident.name id) M.Type.pp ty)) fvars
        (Format.list ";@ " (fun ppf (id,ty) ->
             Format.fprintf ppf "%s:%a" (Ident.name id) M.Type.pp ty)) env;
      *)
      begin match t.typ.desc with
        | TyLambda (ty1, ty2) ->
            begin match fvars with
              | [] ->
                  let env = [p.desc,p.typ] in
                  let o = compile env body in
                  let clean = [ COMMENT ("lambda clean up", [DIP (1, [ DROP 1 ]) ]) ] in
                  [ LAMBDA (ty1, ty2, o @ clean) ]
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
                          COMMENT ("fvar " ^ Ident.name v, [ DUP ; DIP (1, [ CAR ]); CDR ])) fvars
                    in
                    let len = List.length fvars in
                    let clean = [ COMMENT ("lambda clean up", [DIP (1, [ DROP (len + 1) ]) ]) ] in
                    LAMBDA (ity, ty2, extractor @ compile env body @ clean)
                  in
                  let partial_apply =
                    (* Apply fvars from xn to x1 *)
                    List.fold_left (fun st (x,_ty) ->
                        let o = compile_var ~loc ((Ident.dummy,tyUnit (* for lambda *))::env) x in
                        COMMENT ("partial app " ^ Ident.name x, o @ [APPLY]) :: st) [] fvars
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


let split_entry_point t =
  let rec f st t = match t.IML.desc with
    | IML.Let (p, t1, t2) -> f ((p,t1)::st) t2
    | _ -> (List.rev st, t)
  in
  f [] t

let compile_structure t =
  let defs, t = split_entry_point t in
  let ops, env = 
    List.fold_left (fun (ops, env) (p,t) ->
        let os1 = compile env t in
        ops @ [ COMMENT ("let " ^ Ident.name p.IML.desc, os1) ], 
        ((p.desc, p.typ)::env)) ([], []) defs
  in
  (* (parameter, storage) :: []
     -> parameter :: storage :: values
  *)

  (* replace fun by let *)
  let rec get_abst t = match t.IML.desc with
    | IML.Fun (_, _, p, t, _) -> p, t
    | Let (p, t1, t2) -> 
        let p', t2 = get_abst t2 in
        p', { t with desc= Let (p, t1, t2) }
    | _ -> assert false
  in
  let p1, t = get_abst t in
  let p2, t = get_abst t in
  let env = ((p2.desc,p2.typ)::(p1.desc,p1.typ)::env) in
  let os = compile env t in
  [ COMMENT ("defs", [DIP (1, ops)]) 
  ; COMMENT ("entry point init", [DUP ; CDR; DIP (1, [CAR])])
  ; COMMENT ("entry point", os )
  ; COMMENT ("final clean up",
             [ DIP (1, List.init (List.length env) (fun _ -> DROP 1)) ]) ]
  |> clean_failwith
