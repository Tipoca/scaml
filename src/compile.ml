open Spotlib.Spot
open Tools

module M = Michelson
open M.Type
open M.Opcode

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

let rec compile env t = 
  let os = desc env t in
  let comments = 
    List.filter_map (function
        | IML.Attr.Comment s -> Some s
      ) t.IML.attrs
  in
  match comments with
  | [] -> os
  | _ -> [COMMENT (String.concat ", " comments, os)]
      
and desc env t =
  let loc = t.IML.loc in
  match t.IML.desc with
  | IML.Const op -> [ PUSH (t.typ, op) ]
  | Nil -> 
      let ty = match t.typ.desc with
        | TyList ty -> ty
        | _ -> assert false
      in
      [ NIL ty ]
  | Cons (t1, t2) -> 
      let os2 = compile env t2 in
      let os1 = compile ((Ident.dummy, t2.typ)::env) t1 in
      os2 @ os1 @ [ CONS ]
  | IML_None -> 
      let ty = match t.IML.typ.desc with
        | TyOption ty -> ty
        | _ -> assert false
      in
      [ NONE ty ]
  | IML_Some t1 -> 
      let os1 = compile env t1 in
      os1 @ [ SOME ]
  | Left t' ->
      let ty = match t.typ.desc with
        | TyOr (_, ty) -> ty
        | _ -> assert false
      in
      let os = compile env t' in
      os @ [ LEFT ty ]
  | Right t' -> 
      let ty = match t.typ.desc with
        | TyOr (ty, _) -> ty
        | _ -> assert false
      in
      let os = compile env t' in
      os @ [ RIGHT ty ]
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
                          COMMENT ("fvar " ^ Ident.unique_name v, [ DUP ; DIP (1, [ CAR ]); CDR ])) fvars
                    in
                    let len = List.length fvars in
                    let clean = [ COMMENT ("lambda clean up", [DIP (1, [ DROP (len + 1) ]) ]) ] in
                    LAMBDA (ity, ty2, extractor @ compile env body @ clean)
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
  | Contract_create_raw (m, e1, e2, e3) -> (* XXX *)
      List.fold_left (fun os arg ->
          let oarg = compile env arg in
          oarg @ os) [] [e1; e2; e3]
      @ [CREATE_CONTRACT m ; PAIR]
      

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
  [ COMMENT ("defs", if ops = [] then [] else [DIP (1, ops)]) 
  ; COMMENT ("entry point init", [DUP ; CDR; DIP (1, [CAR])])
  ; COMMENT ("entry point", os )
  ; COMMENT ("final clean up", [ DIP (1, [ DROP (List.length env) ]) ])]
  |> clean_failwith
