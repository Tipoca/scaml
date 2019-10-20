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
  | TyLambda (t1, t2, closure_info) ->
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
            [ DIP (f (n-1)); SWAP ]
      in
      [ COMMENT( Ident.name id, f n ) ]

let rec compile env t = 
  let loc = t.IML.loc in
  match t.IML.desc with
  | IML.Const op -> [ PUSH (t.typ, op) ]
  | Nil ty -> [ NIL (closure_type ty) ]
  | Cons (t1, t2) -> 
      let os2 = compile env t2 in
      let os1 = compile ((Ident.dummy, t2.typ)::env) t1 in
      os2 @ os1 @ [ CONS ]
  | IML_None ty -> [ NONE (closure_type ty) ]
  | IML_Some t1 -> 
      let os1 = compile env t1 in
      os1 @ [ SOME ]
  | Left (ty, t) ->
      let os = compile env t in
      os @ [ LEFT (closure_type ty) ]
  | Right (ty, t) -> 
      let os = compile env t in
      os @ [ RIGHT (closure_type ty) ]
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
      @ [COMMENT ("clean " ^ Ident.name pat.desc, [DIP [DROP]])]
  | Switch_or (t, p1, t1, p2, t2) ->
      let os = compile env t in
      let os1 = compile ((p1.desc,p1.typ)::env) t1 in
      let os2 = compile ((p2.desc,p2.typ)::env) t2 in
      os @ [IF_LEFT (os1 @ [DIP [DROP]], os2 @ [DIP [DROP]])]
  | Switch_cons (t, p1, p2, t1, t2) ->
      let os = compile env t in
      let os1 = compile ((p1.desc,p1.typ)::(p2.desc,p2.typ)::env) t1 in
      let os2 = compile env t2 in
      os @ [IF_CONS (os1 @ [DIP [DROP; DROP]], os2)]
  | Switch_none (t, t1, p2, t2) ->
      let os = compile env t in
      let os1 = compile env t1 in
      let os2 = compile ((p2.desc,p2.typ)::env) t2 in
      os @ [IF_NONE (os1, os2 @ [DIP [DROP]])]
  | Fun (_ty1, _ty2, p, body, fvars) ->
      (*
      Format.eprintf "fvars: @[%a@] env: @[%a@]@." 
        (Format.list ";@ " (fun ppf (id,ty) ->
             Format.fprintf ppf "%s:%a" (Ident.name id) M.Type.pp ty)) fvars
        (Format.list ";@ " (fun ppf (id,ty) ->
             Format.fprintf ppf "%s:%a" (Ident.name id) M.Type.pp ty)) env;
      *)
      begin match t.typ.desc with
        | TyLambda (ty1, ty2, cli) ->
            begin match (repr_closure_info cli).closure_desc with
              | CLLink _ -> assert false
              | CLEmpty -> assert false
              | CLList [] ->
                  let env = (p.desc,p.typ)::env in
                  let o = compile env body in
                  let clean = [ COMMENT ("lambda clean up", [DIP [ DROP ] ]) ] in
                  [ LAMBDA (closure_type ty1, closure_type ty2, o @ clean) ]
              | CLList xtys -> 
                  (* (x1:xty1),(x2:xty2),...,(xn:xtyn)
                     => (ty1 * (xty1 option * (xty2 option * ... * xtyn option)))
                  *)
                  let lambda =
                    let ity = tyPair (ty1, closure_env_type xtys) in
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

                    let init_ops = [ COMMENT( "get arg", [ DUP; DIP [ CAR ]; CDR ]) ] in (* to get a *)
                      (* (a,(x1,(x2,..xn))) ::s 
                         (x1o,(x2o,..xno)) :: a :: s 
                      *)
                    (* inside LAMBDA, the env is reset *)
                    let env = [(p.desc,p.typ)] in
                    let rec f ops env = function
                      | [] -> assert false
                      | [(x,ty)] -> 
                          if List.mem_assoc x fvars then 
                            ops @ [ COMMENT ("get " ^ Ident.name x, [IF_NONE ([ UNIT ; FAILWITH ], []) ]) ],
                            (x,ty)::env
                          else
                            ops @ [ COMMENT ("drop " ^ Ident.name x, [ DROP ] ) ],
                            env

                      | (x,ty)::xtys ->
                          let ops, env =
                            if List.mem_assoc x fvars then 
                              ops @ [ DUP ; DIP [ CAR; COMMENT ("get " ^ Ident.name x, [IF_NONE ([ UNIT ; FAILWITH ], [])]) ]; CDR ],
                              (x,ty)::env
                            else
                              ops @ [ COMMENT ("drop " ^ Ident.name x, []); CDR  ],
                              env
                          in
                          f ops env xtys
                    in
                    let ops, env = f init_ops env xtys in
                    (* Format.eprintf "Compiling body with %a (fvars %a)@." MEnv.pp env MEnv.pp xtys; *)
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
                          let os = f env xtys in
                          let env = (Ident.dummy, tyUnit)::env in
                          let os' = compile_var_or_default env x ty in
                          os @ os' @ [ PAIR ]
                    in
                    f env xtys
                  in
                  [ COMMENT ("clos", bindings); lambda; PAIR]
            end
        | _ -> assert false
      end
  | App (t, []) -> compile env t
  | App (f, args) ->
      let ofun = compile env f in
      let env = (Ident.dummy, tyUnit)::env in
      fst @@ List.fold_left (fun (ofun, ftyp) arg ->
          let oarg = compile env arg in
          let ofun = ofun @ oarg @ mk_application ftyp in
          let ftyp = match ftyp.desc with
            | TyLambda (_, ty2, _) -> ty2
            | _ -> assert false
          in
        (ofun, ftyp)) (ofun, f.typ) args

and mk_application fty =
  (* arg :: <lambda/closure> :: s 
     
     arg :: <lambda> :: s   EXEC
     res :: s                
     
     arg :: <closure> :: s           DIP [ DUP; CDR ; DIP [CAR ] ]
     arg :: <env> :: <lambda> :: s   PAIR
     (arg,<env>) :: <lambda> :: s    EXEC
     res :: s
     
  *)
  match fty.desc with
  | TyLambda (_ty1, _ty2, cinfo) ->
      begin match (repr_closure_info cinfo).closure_desc with
        | CLLink _ -> assert false
        | CLEmpty -> assert false
        | CLList [] ->
            (* it is not a closure *)
            [ EXEC ]
        | CLList _xs -> 
            (* it is a closure 
               arg :: (lambda, env) :: s  
               arg :: env :: lambda :: s  <-  DIP [ DUP; CDR; DIP [ CAR ] ]
               (arg,env) :: lambda ::s    <- PAIR
               EXEC!
            *)
            [ DIP [ DUP; CDR; DIP [ CAR ] ]; PAIR ; EXEC ]
      end
  | _ -> assert false

let split_entry_point t =
  let rec f st t = match t.IML.desc with
    | IML.Let (p, t1, { IML.desc= Unit }) ->
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
        ops @ [ COMMENT (Ident.name p.IML.desc, os1) ], 
        ((p.desc, p.typ)::env)) ([], []) defs
  in
  (* (parameter, storage) :: []
     -> parameter :: storage :: values
  *)
  let (_p,t) = entry_point in

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
  [ COMMENT ("defs", [DIP ops]) 
  ; COMMENT ("entry point init", [DUP ; CDR; DIP [CAR]])
  ; COMMENT ("entry point", os )
  ; COMMENT ("final clean up",
             [ DIP (List.init (List.length env) (fun _ -> DROP)) ]) ]
  |> clean_failwith
