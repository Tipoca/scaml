open Spotlib.Spot
open Tools

module M = Michelson
open M.Type
open M.Opcode

module MEnv : sig
  type t = (Ident.t * M.Type.t) list
  val find : Ident.t -> t -> (int * M.Type.t) option
(*
  val add : (Ident.t * M.Type.t) -> t -> t
  val add_list : (Ident.t * M.Type.t) list -> t -> t
  val empty : t
*)
  val pp : Format.formatter -> t -> unit
(*
  val of_list : (Ident.t * M.Type.t) list -> t
  val length : t -> int
  val singleton : (Ident.t * M.Type.t) -> t
*)
end = struct
  type t = (Ident.t * M.Type.t) list

  (*  let singleton x = [x] *)

  let find id env = 
    let rec aux n = function
      | [] -> None
      | (id',ty)::_ when id = id' -> Some (n,ty)
      | _::env -> aux (n+1) env
    in
    aux 0 env

(*
  let add x xs = x :: xs
  let add_list = (@)

  let empty = []
*)

  let pp ppf t =
    Format.fprintf ppf "@[<2>[ %a ]@]"
      (Format.list ";@ "
         (fun ppf (id, ty) ->
            Format.fprintf ppf 
              "%s : %a"
              (Ident.unique_name id)
              M.Type.pp ty)) t

(*
  let of_list xs = xs
    
  let length = List.length
*)
end

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
  | Some (n,_typ) ->
      let rec f = function
        | 0 -> [ DUP ]
        | n -> 
            assert (n > 0);
            [ DIP (f (n-1)); SWAP ]
      in
      f n

let rec compile env t = 
  let loc = t.IML.loc in
  match t.IML.desc with
  | Const op -> [ PUSH (t.typ, op) ]
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
      os @ [ ASSERT; PUSH (TyUnit, Unit) ]
  | AssertFalse -> [ FAIL ]
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
            let env' = (Ident.dummy, TyUnit (* dummy *)) :: env in
            env', os @ os') ts (env, [])
      in
      conv t.IML.typ pre
  | Let (pat, t1, t2) ->
      let os1 = compile env t1 in
      let os2 = compile ((pat.id, pat.typ)::env) t2 in
      COMMENT (Ident.name pat.id, os1) :: os2 
      @ [COMMENT ("clean " ^ Ident.name pat.id, [DIP [DROP]])]
  | Switch_or (t, p1, t1, p2, t2) ->
      let os = compile env t in
      let os1 = compile ((p1.id,p1.typ)::env) t1 in
      let os2 = compile ((p2.id,p2.typ)::env) t2 in
      os @ [IF_LEFT (os1 @ [DIP [DROP]], os2 @ [DIP [DROP]])]
  | Switch_cons (t, p1, p2, t1, t2) ->
      let os = compile env t in
      let os1 = compile ((p1.id,p1.typ)::(p2.id,p2.typ)::env) t1 in
      let os2 = compile env t2 in
      os @ [IF_CONS (os1 @ [DIP [DROP; DROP]], os2)]
  | Switch_none (t, t1, p2, t2) ->
      let os = compile env t in
      let os1 = compile env t1 in
      let os2 = compile ((p2.id,p2.typ)::env) t2 in
      os @ [IF_NONE (os1, os2 @ [DIP [DROP]])]
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
                    let ity = TyPair (ty1, closure_env_type xtys) in
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
                          ops @ [ IF_NONE ([ FAIL ], []) ],
                          if List.mem_assoc x xtys then (x,ty)::env else env
                      | (x,ty)::xtys ->
                          let ops = 
                            ops @
                            [ DUP ; DIP [ CAR; IF_NONE ([ FAIL (* to avoid the stack type differences *)], [])] ; CDR ]
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
                      | (x,_ty)::xtys ->
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
    | TyLambda (_ty1, _ty2, cinfo) ->
        begin match (repr_closure_info cinfo).closure_desc with
          | CLLink _ -> assert false
          | CLList [] ->
              (* it is not a closure *)
              let oarg = compile env arg in
              oarg @ [ EXEC ]
          | CLList _xs -> 
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
        ops @ [ COMMENT (Ident.name p.IML.id, os1) ], 
        ((p.id, p.typ)::env)) ([], []) defs
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
  let env = ((p2.id,p2.typ)::(p1.id,p1.typ)::env) in
  let os = compile env t in
  [ COMMENT ("defs", [DIP ops]) 
  ; COMMENT ("entry point init", [DUP ; CDR; DIP [CAR]])
  ; COMMENT ("entry point", os )
  ; COMMENT ("final clean up",
             [ DIP (List.init (List.length env) (fun _ -> DROP)) ]) ]
  |> clean_fail
    
let implementation sourcefile outputprefix modulename (str, _coercion) =
  Format.eprintf "sourcefile=%s outputprefix=%s modulename=%s@." sourcefile outputprefix modulename;
  let parameter, storage = IML.fix_entrypoint_type sourcefile str in
  let t = 
    try IML.structure [] str with 
    | e -> 
        Printexc.print_backtrace stderr;
        Format.eprintf "IML.structure: %s@." (Printexc.to_string e);
        raise e
  in

  let oc = open_out (outputprefix ^ ".scaml") in
  let ppf = Format.of_out_channel oc in
  Format.fprintf ppf "%a@." IML.pp t;
  close_out oc;

  let code = compile_structure t in
  let m = { M.Module.parameter; storage; code } in

  let oc = open_out (outputprefix ^ ".tz") in
  let ppf = Format.of_out_channel oc in
  Format.eprintf "@[<2>%a@]@." M.Module.pp m;
  Format.fprintf ppf "@[<2>%a@]@." M.Module.pp m;
  close_out oc
