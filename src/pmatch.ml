open Spotlib.Spot
open IML
module Type = Michelson.Type

let create_var =
  let cntr = ref 0 in
  fun n -> incr cntr; Ident.create & n ^ string_of_int !cntr

let mke typ desc = { typ; desc; loc= IML.dummy_loc; attr= [] }
let mkp typ desc = { typ; desc; loc= IML.dummy_loc; attr= () }

(*
type ipat =
  | IPVar of var
  | IPLeft of pat
  | IPRight of pat
  | IPWild

(* rows <-> columns *)
let transpose : 'p list list -> 'p list list = fun rows ->
  if rows = [] then []
  else 
    let ncolumns = List.length & List.hd rows in
    List.init ncolumns (fun i ->
        List.map (fun row ->
            assert (List.length row = ncolumns);
            List.nth row i) rows)

let need_decompose (pats : pat option list) =
  List.exists (function
      | None -> false
      | Some pat ->
          match pat.desc with
          | PPair _ -> true
          | _ -> false) pats

let decompose_pair = function
  | None -> (None, None)
  | Some (pat : pat) -> match pat.desc with
    | PPair (p1, p2) -> (Some p1, Some p2)
    | PVar _ -> (None, None)
    | PWild -> (None, None)
    | PLeft _ | PRight _ -> assert false

let decompose_column : pat option list -> 
  ((var * var) * (pat option list * pat option list)) option = fun column ->
  if need_decompose column then
    let v1 = create_var "v1" in
    let v2 = create_var "v2" in
    Some ( (v1, v2), List.split & List.map decompose_pair column)
  else
    None (* no need *)
*)

let switch_pattern p = match p.desc with
  | PPair _ -> assert false
  | PUnit -> assert false
  | PLeft p -> (Some p, None)
  | PRight p -> (None, Some p)
  | PVar _ | PWild ->
      begin match p.typ.Type.desc with
        | TyPair (ty1, ty2) -> 
            (Some (mkp ty1 PWild), Some (mkp ty2 PWild))
        | _ -> assert false
      end

let switch_column column =
  List.split
  & List.map (function
      | None -> (None, None)
      | Some pat -> switch_pattern pat) column

let filter_cases 
    (column : pat option list) 
    (columns : pat option list list) 
    (es : 'e list)
  =
  List.map (fun col ->
      List.filter_map (function
          | None, _ -> None
          | Some _, e -> Some e) 
      & List.combine column col) (column :: columns),
  List.filter_map (function
      | None, _ -> None
      | Some _, e -> Some e)
    & List.combine column es

let left_right var column vars columns es =
  let var_l = Ident.(create & name var ^ "_l") in
  let var_r = Ident.(create & name var ^ "_r") in
  let vars_l = var_l :: vars in
  let vars_r = var_r :: vars in
  let column_l, column_r = switch_column column in
  let columns_l, es_l = filter_cases column_l columns es in
  let columns_r, es_r = filter_cases column_r columns es in
  (* match v with Left v_l -> ... | Right v_r -> ... *)
  `Match (var, var_l, vars_l, columns_l, es_l, 
               var_r, vars_r, columns_r, es_r)

(*
let print ppf (vars, columns, es) =
  let open Format in
  fprintf ppf "%a@ "
    (list ",@ " (fun ppf id -> fprintf ppf "%s" & Ident.name id))
    vars;
  fprintf ppf "cases: [@[<v>%a@]]"
    (list "@ | " (fun ppf (patos, e) ->
         fprintf ppf "[@[%a@] -> %d]"
           (list ", " 
              (fun ppf pato -> match pato with
                 | None -> string ppf "-"
                 | Some p -> pp_pat ppf p)) patos
           e))
    (List.combine (transpose columns) es)

let case_actions cases =
  List.mapi (fun i (pat, action) ->
      let pats = IdTys.elements & patvars pat in
      i, (pats, action) ) cases

let compile_match e cases =
  let v = create_var "m" in
  let open IML in
  let vars = [v] in
  let columns = transpose & List.map (fun (p,_) -> [Some p]) cases in
  let es = List.map snd cases in
  let esi = List.mapi (fun i _ -> i) es in
  Format.eprintf "MATCH of %a : %a@." IML.pp e Type.pp e.typ;
  Format.eprintf "@[%a@]@." print  (vars, columns, esi);
  let `Match (var, var_l, vars_l, columns_l, es_l,
                    var_r, vars_r, columns_r, es_r) = 
    left_right (List.hd vars) (List.hd columns) (List.tl vars) (List.tl columns) esi 
  in

  let ty = (snd & List.hd cases).typ in

  Format.eprintf "Left @[%a@]@." print (vars_l, columns_l, es_l);
  Format.eprintf "Right @[%a@]@." print (vars_r, columns_r, es_r);

  let t = mk_exp (Let (
      mkp e.typ var,
      e, 
      let ty_l, ty_r = match e.typ.desc with
        | TyOr (ty1, ty2) -> ty1, ty2
        | _ -> 
            Format.eprintf "ty: %a@." Type.pp e.typ;
            assert false
      in
      mk_exp (Switch_or (
          mk_exp (Var (var, e.typ)) e.typ,
          mkp ty_l var_l,
          assert false,
          mkp ty_r var_r,
          assert false))     
        ty)) ty
  in

  Format.eprintf "=> %a@." IML.pp t;
  assert false
*)

type compilation_result =
  | Action of int
  | Bind of Type.t * Ident.t * Ident.t * compilation_result
  | Left_right of Type.t * Ident.t * Type.t * Ident.t * Type.t * Ident.t * compilation_result * compilation_result
  
let rec compile_match vs cases =
  match vs with
  | [v] ->
      (* do we need something complex? *)
      let must_work = List.fold_left (fun st (p,_) -> match p.desc with
          (* since it is typed, no conflict between `Pair and `Or should happen *)
          | PPair _ -> `Pair
          | PLeft _ -> `Or
          | PRight _ -> `Or
          | PUnit -> st
          | PWild -> st
          | PVar _ -> st) `None cases
      in
      begin match must_work with
        | `None ->
            (* they are only wild or var *)
            begin match cases with
            | [] -> assert false
            | ({desc= PWild}, i) :: _ -> Action i
            | ({desc= PVar v'; typ}, i) :: _ -> Bind (typ, v', v, Action i)
            | _ -> assert false (* impossible *)
            end
        | `Pair -> assert false (* not yet *)
        | `Or ->
            let rev_left, rev_right = 
              List.fold_left (fun (rev_left, rev_right) (p, i) ->
                  match p.desc with
                  | PUnit -> assert false (* impossible *)
                  | PPair _ -> assert false (* impossible *)
                  | PWild -> assert false (* not yet *)
                  | PVar _ -> assert false (* not yet *)
                  | PLeft p -> (p,i)::rev_left, rev_right
                  | PRight p -> rev_left, (p,i)::rev_right) ([],[]) cases
            in
            let left = List.rev rev_left in
            let right = List.rev rev_right in
            let vl = create_var "vl" in
            let vr = create_var "vr" in
            let typ = (fst & List.hd cases).typ in 
            let typl, typr = match typ.desc with
              | TyOr (t1, t2) -> t1, t2
              | _ -> assert false
            in
            Left_right (typ, v, typl, vl, typr, vr, 
                        compile_match [vl] left,
                        compile_match [vr] right)
      end
  | _ -> assert false (* not yet *)

let rec build c acts = match c with
  | Action i -> List.nth acts i
  | Bind (typ, v1, v2, c) ->
      (* let v1 = v2 in [build x] *)
      let c = build c acts in
      mke c.typ (Let (mkp typ v1,
                      mke typ (Var (v2, typ)),
                      c))
  | Left_right (typ, v, typ1, v1, typ2, v2, c1 ,c2) ->
      let c1 = build c1 acts in
      let c2 = build c2 acts in
      mke c1.typ (Switch_or (mke typ (Var (v, typ)),
                             mkp typ1 v1, c1,
                             mkp typ2 v2, c2))
      
                  
let compile_match e cases =

  (* actions as functions *)
  let acts = 
    List.mapi (fun i (pat, action) -> 
        let v = create_var (Printf.sprintf "case%d" i) in
        let patvars = IdTys.elements & patvars pat in
        match patvars with
        | [] ->
            (* if [patvars = []], we need a [fun () ->].
               Think about the case of [| _ -> assert false].
            *)
            let pvar = mkp Type.tyUnit & create_var "unit" in
            let f = 
             mke (Type.tyLambda (Type.tyUnit, action.typ))
               (Fun (Type.tyUnit, action.typ, pvar, action))
            in
            (v, 
             f,
             mke action.typ (App (mke f.typ (Var (v, f.typ)),
                                  [mke Type.tyUnit Unit]))) 
        | _ -> 
            let f = List.fold_right (fun (v,ty) st ->
                mke (Type.tyLambda (ty, st.typ))
                  (Fun (ty, st.typ, mkp ty v, st))) patvars action
            in
            let e = 
              mke action.typ (App (mke f.typ (Var (v, f.typ)),
                                   List.map (fun (v,ty) -> mke ty (Var (v,ty))) patvars)) 
            in
            (v, f, e)) cases
  in

  let v = create_var "v" in

  let typ = (snd & List.hd cases).typ in

  (* let casei = fun ... in let v = e in ... *)
  let make x = 
    let match_ = 
      mke typ (Let (mkp e.typ v, e, x))
(*
                    mke typ (Match ( mke e.typ (Var (v, e.typ)), 
                                     List.map (fun (pat, _v, _f, e) ->
                                         (pat, e)) acts))))
*)
    in
    List.fold_right (fun (v, f, _e) st ->
        mke st.typ (Let (mkp f.typ v,
                         f, st))) acts match_
  in

  let x = compile_match [v] (List.mapi (fun i (pat, _) -> (pat, i)) cases) in

  let x = build x (List.map (fun (_,_,e) -> e) acts) in

  let e = make x in
  Format.eprintf "XXX %a@." IML.pp e;
  e

let rec compile e = 
  let mk desc = { e with desc } in
  match e.desc with
  | Const _ | Nil _
  | IML_None _ 
  | Unit
  | Var _
  | AssertFalse -> e
  | Cons (t1, t2) -> 
      let t1 = compile t1 in
      let t2 = compile t2 in
      mk & Cons (t1, t2)
  | IML_Some t -> mk & IML_Some (compile t)
  | Left (ty, t) -> mk & Left (ty, compile t)
  | Right (ty, t) -> mk & Right (ty, compile t)
  | Tuple (t1, t2) -> mk & Tuple (compile t1, compile t2)
  | Assert t -> mk & Assert (compile t)
  | Fun (ty1, ty2, patvar, t) -> mk & Fun (ty1, ty2, patvar, compile t)
  | IfThenElse (t1, t2, t3) -> mk & IfThenElse (compile t1, compile t2, compile t3)
  | App (t, ts) -> mk & App (compile t, List.map compile ts)
  | Prim (s, f, ts) -> mk & Prim (s, f, List.map compile ts)
  | Let (v, t1, t2) -> mk & Let (v, compile t1, compile t2)
  | Switch_or (t, v1, t1, v2, t2) -> 
      mk & Switch_or (compile t, v1, compile t1, v2, compile t2)
  | Switch_cons (t, v1, v2, t1, t2) ->
      mk & Switch_cons (compile t, v1, v2, compile t1, compile t2)
  | Switch_none (t, t1, v2, t2) ->
      mk & Switch_none (compile t, compile t1, v2, compile t2)
  | Match (t, cases) ->
      let t = compile t in
      let cases = List.map (fun (p,e) -> (p,compile e)) cases in
      compile_match t cases (* XXX location *)
