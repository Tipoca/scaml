open Spotlib.Spot
open IML
open Tools
module Type = Michelson.Type
module C =Michelson.Constant

let create_var =
  let cntr = ref 0 in
  fun n -> incr cntr; Ident.create & n ^ string_of_int !cntr

let mke typ desc = { typ; desc; loc= IML.dummy_loc; attr= [] }
let mkp typ desc = { typ; desc; loc= IML.dummy_loc; attr= () }

(* rows <-> columns *)
let transpose : 'p list list -> 'p list list = fun rows ->
  if rows = [] then []
  else 
    let ncolumns = List.length & List.hd rows in
    List.init ncolumns (fun i ->
        List.map (fun row ->
            assert (List.length row = ncolumns);
            List.nth row i) rows)

type matrix = (pat list * int) list

type var = Ident.t * Type.t

type t = 
  | Fail
  | Leaf of (Ident.t * var) list * int
  | Switch of var * (IML.constr * var list * t) list * t option (* default *)

let rec pp ppf =
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
               f "%s %a (%a)" (string_of_constr c) pvs vs pp t
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
               f "%s %a (%a)" (string_of_constr c) pvs vs pp t
           )) xs
        pp d

(* specialize on Left and Right *)
let rec specialize c (matrix : matrix) : matrix =
  List.fold_right (fun (pats, i) st ->
      match pats with
      | [] -> assert false
      | pat::pats ->
          match c, pat.desc with
          | _, PAlias _ -> assert false
          | c, POr (p1, p2) ->
              specialize c [(p1::pats, i)]
              @ specialize c [(p2::pats, i)]
          | c, PConstr (c', ps) ->
              if c = c' then (ps @ pats, i) :: st else st

          (* For wild, we need to build another wild with arg type. 
             XXX Currently we must code for each.  Ugh.
          *)
          | CLeft, PWild -> 
              let typl = match pat.typ.desc with
                | TyOr (typl, _typr) -> typl
                | _ -> assert false
              in
              (mkp typl PWild :: pats, i) :: st

          | CRight, PWild -> 
              let typr = match pat.typ.desc with
                | TyOr (_typl, typr) -> typr
                | _ -> assert false
              in
              (mkp typr PWild :: pats, i) :: st

          | CSome, PWild -> 
              let typ = match pat.typ.desc with
                | TyOption typ -> typ
                | _ -> assert false
              in
              (mkp typ PWild :: pats, i) :: st

          | CNone, PWild -> (pats, i) :: st

          | CCons, PWild -> 
              let typ = match pat.typ.desc with
                | TyList typ -> typ
                | _ -> assert false
              in
              (mkp typ PWild :: mkp pat.typ PWild :: pats, i) :: st

          | CNil, PWild -> (pats, i) :: st

          | CBool _, PWild -> (pats, i) :: st

          | CConstant _, PWild -> (pats, i) :: st

          | _ -> not_yet ()
    ) matrix []

let rec default (matrix : matrix) : matrix =
  List.fold_right (fun (pats, i) st ->
      match pats with
      | [] -> assert false
      | pat::pats ->
          let rec f pat = match pat.desc with
            | PConstr (_, _) -> st
            | PWild -> (pats, i) :: st
            | PAlias (pat, _id, _loc) -> f pat
            | PVar _ -> not_yet ()
            | POr (p1, p2) ->
                default [(p1::pats, i)]
                @ default [(p2::pats, i)]
          in
          f pat
    ) matrix []

(* Extract x's of (p as x) in the column, and make columns for them *)
let unalias_column column : pat list * pat list list =
  let column0 = column in 
  (* extract aliases *)
  let xs, column =
    List.fold_right (fun pat (st, pats) ->
        let rec f pat = match pat.desc with
          | PAlias (pat, x, _) -> 
              let xs, pat = f pat in x::xs, pat
          | PWild | PVar _ | PConstr _ | POr _ -> [], pat
        in
        let xs, pat  = f pat in
        xs @ st, pat::pats) column ([], []) 
  in
  (* create new columns for xs *)
  column,
  List.map (fun x ->
      let rec has_x pat = match pat.desc with
        | PAlias (_, y, _) when x = y -> true
        | PAlias (pat, _, _) -> has_x pat
        | PWild | PVar _ | PConstr _ | POr _ -> false
      in
      List.map (fun pat0 ->
          { pat0 with desc= if has_x pat0 then PVar x else PWild }
        ) column0
    ) xs
                  
(* Extract x's of (p as x) in the column, and make columns for them *)
let unalias_matrix o (matrix : matrix) : _ list * matrix =
  let columns = transpose & List.map fst matrix in
  let ocolumns = 
    List.concat 
    & List.map2 (fun ov column ->
        let column, new_columns = unalias_column column in
        (ov, column) :: List.map (fun c -> ov, c) new_columns) o columns
  in
  List.map fst ocolumns,
  List.map2 (fun pats (_,i) -> pats, i)
    (transpose & List.map snd ocolumns) matrix

let swap i o (matrix : matrix) : _ * matrix =
  let rec f rev_st i xs = match i, xs with
    | 0, x::xs -> x::List.rev rev_st@xs
    | _, [] -> assert false
    | i, x::xs -> f (x::rev_st) (i-1) xs
  in
  f [] i o,
  List.map (fun (ps,e) -> f [] i ps, e) matrix

let pp_omatrix ppf (o, matrix) =
  let open Format in
  fprintf ppf "match %a with@." (list ", " (fun ppf (id,_) -> fprintf ppf "%s" & Ident.name id))
    o;
  List.iter (fun (pats, i) ->
      eprintf "| %a -> %d@."
        (list ", " pp_pat) pats
        i) matrix

let rec cc o matrix = 
  Format.eprintf "compile: %a" pp_omatrix (o,matrix);
  let o', matrix' = unalias_matrix o matrix in
  if (o, matrix) <> (o', matrix') then
    Format.eprintf "simplify: %a" pp_omatrix (o,matrix);
  let o, matrix = o', matrix' in

  match matrix with
  | [] -> Fail
  | (ps, a)::_ ->
      if List.for_all (fun p -> match p.desc with 
          | PAlias _ -> assert false
          | PWild | PVar _ -> true 
          | PConstr _ -> false
          | POr _ -> false
        ) ps 
      then 
        let binder = List.fold_right2 (fun v p st -> match p.desc with
            | PWild -> st
            | PVar v' -> (v',v)::st
            | PAlias _ | PConstr _ | POr _ -> assert false) o ps []
        in
        Leaf (binder, a)
      else 
        (* find column i where at least one pattern which is not a wildcard *)
        let columns = transpose & List.map fst matrix in
        let icolumns = List.mapi (fun i c -> (i,c)) columns in
        let i, column = 
          match 
            List.find_all (fun (_i,c) ->
                List.exists (fun p -> 
                    let rec f p = match p.desc with
                      | PAlias _ -> assert false
                      | PWild | PVar _ -> false
                      | PConstr _ -> true
                      | POr (p1, p2) -> f p1 || f p2
                    in
                    f p
                  ) c) icolumns
          with
          | [] -> assert false
          | (i,c)::_ -> i,c (* blindly select the first *)
        in
        (* algo 3 (a) *)
        let algo o column =

          let constructors = 
            List.sort_uniq compare
            & List.fold_left (fun st p -> 
                let rec f p = match p.desc with
                  | PAlias _ -> assert false
                  | PConstr (c, _) -> [c]
                  | PWild | PVar _ -> []
                  | POr (p1, p2) -> f p1 @ f p2
                in
                f p @ st
              ) [] column
          in
          (* for Left and Right, true and false, 
             think constructors are always full *)
          let constructors = 
            match constructors with
            | [CLeft] | [CRight] -> [CLeft; CRight]
            | [CSome] | [CNone] -> [CSome; CNone]
            | [CCons] | [CNil] -> [CCons; CNil]
            | [(CBool _)] -> [(CBool true) ; (CBool false)]
            | _ -> constructors
          in
          (* XXX weak. this depends on the above code *)
          let is_signature = 
            match constructors with
            | [CLeft; CRight]
            | [CSome; CNone]
            | [CCons; CNil]
            | [CPair]
            | [(CBool true) ; (CBool false)] -> true
            | _ -> false
          in
          assert (constructors <> []);
          Switch (List.hd o, 

                  (let vty = snd (List.hd o) in
                  List.map (fun c ->
                      let o = List.tl o in
                      let vs = 
                        match c with
                        | CLeft -> 
                            let ty = match vty.desc with
                              | TyOr (ty, _) -> ty
                              | _ -> assert false
                            in
                            [ create_var "l", ty ]
                        | CRight -> 
                            let ty = match vty.desc with
                              | TyOr (_, ty) -> ty
                              | _ -> assert false
                            in
                            [ create_var "r", ty ]
                        | CPair -> 
                            let ty1,ty2 = match vty.desc with
                              | TyPair (ty1, ty2) -> ty1, ty2
                              | _ -> assert false
                            in
                            [ create_var "l", ty1 ; 
                              create_var "r", ty2 ]
                        | CCons -> 
                            let ty = match vty.desc with
                              | TyList ty -> ty
                              | _ -> assert false
                            in
                            [ create_var "hd", ty 
                            ; create_var "tl", vty
                            ]
                        | CNil -> []
                        | CSome -> 
                            let ty = match vty.desc with
                              | TyOption ty -> ty
                              | _ -> assert false
                            in
                            [ create_var "x", ty ]
                        | CNone -> []

                        | CBool _ -> []

                        | CConstant _ -> [] (* int/nat/tz *)

                        | x -> 
                            Format.eprintf "xxx %s@." (string_of_constr x);
                            not_yet ()
                      in
                      c, vs, cc (vs @ o) (specialize c matrix)
                    ) constructors),

                  if is_signature then None
                  else Some (cc (List.tl o) (default matrix))
                 )
          in
          if i = 0 then algo o column
          else begin
            let o', matrix' = swap i o matrix in
            cc o' matrix' (* xxx inefficient *)
          end
  
  

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

let mkeq e1 e2 =
  let prim = snd (List.assoc "=" Primitives.primitives) 
             & Type.tyLambda (e1.typ, Type.tyLambda (e2.typ, Type.tyBool)) in
  mke Type.tyBool (Prim ("=", prim, [e1; e2]))

let mkv (id, typ) = mke typ & Var (id, typ)

let rec build aty acts t = match t with
  | Fail -> assert false (* ? *)
  | Leaf (binders, i) -> 
      List.fold_right (fun (v,(v',ty)) st ->
          mke aty (Let (mkp ty v,
                        mke ty (Var (v', ty)),
                        st))) binders
        (List.nth acts i)
  | Switch (_, [], _) -> assert false
  | Switch (v, [CPair, [v1,ty1; v2,ty2], t], None) ->
      let t = build aty acts t in
      mke aty & Let (mkp ty1 v1, mkfst & mkv v, 
                     mke aty & Let (mkp ty2 v2, mksnd & mkv v,
                                    t))
  | Switch (_, [CUnit, [], t], None) -> build aty acts t
  | Switch (v, ( [ CLeft, [vl,tyl], tl
                 ; CRight, [vr,tyr], tr ]
               | [ CRight, [vr,tyr], tr 
                 ; CLeft, [vl,tyl], tl ] ), None) ->
      let tl = build aty acts tl in
      let tr = build aty acts tr in
      mke aty & Switch_or (mkv v, 
                           mkp tyl vl, tl,
                           mkp tyr vr, tr)
                           
  | Switch (v, ( [(CBool true), [], tt ;
                  (CBool false), [], tf]
               | [(CBool false), [], tf ;
                  (CBool true), [], tt] ), None) ->
      let tt = build aty acts tt in
      let tf = build aty acts tf in
      mke aty & IfThenElse (mkv v, tt, tf)
                           
  | Switch (v, ( [CSome, [vs,tys], ts;
                  CNone, [], tn]
               | [CNone, [], tn; 
                  CSome, [vs,tys], ts]), None) ->
      let ts = build aty acts ts in
      let tn = build aty acts tn in
      mke aty & Switch_none (mkv v, tn, mkp tys vs, ts) 
                           
  | Switch (v, ( [CCons, [v1,ty1; v2,ty2], tc;
                  CNil, [], tn]
               | [CNil, [], tn; 
                  CCons, [v1,ty1; v2,ty2], tc]), None) ->
      let tc = build aty acts tc in
      let tn = build aty acts tn in
      mke aty & Switch_cons (mkv v, 
                             mkp ty1 v1, mkp ty2 v2, tc, tn)

  | Switch (_v, _cases, None) -> assert false

  | Switch (v, cases, Some d) ->
      (* all cases must be about constants with infinite members *)
      if not & List.for_all (function (CConstant _, [], _) -> true
                                    | (CConstant _, _, _) -> assert false
                                    | (c, _, _) -> 
                                        prerr_endline (string_of_constr c);
                                        false) cases
      then assert false;
      List.fold_right (fun case telse ->
          match case with 
          | (CConstant c, [], t) ->
              let t = build aty acts t in
              mke aty & IfThenElse (mkeq (mkv v) 
                                      (mke (snd v) & Const c),
                                    t, telse)
          | _ -> assert false) cases  & build aty acts d

            
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
    in
    List.fold_right (fun (v, f, _e) st ->
        mke st.typ (Let (mkp f.typ v,
                         f, st))) acts match_
  in

  let matrix : matrix = List.mapi (fun i (pat, _) -> ([pat], i)) cases in
  let res = cc [v,e.typ] matrix in
  Format.eprintf "pmatch debug: %a@." pp res;

  let e = build typ (List.map (fun (_,_,e) -> e) acts) res in
  Format.eprintf "pmatch debug: %a@." IML.pp e;
  make e

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

