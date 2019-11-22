open Spotlib.Spot
open IML
module Type = Michelson.Type

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
  | Switch of var * (sw * var list * t) list
        
and sw = 
  | SLeft
  | SRight
  | SPair
  | SUnit

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
  | Switch (v, xs) ->
      f "@[<2>Switch %s@ [ @[%a@] ]@]"
        (Ident.name & fst v)
        (Format.list ";@ " (fun ppf -> 
             let f fmt = Format.fprintf ppf fmt in
             let pvs _ppf vs = 
               f "%s" & String.concat "," & List.map (fun (x,_) -> Ident.name x) vs
             in
             function
             | SLeft, vs, t -> f "@[SLeft %a (%a)@]" pvs vs pp t
             | SRight, vs, t -> f "@[SRight %a (%a)@]" pvs vs pp t
             | SPair, vs, t -> f "SPair %a (%a)" pvs vs pp t
             | SUnit, vs, t -> f "SUnit %a (%a)" pvs vs pp t)) xs

(* specialize on Left and Right *)
let specialize c (matrix : matrix) : matrix =
  List.fold_right (fun (pats, i) st ->
      match pats with
      | [] -> assert false
      | pat::pats ->
          match c, pat.desc with
          | SLeft,  PLeft p  -> (p :: pats, i) :: st
          | SRight, PRight p -> (p :: pats, i) :: st
          | _, (PLeft _ | PRight _)  -> st
          | SLeft, PWild -> 
              let typl = match pat.typ.desc with
                | TyOr (typl, _typr) -> typl
                | _ -> assert false
              in
              (mkp typl PWild :: pats, i) :: st
          | SRight, PWild -> 
              let typr = match pat.typ.desc with
                | TyOr (_typl, typr) -> typr
                | _ -> assert false
              in
              (mkp typr PWild :: pats, i) :: st
          | SPair, PPair (p1, p2) -> (p1 :: p2 :: pats, i) :: st
          | _, PWild -> assert false 
          | _, PVar _ -> assert false (* not yet *)
          | _, PUnit -> assert false (* not yet *)
          | _, PPair _ -> assert false (* impos *)
    ) matrix []

let default (matrix : matrix) : matrix =
  List.fold_right (fun (pats, i) st ->
      match pats with
      | [] -> assert false
      | pat::pats ->
          match pat.desc with
          | PLeft _ -> st
          | PRight _ -> st
          | PWild -> (pats, i) :: st
          | PVar _ -> assert false (* not yet *)
          | PUnit -> assert false (* not yet *)
          | PPair _ -> assert false (* impos *)
    ) matrix []

let swap i o (matrix : matrix) : _ * matrix =
  let rec f rev_st i xs = match i, xs with
    | 0, x::xs -> x::List.rev rev_st@xs
    | _, [] -> assert false
    | i, x::xs -> f (x::rev_st) (i-1) xs
  in
  f [] i o,
  List.map (fun (ps,e) -> f [] i ps, e) matrix

let rec cc o matrix = match matrix with
  | [] -> Fail
  | (ps, a)::_ ->
      if List.for_all (fun p -> match p.desc with PWild | PVar _ -> true | _ -> false) ps then 
        let binder = List.fold_right2 (fun v p st -> match p.desc with
            | PWild -> st
            | PVar v' -> (v',v)::st
            | _ -> assert false) o ps []
        in
        Leaf (binder, a)
      else 
        (* find column i where at least one pattern which is not a wildcard *)
        let columns = transpose & List.map fst matrix in
        let icolumns = List.mapi (fun i c -> (i,c)) columns in
        let i, column = 
          match 
            List.find_all (fun (_i,c) ->
                List.exists (fun p -> match p.desc with
                    | PWild | PVar _ -> false
                    | _ -> true) c) icolumns
          with
          | [] -> assert false
          | (i,c)::_ -> i,c (* blindly select the first *)
        in
        (* algo 3 (a) *)
        let algo o column =
          let constructors = 
            List.sort_uniq compare
            & List.fold_left (fun st p -> match p.desc with
                | PLeft  _ -> SLeft  :: st
                | PRight _ -> SRight :: st
                | PPair _ ->  SPair :: st
                | PUnit ->  SUnit :: st
                | PWild | PVar _ -> st) [] column
          in
          (* for Left and Right, think constructors are always full *)
          let constructors = 
            match constructors with
            | [SLeft] | [SRight] -> [SLeft; SRight]
            | _ -> constructors
          in
          let _is_signature = 
            match constructors with
            | [SLeft; SRight]
            | [SRight; SLeft]
            | [SPair] 
            | [SUnit] -> true
            | _ -> false
          in
          assert (constructors <> []);
          Switch (List.hd o, 
                  let vty = snd (List.hd o) in
                  List.map (fun c ->
                      let o = List.tl o in
                      let vs = 
                        match c with
                        | SLeft -> 
                            let ty = match vty.desc with
                              | TyOr (ty, _) -> ty
                              | _ -> assert false
                            in
                            [ create_var "l", ty ]
                        | SRight -> 
                            let ty = match vty.desc with
                              | TyOr (_, ty) -> ty
                              | _ -> assert false
                            in
                            [ create_var "r", ty ]
                        | SPair -> 
                            let ty1,ty2 = match vty.desc with
                              | TyPair (ty1, ty2) -> ty1, ty2
                              | _ -> assert false
                            in
                            [ create_var "l", ty1 ; 
                              create_var "r", ty2 ]
                        | _ -> assert false
                      in
                      c, vs, cc (vs @ o) (specialize c matrix)
                    ) constructors
(*
                    @
                    if is_signature then [] 
                    else cc (List.tl o) (default matrix)
*)
                 )
        in
        if i = 0 then algo o column
        else begin
          Format.eprintf "i=%d@." i;
          let o', matrix' = swap i o matrix in
          cc o' matrix' (* xxx inefficient *)
        end
  
  

let mkfst e =
  let ty = match e.typ.desc with
    | TyPair (ty, _) -> ty
    | _ -> assert false
  in
  let prim = snd (List.assoc "fst" Primitives.primitives) e.typ in
  mke ty (Prim ("fst", prim, [e]))

let mksnd e =
  let ty = match e.typ.desc with
    | TyPair (_, ty) -> ty
    | _ -> assert false
  in
  let prim = snd (List.assoc "snd" Primitives.primitives) e.typ in
  mke ty (Prim ("snd", prim, [e]))

let mkv (id, typ) = mke typ & Var (id, typ)

let rec build aty acts t = match t with
  | Fail -> assert false (* ? *)
  | Leaf (binders, i) -> 
      List.fold_right (fun (v,(v',ty)) st ->
          mke aty (Let (mkp ty v,
                        mke ty (Var (v', ty)),
                        st))) binders
        (List.nth acts i)
  | Switch (_, []) -> assert false
  | Switch (v, [SPair, [v1,ty1; v2,ty2], t]) ->
      let t = build aty acts t in
      mke aty & Let (mkp ty1 v1, mkfst & mkv v, 
                     mke aty & Let (mkp ty2 v2, mksnd & mkv v,
                                    t))
  | Switch (_, (SPair, _, _)::_) -> assert false
  | Switch (_, [SUnit, [], t]) -> build aty acts t
  | Switch (_, (SUnit, _, _)::_) -> assert false
  | Switch (v, ( [SLeft, [vl,tyl], tl ;
                  SRight, [vr,tyr], tr]
               | [SRight, [vr,tyr], tr;
                  SLeft, [vl,tyl], tl] )) ->
      let tl = build aty acts tl in
      let tr = build aty acts tr in
      mke aty & Switch_or (mkv v, 
                           mkp tyl vl, tl,
                           mkp tyr vr, tr)
                           
  | Switch (_, ((SLeft | SRight), _, _ ) :: _) -> assert false
      

            
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

