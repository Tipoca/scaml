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

(* InterMediate Language, or Intermediate ML *)
open Spotlib.Spot
open Tools
    
module M = Michelson
open M.Type

open IML

module VMap = Map.Make(struct type t = Ident.t let compare = compare end)

let count_variables t = 
  let incr v st = match VMap.find_opt v st with
    | None -> VMap.add v 1 st
    | Some n -> VMap.add v (n+1) st
  in
  let rec f t st = match t.desc with
    | Var id -> incr id st

    | Contract_create _
    | Const _ | Nil | IML_None | Unit | AssertFalse -> st

    | IML_Some t | Left t | Right t | Assert t
    | Fun (_, t) -> f t st

    | Let (_, t1, t2) | Cons (t1, t2) | Pair (t1, t2) | Seq (t1, t2) 
    | IfThenElse (t1, t2, None) -> f t1 & f t2 st

    | IfThenElse (t1, t2, Some t3) 
    | Switch_or (t1, _, t2, _, t3)
    | Switch_cons (t1, _, _, t2, t3)
    | Switch_none (t1, t2, _, t3) -> f t1 & f t2 & f t3 st

    | App (t, ts) -> List.fold_right f (t::ts) st
    | Prim (_, _, ts) -> List.fold_right f ts st
    | Set ts  -> List.fold_right f ts st
    | Map tts -> 
        List.fold_right (fun (t1,t2) st -> f t1 & f t2 st) tts st
    | BigMap tts -> 
        List.fold_right (fun (t1,t2) st -> f t1 & f t2 st) tts st
  in
  f t VMap.empty

(* 

   (fun x -> e1) e2  =>  let x = e2 in e1 

   let x = e2 in e1  =>  e1[e2/x]  
     when x appears only once in e1
          or e2 is just a variable

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
              (* (fun x -> e1) e2  =>  let x = e2 in e1 *) 
              f & mk & App ({ desc= Let (pat, t, body);
                              loc= t.loc; (* incorrect *)
                              typ= ty2;
                              attrs= [] },
                            ts)

          | {desc= Let (pv, t1, t2); typ; loc; attrs} ->
              (* (let x = e in f) e2 => let x = e in f e2 *)
              f { desc= Let (pv, t1, mk & App (t2, t::ts)); typ; loc ; attrs }

          | u -> mk & App (u, t::ts)
          end

      | Let (p, ({ desc= Var _ } as t1), t2) -> 
          (* let x = y in e  =>  e[y/x] *)
          let t2 = f t2 in
          add_attrs 
          & f & subst [p.desc, (Attr.add (Attr.Comment ("= " ^ Ident.unique_name p.desc)) t1)] t2

(*
      | Let (p, ({ desc= Fun _ } as t1), t2) -> 
          (* let x = fun .. -> .. in e  =>  e[fun .. -> ../x] *)
          add_attrs 
          & f & subst [p.desc, (Attr.add (Attr.Comment ("= " ^ Ident.unique_name p.desc)) t1)] t2
*)

      | Let (p, t1, t2) -> 
          let t2 = f t2 in
          let vmap = count_variables t2 in
          let not_expand = not & List.mem (Attr.Annot "not_expand") t.attrs in
          begin match VMap.find_opt p.desc vmap with
            | None -> 
                (* let x = e1 in e2 => e2[e1/x] *)
                add_attrs & f t2

            | Some 1 when IdTys.for_all (fun (_, ty) -> Michelson.Type.is_packable ~legacy:false ty) (freevars t1) && not not_expand ->
                (* let x = e1 in e2 => e2[e1/x] *)
                (* contract_self_id must not be inlined into LAMBDAs *)
                (* XXX This is adhoc *)
                add_attrs 
                & f & subst [p.desc, (Attr.add (Attr.Comment ("= " ^ Ident.unique_name p.desc)) t1)] t2

(*  This changes free variable occurrences inside `fun`.  Must be done with care.
   
            | Some 1 when p.desc <> Translate.contract_self_id -> 
                (* let x = e1 in e2 => e2[e1/x] *)
                (* contract_self_id must not be inlined into LAMBDAs *)
                (* XXX This is adhoc *)
                add_attrs 
                & f & subst [p.desc, (Attr.add (Attr.Comment ("= " ^ Ident.unique_name p.desc)) t1)] t2
*)
            | _ -> mk & Let (p, f t1, f t2)
          end

      | Var _ | Const _ | Nil | IML_None | Unit | AssertFalse | Contract_create _ -> 
          add_attrs { t with attrs= [] }
      | IML_Some t -> mk & IML_Some (f t)
      | Left t -> mk & Left (f t)
      | Right t -> mk & Right (f t)
      | Assert t -> mk & Assert (f t)
      | Fun (c, t) -> mk & Fun (c, f t)
      | Cons (t1, t2) -> mk & Cons (f t1, f t2)
      | Pair (t1, t2) -> mk & Pair (f t1, f t2)
      | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (f t1, f t2, Some (f t3))
      | IfThenElse (t1, t2, None) -> mk & IfThenElse (f t1, f t2, None)
      | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (f t1, p1, f t2, p2, f t3)
      | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (f t1, p1, p2, f t2, f t3)
      | Switch_none (t1, t2, p, t3) -> mk & Switch_none (f t1, f t2, p, f t3)
      | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
      | Seq (t1, t2) -> mk & Seq (f t1, f t2)
      | Set ts -> mk & Set (List.map f ts)
      | Map kvs -> mk & Map (List.map (fun (k,v) -> f k, f v) kvs)
      | BigMap kvs -> mk & BigMap (List.map (fun (k,v) -> f k, f v) kvs)
    in
    begin match !attrs with
      | Some _ -> assert false
      | None -> ()
    end;
    res
  in
  f t

