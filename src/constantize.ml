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

module C = Michelson.Constant

open IML

let rec f exp =
  let const c = Some c, { exp with desc= Const c } in
  let non_const desc = None, { exp with desc } in
  let f' exp = snd & f exp in
  let id = None, exp in
  match exp.desc with
  | Var _ -> id
  | AssertFalse -> id
  | Const c -> const c
  | Nil ->
      (* We cannot PUSH (list operation) {}. If we do, we get:
         "operation type forbidden in parameter, storage and constants" *)
      begin match exp.typ.desc with
        | TyList { desc= TyOperation } -> id
        | _ -> const (List [])
      end
  | IML_None -> const & Option None
  | IML_Some e ->
      begin match f e with
        | Some c, _ -> const & C.Option (Some c)
        | None, e -> non_const & IML_Some e
      end
  | Left t ->
      begin match f t with
        | Some c, _ -> const & Left c
        | None, e -> non_const & Left e
      end
  | Right t ->
      begin match f t with
        | Some c, _ -> const & Right c
        | None, e -> non_const & Right e
      end
  | Cons (h,t) ->
      begin match f h, f t with
      | (Some h, _), (Some (C.List t), _) ->
          const & C.List (h::t)
      | (_, eh), (_, et) -> non_const & Cons (eh, et)
      end
  | Pair (l,r) ->
      begin match f l, f r with
        | (Some l, _), (Some r, _) ->
            const & Pair (l, r)
        | (_, l), (_, r) -> non_const & Pair (l,r)
      end
  | Fun (pv, t) ->
      (* XXX We may be able to compile it down to Code constant,
         it must be done in compile.ml *)
      non_const & Fun (pv, f' t)
  | IfThenElse (t1, t2, None) ->
      non_const & IfThenElse (f' t1, f' t2, None)
  | IfThenElse (t1, t2, Some t3) ->
      non_const & IfThenElse (f' t1, f' t2, Some (f' t3))
  | App (t, ts) ->
      non_const & App (f' t, List.map (fun t -> f' t) ts)
  | Prim (s, ty, ts) ->
      non_const & Prim (s, ty, List.map (fun t -> f' t) ts)
  | Let (pv, t1, t2) ->
      non_const & Let (pv, f' t1, f' t2)
  | Assert t ->
      non_const & Assert (f' t)
  | Seq (t1, t2) ->
      non_const & Seq (f' t1, f' t2)
  | Contract_create (cs, l, t1, t2, t3) ->
      non_const & Contract_create (cs, l, f' t1, f' t2, f' t3)
  | Set ts ->
      let ts = List.map f ts in
      let check cs =
        let cs = List.sort compare cs in (* XXX OCaml's compare *)
        let rec check_uniq = function
          | [] | [_] -> ()
          | c1::c2::_ when c1 = c2 -> (* XXX OCaml's compare *)
              errorf_constant ~loc:exp.loc "Set literal contains duplicated value %a" C.pp c1
          | _::xs -> check_uniq xs
        in
        check_uniq cs;
        cs
      in
      let rec loop (stc,ste) ts = match stc, ts with
        | None, [] -> non_const & Set (List.rev ste)
        | Some cs, [] -> const & C.Set (check & List.rev cs)
        | None, (_,t)::ts -> loop (None, t::ste) ts
        | Some _, (None, t)::ts -> loop (None, t::ste) ts
        | Some cs, (Some c, t)::ts -> loop (Some (c::cs), t::ste) ts
      in
      loop (Some [], []) ts
  | Map tts ->
      let tts = List.map (fun (t1,t2) ->
          match f t1, f t2 with
          | (Some c1,t1), (Some c2,t2) -> Some (c1,c2), (t1,t2)
          | (_,t1), (_,t2) -> None, (t1,t2)) tts
      in
      let check kvs =
        let kvs = List.sort (fun (k1,_) (k2,_) -> compare k1 k2) kvs in (* XXX OCaml's compare *)
        let rec check_uniq = function
          | [] | [_] -> ()
          | (c1,_)::(c2,_)::_ when c1 = c2 -> (* XXX OCaml's compare *)
              errorf_constant ~loc:exp.loc "Map literal contains duplicated key %a" C.pp c1
          | _::xs -> check_uniq xs
        in
        check_uniq kvs;
        kvs
      in
      let rec loop (stc,ste) tts = match stc, tts with
        | None, [] -> non_const & Map (List.rev ste)
        | Some ccs, [] -> const & C.Map (check & List.rev ccs)
        | None, (_,tt)::tts -> loop (None, tt::ste) tts
        | Some _, (None, tt)::tts -> loop (None, tt::ste) tts
        | Some ccs, (Some cc, tt)::tts -> loop (Some (cc::ccs), tt::ste) tts
      in
      loop (Some [], []) tts
  | Switch_or (t1, pv1, t2, pv2, t3) ->
      non_const & Switch_or (f' t1, pv1, f' t2, pv2, f' t3)
  | Switch_cons (t1, pv1, pv2, t2, t3) ->
      non_const & Switch_cons (f' t1, pv1, pv2, f' t2, f' t3)
  | Switch_none (t1, t2, pv, t3) ->
      non_const & Switch_none (f' t1, f' t2, pv, f' t3)
  | BigMap tts ->
      (* XXX sort and uniq *)
      non_const & BigMap (List.map (fun (t1,t2) -> f' t1, f' t2) tts)

let f exp = match f exp with
  | None, t -> t
  | Some c, _ -> { exp with desc= Const c }
