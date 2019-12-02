open Spotlib.Spot
open Tools

type side = Left | Right

type 'a tree =
  | Leaf of 'a
  | Branch of 'a tree * 'a tree

(* How to arrange n elements  *)
let rec place xs =
  match xs with 
  | [] -> assert false
  | [x] -> Leaf x
  | _ ->
      (* we need a branch. *)
      let len = List.length xs in
      let rec bits bs =
        if bs * 2 > len then bs
        else bits (bs * 2)
      in
      let bs = bits 2 in
      let nrights = (len - bs) + bs / 2 in
      let nlefts = len - nrights in
      Format.eprintf "binplace: %d => %d %d@." len nlefts nrights;
      assert (nrights > 0);
      assert (nlefts > 0);
      let lefts, rights = List.split_at nlefts xs in
      Branch (place lefts, place rights)

(* How to access i-th element in len elements *)
let rec path i len =
  assert (i < len);
  if len = 1 then []
  else
    let rec bits bs =
      if bs * 2 > len then bs
      else bits (bs * 2)
    in
    let bs = bits 2 in
    let nrights = min (len - bs) bs in
    let nlefts = len - nrights in
    if i <= nlefts then Left :: path i nlefts
    else Right :: path (i - nlefts) nrights

(*
open Types
module MT = Michelson.Type

(* Obtain type encoding *)
let encode _env f t = 
  let open Result.Infix in
  let t = Ctype.repr t in
  match t.desc with
  | Ttuple [] | Ttuple [_] -> assert false
  | Ttuple [_; _] -> assert false (* must be handled already *)
  | Ttuple ts ->
      Result.mapM f ts >>= fun ts ->
      let rec f = function
        | Leaf ty -> ty
        | Branch (t1,t2) -> MT.tyPair (f t1, f t2)
      in 
      Ok (f & place ts)
(*
  | Tconstr (p, params, _) -> 
      let constructors =
        try
          match Env.find_type_descrs p env with
          | [], [] -> (* abstract *)
              errorf "abstract type is not supported" (* XXX *)
          | constrs, [] ->
              let cs = List.map (fun c -> 
                  let args, res = Ctype.instance_constructor c in
                  begin try Ctype.unify env t with _ -> assert false end;
                  args) constrs 
              in
              
          | [], _labels -> assert false
          | exception _ -> assert false (* XXX *)
      in
    end
*)
  | _ -> Error "not supported" (* XXX *)

*)
