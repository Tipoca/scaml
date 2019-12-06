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
    (* XXX code duplicated. fragile against modifications *)
    let rec bits bs =
      if bs * 2 > len then bs
      else bits (bs * 2)
    in
    let bs = bits 2 in
    let nrights = (len - bs) + bs / 2 in
    let nlefts = len - nrights in
    assert (nrights > 0);
    assert (nlefts > 0);
    if i <= nlefts then Left :: path i nlefts
    else Right :: path (i - nlefts) nrights

let rec fold ~leaf ~branch = function
  | Leaf ty -> leaf ty
  | Branch (t1,t2) ->
    let t1 = fold ~leaf ~branch t1 in
    let t2 = fold ~leaf ~branch t2 in
    branch t1 t2