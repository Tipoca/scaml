(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Base = struct

(* Sets over ordered types *)

type 'elt t = Empty | Node of {l:'elt t; v:'elt; r:'elt t; h:int}
  
(* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 *)

let height = function
    Empty -> 0
  | Node {h} -> h

(* Creates a new node with left son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | height l - height r | <= 2.
   Inline expansion of height for better speed. *)

let create l v r =
  let hl = match l with Empty -> 0 | Node {h} -> h in
  let hr = match r with Empty -> 0 | Node {h} -> h in
  Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)}

(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | height l - height r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)

let bal l v r =
  let hl = match l with Empty -> 0 | Node {h} -> h in
  let hr = match r with Empty -> 0 | Node {h} -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Set.bal"
    | Node{l=ll; v=lv; r=lr} ->
        if height ll >= height lr then
          create ll lv (create lr v r)
        else begin
          match lr with
            Empty -> invalid_arg "Set.bal"
          | Node{l=lrl; v=lrv; r=lrr}->
              create (create ll lv lrl) lrv (create lrr v r)
        end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Set.bal"
    | Node{l=rl; v=rv; r=rr} ->
        if height rr >= height rl then
          create (create l v rl) rv rr
        else begin
          match rl with
            Empty -> invalid_arg "Set.bal"
          | Node{l=rll; v=rlv; r=rlr} ->
              create (create l v rll) rlv (create rlr rv rr)
        end
  end else
    Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)}

(* Insertion of one element *)

let rec add compare x = function
    Empty -> Node{l=Empty; v=x; r=Empty; h=1}
  | Node{l; v; r} as t ->
      let c = compare x v in
      if c = 0 then t else
      if c < 0 then
        let ll = add compare x l in
        if l == ll then t else bal ll v r
      else
        let rr = add compare x r in
        if r == rr then t else bal l v rr

let singleton x = Node{l=Empty; v=x; r=Empty; h=1}

(* Beware: those two functions assume that the added v is *strictly*
   smaller (or bigger) than all the present elements in the tree; it
   does not test for equality with the current min (or max) element.
   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_element x = function
  | Empty -> singleton x
  | Node {l; v; r} ->
    bal (add_min_element x l) v r

let rec add_max_element x = function
  | Empty -> singleton x
  | Node {l; v; r} ->
    bal l v (add_max_element x r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_min_element v r
  | (_, Empty) -> add_max_element v l
  | (Node{l=ll; v=lv; r=lr; h=lh}, Node{l=rl; v=rv; r=rr; h=rh}) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      create l v r

(* Smallest and greatest element of a set *)

let rec min_elt = function
    Empty -> raise Not_found
  | Node{l=Empty; v} -> v
  | Node{l} -> min_elt l

let rec min_elt_opt = function
    Empty -> None
  | Node{l=Empty; v} -> Some v
  | Node{l} -> min_elt_opt l

let rec max_elt = function
    Empty -> raise Not_found
  | Node{v; r=Empty} -> v
  | Node{r} -> max_elt r

let rec max_elt_opt = function
    Empty -> None
  | Node{v; r=Empty} -> Some v
  | Node{r} -> max_elt_opt r

(* Remove the smallest element of the given set *)

let rec remove_min_elt = function
    Empty -> invalid_arg "Set.remove_min_elt"
  | Node{l=Empty; r} -> r
  | Node{l; v; r} -> bal (remove_min_elt l) v r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assume | height l - height r | <= 2. *)

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

(* Splitting.  split x s returns a triple (l, present, r) where
    - l is the set of elements of s that are < x
    - r is the set of elements of s that are > x
    - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)

let rec split compare x = function
    Empty ->
      (Empty, false, Empty)
  | Node{l; v; r} ->
      let c = compare x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split compare x l in (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = split compare x r in (join l v lr, pres, rr)

(* Implementation of the set operations *)

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec mem compare x = function
    Empty -> false
  | Node{l; v; r} ->
      let c = compare x v in
      c = 0 || mem compare x (if c < 0 then l else r)

let rec remove compare x = function
    Empty -> Empty
  | (Node{l; v; r} as t) ->
      let c = compare x v in
      if c = 0 then merge l r
      else
        if c < 0 then
          let ll = remove compare x l in
          if l == ll then t
          else bal ll v r
        else
          let rr = remove compare x r in
          if r == rr then t
          else bal l v rr

let rec union compare s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node{l=l1; v=v1; r=r1; h=h1}, Node{l=l2; v=v2; r=r2; h=h2}) ->
      if h1 >= h2 then
        if h2 = 1 then add compare v2 s1 else begin
          let (l2, _, r2) = split compare v1 s2 in
          join (union compare l1 l2) v1 (union compare r1 r2)
        end
      else
        if h1 = 1 then add compare v1 s2 else begin
          let (l1, _, r1) = split compare v2 s1 in
          join (union compare l1 l2) v2 (union compare r1 r2)
        end

let rec inter compare s1 s2 =
  match (s1, s2) with
    (Empty, _) -> Empty
  | (_, Empty) -> Empty
  | (Node{l=l1; v=v1; r=r1}, t2) ->
      match split compare v1 t2 with
        (l2, false, r2) ->
          concat (inter compare l1 l2) (inter compare r1 r2)
      | (l2, true, r2) ->
          join (inter compare l1 l2) v1 (inter compare r1 r2)

let rec diff compare s1 s2 =
  match (s1, s2) with
    (Empty, _) -> Empty
  | (t1, Empty) -> t1
  | (Node{l=l1; v=v1; r=r1}, t2) ->
      match split compare v1 t2 with
        (l2, false, r2) ->
          join (diff compare l1 l2) v1 (diff compare r1 r2)
      | (l2, true, r2) ->
          concat (diff compare l1 l2) (diff compare r1 r2)

type 'elt enumeration = End | More of 'elt * 'elt t * 'elt enumeration

let rec cons_enum s e =
  match s with
    Empty -> e
  | Node{l; v; r} -> cons_enum l (More(v, r, e))

let rec compare_aux compare e1 e2 =
    match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
      let c = compare v1 v2 in
      if c <> 0
      then c
      else compare_aux compare (cons_enum r1 e1) (cons_enum r2 e2)

let compare cmp s1 s2 =
  compare_aux cmp (cons_enum s1 End) (cons_enum s2 End)

let equal cmp s1 s2 =
  compare cmp s1 s2 = 0

let rec subset compare s1 s2 =
  match (s1, s2) with
    Empty, _ ->
      true
  | _, Empty ->
      false
  | Node {l=l1; v=v1; r=r1}, (Node {l=l2; v=v2; r=r2} as t2) ->
      let c = compare v1 v2 in
      if c = 0 then
        subset compare l1 l2 && subset compare r1 r2
      else if c < 0 then
        subset compare (Node {l=l1; v=v1; r=Empty; h=0}) l2 && subset compare r1 t2
      else
        subset compare (Node {l=Empty; v=v1; r=r1; h=0}) r2 && subset compare l1 t2

let rec iter f = function
    Empty -> ()
  | Node{l; v; r} -> iter f l; f v; iter f r

let rec fold f s accu =
  match s with
    Empty -> accu
  | Node{l; v; r} -> fold f r (f v (fold f l accu))

let rec for_all p = function
    Empty -> true
  | Node{l; v; r} -> p v && for_all p l && for_all p r

let rec exists p = function
    Empty -> false
  | Node{l; v; r} -> p v || exists p l || exists p r

let rec filter p = function
    Empty -> Empty
  | (Node{l; v; r}) as t ->
      (* call [p] in the expected left-to-right order *)
      let l' = filter p l in
      let pv = p v in
      let r' = filter p r in
      if pv then
        if l==l' && r==r' then t else join l' v r'
      else concat l' r'

let rec partition p = function
    Empty -> (Empty, Empty)
  | Node{l; v; r} ->
      (* call [p] in the expected left-to-right order *)
      let (lt, lf) = partition p l in
      let pv = p v in
      let (rt, rf) = partition p r in
      if pv
      then (join lt v rt, concat lf rf)
      else (concat lt rt, join lf v rf)

let rec cardinal = function
    Empty -> 0
  | Node{l; r} -> cardinal l + 1 + cardinal r

let rec elements_aux accu = function
    Empty -> accu
  | Node{l; v; r} -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let choose = min_elt

let choose_opt = min_elt_opt

let rec find compare x = function
    Empty -> raise Not_found
  | Node{l; v; r} ->
      let c = compare x v in
      if c = 0 then v
      else find compare x (if c < 0 then l else r)

let rec find_first_aux v0 f = function
    Empty ->
      v0
  | Node{l; v; r} ->
      if f v then
        find_first_aux v f l
      else
        find_first_aux v0 f r

let rec find_first f = function
    Empty ->
      raise Not_found
  | Node{l; v; r} ->
      if f v then
        find_first_aux v f l
      else
        find_first f r

let rec find_first_opt_aux v0 f = function
    Empty ->
      Some v0
  | Node{l; v; r} ->
      if f v then
        find_first_opt_aux v f l
      else
        find_first_opt_aux v0 f r

let rec find_first_opt f = function
    Empty ->
      None
  | Node{l; v; r} ->
      if f v then
        find_first_opt_aux v f l
      else
        find_first_opt f r

let rec find_last_aux v0 f = function
    Empty ->
      v0
  | Node{l; v; r} ->
      if f v then
        find_last_aux v f r
      else
        find_last_aux v0 f l

let rec find_last f = function
    Empty ->
      raise Not_found
  | Node{l; v; r} ->
      if f v then
        find_last_aux v f r
      else
        find_last f l

let rec find_last_opt_aux v0 f = function
    Empty ->
      Some v0
  | Node{l; v; r} ->
      if f v then
        find_last_opt_aux v f r
      else
        find_last_opt_aux v0 f l

let rec find_last_opt f = function
    Empty ->
      None
  | Node{l; v; r} ->
      if f v then
        find_last_opt_aux v f r
      else
        find_last_opt f l

let rec find_opt compare x = function
    Empty -> None
  | Node{l; v; r} ->
      let c = compare x v in
      if c = 0 then Some v
      else find_opt compare x (if c < 0 then l else r)

let try_join compare l v r =
  (* [join l v r] can only be called when (elements of l < v <
     elements of r); use [try_join l v r] when this property may
     not hold, but you hope it does hold in the common case *)
  if (l = Empty || compare (max_elt l) v < 0)
  && (r = Empty || compare v (min_elt r) < 0)
  then join l v r
  else union compare l (add compare v r)

let rec map compare f = function
  | Empty -> Empty
  | Node{l; v; r} as t ->
     (* enforce left-to-right evaluation order *)
     let l' = map compare f l in
     let v' = f v in
     let r' = map compare f r in
     if l == l' && v == v' && r == r' then t
     else try_join compare l' v' r'

let of_sorted_list l =
  let rec sub n l =
    match n, l with
    | 0, l -> Empty, l
    | 1, x0 :: l -> Node {l=Empty; v=x0; r=Empty; h=1}, l
    | 2, x0 :: x1 :: l ->
        Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1; r=Empty; h=2}, l
    | 3, x0 :: x1 :: x2 :: l ->
        Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1;
             r=Node{l=Empty; v=x2; r=Empty; h=1}; h=2}, l
    | n, l ->
      let nl = n / 2 in
      let left, l = sub nl l in
      match l with
      | [] -> assert false
      | mid :: l ->
        let right, l = sub (n - nl - 1) l in
        create left mid right, l
  in
  fst (sub (List.length l) l)

let of_list compare l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add compare x1 (singleton x0)
  | [x0; x1; x2] -> add compare x2 (add compare x1 (singleton x0))
  | [x0; x1; x2; x3] -> add compare x3 (add compare x2 (add compare x1 (singleton x0)))
  | [x0; x1; x2; x3; x4] -> add compare x4 (add compare x3 (add compare x2 (add compare x1 (singleton x0))))
  | _ -> of_sorted_list (List.sort_uniq compare l)

let add_seq compare i m =
  Seq.fold_left (fun s x -> add compare x s) m i

let of_seq compare i = add_seq compare i empty

let rec seq_of_enum_ c () = match c with
  | End -> Seq.Nil
  | More (x, t, rest) -> Seq.Cons (x, seq_of_enum_ (cons_enum t rest))

let to_seq c = seq_of_enum_ (cons_enum c End)

let to_seq_from compare low s =
  let rec aux low s c = match s with
    | Empty -> c
    | Node {l; r; v; _} ->
        begin match compare v low with
          | 0 -> More (v, r, c)
          | n when n<0 -> aux low r c
          | _ -> aux low l (More (v, r, c))
        end
  in
  seq_of_enum_ (aux low s End)

end


type 'elt t = { compare : 'elt -> 'elt -> int ; s : 'elt Base.t }
  
(* Insertion of one element *)

let add x { compare ; s } = { compare ; s = Base.add compare x s }

let singleton compare x = { compare ; s = Base.singleton x }

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_min_element v r
  | (_, Empty) -> add_max_element v l
  | (Node{l=ll; v=lv; r=lr; h=lh}, Node{l=rl; v=rv; r=rr; h=rh}) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      create l v r

(* Smallest and greatest element of a set *)

let rec min_elt = function
    Empty -> raise Not_found
  | Node{l=Empty; v} -> v
  | Node{l} -> min_elt l

let rec min_elt_opt = function
    Empty -> None
  | Node{l=Empty; v} -> Some v
  | Node{l} -> min_elt_opt l

let rec max_elt = function
    Empty -> raise Not_found
  | Node{v; r=Empty} -> v
  | Node{r} -> max_elt r

let rec max_elt_opt = function
    Empty -> None
  | Node{v; r=Empty} -> Some v
  | Node{r} -> max_elt_opt r

(* Remove the smallest element of the given set *)

let rec remove_min_elt = function
    Empty -> invalid_arg "Set.remove_min_elt"
  | Node{l=Empty; r} -> r
  | Node{l; v; r} -> bal (remove_min_elt l) v r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assume | height l - height r | <= 2. *)

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

(* Splitting.  split x s returns a triple (l, present, r) where
    - l is the set of elements of s that are < x
    - r is the set of elements of s that are > x
    - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)

let rec split compare x = function
    Empty ->
      (Empty, false, Empty)
  | Node{l; v; r} ->
      let c = compare x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split compare x l in (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = split compare x r in (join l v lr, pres, rr)

(* Implementation of the set operations *)

let empty compare = { compare ; s = Empty }

let is_empty = function { s = Empty } -> true | _ -> false

let mem x { compare ; s } = Base.mem compare x s

let remove x { compare ; s } = { compare ; s = Base.remove compare x s }

let union { s = s1 } { compare ; s = s2 } = { compare ; s = Base.union compare s1 s2 } 

let inter { s = s1 } { compare ; s = s2 } = { compare ; s = Base.inter compare s1 s2 }

let rec diff compare s1 s2 =
  match (s1, s2) with
    (Empty, _) -> Empty
  | (t1, Empty) -> t1
  | (Node{l=l1; v=v1; r=r1}, t2) ->
      match split compare v1 t2 with
        (l2, false, r2) ->
          join (diff compare l1 l2) v1 (diff compare r1 r2)
      | (l2, true, r2) ->
          concat (diff compare l1 l2) (diff compare r1 r2)

type 'elt enumeration = End | More of 'elt * 'elt t * 'elt enumeration

let rec cons_enum s e =
  match s with
    Empty -> e
  | Node{l; v; r} -> cons_enum l (More(v, r, e))

let rec compare_aux compare e1 e2 =
    match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
      let c = compare v1 v2 in
      if c <> 0
      then c
      else compare_aux compare (cons_enum r1 e1) (cons_enum r2 e2)

let compare cmp s1 s2 =
  compare_aux cmp (cons_enum s1 End) (cons_enum s2 End)

let equal cmp s1 s2 =
  compare cmp s1 s2 = 0

let rec subset compare s1 s2 =
  match (s1, s2) with
    Empty, _ ->
      true
  | _, Empty ->
      false
  | Node {l=l1; v=v1; r=r1}, (Node {l=l2; v=v2; r=r2} as t2) ->
      let c = compare v1 v2 in
      if c = 0 then
        subset compare l1 l2 && subset compare r1 r2
      else if c < 0 then
        subset compare (Node {l=l1; v=v1; r=Empty; h=0}) l2 && subset compare r1 t2
      else
        subset compare (Node {l=Empty; v=v1; r=r1; h=0}) r2 && subset compare l1 t2

let rec iter f = function
    Empty -> ()
  | Node{l; v; r} -> iter f l; f v; iter f r

let rec fold f s accu =
  match s with
    Empty -> accu
  | Node{l; v; r} -> fold f r (f v (fold f l accu))

let rec for_all p = function
    Empty -> true
  | Node{l; v; r} -> p v && for_all p l && for_all p r

let rec exists p = function
    Empty -> false
  | Node{l; v; r} -> p v || exists p l || exists p r

let rec filter p = function
    Empty -> Empty
  | (Node{l; v; r}) as t ->
      (* call [p] in the expected left-to-right order *)
      let l' = filter p l in
      let pv = p v in
      let r' = filter p r in
      if pv then
        if l==l' && r==r' then t else join l' v r'
      else concat l' r'

let rec partition p = function
    Empty -> (Empty, Empty)
  | Node{l; v; r} ->
      (* call [p] in the expected left-to-right order *)
      let (lt, lf) = partition p l in
      let pv = p v in
      let (rt, rf) = partition p r in
      if pv
      then (join lt v rt, concat lf rf)
      else (concat lt rt, join lf v rf)

let rec cardinal = function
    Empty -> 0
  | Node{l; r} -> cardinal l + 1 + cardinal r

let rec elements_aux accu = function
    Empty -> accu
  | Node{l; v; r} -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let choose = min_elt

let choose_opt = min_elt_opt

let rec find compare x = function
    Empty -> raise Not_found
  | Node{l; v; r} ->
      let c = compare x v in
      if c = 0 then v
      else find compare x (if c < 0 then l else r)

let rec find_first_aux v0 f = function
    Empty ->
      v0
  | Node{l; v; r} ->
      if f v then
        find_first_aux v f l
      else
        find_first_aux v0 f r

let rec find_first f = function
    Empty ->
      raise Not_found
  | Node{l; v; r} ->
      if f v then
        find_first_aux v f l
      else
        find_first f r

let rec find_first_opt_aux v0 f = function
    Empty ->
      Some v0
  | Node{l; v; r} ->
      if f v then
        find_first_opt_aux v f l
      else
        find_first_opt_aux v0 f r

let rec find_first_opt f = function
    Empty ->
      None
  | Node{l; v; r} ->
      if f v then
        find_first_opt_aux v f l
      else
        find_first_opt f r

let rec find_last_aux v0 f = function
    Empty ->
      v0
  | Node{l; v; r} ->
      if f v then
        find_last_aux v f r
      else
        find_last_aux v0 f l

let rec find_last f = function
    Empty ->
      raise Not_found
  | Node{l; v; r} ->
      if f v then
        find_last_aux v f r
      else
        find_last f l

let rec find_last_opt_aux v0 f = function
    Empty ->
      Some v0
  | Node{l; v; r} ->
      if f v then
        find_last_opt_aux v f r
      else
        find_last_opt_aux v0 f l

let rec find_last_opt f = function
    Empty ->
      None
  | Node{l; v; r} ->
      if f v then
        find_last_opt_aux v f r
      else
        find_last_opt f l

let rec find_opt compare x = function
    Empty -> None
  | Node{l; v; r} ->
      let c = compare x v in
      if c = 0 then Some v
      else find_opt compare x (if c < 0 then l else r)

let try_join compare l v r =
  (* [join l v r] can only be called when (elements of l < v <
     elements of r); use [try_join l v r] when this property may
     not hold, but you hope it does hold in the common case *)
  if (l = Empty || compare (max_elt l) v < 0)
  && (r = Empty || compare v (min_elt r) < 0)
  then join l v r
  else union compare l (add compare v r)

let rec map compare f = function
  | Empty -> Empty
  | Node{l; v; r} as t ->
     (* enforce left-to-right evaluation order *)
     let l' = map compare f l in
     let v' = f v in
     let r' = map compare f r in
     if l == l' && v == v' && r == r' then t
     else try_join compare l' v' r'

let of_sorted_list l =
  let rec sub n l =
    match n, l with
    | 0, l -> Empty, l
    | 1, x0 :: l -> Node {l=Empty; v=x0; r=Empty; h=1}, l
    | 2, x0 :: x1 :: l ->
        Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1; r=Empty; h=2}, l
    | 3, x0 :: x1 :: x2 :: l ->
        Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1;
             r=Node{l=Empty; v=x2; r=Empty; h=1}; h=2}, l
    | n, l ->
      let nl = n / 2 in
      let left, l = sub nl l in
      match l with
      | [] -> assert false
      | mid :: l ->
        let right, l = sub (n - nl - 1) l in
        create left mid right, l
  in
  fst (sub (List.length l) l)

let of_list compare l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add compare x1 (singleton x0)
  | [x0; x1; x2] -> add compare x2 (add compare x1 (singleton x0))
  | [x0; x1; x2; x3] -> add compare x3 (add compare x2 (add compare x1 (singleton x0)))
  | [x0; x1; x2; x3; x4] -> add compare x4 (add compare x3 (add compare x2 (add compare x1 (singleton x0))))
  | _ -> of_sorted_list (List.sort_uniq compare l)

let add_seq compare i m =
  Seq.fold_left (fun s x -> add compare x s) m i

let of_seq compare i = add_seq compare i empty

let rec seq_of_enum_ c () = match c with
  | End -> Seq.Nil
  | More (x, t, rest) -> Seq.Cons (x, seq_of_enum_ (cons_enum t rest))

let to_seq c = seq_of_enum_ (cons_enum c End)

let to_seq_from compare low s =
  let rec aux low s c = match s with
    | Empty -> c
    | Node {l; r; v; _} ->
        begin match compare v low with
          | 0 -> More (v, r, c)
          | n when n<0 -> aux low r c
          | _ -> aux low l (More (v, r, c))
        end
  in
  seq_of_enum_ (aux low s End)


