(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                     Copyright 2019  DaiLambda, Inc.                    *)
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
module Type = M.Type

type ('desc, 'attrs) with_loc_and_type =
  { desc  : 'desc
  ; loc   : Location.t
  ; typ   : Type.t
  ; attrs : 'attrs
  }

module IdTys = Set.Make(struct type t = Ident.t * Type.t let compare (id1,_) (id2,_) = compare id1 id2 end)

module PatVar = struct
  type t = (Ident.t, unit) with_loc_and_type
  
  let pp ppf var = Format.fprintf ppf "%s" (Ident.unique_name var.desc)
end

module Attr = struct
  type t = 
    | Comment of string
  
  type ts = t list
  
  let add a t = { t with attrs= a :: t.attrs }
  let adds attrs t = { t with attrs= attrs @ t.attrs }
end

type contract_source =
  | Tz_code of string
  | Tz_file of string

type t = (desc, Attr.ts) with_loc_and_type

and desc =
  | Const of M.Constant.t
  | Nil
  | Cons of t * t
  | IML_None
  | IML_Some of t
  | Left of t
  | Right of t
  | Unit
  | Var of Ident.t
  | Pair of t * t
  | Assert of t
  | AssertFalse
  | Fun of PatVar.t * t
  | IfThenElse of t * t * t option
  | App of t * t list
  | Prim of string * (M.Opcode.t list -> M.Opcode.t list) * t list
  | Let of PatVar.t * t * t
  | Switch_or of t * PatVar.t * t * PatVar.t * t
  | Switch_cons of t * PatVar.t * PatVar.t * t * t
  | Switch_none of t * t * PatVar.t * t
  | Contract_create of contract_source * Location.t * t * t * t
  | Seq of t * t
  | Set of t list
  | Map of (t * t) list

let pp ppf = 
  let p = Format.pp_print_string ppf in
  let f fmt = Format.fprintf ppf fmt in
  let rec pp _ppf t =
    match t.attrs with
    | [] -> f "(%a : %a)" desc t Type.pp t.typ
    | _ -> f "(%a <%a> : %a)" desc t attr t.attrs Type.pp t.typ

  and attr _ppf attrs = 
    List.iter (function
        | Attr.Comment s -> f "/* %s */" s) attrs
                         
  and desc _ppf t = match t.desc with
    | Const c -> f "%a" M.Constant.pp c
    | Nil -> f "[]"
    | Cons (t1, t2) -> f "%a :: %a" pp t1 pp t2
    | IML_None -> f "None"
    | IML_Some t -> f "Some %a" pp t
    | Left t -> f "Left %a" pp t
    | Right t -> f "Right %a" pp t
    | Unit -> p "()"
    | Var id -> f "%s" (Ident.unique_name id)
    | Pair (t1, t2) -> f "@[%a,@ %a@]" pp t1 pp t2
    | Assert t -> f "assert %a" pp t
    | AssertFalse -> p "assert false"
    | Fun (pat, body) ->
        f "@[<2>fun %a ->@ %a@]"
          PatVar.pp pat pp body
    | IfThenElse (t1, t2, Some t3) -> 
        f "@[if %a@ then %a@ else %a@]"
          pp t1 pp t2 pp t3
    | IfThenElse (t1, t2, None) -> 
        f "@[if %a@ then %a@]"
          pp t1 pp t2
    | App (t1, ts) -> 
        f "%a %a" pp t1 Format.(list " " (fun ppf t -> fprintf ppf "%a" pp t)) ts
    | Prim (n, _ops, ts) ->
        f "%s %a" 
          n
          Format.(list " " (fun ppf t -> fprintf ppf "%a" pp t)) ts
    | Let (p, t1, t2) ->
        f "@[@[<2>let %a@ = %a@]@ in@ %a@]"
          PatVar.pp p pp t1 pp t2
    | Switch_or (t, p1, t1, p2, t2) ->
        f "@[match %a with@ | @[<2>Left %a ->@ %a!]@ | @[<2>Right %a ->@ %a@]@]"
          pp t
          PatVar.pp p1 pp t1 
          PatVar.pp p2 pp t2
    | Switch_cons (t, p1, p2, t1, t2) ->
        f "@[match %a with@ | @[<2>%a::%a ->@ %a!]@ | @[<2>[] ->@ %a@]@]"
          pp t
          PatVar.pp p1
          PatVar.pp p2
          pp t1 
          pp t2
    | Switch_none (t, t1, p2, t2) ->
        f "@[match %a with@ | @[<2>None ->@ %a@]@ | @[Some %a@ -> %a@]@]"
          pp t
          pp t1 
          PatVar.pp p2 pp t2
    | Contract_create (Tz_code s, _, t1, t2, t3) ->
        f "@[<2>Contract.create_from_tz_code@ %S %a %a %a@]" 
          s
          pp t1 pp t2 pp t3
    | Contract_create (Tz_file s, _, t1, t2, t3) ->
        f "@[<2>Contract.create_from_tz_file@ %S %a %a %a@]" 
          s
          pp t1 pp t2 pp t3
    | Seq (t1, t2) -> f "%a; %a" pp t1 pp t2
    | Set ts -> f "Set [ @[%a@] ]" (Format.list ";@ " pp) ts
    | Map tts -> f "Map [ @[%a@] ]" 
                   (Format.list ";@ " (fun _ppf (x,y) ->
                        f "(%a, %a)" pp x pp y)) tts
                  
  in
  pp ppf

let save path t = 
  let oc = open_out path in
  let ppf = Format.of_out_channel oc in
  Format.fprintf ppf "%a@." pp t;
  close_out oc

let rec freevars t = 
  let open IdTys in
  let psingleton p = singleton (p.desc, p.typ) in
  match t.desc with
  | Const _ | Nil | IML_None | Unit | Contract_create _ -> empty
  | Cons (t1,t2) | Pair (t1,t2) | Seq (t1,t2) -> union (freevars t1) (freevars t2)
  | Left t | Right t | IML_Some t | Assert t -> freevars t
  | AssertFalse -> empty
  | Var id -> singleton (id, t.typ)
  | IfThenElse (t1,t2,Some t3) -> union (freevars t1) (union (freevars t2) (freevars t3))
  | IfThenElse (t1,t2,None) -> union (freevars t1) (freevars t2)
  | App (t,ts) ->
      List.fold_left (fun acc t -> union acc (freevars t)) empty (t::ts)
  | Prim (_,_,ts) ->
      List.fold_left (fun acc t -> union acc (freevars t)) empty ts
  | Fun (pat,t) -> 
      diff (freevars t) (psingleton pat)
  | Let (pat, t1, t2) ->
      diff (union (freevars t1) (freevars t2)) (psingleton pat)
  | Switch_or (t, p1, t1, p2, t2) ->
      union (freevars t)
        (union 
           (diff (freevars t1) (psingleton p1))
           (diff (freevars t2) (psingleton p2)))
  | Switch_cons (t, p1, p2, t1, t2) ->
      union (freevars t)
        (union 
           (diff (diff (freevars t1) (psingleton p1)) (psingleton p2))
           (freevars t2))
  | Switch_none (t, t1, p2, t2) ->
      union (freevars t)
        (union 
           (freevars t1)
           (diff (freevars t2) (psingleton p2)))
  | Set ts -> unions (List.map freevars ts)
  | Map tts -> unions (List.map (fun (t1,t2) -> union (freevars t1) (freevars t2)) tts)

(* t2[t_i/id_i] 
   XXX very inefficient.  should be removed somehow.
*)
let subst id_t_list t2 =
  let rec f t = 
    let mk desc = { t with desc } in
    match t.desc with
    | Var id ->
        begin match List.assoc_opt id id_t_list with
          | None -> t
          | Some t' -> Attr.adds t.attrs t'
        end
    | Const _ | Nil | IML_None | Unit | AssertFalse 
    | Contract_create _ -> t

    | IML_Some t -> mk & IML_Some (f t)
    | Left t -> mk & Left (f t)
    | Right t -> mk & Right (f t)
    | Assert t -> mk & Assert (f t)
    | Fun (pat, t) -> mk & Fun (pat, f t)
    | Let (p, t1, t2) -> mk & Let (p, f t1, f t2)
    | Cons (t1, t2) -> mk & Cons (f t1, f t2)
    | Pair (t1, t2) -> mk & Pair (f t1, f t2)
    | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (f t1, f t2, Some (f t3))
    | IfThenElse (t1, t2, None) -> mk & IfThenElse (f t1, f t2, None)
    | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (f t1, p1, f t2, p2, f t3)
    | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (f t1, p1, p2, f t2, f t3)
    | Switch_none (t1, t2, p, t3) -> mk & Switch_none (f t1, f t2, p, f t3)
    | App (t, ts) -> mk & App (f t, List.map f ts)
    | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
    | Seq (t1, t2) -> mk & Seq (f t1, f t2)
    | Set ts  -> mk & Set (List.map f ts)
    | Map tts -> mk & Map (List.map (fun (k,v) -> f k, f v) tts)
  in
  f t2

(* t2[id'_i/id_i] 
   Same as subst, but variables are renamed to variables.
   It keeps the original locations.
   XXX very inefficient.  should be removed somehow.
*)
let alpha_conv id_t_list t2 =
  let rec f t = 
    let mk desc = { t with desc } in
    match t.desc with
    | Var id ->
        begin match List.assoc_opt id id_t_list with
          | None -> t
          | Some id' -> { t with desc= Var id' }
        end
    | Const _ | Nil | IML_None | Unit | AssertFalse 
    | Contract_create _ -> t

    | IML_Some t -> mk & IML_Some (f t)
    | Left t -> mk & Left (f t)
    | Right t -> mk & Right (f t)
    | Assert t -> mk & Assert (f t)
    | Fun (pat, t) -> mk & Fun (pat, f t)
    | Let (p, t1, t2) -> mk & Let (p, f t1, f t2)
    | Cons (t1, t2) -> mk & Cons (f t1, f t2)
    | Pair (t1, t2) -> mk & Pair (f t1, f t2)
    | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (f t1, f t2, Some (f t3))
    | IfThenElse (t1, t2, None) -> mk & IfThenElse (f t1, f t2, None)
    | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (f t1, p1, f t2, p2, f t3)
    | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (f t1, p1, p2, f t2, f t3)
    | Switch_none (t1, t2, p, t3) -> mk & Switch_none (f t1, f t2, p, f t3)
    | App (t, ts) -> mk & App (f t, List.map f ts)
    | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
    | Seq (t1, t2) -> mk & Seq (f t1, f t2)
    | Set ts  -> mk & Set (List.map f ts)
    | Map tts -> mk & Map (List.map (fun (k,v) -> f k, f v) tts)
  in
  f t2
