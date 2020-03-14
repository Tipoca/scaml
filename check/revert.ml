open Spotlib.Spot
open Typerep_lib.Std
open SCaml_compiler_lib

module Revert = struct
  open Michelson.Constant

  let name = "Revert"
  let required = []

  type 'a t = Michelson.Constant.t -> 'a option
      
  include Typerep_lib.Variant_and_record_intf.M(struct
      type nonrec 'a t = 'a t
    end)

  exception Unsupported of string
  let unsupported n = raise (Unsupported n)

  open Option.Infix

  let int _ = unsupported "int"
  let int32 _ = unsupported "int32"
  let int64 _ = unsupported "int64"
  let nativeint _ = unsupported "nativeint"
  let char _ = unsupported "char"
  let float _ = unsupported "float"
  let string = function
    | String s -> Some s
    | _ -> None
  let bytes _ = unsupported "bytes"
  let bool = function
    | Bool b -> Some b
    | _ -> None
  let unit = function
    | Unit -> Some ()
    | _ -> None
  let option f x = match x with
    | Option None -> Some None
    | Option (Some x) -> f x >>| fun x -> Some x
    | _ -> None
  let list f = function
    | List xs -> Option.mapM f xs
    | _ -> None
  let array _ _ = unsupported "array"
  let lazy_t _ _ = unsupported "lazy_t"
  let ref_ _ _ = unsupported "ref"
  let function_ _ _ _ = unsupported "function"

  let rec access v sides = match v, sides with
    | _, [] -> Some v
    | Pair (v, _), Binplace.Left::sides -> access v sides
    | Pair (_, v), Right::sides -> access v sides
    | _ -> None

  let tuple n v = 
    Option.mapM (fun sides -> access v sides)
    & List.init n & fun i -> Binplace.path i n

  let tuple2 f1 f2 v = tuple 2 v >>= function
    | [x1; x2] -> f1 x1 >>= fun x1 -> f2 x2 >>= fun x2 -> Some (x1, x2)
    | _ -> None
  let tuple3 f1 f2 f3 v = tuple 3 v >>= function
    | [x1; x2; x3] -> f1 x1 >>= fun x1 -> f2 x2 >>= fun x2 -> f3 x3 >>= fun x3 -> Some (x1, x2, x3)
    | _ -> None
  let tuple4 f1 f2 f3 f4 v = tuple 4 v >>= function
    | [x1; x2; x3; x4] -> f1 x1 >>= fun x1 -> f2 x2 >>= fun x2 -> f3 x3 >>= fun x3 -> f4 x4 >>= fun x4 -> Some (x1, x2, x3, x4)
    | _ -> None
  let tuple5 f1 f2 f3 f4 f5 v = tuple 5 v >>= function
    | [x1; x2; x3; x4; x5] -> f1 x1 >>= fun x1 -> f2 x2 >>= fun x2 -> f3 x3 >>= fun x3 -> f4 x4 >>= fun x4 -> f5 x5 >>= fun x5 -> Some (x1, x2, x3, x4, x5)
    | _ -> None

  let record : 'a Record.t -> 'a t = fun r m ->
    let len = Record.length r in
    tuple len m >>= fun ms ->
    let get f = 
      let i = Field.index f in
      match Field.traverse f (List.nth ms i) with
      | Some x -> x
      | None -> raise Exit
    in
    let fields = { Record.get } in
    match Record.create r fields with
    | exception Exit -> None
    | v -> Some v

  let variant : 'a Variant.t -> 'a t = fun v m ->
    let _, rev_nulls, rev_nonnulls = 
      Variant.fold v ~init:(0,[],[]) ~f:(fun (i, nulls, nonnulls) (Tag tag) ->
          let arity = Tag.arity tag in
          if arity = 0 then (i+1, i::nulls, nonnulls) else (i+1, nulls, i::nonnulls))
    in
    let nulls = List.rev rev_nulls in
    let nonnulls = List.rev rev_nonnulls in
    let n_nulls = List.length nulls in
    let n_nonnulls = List.length nonnulls in
    match n_nulls, n_nonnulls with
    | 0, 0 -> assert false
    | _, 0 ->
        begin match m with
          | Int z ->
            if z >= Z.of_int n_nulls || z < Z.zero then None
            else
              let i = Z.to_int z in
              let pos = List.nth nulls i in
              assert (i = pos);
              let Tag tag = Variant.tag v pos in
              begin match Tag.create tag with
              | Const v -> Some v
              | _ -> assert false
              end
          | _ -> None
        end
    | 0, _ ->
        let tree = Binplace.place (List.init n_nonnulls (fun x -> x)) in
        let rec find m tree = match m, tree with
          | m, Binplace.Leaf i -> Some (m, i)
          | Left m, Branch (tree, _) -> find m tree
          | Right m, Branch (_, tree) -> find m tree
          | _ -> None
        in
        find m tree >>= fun (m, i) ->
        let Tag tag = Variant.tag v i in
        begin match Tag.create tag with
          | Const _ -> assert false
          | Args f -> Tag.traverse tag m >>| f
        end
    | _, _ ->
        begin match m with
          | Left (Int z) ->
            if z >= Z.of_int n_nulls || z < Z.zero then None
            else
              let i = Z.to_int z in
              let pos = List.nth nulls i in
              let Tag tag = Variant.tag v pos in
              begin match Tag.create tag with
              | Const v -> Some v
              | _ -> assert false
              end
          | Right m ->
              let tree = Binplace.place (List.init n_nonnulls (fun x -> x)) in
              let rec find m tree = match m, tree with
                | m, Binplace.Leaf i -> Some (m, i)
                | Left m, Branch (tree, _) -> find m tree
                | Right m, Branch (_, tree) -> find m tree
                | _ -> None
              in
              find m tree >>= fun (m, i) ->
              let pos = List.nth nonnulls i in
              let Tag tag = Variant.tag v pos in
              begin match Tag.create tag with
                | Const _ -> assert false
                | Args f -> Tag.traverse tag m >>| f
              end
          | _ -> None
        end

  module Named = Type_generic.Make_named_for_closure(struct
      type 'a input = Michelson.Constant.t
      type 'a output = 'a option
      type 'a t = 'a input -> 'a output
    end)
end

open SCaml

exception Overflow
exception Rounded

let of_michelson typerep v = 
  let module M = Type_generic.Make(Revert) in
  M.register typerep_of_int (function
      | Int z -> 
          let i = Z.to_int z in
          if Z.of_int i <> z then raise Overflow
          else Some (Int i)
      | _ -> None);
  M.register typerep_of_nat (function
      | Int z -> 
          let i = Z.to_int z in
          if Z.of_int i <> z then raise Overflow
          else if i < 0 then None
          else Some (Nat i)
      | _ -> None);
  M.register typerep_of_tz (function
      | Int z -> 
          let i = Z.to_int z in
          if Z.of_int i <> z then raise Overflow
          else 
            let f = float i /. 1000000. in
            if int_of_float (f *. 1000000.) <> i then raise Rounded
            else Some (Tz f)
      | _ -> None);
  let `generic f = M.of_typerep typerep in
  f v
