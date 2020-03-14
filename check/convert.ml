open Spotlib.Spot
open Typerep_lib.Std
open SCaml_compiler_lib

module Convert = struct
  open Michelson.Constant

  let name = "Convert"
  let required = []

  type 'a t = 'a -> Michelson.Constant.t
      
  include Typerep_lib.Variant_and_record_intf.M(struct
      type nonrec 'a t = 'a t
    end)

  exception Unsupported of string
  let unsupported n = raise (Unsupported n)

  let int _ = unsupported "int"
  let int32 _ = unsupported "int32"
  let int64 _ = unsupported "int64"
  let nativeint _ = unsupported "nativeint"
  let char _ = unsupported "char"
  let float _ = unsupported "float"
  let string s = String s
  let bytes _ = unsupported "bytes"
  let bool b = Bool b
  let unit () = Unit
  let option f = function
    | None -> Option None
    | Some x -> Option (Some (f x))
  let list f xs = List (List.map f xs)
  let array _ _ = unsupported "array"
  let lazy_t _ _ = unsupported "lazy_t"
  let ref_ _ _ = unsupported "ref"
  let function_ _ _ _ = unsupported "function"
      
  let tuple xs =
    Binplace.fold (Binplace.place xs)
      ~leaf: (fun x -> x) 
      ~branch: (fun x y -> Pair (x,y))

  let tuple2 f1 f2 (x1, x2) = tuple [f1 x1; f2 x2]
  let tuple3 f1 f2 f3 (x1, x2, x3) = tuple [f1 x1; f2 x2; f3 x3]
  let tuple4 f1 f2 f3 f4 (x1, x2, x3, x4) = tuple [f1 x1; f2 x2; f3 x3; f4 x4]
  let tuple5 f1 f2 f3 f4 f5 (x1, x2, x3, x4, x5) = tuple [f1 x1; f2 x2; f3 x3; f4 x4; f5 x5]

  let record : 'a Record.t -> 'a t = fun r value ->
    let xs = List.rev & Record.fold r ~init:[] ~f:(fun acc (Field f) ->
        let c = Field.traverse f in
        let k = Field.label f in
        let v = Field.get f value in
        (k, c v) :: acc)
    in
    match xs with
    | [] -> assert false
    | [_k, m] -> m
    | _ ->
        Binplace.fold (Binplace.place xs)
          ~leaf: (fun (_k,x) -> x) 
          ~branch: (fun x y -> Pair (x,y))

  let variant : 'a Variant.t -> 'a t = fun v value ->
    let nulls, nonnulls = 
      Variant.fold v ~init:(0,0) ~f:(fun (nulls, nonnulls) (Tag tag) ->
          let arity = Tag.arity tag in
          if arity = 0 then (nulls + 1, nonnulls) else (nulls, nonnulls+1))
    in
    match nulls, nonnulls with
    | 0, 0 -> assert false
    | _, 0 ->
        let Value (tag, _) = Variant.value v value in
        Int (Z.of_int (Tag.ocaml_repr tag ))
    | 0, _ ->
        let Value (tag, args) = Variant.value v value in
        let args = Tag.traverse tag args in
        let i = Tag.ocaml_repr tag in
        let sides = Binplace.path i nonnulls in
        let rec f = function
          | [] -> args
          | Binplace.Left::xs -> Left (f xs)
          | Binplace.Right::xs -> Right (f xs)
        in
        f sides
    | _, _ ->
        let Value (tag, args) = Variant.value v value in
        let arity = Tag.arity tag in
        if arity = 0 then Left (Int (Z.of_int (Tag.ocaml_repr tag)))
        else
          let args = Tag.traverse tag args in
          let i = Tag.ocaml_repr tag in
          let sides = Binplace.path i nonnulls in
          let rec f = function
            | [] -> args
            | Binplace.Left::xs -> Left (f xs)
            | Binplace.Right::xs -> Right (f xs)
          in
          Right (f sides)

  module Named = Type_generic.Make_named_for_closure(struct
      type 'a input = 'a
      type 'a output = Michelson.Constant.t
      type 'a t = 'a input -> 'a output
    end)
end

open SCaml

let to_michelson typerep v = 
  let module M = Type_generic.Make(Convert) in
  M.register typerep_of_int (fun (Int n) -> Int (Z.of_int n));
  M.register typerep_of_nat (fun (Nat n) -> Int (Z.of_int n));
  M.register typerep_of_tz (fun (Tz f) -> Int (Z.of_float (f *. 1000000.)));
  let `generic f = M.of_typerep typerep in
  f v
