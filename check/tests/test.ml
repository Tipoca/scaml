open Typerep_lib.Std (* must be before open SCaml *)
open SCaml
open SCaml_compiler_lib
open SCamtest.Convert
open SCamtest.Revert

let f tr v =
  let m = to_michelson tr v in
  Format.eprintf "%a@." Michelson.Constant.pp m;
  match of_michelson tr m with
  | Some v' -> assert (v = v')
  | None -> assert false

type t = nat list * tz option [@@deriving typerep]
let () = f typerep_of_t ([Nat 1; Nat 2; Nat 3], Some (Tz 1.0))

type t2 = (string, nat) sum list [@@deriving typerep]
let () = f typerep_of_t2 [Left "hello"; Right (Nat 2)]

type t3 = { x : int; y : nat; z : tz }  [@@deriving typerep]
let () = f typerep_of_t3 { x= Int 1; y = Nat 2; z= Tz 1.0 }

type t4 = Foo | Bar of int | Bee of nat | Boo of string [@@deriving typerep]
type t5 = t4 list [@@deriving typerep]
let () = f typerep_of_t5 [Foo; Bar (Int 1); Bee (Nat 1); Boo "hello"]

