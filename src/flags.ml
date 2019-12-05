open Spotlib.Spot
open Typedtree
open Parsetree
open Tools

open Ocaml_conv.Default

type t = 
  { iml_optimization : bool
  ; iml_pattern_match : bool
  } [@@deriving conv{ocaml}]

let pp = Camlon.Ocaml.format_with ocaml_of_t
      
let eval flags ({Location.txt=k; loc}, v) =
  match String.concat "." & Longident.flatten k, v with
  | "iml_optimization", `Bool b -> { flags with iml_optimization= b }
  | "iml_optimization", _ -> errorf ~loc "attribute type error: must be a bool"
  | "iml_pattern_match", `Bool b -> { flags with iml_pattern_match= b }
  | "iml_pattern_match", _ -> errorf ~loc "attribute type error: must be a bool"
  | n, _ -> errorf ~loc "Unknown attribute %s" n

let flags = ref { iml_optimization= true; iml_pattern_match= true }
