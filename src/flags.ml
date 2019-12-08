open Spotlib.Spot

open Ocaml_conv.Default

type t = 
  { iml_optimization : bool
  ; iml_pattern_match : bool
  ; scaml_debug : bool
  ; scaml_convert : bool
  } [@@deriving conv{ocaml}]

let pp = Camlon.Ocaml.format_with ocaml_of_t
      
let eval flags (k, v) =
  let must_be_a_bool () = Error "attribute type error: must be a bool" in
  match String.concat "." & Longident.flatten k, v with
  | "iml_optimization", `Bool b -> Ok { flags with iml_optimization= b }
  | "iml_optimization", _ -> must_be_a_bool ()
  | "iml_pattern_match", `Bool b -> Ok { flags with iml_pattern_match= b }
  | "iml_pattern_match", _ -> must_be_a_bool ()
  | "scaml_debug", `Bool b -> Ok { flags with scaml_debug= b }
  | "scaml_debug", _ -> must_be_a_bool ()
  | "scaml_convert", `Bool b -> Ok { flags with scaml_convert= b }
  | "scaml_convert", _ -> must_be_a_bool ()
  | n, _ -> Error (Printf.sprintf "Unknown attribute %s" n)

let flags = ref 
    { iml_optimization  = true
    ; iml_pattern_match = true 
    ; scaml_debug       = begin try ignore (Sys.getenv "SCAML_DEBUG"); true with _ -> false end 
    ; scaml_convert     = false
    }

let update f = flags := f !flags
let if_debug f = if !flags.scaml_debug then f ()

