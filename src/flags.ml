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

open Spotlib.Spot

open Ocaml_conv.Default

type t = 
  { iml_optimization : bool
  ; iml_pattern_match : bool
  ; scaml_debug : bool
  ; scaml_convert : bool
  ; scaml_noscamlib : bool
  ; dump_iml0 : bool
  ; dump_iml : bool
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
  | "scaml_noscamlib", `Bool b -> Ok { flags with scaml_noscamlib= b }
  | "scaml_noscamlib", _ -> must_be_a_bool ()
  | "dump_iml0", `Bool b -> Ok { flags with dump_iml0= b }
  | "dump_iml0", _ -> must_be_a_bool ()
  | "dump_iml", `Bool b -> Ok { flags with dump_iml= b }
  | "dump_iml", _ -> must_be_a_bool ()
  | n, _ -> Error (Printf.sprintf "Unknown attribute %s" n)

let flags = ref 
    { iml_optimization  = true
    ; iml_pattern_match = true 
    ; scaml_debug       = begin try ignore (Sys.getenv "SCAML_DEBUG"); true with _ -> false end 
    ; scaml_convert     = false
    ; scaml_noscamlib   = false
    ; dump_iml0         = false
    ; dump_iml          = false
    }

let update f = flags := f !flags
let if_debug f = if !flags.scaml_debug then f ()

