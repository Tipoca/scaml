(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Spotlib.Spot

type t = 
  { iml_optimization  : bool
  ; iml_pattern_match : bool
  ; scaml_debug       : bool
  ; scaml_convert     : bool (** type and value conversion mode *)
  ; scaml_noscamlib   : bool (** do not add -I `opam config var prefix`/scaml *)
  ; dump_iml0         : bool
  ; dump_iml          : bool
  }

val flags : t ref

val pp : Format.t -> t -> unit
val eval : t -> Longident.t * [`Bool of bool | `Constant of Parsetree.constant ] -> (t, string) Result.t
val update : (t -> t) -> unit
val if_debug : (unit -> unit) -> unit
