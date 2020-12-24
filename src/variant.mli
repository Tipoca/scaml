(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                     Copyright 2020  DaiLambda, Inc.                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** For the given type expression of a variant type,
    returns the names and instantiated types of the constructors *)
val variant_type
  : Env.t
  -> Types.type_expr
  -> Types.constructor_description list
  -> string list option (* nullaries *)
     * (string * Types.type_expr list) list (* non nullaries *)
