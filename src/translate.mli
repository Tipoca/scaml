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

val contract_self_id : Ident.t
  
val implementation 
  : string (* Source file name.  Used for error reporting *)
  -> Typedtree.structure 
  -> Michelson.Type.t (* Parameter *) * Michelson.Type.t (* Storage *) * IML.t

val convert
  : Typedtree.structure 
  -> [> `Type of Ident.t * Michelson.Type.t | `Value of Ident.t option * IML.t ] list

