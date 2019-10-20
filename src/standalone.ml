(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The batch compiler *)

open Format
open Compenv

let tool_name = "scamlc"

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let interface _ppf _sourcefile _outputprefix = assert false

let implementation ppf sourcefile outputprefix =
  Profile.record_call sourcefile (fun () ->
    Compmisc.init_path false;
    let modulename = module_of_filename ppf sourcefile outputprefix in
    Env.set_unit_name modulename;
    let env = Compmisc.initial_env() in
    try
      let (typedtree, coercion) =
        Pparse.parse_implementation ~tool_name ppf sourcefile
        ++ print_if ppf Clflags.dump_parsetree Printast.implementation
        ++ print_if ppf Clflags.dump_source Pprintast.structure
        ++ Profile.(record typing)
            (Typemod.type_implementation sourcefile outputprefix modulename env)
        ++ print_if ppf Clflags.dump_typedtree
          Printtyped.implementation_with_coercion
     in
     SCamlc.implementation sourcefile outputprefix modulename (typedtree, coercion)
    with x ->
      Stypes.dump (Some (outputprefix ^ ".annot"));
      raise x
  )
