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

open Ppxlib
open Ocaml_common

(* XXX Maybe not precise, but I do not know the proper way to get the source 
   code name *)
let get_source_of_str = function
  | [] -> None
  | s::_ -> Some s.pstr_loc.Location.loc_start.Lexing.pos_fname

(* modified version of Compile_common.with_info *)
let with_info str k =
  let open Ocaml_common in
  Compmisc.init_path ();
  let source_file = 
    match get_source_of_str str with
    | Some fn -> fn
    | None -> "noname"
  in
  let output_prefix = Compenv.output_prefix source_file in
  let module_name = Compenv.module_of_filename source_file output_prefix in
  Env.set_unit_name module_name;
  let env = Compmisc.initial_env () in
  let dump_file = String.concat "." [output_prefix; ".cmotmp"] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file @@ fun ppf_dump ->
  k { Compile_common.source_file
    ; module_name
    ; output_prefix
    ; env
    ; ppf_dump
    ; tool_name= "scaml.ppx"
    ; native= false
    }

(* returns [true] if [str] contains [scaml] attribute *)
let is_with_scaml str =
  let module M = struct
    class iter = object
      inherit Ast_traverse.iter
      method! attribute a =
        if a.attr_name.txt = "SCaml" then raise Exit
    end
  end
  in
  let i = new M.iter in
  try i#structure str; false with Exit -> true

let preprocess str info =
  Clflags.dont_write_files := true;
  (* We need OCaml's str, not one for Ppxlib *)
  let str' = Ppxlib_ast.Selected_ast.to_ocaml Structure str in
  let typed = Compile_common.typecheck_impl info str' in
  let typed = 
    let str, coe = typed in 
    let str = SCaml_compiler_lib.Translate.filter_by_SCaml_attribute str in
    str, coe
  in
  let module_ = 
    (* Exceptions must be raised as are, to be handled nicely by
       ocamlc and merlin *)
    SCaml_compiler_lib.SCamlc.compile_only
      info.source_file info.output_prefix info.module_name
      typed
  in
  (* We have to put the def NOT at the bottom but at the head,
     since it must precede the call of SCamlPPX.emit *)
  [ let open Ast_builder.Default in
    pstr_value ~loc:Location.none
      Nonrecursive
      [ value_binding ~loc:Location.none 
          ~pat:(punit ~loc:Location.none)
          ~expr:(eapply ~loc:Location.none
                   (evar ~loc:Location.none "SCaml_compiler_lib.SCamlPPX.register")
                   [estring ~loc:Location.none 
                      (Marshal.to_string module_ [])])
      ]
    ]
  @ str 

let log fmt =
  Format.kasprintf (fun s -> 
      let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 "/tmp/scaml.ppx.log" in
      output_string oc s;
      close_out oc) fmt

let impl str =
  let tool_name = Ast_mapper.tool_name () in
  match tool_name with
  | "ocamldep" -> str
  | _ when not @@ is_with_scaml str -> str
  | "merlin" | "ocamlc" | "ocamlopt" ->
      let n = Option.value ~default:"???" @@ get_source_of_str str in
      log "processing %s %s@." tool_name n;
      with_info str @@ preprocess str
  | _ -> 
      Format.eprintf "scaml.ppx: called from unknown tool %s.  Skip processing" tool_name;
      str

let () = Driver.register_transformation ~impl "scaml.ppx"
