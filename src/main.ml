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

open Clflags
open Compenv

module Compile = struct
  
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
       SCamlc.compile sourcefile outputprefix modulename (typedtree, coercion)
      with x ->
        Stypes.dump (Some (outputprefix ^ ".annot"));
        raise x
    )
                         
end

let usage = "Usage: scamlc <options> <files>\nOptions are:"

(* Error messages to standard error formatter *)
let ppf = Format.err_formatter

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

let scaml_print_version_and_library compiler =
  Printf.printf "The SCaml %s, version %s for Tezos protocol version %s" 
    compiler Version.scaml Version.protocol; print_newline();
  Printf.printf "SCaml library directory: ";
  print_string begin match !SCamlc.scamlib with None -> "none" | Some d -> d end; print_newline ();
  Printf.printf "The OCaml %s, version " compiler;
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let scaml_print_version_string () =
  print_string Config.version; print_newline(); exit 0

module Options = Main_args.Make_bytecomp_options (struct
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
  let _absname = set Location.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = Compenv.defer (ProcessObjects (Misc.rev_split_words s))
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _compat_32 = set bytecode_compatible_32
  let _config = show_config
  let _custom = set custom_runtime
  let _no_check_prims = set no_check_prims
  let _dllib s = defer (ProcessDLLs (Misc.rev_split_words s))
  let _dllpath s = dllpaths := !dllpaths @ [s]
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I s = include_dirs := s :: !include_dirs
  let _impl = impl
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _keep_docs = set keep_docs
  let _no_keep_docs = unset keep_docs
  let _keep_locs = set keep_locs
  let _no_keep_locs = unset keep_locs
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _alias_deps = unset transparent_modules
  let _no_alias_deps = set transparent_modules
  let _app_funct = set applicative_functors
  let _no_app_funct = unset applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _opaque = set opaque
  let _open s = open_modules := s :: !open_modules
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _output_complete_obj () =
    output_c_object := true;
    output_complete_object := true;
    custom_runtime := true
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = first_ppx := s :: !first_ppx
  let _plugin p = Compplugin.load p
  let _principal = set principal
  let _no_principal = unset principal
  let _rectypes = set recursive_types
  let _no_rectypes = unset recursive_types
  let _runtime_variant s = runtime_variant := s
  let _safe_string = unset unsafe_string
  let _short_paths = unset real_paths
  let _strict_sequence = set strict_sequence
  let _no_strict_sequence = unset strict_sequence
  let _strict_formats = set strict_formats
  let _no_strict_formats = unset strict_formats
  let _thread = set use_threads
  let _vmthread = set use_vmthreads
  let _unboxed_types = set unboxed_types
  let _no_unboxed_types = unset unboxed_types
  let _unsafe = set fast
  let _unsafe_string = set unsafe_string
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v () = scaml_print_version_and_library "compiler"
  let _version = print_version_string
  let _vnum = print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _warn_help = Warnings.help_warnings
  let _color option =
    begin match parse_color_setting option with
          | None -> ()
          | Some setting -> color := Some setting
    end
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _dno_unique_ids = unset unique_ids
  let _dunique_ids = set unique_ids  
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let _dtimings () = profile_columns := [ `Time ]
  let _dprofile () = profile_columns := Profile.all_columns

  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0

  let anonymous = anonymous
end)

let main () =
  Clflags.add_arguments __LOC__ Options.list;
  Clflags.add_arguments __LOC__
    ["-depend", Arg.Unit Makedepend.main_from_option,
     "<options> Compute dependencies (use 'ocamlc -depend -help' for details)"
    
    (* SCaml *)
    ; "--scaml-debug", Arg.Unit (fun () -> Flags.(flags := { !flags with scaml_debug = true })),
      "Print SCaml debug messages"
    ; "--scaml-convert", Arg.Unit (fun () -> Flags.(flags := set_mode !flags ConvertAll)),
      "Convert types and values, instead of compling a smart contract"
    ; "--scaml-convert-value", Arg.String (fun s -> Flags.(flags := set_mode !flags
                                                                      (ConvertSingleValue s))),
      "<ident> Convert a single value, instead of compling a smart contract"
    ; "--scaml-convert-type", Arg.String (fun s -> Flags.(flags := set_mode !flags
                                                                      (ConvertSingleType s))),
      "<ident> Convert a single type, instead of compling a smart contract"
    ; "--scaml-revert", Arg.String (fun s -> Flags.(flags := set_mode !flags (Revert s))),
      "Revert values, instead of compling a smart contract"
    ; "--scaml-noscamlib", Arg.Unit (fun () -> Flags.(flags := { !flags with scaml_noscamlib = true })),
      "Do not add default directory for SCamlib to the list of include directories"
    ; "--scaml-version", Arg.Unit (fun () -> 
          print_string Version.scaml; print_newline(); exit 0),
      "Print SCaml version and exit"
    ; "--scaml-dump-iml0", Arg.Unit (fun () -> Flags.(flags := { !flags with dump_iml0 = true })),
      "Dump IML code before optimization to .iml0 file"
    ; "--scaml-dump-iml", Arg.Unit (fun () -> Flags.(flags := { !flags with dump_iml = true })),
      "Dump the final IML code to .iml file"
    ];
  try
    readenv ppf Before_args;
    SCamlc.init ();
    Clflags.parse_arguments anonymous usage;
    Compmisc.read_color_env ppf;
    begin try
      Compenv.process_deferred_actions
        (ppf,
         Compile.implementation,
         Compile.interface,
         ".cmo",
         ".cma");
      (* scaml_types_and_values () *)
    with Arg.Bad msg ->
      begin
        prerr_endline msg;
        Clflags.print_arguments usage;
        exit 2
      end
    end;
    readenv ppf Before_link;
    if
      List.length (List.filter (fun x -> !x)
                      [make_archive;make_package;compile_only;output_c_object])
        > 1
    then
      if !print_types then
        fatal "Option -i is incompatible with -pack, -a, -output-obj"
      else
        fatal "Please specify at most one of -pack, -a, -c, -output-obj";
    if !make_archive then assert false
    else if !make_package then assert false
  with x ->
    Location.report_exception ppf x;
    exit 2

let () =
  main ();
  Profile.print Format.std_formatter !Clflags.profile_columns;
  exit 0
