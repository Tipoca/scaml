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
open Tools

module M = Michelson

let init () =
  (* If --scaml-noscamlib is specified, None.

    If SCAMLIB is specified, SCAMLIB is chosen.

    Otherwise, `opam config var prefix`/lib/scaml is used.
    If `opam config var prefix` does not print a directory nor crashes,
    scamlc prints out a warning and continues with None
  *)
  let scamlib =
    if !Flags.flags.scaml_noscamlib then None
    else 
      match Sys.getenv "SCAMLIB" with
      | dir -> Some dir
      | exception Not_found ->
          (* exec opam config var prefix *)
          match 
            let open Command in
            exec ["opam"; "config"; "var"; "prefix"]
            |> stdout
            |> wait
            |> must_exit_with 0
          with
          | dir::_ -> Some (String.chop_eols dir ^/ "lib/scaml")
          | [] ->  
              Format.eprintf "Warning: Command 'opam config var prefix' answered nothing@."; None
          | exception (Failure s) -> 
              Format.eprintf "Warning: Command 'opam config var prefix' has failed: %s" s; None
          | exception e ->
              Format.eprintf "Warning: Command 'opam config var prefix' raised an exception: %s" (Printexc.to_string e); None
                
  in
  match scamlib with
  | None -> ()
  | Some dir -> Clflags.include_dirs := !Clflags.include_dirs @ [dir]

let implementation sourcefile outputprefix _modulename (str, _coercion) =
  let parameter, storage, t = Translate.implementation sourcefile str in

  if Flags.(!flags.dump_iml0) then IML.save (outputprefix ^ ".iml0") t;

  let t = if Flags.(!flags.iml_optimization) then Optimize.optimize t else t in

  if Flags.(!flags.dump_iml) then IML.save (outputprefix ^ ".iml") t;

(*
  Nonserialize.check t;
*)

  let code = Compile.structure t in
  let m = { M.Module.parameter; storage; code } in

  let oc = open_out (outputprefix ^ ".tz") in
  let ppf = Format.of_out_channel oc in
  Flags.if_debug (fun () -> Format.eprintf "@[<2>%a@]@." M.Module.pp m);
  Format.fprintf ppf "@[<2>%a@]@." M.Module.pp m;
  close_out oc

let convert _sourcefile _outputprefix _modulename (str, _coercion) =
  let ts = Translate.convert str in
  let ts = List.map (fun t -> match t with
      | `Type _ -> t
      | `Value (ido, t) when Flags.(!flags.iml_optimization) ->
          `Value (ido, Optimize.optimize t)
      | `Value _ -> t) ts 
  in
  List.iter (function
      | `Type (id, t) -> 
          Format.printf "type %s: @[<2>%a@]@." (Ident.name id) M.Type.pp t
      | `Value (n, t) ->
          begin match Compile.constant t with
            | None -> errorf_constant ~loc:t.loc "Constant expression expected"
            | Some c -> 
                match n with
                | None -> 
                    Format.printf "noname: @[<2>%a@]@." M.Constant.pp c
                | Some id ->
                    Format.printf "%s: @[<2>%a@]@." (Ident.name id) M.Constant.pp c
          end) ts
    
let revert m _sourcefile _outputprefix _modulename (str, _coercion) =
  match File.to_string m with
  | Error (`Exn e) -> raise e
  | Ok m ->
      match Revert.do_revert str m with
      | Error e -> failwith e
      | Ok parsetree -> 
          Format.eprintf "%a@." Pprintast.expression parsetree

let compile sourcefile outputprefix modulename (typedtree, coercion) =
  let f = match !Flags.flags.scaml_mode with
    | None | Some Compile -> implementation
    | Some Convert -> convert
    | Some (Revert s) -> revert s
  in
  f sourcefile outputprefix modulename (typedtree, coercion)
