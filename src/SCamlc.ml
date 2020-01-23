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
open Tools

module M = Michelson

let init () =
  if not !Flags.flags.scaml_noscamlib then begin
    (* exec opam config var prefix *)
    let dir = match 
        let open Command in
        exec ["opam"; "config"; "var"; "prefix"]
        |> stdout
        |> wait
        |> must_exit_with 0
      with
      | dir::_ -> String.chop_eols dir ^/ "lib/scaml"
      | [] ->  
          internal_error ~loc:Location.none "Command 'opam config var prefix' answered nothing"
      | exception (Failure s) -> 
          internal_error ~loc:Location.none "Command 'opam config var prefix' has failed: %s" s
      | exception e ->
          internal_error ~loc:Location.none "Command 'opam config var prefix' raised an exception: %s" (Printexc.to_string e)
    in
    Clflags.include_dirs := !Clflags.include_dirs @ [dir];
    (* List.iter (fun s -> prerr_endline @@ "Include: " ^ s) !Clflags.include_dirs *)
  end
  
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
            | None -> errorf ~loc:t.loc "Constant expression expected"
            | Some c -> 
                match n with
                | None -> 
                    Format.printf "noname: @[<2>%a@]@." M.Constant.pp c
                | Some id ->
                    Format.printf "%s: @[<2>%a@]@." (Ident.name id) M.Constant.pp c
          end) ts
    
let compile sourcefile outputprefix modulename (typedtree, coercion) =
  let f = 
    if !Flags.flags.scaml_convert then convert
    else implementation
  in
  f sourcefile outputprefix modulename (typedtree, coercion)
