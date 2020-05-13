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

let scamlib = ref None

let init () =
  (* If --scaml-noscamlib is specified, None.

    If SCAMLIB is specified, SCAMLIB is chosen.

    Otherwise, `opam config var prefix`/lib/scaml is used.
    If `opam config var prefix` does not print a directory nor crashes,
    scamlc prints out a warning and continues with None
  *)
  scamlib := begin
    if !Flags.flags.scaml_noscamlib then None
    else 
      match Sys.getenv "SCAMLIB" with
      | dir -> Some dir
      | exception Not_found ->
          (* exec opam config var prefix *)
          begin match 
              let open Command in
              exec ["opam"; "config"; "var"; "prefix"]
              |> stdout
              |> wait
              |> must_exit_with 0
            with
            | dir::_ -> Some (String.chop_eols dir ^/ "lib/scaml/scamlib")
            | [] ->  
                Format.eprintf "Warning: Command 'opam config var prefix' answered nothing@."; None
            | exception (Failure s) -> 
                Format.eprintf "Warning: Command 'opam config var prefix' has failed: %s" s; None
            | exception e ->
                Format.eprintf "Warning: Command 'opam config var prefix' raised an exception: %s" (Printexc.to_string e); None
          end;
  end;
  match !scamlib with
  | None -> ()
  | Some dir -> Clflags.include_dirs := !Clflags.include_dirs @ [dir]

(* XXX This is awful hack *)
type module_ = 
  { name : string
  ; sourcefile : string
  ; outputprefix : string
  ; global_entry : (Michelson.Type.t * Michelson.Type.t * IML.t) option
  ; defs : (IML.PatVar.t * IML.t) list
  }
  
let rev_compiled = ref ([] : module_ list)

let compile_only sourcefile outputprefix modulename (str, _coercion) =
  let (gento, defs), secs = with_time & fun () -> 
      Translate.implementation true sourcefile outputprefix str 
  in
  Flags.if_time (fun () -> Format.eprintf "Translated in %f secs@." secs);
  
  (* To make iml0 and iml, we must connect these definitions *)
  let t = Translate.connect defs in
  if Flags.(!flags.dump_iml0) then IML.save (outputprefix ^ ".iml0") t;
  let t = 
    if Flags.(!flags.iml_optimization) then begin
      let res, secs = with_time & fun () -> Optimize.optimize t in
      Flags.if_time (fun () -> Format.eprintf "Optimized in %f secs@." secs);
      res
    end else t 
  in
  if Flags.(!flags.dump_iml) then IML.save (outputprefix ^ ".iml") t;

  rev_compiled := { name=modulename; sourcefile; outputprefix; global_entry= gento; defs } :: !rev_compiled

let link modules =
  match List.rev modules with
  | [] -> assert false
  | last::rest ->
      let global_entry = 
        List.iter (fun m -> 
            if m.global_entry <> None then
              errorf_link ~loc:(Location.in_file m.sourcefile) 
                "Only the last linked module can have entry points") rest;
        match last.global_entry with
        | Some global_entry -> global_entry
        | None ->
            errorf_link ~loc:(Location.in_file last.sourcefile) 
              "The last module must define at least one entry point"
      in
      let defs : (IML.PatVar.t * IML.t) list = 
        List.concat_map (fun m ->
            List.concat_map (fun (pv, t) -> 
                [ (pv, t); 
                  ( { pv with IML.desc= Ident.create_persistent (m.name ^ "." ^ Ident.name pv.IML.desc) }, 
                    { t with IML.desc= IML.Var pv.desc } ) ]
              ) m.defs) modules
      in
      let (parameter, storage, t), secs = with_time & fun () -> Translate.link global_entry defs in
      Flags.if_time (fun () -> Format.eprintf "Linked in %f secs@." secs);
      (*
         Storage 

           type t = A 

         produces

           storage (int %A :t) ;

         which is illegal
       *)
      let parameter = Compile.clean_field_annot parameter in
      let storage = Compile.clean_field_annot storage in

      if Flags.(!flags.dump_iml0) then IML.save (last.outputprefix ^ "__link.iml0") t;

      let t = 
        if Flags.(!flags.iml_optimization) then begin
          let res, secs = with_time & fun () -> Optimize.optimize t in
          Flags.if_time (fun () -> Format.eprintf "Optimized in %f secs@." secs);
          res
        end else t in

      if Flags.(!flags.dump_iml) then IML.save (last.outputprefix ^ "__link.iml") t;

      let module Compile = Compile.Make(struct let allow_big_map = false end) in
      let code, secs = with_time & fun () -> Compile.structure t in
      Flags.if_time (fun () -> Format.eprintf "Compiled in %f secs@." secs);
      let m = { M.Module.parameter; storage; code } in

      let oc = open_out (last.outputprefix ^ ".tz") in
      let ppf = Format.of_out_channel oc in
      Flags.if_debug (fun () -> Format.eprintf "@[<2>%a@]@." M.Module.pp m);
      Format.fprintf ppf "@[<2>%a@]@." M.Module.pp m;
      close_out oc

let convert_all _sourcefile _outputprefix _modulename (str, _coercion) =
  let module Compile = Compile.Make(struct let allow_big_map = true end) in
  let ts = Translate.convert str in
  let ts = List.map (fun t -> match t with
      | `Type _ -> t
      | `Value (ido, t) when Flags.(!flags.iml_optimization) ->
          `Value (ido, Optimize.optimize t)
      | `Value _ -> t) ts 
  in
  List.iter (function
      | `Type (id, t) -> 
          Format.printf "type %s: @[%a@]@." (Ident.name id) M.Type.pp t
      | `Value (n, t) ->
          begin match Compile.constant t with
            | None -> errorf_constant ~loc:t.loc "Constant expression expected"
            | Some c -> 
                match n with
                | None -> 
                    Format.printf "noname: @[%a@]@." M.Constant.pp c
                | Some id ->
                    Format.printf "%s: @[%a@]@." (Ident.name id) M.Constant.pp c
          end) ts

let convert_value ident _sourcefile _outputprefix _modulename (str, _coercion) =
  let module Compile = Compile.Make(struct let allow_big_map = true end) in
  let ts = Translate.convert str in
  let t = try List.find
                (fun t -> match t with
                   | `Value (Some id, _) -> Ident.name id = ident
                   | _ -> false
                ) ts
          with Not_found -> errorf_convert_ident ~loc:Location.none
                              "no such value: %s" ident
  in 
  match t with
  | `Value (_, t) ->
     let t = if Flags.(!flags.iml_optimization) then Optimize.optimize t else t in
     begin match Compile.constant t with
     | None -> errorf_constant ~loc:t.loc "Constant expression expected"
     | Some c -> Format.printf "@[%a@]@." M.Constant.pp c
     end
  | _ -> assert false

let convert_type ident _sourcefile _outputprefix _modulename (str, _coercion) =
  let ts = Translate.convert str in
  let t = try List.find
                (fun t -> match t with
                          | `Type (id, _) -> Ident.name id = ident
                          | _ -> false
                ) ts
          with Not_found -> errorf_convert_ident ~loc:Location.none
                              "no such type: %s" ident
  in
  match t with
  | `Type (_, t) -> Format.printf "@[%a@]@." M.Type.pp t
  | _ -> assert false

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
    | None | Some Compile -> compile_only
    | Some ConvertAll -> convert_all
    | Some (ConvertSingleValue ident) -> convert_value ident
    | Some (ConvertSingleType ident) -> convert_type ident
    | Some (Revert s) -> revert s
  in
  f sourcefile outputprefix modulename (typedtree, coercion)
