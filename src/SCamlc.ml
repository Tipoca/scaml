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
    List.iter prerr_endline !Clflags.include_dirs
  end
  
let implementation sourcefile outputprefix _modulename (str, _coercion) =
  let parameter, storage, t = IML.implementation sourcefile str in

  IML.save (outputprefix ^ ".iml0") t;

  let t = if Flags.(!flags.iml_optimization) then IML.optimize t else t in

  IML.save (outputprefix ^ ".iml") t;

  let code = Compile.structure t in
  let m = { M.Module.parameter; storage; code } in

  let oc = open_out (outputprefix ^ ".tz") in
  let ppf = Format.of_out_channel oc in
  Flags.if_debug (fun () -> Format.eprintf "@[<2>%a@]@." M.Module.pp m);
  Format.fprintf ppf "@[<2>%a@]@." M.Module.pp m;
  close_out oc

let convert _sourcefile _outputprefix _modulename (str, _coercion) =
  IML.convert str

let compile sourcefile outputprefix modulename (typedtree, coercion) =
  let f = 
    if !Flags.flags.scaml_convert then convert
    else implementation
  in
  f sourcefile outputprefix modulename (typedtree, coercion)
