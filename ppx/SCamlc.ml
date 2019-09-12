open Spotlib.Spot

module M = Michelson

let implementation sourcefile outputprefix _modulename (str, _coercion) =
  (* Format.eprintf "sourcefile=%s outputprefix=%s modulename=%s@." sourcefile outputprefix modulename; *)
  let parameter, storage = IML.fix_entrypoint_type sourcefile str in
  let t = 
    try IML.structure ~parameter [] str with 
    | e -> 
        Printexc.print_backtrace stderr;
        Format.eprintf "IML.structure: %s@." (Printexc.to_string e);
        raise e
  in

  let oc = open_out (outputprefix ^ ".iml") in
  let ppf = Format.of_out_channel oc in
  Format.fprintf ppf "%a@." IML.pp t;
  close_out oc;

  let code = Compile.compile_structure t in
  let m = { M.Module.parameter; storage; code } in

  let oc = open_out (outputprefix ^ ".tz") in
  let ppf = Format.of_out_channel oc in
  Format.eprintf "@[<2>%a@]@." M.Module.pp m;
  Format.fprintf ppf "@[<2>%a@]@." M.Module.pp m;
  close_out oc
