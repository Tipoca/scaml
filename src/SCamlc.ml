open Spotlib.Spot

module M = Michelson

let implementation sourcefile outputprefix _modulename (str, _coercion) =
  (* Format.eprintf "sourcefile=%s outputprefix=%s modulename=%s@." sourcefile outputprefix modulename; *)
  let parameter, storage, t = IML.implementation sourcefile str in

  IML.save (outputprefix ^ ".iml0") t;

  let t = Pmatch.compile t in

  IML.save (outputprefix ^ ".iml1") t;

  let t = IML.optimize t in

  IML.save (outputprefix ^ ".iml") t;

  let code = Compile.compile_structure t in
  let m = { M.Module.parameter; storage; code } in

  let oc = open_out (outputprefix ^ ".tz") in
  let ppf = Format.of_out_channel oc in
  Format.eprintf "@[<2>%a@]@." M.Module.pp m;
  Format.fprintf ppf "@[<2>%a@]@." M.Module.pp m;
  close_out oc
