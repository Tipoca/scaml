open Spotlib.Spot

module M = Michelson

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

let encode_type sourcefile outputprefix _modulename (str, _coercion) =
  let parameter, storage, t = IML.implementation sourcefile str in

  IML.save (outputprefix ^ ".iml0") t;

  let t = IML.optimize t in

  IML.save (outputprefix ^ ".iml") t;

  let code = Compile.structure t in
  let m = { M.Module.parameter; storage; code } in

  let oc = open_out (outputprefix ^ ".tz") in
  let ppf = Format.of_out_channel oc in
  Flags.if_debug (fun () -> Format.eprintf "@[<2>%a@]@." M.Module.pp m);
  Format.fprintf ppf "@[<2>%a@]@." M.Module.pp m;
  close_out oc
