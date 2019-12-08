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

let convert _sourcefile _outputprefix _modulename (str, _coercion) =
  IML.convert str

let compile sourcefile outputprefix modulename (typedtree, coercion) =
  let f = 
    if !Flags.flags.scaml_convert then convert
    else implementation
  in
  f sourcefile outputprefix modulename (typedtree, coercion)
