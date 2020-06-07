let rev_modules = ref []

let register s = 
  let module_ : SCamlComp.Module.t = Marshal.from_string s 0 in
  Format.eprintf "Got %a@." SCamlComp.Module.pp module_;
  rev_modules := module_ :: !rev_modules

let emit ~outputprefix = 
  let modules = List.rev !rev_modules in
  Format.eprintf "Linking %s...@." (String.concat "," (List.map (fun m -> m.SCamlComp.Module.name) modules));
  try SCamlComp.link (Some outputprefix) modules with
  | Location.Error e ->
      Location.print_report Format.err_formatter e;
      exit 1

      