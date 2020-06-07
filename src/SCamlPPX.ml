let rev_modules = ref []

let register s = 
  let module_ : SCamlc.Module.t = Marshal.from_string s 0 in
  Format.eprintf "Got %a@." SCamlc.Module.pp module_;
  rev_modules := module_ :: !rev_modules

let emit ~outputprefix = 
  let modules = List.rev !rev_modules in
  Format.eprintf "Linking %s...@." (String.concat "," (List.map (fun m -> m.SCamlc.Module.name) modules));
  try SCamlc.link (Some outputprefix) modules with
  | Location.Error e ->
      Location.print_report Format.err_formatter e;
      exit 1

      
