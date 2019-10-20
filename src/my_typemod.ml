[@@@warning "-27-32-41"]

include Typemod

let type_interface sourcefile _env _ast =
  Location.raise_errorf ~loc:(Location.in_file sourcefile)
    "SCaml does not support compilation of interfaces"

let type_implementation sourcefile outputprefix modulename initial_env ast =
  try
    let res = type_implementation sourcefile outputprefix modulename initial_env ast in
    SCamlc.implementation sourcefile outputprefix modulename res;
    res
  with
  | e -> 
    Location.report_exception Format.err_formatter e;
    (* raise e *)
    exit 1
