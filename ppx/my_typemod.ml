[@@@warning "-27-32-41"]

include Typemod

let type_interface sourcefile _env _ast =
  Location.raise_errorf ~loc:(Location.in_file sourcefile)
    "SCaml does not support compilation of interfaces"

let type_implementation sourcefile outputprefix modulename initial_env ast =
  let res = type_implementation sourcefile outputprefix modulename initial_env ast in
  let () = 
    try
      Compile.implementation sourcefile outputprefix modulename res 
    with
    | (Location.Error e as exn) ->
        Format.eprintf "%a@." Location.report_error e;
        raise exn
  in
  res
