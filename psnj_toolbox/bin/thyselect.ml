let thyselect negative_only positive_only =
  match Ezjsonm.value_from_channel_result stdin with
  | Error (`Not_a_t v) ->
      Format.eprintf "@[Value is not a valid JSON document:@]@.";
      Ezjsonm.to_channel stderr v;
      Format.eprintf "@.";
      exit 1
  | Error _ ->
      Format.eprintf "Ill-formed value@.";
      exit 1
  | Ok top ->
      let thys =
        match Ezjsonm.find top [ "theories" ] with
        | `A vs -> vs
        | _ ->
            Format.eprintf
              "Ill-formed json: the value of key \"theories\" must be an \
               array@.";
            exit 1
      in
      List.iter
        (fun t ->
          let tname = Ezjsonm.(find t [ "name" ] |> get_string) in
          let disabled =
            try Ezjsonm.(find t [ "disabled" ] |> get_bool)
            with Not_found -> false
          in
          if
            ((not negative_only) && not positive_only)
            || (negative_only && disabled)
            || (positive_only && not disabled)
          then Format.printf "%s@\n" tname)
        thys

open Cmdliner

let negative =
  let doc = "Print only disabled theories" in
  Arg.(value & flag & info [ "negative"; "n" ] ~doc)

let positive =
  let doc = "Print only enabled theories" in
  Arg.(value & flag & info [ "positive"; "p" ] ~doc)

let cmd =
  let doc = "Print theorie of theory definitions files" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) is a filter that take as input a JSON file that describe the \
         content of a PVS file and outputs theory names. The input must be of \
         the form";
      `Pre "{ \"source\": SRC, \"theories\": [THY, ...] }";
      `P
        "Where $(b,SRC) is the name of the file containing the thories, and \
         the theories $(b,THY) are objects of the form";
      `Pre "{ \"name\": NAME, \"disabled\": DIS }";
      `P
        "where $(b,NAME) is the name of a theory (as a string) and $(b,DIS) is \
         true if the theory shouldn't be translated. The key/value pair \
         $(b,disabled) is optional, theories are enabled by default.";
    ]
  in
  ( Term.(const thyselect $ negative $ positive),
    Term.info "thyselect" ~doc ~exits ~man )
