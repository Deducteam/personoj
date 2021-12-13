let split prefix =
  try
    while true do
      let line = input_line stdin in
      let json = Ezjsonm.from_string line in
      let name = Ezjsonm.(find json [ "name" ] |> get_string) in
      let fname = String.concat "_" [ prefix; name ] in
      let oc = open_out fname in
      at_exit (fun () -> close_out oc);
      output_string oc line;
      output_char oc '\n'
    done
  with End_of_file -> ()

open Cmdliner

let prefix =
  let doc = "Prepend file names with string $(docv)" in
  Arg.(value & opt string "" & info [ "p" ] ~doc ~docv:"STR")

let cmd =
  let exits = Term.default_exits in
  let doc = "Split a list of JSON objects across multiple files" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) reads a newline-seprated list of json objects on its \
         standard input and copies each object to a file $(i,pr)_$(i,n).json \
         where $(i,n) is the value associated to the (json) name $(b,name) and \
         $(i,pr) is a chosen prefix.";
      `S Manpage.s_examples;
      `P "Faced with input";
      `Pre
        {|{ "name": "foo", "bar": "lorem ipsum dolor" }
{ "name": "bar", "bar": "dolor sit amet" }
{ "name": "foo", "bar": "consectetur" }|};
      `P "$(tname) -p frobnify produces two files: $(b,frobnify_foo.json) with";
      `Pre
        {|{ "name": "foo", "bar": "lorem ipsum dolor" }
{ "name": "foo", "bar": "consectetur" }|};
      `P "and a file $(b,frobnify_bar.json) with";
      `Pre {|{ "name": "bar", "bar": "dolor sit amet" }|};
    ]
  in
  (Term.(const split $ prefix), Term.info "jsplit" ~doc ~exits ~man)
