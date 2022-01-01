open Psnj
module D = Deps

let dopth fixed =
  if fixed then Format.set_geometry ~max_indent:16 ~margin:80;
  let id2path =
    (* Read ids and paths from stdin *)
    let id2path = ref D.IdMap.empty in
    (try
       while true do
         let id = input_line stdin in
         let pth = input_line stdin in
         id2path := D.(IdMap.add (Id.make id) (Path.of_string pth) !id2path)
       done
     with End_of_file -> ());
    !id2path
  in
  try
    let tbl = D.of_paths id2path in
    D.pp_makefile Format.std_formatter tbl
  with Invalid_argument err ->
    Format.eprintf "Invalid input (%s)@." err;
    exit 1

open Cmdliner

let fixed =
  let doc = "Fix terminal geometry to avoid depending on environment" in
  Arg.(value & flag & info [ "fixed" ] ~doc)

let cmd =
  let exits = Term.default_exits in
  let doc = "Build a Makefile dependency list from paths" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) is a filter that transform newline-separated list of strings \
         and paths into a Makefile dependency list. A path is a dot-separated \
         list of integers (such as 0.1.1) or the word $(b,root). A string \
         $(i,s1) depends on a string $(i,s2) if the path of $(i,s1) is a \
         successor of the path of $(i,s2). A path $(i,p) is a successor of \
         $(i,q) if $(i,p) = $(i,q).$(i,i) where $(i,i) is a natural number.";
      `S Manpage.s_examples;
      `P "With input";
      `Pre {|foo
root
bar
0
baz
0.1
frob
1|};
      `P "$(tname) outputs (the order is not specified";
      `Pre {|foo: bar frob
bar: baz|};
    ]
  in
  (Term.(const dopth $ fixed), Term.info "dopth" ~exits ~man ~doc)
