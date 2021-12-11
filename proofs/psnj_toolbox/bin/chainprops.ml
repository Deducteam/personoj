open Psnj_toolbox
open Parsing
module C = Chainprops

let chainprops deps imply pp_deps =
  let ic = open_in deps in
  let deps = Deps.of_makefile ic in
  close_in ic;
  if pp_deps then (
    Format.(
      eprintf "=== Dependencies ===@\n";
      Deps.pp_makefile Format.err_formatter deps;
      eprintf "@\n====================@."));
  let imply = Syntax.P.iden imply in
  let props = Chainprops.propositions (Parser.parse stdin) in
  let inferences = Chainprops.merge imply deps props in
  let open Format in
  pp_print_list ~pp_sep:pp_print_newline C.pp std_formatter inferences;
  pp_print_newline std_formatter ()

open Cmdliner

let deps =
  let doc =
    "Dependencies of propositions inside Dedukti file. A line $(b,tgt: hyp0 \
     hyp1) specifies that proposition $(b,tgt) is deduced from $(b,hyp0) and \
     $(b,hyp1)."
  in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:"DEPS")

let imply =
  let doc = "Use symbol $(docv) for implication" in
  Arg.(value & opt string "imp" & info [ "imp" ] ~doc ~docv:"IMP")

let pp_deps =
  let doc = "Print parsed dependencies to stderr (for debugging purposes)" in
  Arg.(value & flag & info [ "pp-deps" ] ~doc)

let cmd =
  let doc = "Build a proof tree from propositions and dependencies" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "psnj-chainprops tranform a list of propositions given on its standard \
         input into a list of inferences represented as implications. The \
         hypothesese and conclusions of inferences are specified in a \
         dependency file which follows a Makefile syntax.";
      `S Manpage.s_examples;
      `P "Given a file foo.dep";
      `Pre "tgt: hyp0 hyp1";
      `P "and the input";
      `Pre "symbol tgt: P;\nsymbol hyp0: H0;\nsymbol hyp1: H1;";
      `P "$(b,psnj-chainprops foo.dep) outputs";
      `Pre
        "symbol hyp0: H0;\nsymbol hyp1: H1;\nsymbol tgt: @imp H0 (@imp H1 P);";
    ]
  in
  ( Term.(const chainprops $ deps $ imply $ pp_deps),
    Term.info "chainprops" ~doc ~exits ~man )
