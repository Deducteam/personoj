open Cmdliner
open Common
open Parsing
module S = Syntax

let appaxiom fixed =
  if fixed then Format.set_geometry ~max_indent:16 ~margin:80;
  let ast = Parser.parse stdin in
  let process Pos.{ elt = cmd; pos } =
    match cmd with
    | S.P_symbol s ->
        let script =
          Some ([ Pos.none @@ S.P_tac_why3 None ], Pos.none @@ S.P_proof_end)
        in
        let cmd = S.P_symbol { s with p_sym_prf = script; p_sym_def = true } in
        Format.printf "%a@\n" Pretty.command (Pos.make pos cmd)
    | _ ->
        Format.eprintf "Ill-formed input: only axioms allowed@.";
        exit 1
  in
  Stream.iter process ast

let fixed =
  let doc =
    "Fix the geometry of the output to have terminal intependant output"
  in
  Arg.(value & flag & info [ "fixed" ] ~doc)

let cmd =
  let doc = "Transform axioms into automatically proved theorems" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) is a filter that adds a proof script to each axioms passed \
         on the standard input.";
      `S Manpage.s_examples;
      `P "Faced with input";
      `Pre "symbol foo: TYPE;";
      `P "$(tname) returns";
      `Pre "symbol foo: TYPE :=\nbegin\n  why3;\nend;";
    ]
  in
  (Term.(const appaxiom $ fixed), Term.info "psnj-autosolve" ~doc ~exits ~man)

let () = Term.(exit @@ eval cmd)
