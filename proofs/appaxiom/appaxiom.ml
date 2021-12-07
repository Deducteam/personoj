open Cmdliner
open Common
open Parsing
module S = Syntax

let map_sym_typ (f : S.p_term -> S.p_term) : S.p_command -> S.p_command =
  let f cmd =
    match cmd with
    | S.P_symbol s -> S.P_symbol { s with p_sym_typ = Option.map f s.p_sym_typ }
    | _ -> cmd
  in
  Pos.map f

let appaxiom sym =
  let ast = Parser.parse stdin in
  (* Set geometry to have terminal independent output *)
  Format.set_geometry ~max_indent:8 ~margin:72;
  let app typ = S.P.appl (S.P.iden sym) typ in
  let process cmd =
    let open Format in
    printf "%a@\n" Pretty.command (map_sym_typ app cmd)
  in
  Stream.iter process ast

let sym =
  let doc = "Apply symbol $(docv) on top of declarations." in
  Arg.(value & opt string "Prf" & info [ "app"; "a" ] ~doc ~docv:"SYM")

let cmd =
  let doc = "Remove or add the top symbol of symbol declarations" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) is a filter that apply some symbol on top of the type of \
         symbol declarations.";
      `S Manpage.s_examples;
      `Pre "echo 'symbol true : imp P P;' | psnj-appaxiom --add 'Prf'";
      `P "outputs";
      `Pre "symbol true : @Prf (=> P P);";
      `S Manpage.s_bugs;
      `P
        "$(tname) operates a syntactic transformation. If a symbol has a \
         definition, it won't be changed, so the type of the definition will \
         surely not match the declared type. To change definitions based on \
         their type, coercions are needed.";
    ]
  in
  (Term.(const appaxiom $ sym), Term.info "psnj-appaxiom" ~doc ~exits ~man)

let () = Term.(exit @@ eval cmd)
