open Core
open Common
open Parsing
open Lplib
open Extra
module P = Process

let translate_file (lib_root : string option) (mapfile : string) (src : string)
    : unit =
  (* Get mappings *)
  let mapping = Lpvs.Mappings.of_file mapfile in
  let pcertmap, depconnectives, connectives =
    let f s =
      try StrMap.find s mapping
      with Not_found ->
        failwith
          (Format.sprintf "Section \"%s\" not found in \"%s\"" "pcert" src)
    in
    (f "pcert", f "depconnectives", f "connectives")
  in
  (* Setup lp *)
  (* Silence lambdapi to have environment-independent output. *)
  Timed.(Console.verbose := 0);
  Library.set_lib_root lib_root;
  Console.State.push ();
  Package.apply_config src;
  let mp = Library.path_of_file LpLexer.escape src in
  let sign = Sig_state.create_sign mp in
  let ss = Sig_state.of_sign sign in
  let pcert_ss =
    let ast =
      Parser.parse_string "lpvs"
        "require open lpvs.encoding.lhol lpvs.encoding.pcert \
         lpvs.encoding.depconnectives;"
    in
    P.compile_ast ss ast
  in
  let module Pcert = (val Lpvs.Encodings.mkpcert pcertmap pcert_ss) in
  Console.out 1 "Loaded PVS-Cert encoding";
  let module DepConn =
  (val let dep_conn_ss =
         let ast =
           (* WARNING: [open] is used because the [require open] of the
              previous command has some side effects which records that it
              has been required. *)
           Parser.parse_string "lpvs"
             "open lpvs.encoding.lhol; open lpvs.encoding.depconnectives;"
         in
         P.compile_ast ss ast
       in
       Lpvs.Encodings.mkconnectors depconnectives dep_conn_ss)
  in
  let prop_calc_ss =
    let ast =
      Parser.parse_string "lpvs"
        "open lpvs.encoding.lhol;require open lpvs.encoding.connectives;"
    in
    P.compile_ast ss ast
  in
  let module Propc =
  (val Lpvs.Encodings.mkconnectors connectives prop_calc_ss)
  in
  Console.out 1 "Loaded classical propositional calculus";
  let module Tran = Lpvs.LpCert.PropOfPcert (Pcert) (DepConn) (Propc) in
  let ast = Parser.parse_file src in
  let _ss = P.compile_ast pcert_ss ast in
  let syms = P.get_symbols sign in
  let tr_pp name (ty, _) =
    try
      let propty = Tran.f ty in
      Format.printf "@[symbol %s:@ %a;@]@." name Print.pp_term propty
    with Tran.CannotTranslate t ->
      Format.eprintf "Cannot translate %a@." Print.pp_term t
  in
  StrMap.iter tr_pp syms

open Cmdliner

let lib_root =
  let doc = "Library root" in
  Arg.(
    value & opt (some string) None & info [ "lib-root" ] ~doc ~docv:"LIBROOT")

let mapfile =
  let doc = "Maps Dedukti symbols into the runtime process." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"JSON")

let src =
  let doc = "Dedukti file containing axioms to simplify" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"FILE")

let cmd =
  let doc = "Convert PVS-Cert encoded file quasi first order encoded files" in
  let man =
    [
      `S Manpage.s_description
    ; `P
        "$(tname) is a filter that transforms Dedukti files containing alist \
         of axioms expressed in PVS-Cert into a list of axioms expressed in \
         something close to first order logic."
    ; `P
        "To convert files, the program needs identify the symbols of the \
         encoding. The mapping allows to indicate the name of such symbols. \
         This mapping is a JSON object with the following structure"
    ; `Pre
        "{ \"pcert\": ...;\n\
        \  \"depconnectives\": ...;\n\
        \  \"connectives\": ... }"
    ; `P
        "where the value associated to \"pcert\" is an object that define the \
         following form"
    ; `Pre
        "{ \"Set\": STRING, \"Element\": STRING,\n\
        \  \"Propositions\": STRING, \"Proof\": STRING,\n\
        \  \"forall\": STRING, \"arrow\": STRING, \"implication\": STRING,\n\
        \  \"o\": STRING,\n\
        \  \"subset\": STRING, \"pair\": STRING, \"value\": STRING, \"proof\": \
         STRING }"
    ; `P
        "and the values associated to both \"depconnectives\" and \
         \"connectives\" must be of the form"
    ; `Pre
        "{ \"truth\": STRING; \"falsity\": STRING;\n\
        \  \"implication\": STRING;\n\
        \  \"negation\": STRING; \n\
        \  \"conjunction\": STRING; \"disjunction\": STRING;\n\
        \  \"existential\": STRING; \"universal\": STRING }"
    ; `S Manpage.s_examples
    ; `P "Let qfo.json be the following json file"
    ; `Pre
        {|{
  "pcert": {
    "Set": "Set",
    "Element": "El",
    "Propositions": "Prop",
    "Proof": "Prf",
    "forall": "∀",
    "arrow": "arrd",
    "implication": "⇒",
    "o": "prop",
    "subset": "psub",
    "pair": "pair",
    "value": "fst",
    "proof": "snd"
  },
  "depconnectives": {
    "truth": "true",
    "falsity": "false",
    "negation": "¬",
    "implication": "⇒",
    "conjunction": "∧",
    "disjunction": "∨",
    "existential": "∃",
    "universal": "∀"
  },
  "connectives": {
    "truth": "top",
    "falsity": "bot",
    "negation": "not",
    "implication": "imp",
    "conjunction": "conj",
    "disjunction": "disj",
    "existential": "ex",
    "universal": "all"
  }
}|}
    ; `P "and with the following input,"
    ; `Pre "symbol true : Prf (∀ {prop} (λ p: El prop, p ⇒ (λ _: Prf p, p)));"
    ; `P "The program outputs"
    ; `Pre "symbol true: Prf (∀ (λ p, imp p p));"
    ; `S Manpage.s_bugs
    ; `P "The program is not a filter."
    ; `P
        "In the output, some symbols are fully qualified and others are not. \
         This is due to the fact that lambdapi handles imperatively which \
         signature is opened to print terms which makes it difficult to know \
         which symbol is supposed to be visible or not. The issue it not \
         represented in the example."
    ]
  in
  let exits = Term.default_exits in
  ( Term.(const translate_file $ lib_root $ mapfile $ src)
  , Term.info "psnj-qfo" ~doc ~man ~exits )

let () = Term.(exit @@ eval cmd)
