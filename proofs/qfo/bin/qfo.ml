open Core
open Common
open Parsing
open Lplib
open Extra

(** Some Lambdapi utils *)

let compile_ast (sig_st : Sig_state.t) (ast : Syntax.ast) : Sig_state.t =
  let open Handle in
  let ss = ref sig_st in
  let compile = Compile.Pure.compile false in
  let consume cmd = ss := Command.handle compile !ss cmd in
  Stream.iter consume ast; !ss

(** [get_symbols sign] returns a map from symbol names to their type
    for all symbols of [sign] (with position). *)
let get_symbols (sign : Sign.t) =
  let open Timed in
  let syms = !(sign.Sign.sign_symbols) in
  StrMap.map (fun (sym, pos) -> (!(sym.Term.sym_type), pos)) syms

(** [exit_on_fatal f] executes thunk [f] and handles any fatal error
    [Fatal] gracefully. *)
let exit_on_fatal (f : unit -> unit) =
  try f ()
  with Error.Fatal (pos, msg) ->
    (match pos with
    | Some p -> Format.eprintf "[%a] %s@." Pos.pp p msg
    | None -> Format.eprintf "%s@." msg);
    exit 1

(** Main function *)

let translate (lib_root : string option) (map_dir : (string * string) list)
    (mapfile : string) : unit =
  (* Get symbol mappings *)
  let mapping = PvsLp.Mappings.of_file mapfile in
  let pcertmap, depconnectives, connectives =
    let f s =
      try StrMap.find s mapping
      with Not_found ->
        failwith
          (Format.sprintf "Section \"%s\" not found in \"%s\"" "pcert" mapfile)
    in
    (f "pcert", f "depconnectives", f "connectives")
  in
  (* Setup lp *)
  (* Silence lambdapi to have environment-independent output. *)
  Timed.(Console.verbose := 0);
  Library.set_lib_root lib_root;
  List.iter Library.add_mapping map_dir;
  Console.State.push ();
  (* Try to find lambdapi pkgs from current working directory, and do
     nothing if it fails *)
  try
    Package.apply_config (Sys.getcwd ());
    Format.eprintf "Loaded package file from \"%s\"@." (Sys.getcwd ())
  with Error.Fatal _ ->
    ();
    let mp = [ "<stdin>" ] in
    let sign = Sig_state.create_sign mp in
    let ss = Sig_state.of_sign sign in
    let pcert_ss =
      let ast =
        Parser.parse_string "lpvs"
          "require open lpvs.encoding.lhol lpvs.encoding.pcert \
           lpvs.encoding.depconnectives;"
      in
      compile_ast ss ast
    in
    let module Pcert = (val PvsLp.Encodings.mkpcert pcertmap pcert_ss) in
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
           compile_ast ss ast
         in
         PvsLp.Encodings.mkconnectors depconnectives dep_conn_ss)
    in
    let prop_calc_ss =
      let ast =
        Parser.parse_string "lpvs"
          "open lpvs.encoding.lhol;require open lpvs.encoding.connectives;"
      in
      compile_ast ss ast
    in
    let module Propc =
    (val PvsLp.Encodings.mkconnectors connectives prop_calc_ss)
    in
    Console.out 1 "Loaded classical propositional calculus";
    let module Tran = PvsLp.LpCert.PropOfPcert (Pcert) (DepConn) (Propc) in
    let ast = Parser.parse stdin in
    let _ss = compile_ast pcert_ss ast in
    let syms = get_symbols sign in
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

let map_dir =
  let doc =
    "Map all the modules having MOD as a prefix of their module path to files \
     under the directory DIR. The corresponding modules under the library root \
     are then rendered inaccessible. This option is useful during the \
     development of a library, before it can be installed in the expected \
     folder under the library root."
  in
  Arg.(
    value
    & opt_all (pair ~sep:':' string dir) []
    & info [ "map-dir" ] ~docv:"MOD:DIR" ~doc)

let mapfile =
  let doc = "Maps Dedukti symbols into the runtime process." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"JSON")

let cmd =
  let doc = "Convert PVS-Cert encoded file quasi first order encoded files" in
  let man =
    [
      `S Manpage.s_description
    ; `P
        "$(tname) is a filter that transforms a list of Dedukti axioms encoded \
         in PVS-Cert with dependent logical connectives into a list of axioms \
         expressed in something close to first order logic."
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
    ; `P
        "Unlike in lambdapi, because standard input is parsed, the option \
         $(b,--map-dir) should in general be used."
    ; `P
        "In the output, some symbols are fully qualified and others are not. \
         This is due to the fact that lambdapi handles imperatively which \
         signature is opened to print terms which makes it difficult to know \
         which symbol is supposed to be visible or not. The issue it not \
         represented in the example."
    ]
  in
  let exits = Term.default_exits in
  ( Term.(const translate $ lib_root $ map_dir $ mapfile)
  , Term.info "psnj-qfo" ~doc ~man ~exits )

let () = Term.(exit @@ eval cmd)
