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

let print_err (pos : Pos.popt option) (msg : string) : unit =
  match pos with
  | Some p -> Format.eprintf "[%a] %s@." Pos.pp p msg
  | None -> Format.eprintf "%s@." msg

let new_sig_state (mp : Path.t) : Sig_state.t =
  Sig_state.(of_sign (create_sign mp))

(** Main function *)

let translate (lib_root : string option) (map_dir : (string * string) list)
    (mapfile : string) : unit =
  (* Get symbol mappings *)
  let mapping = PvsLp.Mappings.of_file mapfile in
  let pcertmap, depconnectives, connectives =
    let f s =
      try StrMap.find s mapping
      with Not_found ->
        Format.eprintf "Section \"%s\" not found in \"%s\"" s mapfile;
        exit 1
    in
    (f "pcert", f "depconnectives", f "connectives")
  in
  (* Setup lp *)
  (* Silence lambdapi to have environment-independent output. *)
  (try
     Timed.(Console.verbose := 0);
     Library.set_lib_root lib_root;
     List.iter Library.add_mapping map_dir;
     Console.State.push ()
   with Error.Fatal (pos, msg) -> print_err pos msg; exit 1);
  (* Try to find lambdapi pkgs from current working directory, and do
     nothing if it fails *)
  (try
     Package.apply_config (Sys.getcwd ());
     Format.eprintf "Loaded package file from \"%s\"@." (Sys.getcwd ())
   with Error.Fatal _ -> ());
  let mp = [ "<stdin>" ] in
  let pcert_ss =
    try
      let ss = new_sig_state mp in
      let ast =
        Parser.parse_string "lpvs"
          "require open lpvs.lhol lpvs.pcert lpvs.depconnectives;"
      in
      compile_ast ss ast
    with Error.Fatal (pos, msg) ->
      Format.eprintf "Couldn't initialise PVS-Cert signature@\n";
      print_err pos msg;
      exit 1
  in
  let module Pcert = (val PvsLp.Encodings.mkpcert pcertmap pcert_ss) in
  Console.out 1 "Loaded PVS-Cert encoding";
  let module DepConn =
  (val let dep_conn_ss =
         try
           let ss = new_sig_state mp in
           let ast =
             Parser.parse_string "lpvs"
               "require open lpvs.lhol lpvs.depconnectives;"
           in
           compile_ast ss ast
         with Error.Fatal (p, m) ->
           Format.eprintf
             "Couldn't initalise dependent connectives signature@\n";
           print_err p m;
           exit 2
       in
       PvsLp.Encodings.mkconnectors depconnectives dep_conn_ss)
  in
  let prop_calc_ss =
    let ss = new_sig_state mp in
    let ast =
      Parser.parse_string "lpvs" "require open lpvs.lhol lpvs.connectives;"
    in
    compile_ast ss ast
  in
  let module Propc =
  (val PvsLp.Encodings.mkconnectors connectives prop_calc_ss)
  in
  Console.out 1 "Loaded classical propositional calculus";
  let module Tran = PvsLp.LpCert.PropOfPcert (Pcert) (DepConn) (Propc) in
  let qfo_ss =
    (* Create a sig state with PVS-Cert and the specification*)
    try
      let ss = new_sig_state [ "<qfo>" ] in
      let ast =
        Parser.parse_string "<qfo>" "require open lpvs.lhol lpvs.pcert;"
      in
      compile_ast ss ast
    with Error.Fatal (p, m) ->
      Format.eprintf "Couln't initialise qfo signature:@\n";
      print_err p m;
      exit 2
  in
  ignore @@ compile_ast qfo_ss (Parser.parse stdin);
  let syms = get_symbols Sig_state.(qfo_ss.signature) in
  let tr_pp name (ty, _) =
    try
      let propty = Tran.f ty in
      Format.printf "@[symbol %s:@ %a;@]@." name Print.pp_term propty
    with Tran.CannotTranslate t ->
      Format.eprintf "Cannot translate %a@." Print.pp_term t
  in
  (* Prepare for printing *)
  let printing_ss =
    (* Signature state used to print terms *)
    try
      let ss = new_sig_state [ "<qfo.print>" ] in
      let open_cmd = "require open lpvs.lhol lpvs.pcert lpvs.connectives;" in
      compile_ast ss (Parser.parse_string "<qfo.print>" open_cmd)
    with Error.Fatal (p, msg) ->
      Format.eprintf "Couldn't initialise signature for printing@\n";
      print_err p msg;
      exit 2
  in
  Timed.(
    Print.sig_state := printing_ss;
    Print.print_implicits := true;
    Print.print_domains := true);
  StrMap.iter tr_pp syms

open Cmdliner

let lib_root =
  let doc = "See manual of lambdapi(1)" in
  Arg.(
    value & opt (some string) None & info [ "lib-root" ] ~doc ~docv:"LIBROOT")

let map_dir =
  let doc = "See manual of lambdapi(1)" in
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
    ; `P
        "The object \"pcert\" define what symbol is used to encode PVS-Cert \
         while \"depconnectives\" and \"connectives\" define logical \
         connectors: connectors from \"depconnectives\" are replaced by \
         connectors from \"connectives\"."
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
    ]
  in
  let exits = Term.default_exits in
  ( Term.(const translate $ lib_root $ map_dir $ mapfile)
  , Term.info "psnj-qfo" ~doc ~man ~exits )

let () = Term.(exit @@ eval cmd)
