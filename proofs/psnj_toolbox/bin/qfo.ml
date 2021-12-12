open Core
open Common
open Parsing
open Lplib
open Extra

(** Some Lambdapi utils *)

let print_err (pos : Pos.popt option) (msg : string) : unit =
  match pos with
  | Some p -> Format.eprintf "[%a] %s@." Pos.pp p msg
  | None -> Format.eprintf "%s@." msg

let compile_ast (sig_st : Sig_state.t) (ast : Syntax.ast) : Sig_state.t =
  let open Handle in
  let ss = ref sig_st in
  let compile = Compile.Pure.compile false in
  let consume cmd = ss := Command.handle compile !ss cmd in
  Stream.iter consume ast;
  !ss

(** [compile_props sig_st ast] compiles an AST made of symbol declarations
    of the form [symbol foo: bar;]. Term [bar] need not be of type [TYPE]. *)
let compile_props (sig_st : Sig_state.t) (ast : Syntax.ast) :
    (string * Term.term) list =
  let ss = ref sig_st in
  let out = ref [] in
  let consume cmd =
    match cmd.Pos.elt with
    (* TODO: check that proposition has type Prop rather than just inferring the type *)
    | Syntax.P_symbol Syntax.{ p_sym_nam; p_sym_typ; _ } ->
        let pty =
          match p_sym_typ with
          | Some t -> t
          | None ->
              Format.eprintf "Invalid input: no type given@.";
              exit 1
        in
        let ty =
          try
            let te = Scope.scope_term false !ss [] pty in
            fst (Unif.Infer.infer [] (Pos.none te))
          with Error.Fatal (p, msg) ->
            print_err p msg;
            exit 1
        in
        out := (p_sym_nam.elt, ty) :: !out
    | _ ->
        Format.eprintf "Invalid input: only symbol declarations allowed@.";
        exit 1
  in
  Stream.iter consume ast;
  List.rev !out

let new_sig_state (mp : Path.t) : Sig_state.t =
  Sig_state.(of_sign (create_sign mp))

(** Main function *)

let translate (lib_root : string option) (map_dir : (string * string) list)
    (mapfile : string) (eval : string list) : unit =
  (* Get symbol mappings *)
  let mapping = PsnjQfo.Mappings.of_file mapfile in
  let pvs_cert, pvs_connectives, propositional_connectives =
    let f s =
      try StrMap.find s mapping
      with Not_found ->
        Format.eprintf "Section \"%s\" not found in \"%s\"" s mapfile;
        exit 1
    in
    (f "pvs_cert", f "pvs_connectives", f "propositional_connectives")
  in
  (* Setup lp *)
  (* Silence lambdapi to have environment-independent output. *)
  (try
     Timed.(Console.verbose := 0);
     Library.set_lib_root lib_root;
     List.iter Library.add_mapping map_dir;
     Console.State.push ()
   with Error.Fatal (pos, msg) ->
     print_err pos msg;
     exit 1);
  (* Try to find lambdapi pkgs from current working directory, and do
     nothing if it fails *)
  (try
     Package.apply_config (Sys.getcwd ());
     Format.eprintf "Loaded package file from \"%s\"@." (Sys.getcwd ())
   with Error.Fatal _ -> ());
  let mp = [ "<stdin>" ] in
  let pvs_cert_ss =
    try
      let ss = new_sig_state mp in
      let ast =
        Parser.parse_string "<qfo>"
          "require open qfo.encoding.lhol qfo.encoding.pvs_cert \
           qfo.encoding.pvs_connectives;"
      in
      compile_ast ss ast
    with Error.Fatal (pos, msg) ->
      Format.eprintf "Couldn't initialise PVS-Cert signature@\n";
      print_err pos msg;
      exit 1
  in
  let ps = PsnjQfo.Encodings.mkpredicate_subtyping pvs_cert pvs_cert_ss in
  Console.out 1 "Loaded PVS-Cert encoding";
  let pvs_c =
    let pvs_connectives_ss =
      try
        let ss = new_sig_state mp in
        let ast =
          Parser.parse_string "<qfo>"
            "require open qfo.encoding.lhol qfo.encoding.pvs_connectives;"
        in
        compile_ast ss ast
      with Error.Fatal (p, m) ->
        Format.eprintf "Couldn't initalise PVS connectives signature@\n";
        print_err p m;
        exit 2
    in
    PsnjQfo.Encodings.mkconnectives pvs_connectives pvs_connectives_ss
  in
  let prop_calc_ss =
    let ss = new_sig_state mp in
    let ast =
      Parser.parse_string "<qfo>"
        "require open qfo.encoding.lhol qfo.encoding.propositional_connectives;"
    in
    compile_ast ss ast
  in
  let prop_c =
    PsnjQfo.Encodings.mkconnectives propositional_connectives prop_calc_ss
  in
  Console.out 1 "Loaded classical propositional calculus";
  let qfo_ss =
    (* Create a sig state with PVS-Cert and the specification*)
    try
      let ss = new_sig_state [ "<qfo>" ] in
      let ast =
        Parser.parse_string "<qfo>"
          "require open qfo.encoding.lhol qfo.encoding.pvs_cert \
           qfo.encoding.pvs_connectives;"
      in
      compile_ast ss ast
    with Error.Fatal (p, m) ->
      Format.eprintf "Couln't initialise qfo signature:@\n";
      print_err p m;
      exit 2
  in
  (* Evaluate command line given code in [qfo_ss] *)
  let qfo_ss =
    List.fold_right
      (fun e ss -> compile_ast ss (Parser.parse_string "<eval>" e))
      eval qfo_ss
  in
  (* Load propositions from stdin in [qfo_ss] *)
  let props = compile_props qfo_ss (Parser.parse stdin) in
  let transpile_print (name, ty) =
    let open PsnjQfo.Transpile in
    try
      let propty = translate_term ~ps ~prop_c ~pvs_c ty in
      Format.printf "@[symbol %s:@ %a;@]@." name Print.pp_term propty
    with CannotTranslate t ->
      Format.eprintf "Cannot translate %a@." Print.pp_term t
  in
  (* Prepare for printing *)
  let printing_ss =
    (* Signature state used to print terms *)
    try
      let ss = new_sig_state [ "<qfo.print>" ] in
      let open_cmd =
        "require open qfo.encoding.lhol qfo.encoding.pvs_cert \
         qfo.encoding.propositional_connectives;"
      in
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
  List.iter transpile_print props

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

let lp_eval =
  let doc = "Eval string $(docv) before reading standard input" in
  Arg.(value & opt_all string [] & info [ "eval"; "e" ] ~doc ~docv:"LP")

let cmd =
  let doc = "Convert PVS-Cert encoded file quasi first order encoded files" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) is a filter that transforms a list of Dedukti axioms encoded \
         in PVS-Cert with dependent logical connectives into a list of axioms \
         expressed in something close to simple type theory with non dependent \
         logical connectives.";
      `P
        "The program requires all its source files to be in the package \
         $(b,qfo). The easiest way to do that is to place the source files in \
         a directory where there is a $(b,lambdapi.pkg) file with the line \
         $(b,root_path=qfo).";
      `P
        "To convert files, the program needs identify the symbols of the \
         encoding. The mapping allows to indicate the name of such symbols. \
         This mapping is a JSON object with the following structure";
      `Pre
        "{ \"pvs_cert\": ...,\n\
        \  \"pvs_connectives\": ...,\n\
        \  \"propositional_connectives\": ... }";
      `P
        "where the value associated to \"pvs_cert\" is an object that define \
         the following form";
      `Pre "{ \"subset\": STRING }";
      `P
        "and the values associated to both \"pvs_connectives\" and \
         \"propositional_connectives\" must be of the form";
      `Pre
        "{ \"truth\": STRING; \"falsity\": STRING;\n\
        \  \"implication\": STRING;\n\
        \  \"negation\": STRING; \n\
        \  \"conjunction\": STRING; \"disjunction\": STRING;\n\
        \  \"existential\": STRING; \"universal\": STRING }";
      `P
        "The object \"pvs_cert\" define symbols used to encode PVS-Cert while \
         \"pvs_connectives\" and \"propositional_connectives\" define logical \
         connectors: connectors from \"pvs_connectives\" are replaced by \
         connectors from \"propositional_connectives\".";
      `P
        "Symbols mentioned in $(b,\"pvs_cert\") are expected to be found in \
         modules $(b,qfo.encoding.pvs_cert) and $(b,qfo.encoding.lhol), \
         symbols mentioned in $(b,\"pvs_connectives\") are expected to be \
         found in module $(b,qfo.encoding.pvs_connectives) and symbols \
         mentioned in $(b,\"propositional_connectives\") are expected to be \
         found in module $(b,qfo.encoding.proposisitional_connectives).";
      `P
        "Furthermore, if the standard input uses symbols from some other \
         module \"mod\", it can be opened using $(b,-e 'require open mod;').";
      `S Manpage.s_examples;
      `P "Let qfo.json be the following json file";
      `Pre
        {|{
  "pvs_cert": {
    "subset": "psub"
  },
  "pvs_connectives": {
    "truth": "true",
    "falsity": "false",
    "negation": "¬",
    "implication": "⇒",
    "conjunction": "∧",
    "disjunction": "∨",
    "existential": "∃",
    "universal": "∀"
  },
  "propositional_connectives": {
    "truth": "top",
    "falsity": "bot",
    "negation": "not",
    "implication": "imp",
    "conjunction": "conj",
    "disjunction": "disj",
    "existential": "ex",
    "universal": "all"
  }
}|};
      `P "and with the following input,";
      `Pre "symbol true : ∀ {prop} (λ p: El prop, p ⇒ (λ _: Prf p, p));";
      `P "The program outputs";
      `Pre "symbol true: @∀ prop (λ p: El prop, imp p p);";
      `S Manpage.s_bugs;
      `P
        "If the input opens some module (using \"open mod\"), then symbols \
         from this module will appear fully qualified.";
    ]
  in
  let exits = Term.default_exits in
  ( Term.(const translate $ lib_root $ map_dir $ mapfile $ lp_eval),
    Term.info "qfo" ~doc ~man ~exits )
