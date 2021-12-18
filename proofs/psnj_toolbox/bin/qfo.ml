open Core
open Common
open Parsing
open Lplib

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

(** [rule ss ast] retrieves the rule declarations in [ast], scopes them in
    [ss] and return them in a flat list. *)
let rule (ss : Sig_state.t) (ast : Syntax.ast) : (Term.sym * Term.rule) list =
  let rules = ref [] in
  let consume cmd =
    match cmd.Pos.elt with
    | Syntax.P_rules rs ->
        let pre_rules = List.map (Scope.scope_rule true ss) rs in
        let pre_rules =
          List.map
            (fun Pos.{ elt = pr; _ } ->
              (pr.Scope.pr_sym, Scope.rule_of_pre_rule pr))
            pre_rules
        in
        rules := pre_rules :: !rules
    | _ -> assert false
  in
  (try Stream.iter consume ast with Error.Fatal (p, msg) -> print_err p msg);
  List.(rev !rules |> flatten)

let transpile config eval load meta_rule meta_load =
  Library.set_lib_root None;
  (match Package.find_config config with
  | Some c -> Package.apply_config c
  | None -> ());
  Timed.(Console.verbose := 0);
  let sign = Sig_state.create_sign [ "qfo" ] in
  let ss = Sig_state.of_sign sign in
  let ss =
    List.fold_right
      (fun e ss -> compile_ast ss (Parser.Lp.parse_string "<eval>" e))
      eval ss
  in
  let ss =
    List.fold_right
      (fun f ss -> compile_ast ss (Parser.Lp.parse_file f))
      load ss
  in
  let rules =
    try
      List.(
        map (fun s -> rule ss (Parser.Lp.parse_string "meta_rule" s)) meta_rule
        @ map (fun f -> rule ss (Parser.Lp.parse_file f)) meta_load
        |> flatten)
    with Error.Fatal (p, msg) ->
      print_err p msg;
      exit 1
  in
  let props = compile_props ss (Parser.Lp.parse stdin) in
  let props =
    List.map
      (fun (s, ty) ->
        (s, Psnj.LpTool.rewrite_with ss.Sig_state.signature rules ty))
      props
  in
  Timed.(Print.print_domains := true);
  (* print implicits? *)
  List.iter
    (fun (s, ty) -> Format.printf "@[symbol %s:@ %a;@]@." s Print.pp_term ty)
    props

open Cmdliner

let config =
  let doc = "Load lambdapi configuration file from directory $(docv)" in
  Arg.(
    value & opt dir (Sys.getcwd ()) & info [ "c"; "config" ] ~doc ~docv:"DIR")

let lp_eval =
  let doc = "Eval string $(docv) before reading input" in
  Arg.(value & opt_all string [] & info [ "eval"; "e" ] ~doc ~docv:"LP")

let lp_load =
  let doc =
    "Load lambdapi file $(docv) in current signature before reading input"
  in
  Arg.(value & opt_all file [] & info [ "load"; "l" ] ~doc ~docv:"FILE")

let meta_rule =
  let doc = "Eval string $(docv) as a meta rewrite rule declaration" in
  Arg.(value & opt_all string [] & info [ "meta-eval"; "m" ] ~doc ~docv:"LP")

let meta_load =
  let doc = "Load rules in file $(docv) as meta rewrite rules" in
  Arg.(value & opt_all string [] & info [ "meta-load" ] ~doc ~docv:"FILE")

let cmd =
  let doc = "Convert PVS-Cert encoded file quasi first order encoded files" in
  let exits = Term.default_exits in
  ( Term.(const transpile $ config $ lp_eval $ lp_load $ meta_rule $ meta_load),
    Term.info "qfo" ~doc ~exits )
