open Core
open Common
open Handle
open Parsing
open Lplib
open Extra

let compile_ast (sig_st : Sig_state.t) (ast : Syntax.ast) : Sig_state.t =
  let ss = ref sig_st in
  let consume cmd = ss := Command.handle (Compile.compile false) !ss cmd in
  Stream.iter consume ast; !ss

(** [get_symbols sign] returns a map from symbol names to their type
    for all symbols of [sign] (with position). *)
let get_symbols (sign : Sign.t) =
  let open Timed in
  let syms = !(sign.Sign.sign_symbols) in
  StrMap.map (fun (sym, pos) -> (!(sym.Term.sym_type), pos)) syms

let usage = "Usage: lpvs [--lib-root] FILE"
let lib_root = ref ""
let speclist = [ ("--lib-root", Arg.Set_string lib_root, "Library root") ]

let translate_file (src : string) =
  Console.State.push ();
  Package.apply_config src;
  let mp = Library.path_of_file LpLexer.escape src in
  let sign = Sig_state.create_sign mp in
  let ss = Sig_state.of_sign sign in
  let pcert_ss =
    let ast =
      Parser.parse_string "lpvs"
        "require open lpvs.encoding.lhol lpvs.encoding.pvs_cert;"
    in
    compile_ast ss ast
  in
  let module Encoding = (val Lpvs.ToCert.make pcert_ss) in
  let module Pcert = Lpvs.ToCert.Make (Encoding) in
  Console.out 1 "Loaded PVS-Cert encoding";
  let ast = Parser.parse_file src in
  let _ss = compile_ast pcert_ss ast in
  let syms = get_symbols sign in
  let pcertast = StrMap.map (fun (sym, _) -> Pcert.import sym) syms in
  let lp name ty = Format.printf "@[symbol@ %s:@ %a;@]@." name Lpvs.ToCert.pp ty in
  let tptp name ty =
    let e = Lpvs.ToCert.tptp_of ty in
    Format.printf "fof(@[%s,@ %a@]).@." name Lpvs.Tptp.Term.pp e
  in
  StrMap.iter lp pcertast; StrMap.iter tptp pcertast

let () =
  let files = ref [] in
  Arg.parse speclist (fun f -> files := f :: !files) usage;
  Library.set_lib_root (if !lib_root = "" then None else Some !lib_root);
  (* Silence lambdapi to have environment-independent output. *)
  Timed.(Console.verbose := 0);
  let f = List.hd !files in
  try translate_file f
  with Error.Fatal (pos, msg) -> (
    match pos with
    | Some p -> Format.eprintf "[%a] %s@." Pos.pp p msg
    | None -> Format.eprintf "%s@." msg)
