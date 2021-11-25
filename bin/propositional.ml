open Core
open Common
open Parsing
open Lplib
open Extra
module P = Process

let usage = "Usage: lpvs-propositional [--lib-root DIR] FILE"
let speclist = Arg.align P.speclist

let translate_file (src : string) =
  Console.State.push ();
  Package.apply_config src;
  let mp = Library.path_of_file LpLexer.escape src in
  let sign = Sig_state.create_sign mp in
  let ss = Sig_state.of_sign sign in
  let pcert_ss =
    let ast =
      Parser.parse_string "lpvs"
        "require open lpvs.encoding.lhol lpvs.encoding.pvs_cert \
         lpvs.encoding.logical;"
    in
    P.compile_ast ss ast
  in
  let module Pcert = (val Lpvs.Encodings.mkpcert pcert_ss) in
  Console.out 1 "Loaded PVS-Cert encoding";
  let prop_calc_ss =
    let ast =
      Parser.parse_string "lpvs"
        "open lpvs.encoding.lhol;require open lpvs.encoding.kpl;"
    in
    P.compile_ast ss ast
  in
  let module Propc = (val Lpvs.Encodings.mkkpropositional prop_calc_ss) in
  Console.out 1 "Loaded classical propositional calculus";
  let module Tran = Lpvs.LpCert.PropOfPcert (Pcert) (Propc) in
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

let () =
  let files = ref [] in
  Arg.parse speclist (fun f -> files := f :: !files) usage;
  files := List.rev !files;
  Library.set_lib_root (if !P.lib_root = "" then None else Some !P.lib_root);
  (* Silence lambdapi to have environment-independent output. *)
  Timed.(Console.verbose := 0);
  List.iter (fun src -> P.exit_on_fatal (fun () -> translate_file src)) !files
