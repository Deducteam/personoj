open Core
open Common
open Parsing
open Lplib
open Extra
module P = Process

let usage = "Usage: lpvs-tptp [--lib-root] FILE"
let pp_cert = ref false

let speclist =
  Arg.align
    (P.speclist
    @ [
        ( "--pp-cert"
        , Arg.Set pp_cert
        , " Print PVS-Cert terms as lambdapi symbol declarations" )
      ])

let translate_file (src : string) =
  Console.State.push ();
  Package.apply_config src;
  let mp = Library.path_of_file LpLexer.escape src in
  let sign = Sig_state.create_sign mp in
  let ss = Sig_state.of_sign sign in
  let pcert_ss =
    let ast =
      Parser.parse_string "lpvs"
        "require open lpvs.encoding.mpl lpvs.encoding.lhol \
         lpvs.encoding.pvs_cert;"
    in
    P.compile_ast ss ast
  in
  let module PcertEnc = (val Lpvs.Encodings.mkpcert pcert_ss) in
  let module Pcert = Lpvs.Cert.Make (PcertEnc) in
  Console.out 1 "Loaded PVS-Cert encoding";
  let ast = Parser.parse_file src in
  let _ss = P.compile_ast pcert_ss ast in
  let syms = P.get_symbols sign in
  let pcertast = StrMap.map (fun (sym, _) -> Pcert.import sym) syms in
  let lp name ty =
    let out =
      if !pp_cert then Format.printf else Format.ifprintf Format.std_formatter
    in
    out "@[symbol@ %s:@ %a;@]@." name Lpvs.Cert.pp ty
  in
  let tptp name ty =
    try
      let e = Lpvs.Cert.tptp_of ty in
      Format.printf "fof(@[%s,@ %a@]).@." name Lpvs.Tptp.Term.pp e
    with Lpvs.Cert.CannotTranslate t ->
      Format.eprintf "Cannot translate %a@." Lpvs.Cert.pp t
  in
  StrMap.iter lp pcertast; StrMap.iter tptp pcertast

let () =
  let files = ref [] in
  Arg.parse speclist (fun f -> files := f :: !files) usage;
  Library.set_lib_root (if !P.lib_root = "" then None else Some !P.lib_root);
  (* Silence lambdapi to have environment-independent output. *)
  Timed.(Console.verbose := 0);
  List.iter (fun src -> P.exit_on_fatal (fun () -> translate_file src)) !files
