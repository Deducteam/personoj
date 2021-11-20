open Core
open Common
open Handle
open Parsing
open Lplib
open Extra

let lib_root : string ref = ref ""

let speclist =
  [ ("--lib-root", Arg.Set_string lib_root, " Set lambdapi library root") ]

let compile_ast (sig_st : Sig_state.t) (ast : Syntax.ast) : Sig_state.t =
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
