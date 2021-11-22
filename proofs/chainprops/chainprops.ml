(** Parse dependencies file *)
module Deps : sig
  type t = string * string list

  val pp : Format.formatter -> t -> unit

  val parse : in_channel -> t list
end = struct
  open Angstrom

  type t = string * string list

  let is_space = function ' ' | '\t' -> true | _ -> false

  let blank = skip_while is_space

  let blank1 = satisfy is_space *> blank

  let eol = string "\n\r" <|> string "\n"

  let colon = blank *> char ':' <* blank

  (* Word characters taken from the parser of lambdapi. *)
  let is_wordchar = function
    | ' ' | '\r' | '\t' | '\n' | '(' | ')' | '{' | '}' | '[' | ']' | ':' | '.'
    | '`' | '"' ->
        false
    | _ -> true

  let word = take_while1 is_wordchar

  let line = both (word <* colon) (sep_by blank1 word)

  let deps = many (line <* many1 eol)

  let pp ppf ((s, deps) : t) =
    let open Format in
    let pp_sep = pp_print_space in
    fprintf ppf "@[<h>%s:@ %a@]" s (pp_print_list ~pp_sep pp_print_string) deps

  let parse (ic : in_channel) : t list =
    let file = really_input_string ic (in_channel_length ic) in
    match parse_string ~consume:All deps file with
    | Ok v -> v
    | Error msg -> failwith msg
end

(** Transform a list of propositions into inference steps. *)

open Common
open Parsing
module S = Syntax

type decl = string * S.p_term
(** A proposition declaration with a name and a type. *)

(** [pp ppf decl] pretty prints declaration [decl] to formatter [ppf]. *)
let pp (ppf : Format.formatter) ((n, ty) : decl) : unit =
  Format.fprintf ppf "@[symbol %s:@ %a;@]" n Pretty.term ty

(** [propositions ast] returns the list of propositions as pairs
    [(name,ty)] where [name] is the name of the proposition and [ty]
    is its type. *)
let propositions (ast : S.ast) : decl list =
  let props : decl list ref = ref [] in
  let match_decl = function
    | S.P_symbol { p_sym_nam; p_sym_typ = Some ty; _ } -> (p_sym_nam.elt, ty)
    | _ ->
        failwith
          "Invalid Dedukti source file: only symbol declarations are supported"
  in
  Stream.iter (fun c -> props := match_decl c.Pos.elt :: !props) ast;
  !props

(** [merge imp deps props] transform each proposition of [props] into
    implications (the implication is defined by [imp]) from its
    dependencies specified in [deps] to itself. *)
let merge (imp : S.p_term) (deps : Deps.t list) (props : decl list) : decl list
    =
  let fn ((name, ty) : decl) : decl =
    let deps = try List.assoc name deps with Not_found -> [] in
    let f dep acc =
      let d_ty = List.assoc dep props in
      Syntax.P.(appl (appl imp d_ty) acc)
    in
    let ty = List.fold_right f deps ty in
    (name, ty)
  in
  List.map fn props

let chainprops src deps imply pp_deps =
  let ic = open_in deps in
  let deps = Deps.parse ic in
  close_in ic;
  if pp_deps then (
    Format.(
      eprintf "=== Dependencies ===@\n";
      pp_print_list ~pp_sep:pp_print_newline Deps.pp err_formatter deps;
      eprintf "@\n====================@."));
  let imply = Syntax.P.iden imply in
  let props = propositions (Parser.parse_file src) in
  let inferences = merge imply deps props in
  let open Format in
  pp_print_list ~pp_sep:pp_print_newline pp std_formatter inferences;
  pp_print_newline std_formatter ()

(** CLI *)

open Cmdliner

let src =
  let doc = "Translate Dedukti $(docv)" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:"SRC")

let deps =
  let doc = "Dependencies of propositions inside Dedukti file" in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~doc ~docv:"DEPS")

let imply =
  let doc = "Use symbol $(docv) as implication" in
  Arg.(value & opt string "imp" & info [ "imp" ] ~doc ~docv:"IMP")

let pp_deps =
  let doc = "Print parsed dependencies to stderr (for debugging purposes)" in
  Arg.(value & flag & info [ "pp-deps" ] ~doc)

let cmd =
  let doc = "Build a proof tree from propositions and dependencies" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Given a list of propositions and dependencies between them, create \
         inferences steps as implications from dependencies to the target \
         proposition.";
      `S Manpage.s_examples;
      `P "Given two files foo.lp:";
      `Pre "symbol tgt: P;\nsymbol hyp0: H0;\nsymbol hyp1: H1;";
      `P "and foo.dep";
      `Pre "tgt: hyp0 hyp1";
      `P "The command psnj-chainprops foo.lp foo.dep outputs";
      `Pre
        "symbol hyp1: H1;\nsymbol hyp0: H0;\nsymbol tgt: @imp H0 (@imp H1 P);";
    ]
  in
  ( Term.(const chainprops $ src $ deps $ imply $ pp_deps),
    Term.info "psnj-chainprops" ~doc ~exits ~man )

let () = Term.(exit @@ eval cmd)
