(** Parse dependencies file *)
module Deps : sig
  type t

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

  let word = take_till (fun c -> is_space c || c = '\n' || c = '\r')

  let line = both (word <* colon) (sep_by blank1 word)

  let deps = sep_by (many1 eol) line

  let pp ppf ((s, deps) : t) =
    let open Format in
    let pp_sep = pp_print_space in
    fprintf ppf "@[<h>%s:@ %a@]" s (pp_print_list ~pp_sep pp_print_string) deps

  let parse (ic : in_channel) : t list =
    let file = really_input_string ic (in_channel_length ic) in
    match parse_string ~consume:Prefix deps file with
    | Ok v -> v
    | Error msg -> failwith msg
end

open Cmdliner

let src =
  let doc = "Translate Dedukti $(docv)" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc ~docv:"SRC")

let deps =
  let doc = "Dependencies of propositions inside Dedukti file" in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~doc ~docv:"DEPS")

let chainprops _src deps =
  let ic = open_in deps in
  let deps = Deps.parse ic in
  close_in ic;
  let pp_sep = Format.pp_print_newline in
  Format.(printf "%a@." (pp_print_list ~pp_sep Deps.pp) deps)

let cmd =
  let doc = "Build a proof tree from propositions and dependencies" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Given a list of propositions and dependencies between them, create \
         implications such that the dependencies imply the source.";
      `S Manpage.s_examples;
      `P "Given two files foo.lp:";
      `Pre "symbol tgt: P;\nsymbol hyp0: H0;\nsymbol hyp1: H1;";
      `P "and foo.dep";
      `Pre "tgt: hyp0 hyp1";
      `P "The command psnj-chainprops foo.lp foo.dep outputs";
      `Pre "symbol tgt: H0 => H1 => P;";
    ]
  in
  ( Term.(const chainprops $ src $ deps),
    Term.info "psnj-chainprops" ~doc ~exits ~man )

let () = Term.(exit @@ eval cmd)
