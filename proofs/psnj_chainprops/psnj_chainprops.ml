(** Parse dependencies file *)
module Deps = struct
  open Angstrom

  type t = string * string list

  let is_space = function ' ' | '\t' -> true | _ -> false

  let blank = skip_while is_space

  let blank1 = satisfy is_space *> blank

  let colon = blank *> char ':' <* blank

  let word = take_till is_space

  let line = both (word <* colon) (sep_by blank1 word)

  let pp ppf ((s, deps) : t) =
    let open Format in
    let pp_sep = pp_print_space in
    fprintf ppf "@[<h>%s:@ %a@]" s (pp_print_list ~pp_sep pp_print_string) deps

  let parse (ic : in_channel) : t list =
    let deps = ref [] in
    (try
       while true do
         match parse_string ~consume:Prefix line (input_line ic) with
         | Ok v -> deps := v :: !deps
         | Error msg -> failwith msg
       done;
       assert false (* End of line should be reached*)
     with End_of_file -> List.rev !deps)
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
  let man = [] in
  ( Term.(const chainprops $ src $ deps),
    Term.info "psnj-chainprops" ~doc ~exits ~man )

let () = Term.(exit @@ eval cmd)
