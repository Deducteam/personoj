(** Paths are (possibly) empty lists of natural numbers. *)
module Path = struct
  type t = int list

  let is_predecessor (p : t) (q : t) : bool =
    match q with [] -> false | _ :: q_tl -> q_tl = p

  let of_string (s : string) : t =
    if s = "root" then []
    else
      try String.split_on_char '.' s |> List.rev_map int_of_string
      with Failure err ->
        Format.eprintf "Invalid path: %s@." s;
        invalid_arg err
end

module Id = struct
  type t = string

  let make (name : string) : t = name
  let pp = Format.pp_print_string
  let compare = String.compare
end

module IdMap = Map.Make (struct
  type t = Id.t

  let compare = Id.compare
end)

type t = Id.t list IdMap.t

(** [add id d t] adds dependency [d] to [id] in table [t]. If [id]
    is not bound, it is added to the table.  *)
let add (id : Id.t) (d : Id.t) (t : t) : t =
  let deps =
    match IdMap.find_opt id t with None -> [] | Some deps -> d :: deps
  in
  IdMap.add id (d :: deps) t

let get (id : Id.t) (m : t) : Id.t list = IdMap.find id m

(** [update x x_pth tbl id_paths] updates the entries of the table
    [tbl] adding [id]: if [(e,q)] is an element of [id_paths], [x_th]
    is the successor of [q] then [x] is added to the dependencies of
    element [e] of the table *)
let update (x : Id.t) (x_pth : Path.t) (tbl : t) (id_paths : Path.t IdMap.t) : t
    =
  let f (y : Id.t) (y_pth : Path.t) (t : t) : t =
    if Path.is_predecessor y_pth x_pth then add y x t else t
  in
  IdMap.fold f id_paths tbl

let of_paths (m : Path.t IdMap.t) : t =
  IdMap.fold (fun x pth acc -> update x pth acc m) m IdMap.empty

let pp_makefile (ppf : Format.formatter) (tbl : t) : unit =
  let open Format in
  let pp_deplist ppf (l : Id.t list) : unit =
    let pp_sep ppf () = pp_print_space ppf () in
    let l = List.sort_uniq Id.compare l in
    pp_print_list ~pp_sep Id.pp ppf l
  in
  let pprint_d (id : Id.t) (d : Id.t list) : unit =
    fprintf ppf "@[<h>%a:@ %a@]@\n" Id.pp id pp_deplist d
  in
  IdMap.iter pprint_d tbl

module P = struct
  open Angstrom

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

  let parse (ic : in_channel) : (string * string list) list =
    let file = really_input_string ic (in_channel_length ic) in
    match parse_string ~consume:All deps file with
    | Ok v -> v
    | Error msg -> failwith msg
end

let of_makefile (ic : in_channel) : t =
  let elts = P.parse ic in
  List.fold_right
    (fun (src, tgts) acc -> IdMap.add src tgts acc)
    elts IdMap.empty
