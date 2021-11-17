module Path = struct
  type t = int list

  (** [is_predecessor p q] is true if [q = p.i] ([p] is closer to the root). *)
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

  let make (name : string) = name

  let hash : t -> int = Hashtbl.hash

  let pp = Format.pp_print_string

  let compare = String.compare
end

let id2path : (Id.t * Path.t) list ref = ref []

module Deps = struct
  module IdMap = Map.Make (struct
    type t = Id.t

    let compare = Id.compare
  end)

  type t = Id.t list IdMap.t

  let empty = IdMap.empty

  (** [add id d t] adds dependency [d] to [id] in table [t]. If [id]
      is not bound, it is added to the table.  *)
  let add (id : Id.t) (d : Id.t) (t : t) : t =
    let deps =
      match IdMap.find_opt id t with None -> [] | Some deps -> d :: deps
    in
    IdMap.add id (d :: deps) t

  let find (id : Id.t) (t : t) : Id.t list = IdMap.find id t

  (** [update x x_pth tbl] updates the entries of the table [tbl] adding [id]: if
      [(e,q)] is an element of [id2path], [p] is the successor of [q] then
      [id] is added to the dependencies of [e] element [e] of the table *)
  let update (x : Id.t) (x_pth : Path.t) (tbl : t) : t =
    let f (t : t) ((y, y_pth) : Id.t * Path.t) : t =
      if Path.is_predecessor y_pth x_pth then add y x t else t
    in
    List.fold_left f tbl !id2path

  let pp (ppf : Format.formatter) (tbl : t) : unit =
    let open Format in
    let pp_deplist ppf (l : Id.t list) : unit =
      let pp_sep ppf () = pp_print_space ppf () in
      let l = List.sort_uniq Id.compare l in
      pp_print_list ~pp_sep Id.pp ppf l
    in
    let pprint_d (id : Id.t) (d : Id.t list) : unit =
      fprintf ppf "%a: @[<h>%a@]@\n" Id.pp id pp_deplist d
    in
    IdMap.iter pprint_d tbl
end

let () =
  (try
     while true do
       let id = input_line stdin in
       let pth = input_line stdin in
       id2path := (Id.make id, Path.of_string pth) :: !id2path
     done
   with End_of_file -> ());
  let tbl = Deps.empty in
  try
    let tbl =
      List.fold_left (fun acc (x, pth) -> Deps.update x pth acc) tbl !id2path
    in
    Deps.pp Format.std_formatter tbl
  with Invalid_argument err ->
    Format.eprintf "Invalid input (%s)@." err;
    exit 1
