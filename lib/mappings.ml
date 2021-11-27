(** Load symbol mappings. The translation functions need some symbols
     to work on. A symbol mapping specify which symbol of a Dedukti encoding implements such symbol. *)

module J = Yojson.Basic
open Lplib
open Extra

type t = (string * string) list StrMap.t

(** [parse obj] parses json object [obj] into a map from module names
    (as strings) to symbol mappings (as assoc list of strings). *)
let parse (obj : J.t) : t =
  let f (modl, map) =
    let map =
      J.Util.to_assoc map |> List.map (fun (x, y) -> (x, J.Util.to_string y))
    in
    StrMap.add modl map
  in
  List.fold_right f (J.Util.to_assoc obj) StrMap.empty

(** [of_file src] parses the json object in file [src]. *)
let of_file (src : string) : t = J.from_file src |> parse
