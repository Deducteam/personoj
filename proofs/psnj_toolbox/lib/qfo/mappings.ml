(** Load symbol mappings. The translation functions need some symbols
     to work on. A symbol mapping specify which symbol of a Dedukti
     encoding implements such symbol. *)

open Lplib
open Extra

type t = (string * string) list StrMap.t

(** [parse obj] parses json object [obj] into a map from module names
    (as strings) to symbol mappings (as assoc list of strings). *)
let parse (obj : Ezjsonm.t) : t =
  let f (modl, builtins) =
    let dict =
      Ezjsonm.get_dict builtins
      |> List.map (fun (k, v) -> (k, Ezjsonm.get_string v))
    in
    StrMap.add modl dict
  in
  List.fold_right f Ezjsonm.(get_dict (value obj)) StrMap.empty

(** [of_file ic] parses the json object from channel [ic]. *)
let of_channel (ic : in_channel) : t = Ezjsonm.from_channel ic |> parse
