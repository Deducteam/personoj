open Core
module T = Term
module B = Bindlib

(** [app_first x funs] applies the functions in [funs] to [x] until one 
    returns [Some y]. *)
let rec app_first (x : 'a) (funs : ('a -> 'b option) list) : 'b option =
  match funs with
  | [] -> None
  | f :: fs -> ( match f x with Some y -> Some y | None -> app_first x fs)

(** Maps on [T.term] variables. *)
module VMap = Map.Make (struct
  type t = T.tvar

  let compare = B.compare_vars
end)
