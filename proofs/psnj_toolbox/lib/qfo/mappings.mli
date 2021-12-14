open Lplib
open Extra

type t = (string * string) list StrMap.t
(** Map from module name (either "pvs_cert", "pvs_connectives" or
    ["propositional_connectives"]) to association from label
    (e.g. "truth") to a symbol name. See the manual page. *)

val of_channel : in_channel -> t
(** [of_channel ic] create a mapping from channel [ic]. *)
