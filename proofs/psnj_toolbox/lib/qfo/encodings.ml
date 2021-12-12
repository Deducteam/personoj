open Common
open Core
module T = Term

(** [find_sym sig_st symp] seeks symbol identified with [symp] in
    signature state [sig_st]. *)
let find_sym (sig_st : Sig_state.t) (symp : Path.t * string) : T.sym =
  try Sig_state.find_sym ~prt:true ~prv:true sig_st (Pos.none symp)
  with Not_found -> invalid_arg "find_sym: symbol not found"

(** Find symbol [sym] from an {b opened} signature. *)
let fsym (sig_st : Sig_state.t) (sym : string) : T.sym =
  find_sym sig_st ([], sym)

type mapping = (string * string) list

type predicate_subtyping = { subset : T.sym }
(** PVS-Cert *)

let mkpredicate_subtyping (map : mapping) (sig_st : Sig_state.t) :
    predicate_subtyping =
  let s (name : string) : T.sym =
    try fsym sig_st (List.assoc name map)
    with Not_found ->
      failwith (Format.sprintf "Symbol \"%s\" not found in mapping" name)
  in
  { subset = s "subset" }

type connectives = {
  truth : T.sym;
  falsity : T.sym;
  implication : T.sym;
  negation : T.sym;
  conjunction : T.sym;
  disjunction : T.sym;
  existential : T.sym;
  universal : T.sym;
}
(** Logical connectives *)

let mkconnectives (map : (string * string) list) (sig_st : Sig_state.t) :
    connectives =
  let s (name : string) : T.sym =
    try fsym sig_st (List.assoc name map)
    with Not_found ->
      failwith (Format.sprintf "Symbol \"%s\" not found in mapping" name)
  in
  {
    truth = s "truth";
    falsity = s "falsity";
    negation = s "negation";
    implication = s "implication";
    conjunction = s "conjunction";
    disjunction = s "disjunction";
    existential = s "existential";
    universal = s "universal";
  }
