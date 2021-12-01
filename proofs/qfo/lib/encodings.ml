(** Define several encodings and functions to load them from signature
    states.

    An encoding is represented by a module made of symbols. Having these
    symbols is required when we want to create terms in this encoding. To
    destruct on an encoding, having the name is enough. For each fragment
    [F], there is a function [mkf] that takes a signature state and
    extracts the symbol that are part of [F].  *)

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

(** Lambda HOL *)
module type LHOL = sig
  val element : T.sym
  val set : T.sym
  val proof : T.sym
  val forall : T.sym
  val propositions : T.sym
  val o : T.sym
  val implication : T.sym
  val arrow : T.sym
end

let mklhol (map : mapping) (sig_st : Sig_state.t) : (module LHOL) =
  let s (name : string) : T.sym =
    try fsym sig_st (List.assoc name map)
    with Not_found ->
      failwith (Format.sprintf "Symbol \"%s\" not found in mapping" name)
  in
  (module struct
    let set = s "Set"
    let element = s "Element"
    let propositions = s "Propositions"
    let proof = s "Proof"
    let forall = s "forall"
    let o = s "o"
    let implication = s "implication"
    let arrow = s "arrow"
  end)

(** PVS-Cert *)
module type PREDICATE_SUBTYPING = sig
  val subset : T.sym
  val pair : T.sym
  val value : T.sym
  val proof : T.sym
  val symbols : T.sym list
end

let mkpredicate_subtyping (map : mapping) (sig_st : Sig_state.t) :
    (module PREDICATE_SUBTYPING) =
  let s (name : string) : T.sym =
    try fsym sig_st (List.assoc name map)
    with Not_found ->
      failwith (Format.sprintf "Symbol \"%s\" not found in mapping" name)
  in
  (module struct
    let subset = s "subset"
    let pair = s "pair"
    let value = s "value"
    let proof = s "proof"
    let symbols = [ subset; pair; value; proof ]
  end)

module type PCERT = sig
  include LHOL include PREDICATE_SUBTYPING
end

let mkpcert (pcertmap : mapping) (sig_st : Sig_state.t) : (module PCERT) =
  let (module Lhol) = mklhol pcertmap sig_st
  and (module Prst) = mkpredicate_subtyping pcertmap sig_st in
  (module struct include Lhol include Prst end)

(** Logical connectives *)
module type CONNECTORS = sig
  val truth : T.sym
  val falsity : T.sym
  val implication : T.sym
  val negation : T.sym
  val conjunction : T.sym
  val disjunction : T.sym
  val existential : T.sym
  val universal : T.sym
end

let mkconnectors (map : (string * string) list) (sig_st : Sig_state.t) :
    (module CONNECTORS) =
  let s (name : string) : T.sym =
    try fsym sig_st (List.assoc name map)
    with Not_found ->
      failwith (Format.sprintf "Symbol \"%s\" not found in mapping" name)
  in
  (module struct
    let truth = s "truth"
    let falsity = s "falsity"
    let negation = s "negation"
    let implication = s "implication"
    let conjunction = s "conjunction"
    let disjunction = s "disjunction"
    let existential = s "existential"
    let universal = s "universal"
  end)
