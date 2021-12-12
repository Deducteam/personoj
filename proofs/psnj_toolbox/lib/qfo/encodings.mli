(** Define several encodings and functions to load them from signature
    states.

    An encoding is represented by a record of symbols. Having these
    symbols is required when we want to create terms in this encoding. To
    destruct on an encoding, having the name is enough. For each fragment
    [F], there is a function [mkf] that takes a signature state and
    extracts the symbol that are part of [F].  *)

open Core

type mapping = (string * string) list
(** A mapping [(name,sym)] indicates that record element [name] is to
    be mapped to symbol [sym] that will be found in a signature.*)

type predicate_subtyping = { subset : Term.sym }
(** The symbols used to encode predicate subtyping. *)

val mkpredicate_subtyping : mapping -> Sig_state.t -> predicate_subtyping

type connectives = {
  truth : Term.sym;
  falsity : Term.sym;
  implication : Term.sym;
  negation : Term.sym;
  conjunction : Term.sym;
  disjunction : Term.sym;
  existential : Term.sym;
  universal : Term.sym;
}
(** Logical connectives. They may be dependent or non-dependent. *)

val mkconnectives : mapping -> Sig_state.t -> connectives
