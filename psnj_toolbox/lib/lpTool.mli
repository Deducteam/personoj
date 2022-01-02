open Core

val rewrite_with :
  Sign.t -> (Term.sym * Term.rule) list -> Term.term -> Term.term
(** [rewrite_with sign rs t] rewrites term [t] (up to snf) using only the
    rewrite rules that are in [rs] on top of signature [sign]. The
    command is pure. *)
