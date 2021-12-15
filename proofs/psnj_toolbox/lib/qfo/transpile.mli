open Core
module T = Term
module LibT = LibTerm
module E = Encodings

val translate_term :
  ps:E.predicate_subtyping ->
  pvs_c:E.connectives ->
  prop_c:E.connectives ->
  T.term ->
  (T.term, T.term * string) Result.t
(** [translate_term ~ps ~pvs_c ~prop_c t] translates term [t] from the
    encoding of PVS-Cert defined by symbols in [ps] and [pvs_c] to Simple
    Type Theory with connectives in [prop_c]. *)
