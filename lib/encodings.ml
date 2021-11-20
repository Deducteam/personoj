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

let fsym (sig_st : Sig_state.t) (sym : string) : T.sym =
  find_sym sig_st ([], sym)

(** Lambda HOL *)
module type LHOL = sig
  val el : T.sym
  val set : T.sym
  val prf : T.sym
  val fa : T.sym
  val prop : T.sym
  val small_prop : T.sym
  val impd : T.sym
  val arrd : T.sym
  val arr : T.sym
end

let mklhol (sig_st : Sig_state.t) : (module LHOL) =
  let s : string -> T.sym = fsym sig_st in
  (module struct
    let el = s "El"
    let set = s "Set"
    let prf = s "Prf"
    let prop = s "Prop"
    let small_prop = s "prop"
    let fa = s "∀"
    let impd = s "⇒"
    let arrd = s "arrd"
    let arr = s "~>"
  end)

(** PVS-Cert *)
module type PREDICATE_SUBTYPING = sig
  val psub : T.sym val pair : T.sym val fst : T.sym val snd : T.sym
end

let mkpredicate_subtyping (sig_st : Sig_state.t) : (module PREDICATE_SUBTYPING) =
  let s : string -> T.sym = fsym sig_st in
  (module struct
    let psub = s "psub" let pair = s "pair" let fst = s "fst" let snd = s "snd"
  end)

module type PCERT = sig include LHOL include PREDICATE_SUBTYPING end
let mkpcert (sig_st: Sig_state.t): (module PCERT) =
  let (module Lhol) = mklhol sig_st 
  and (module Prst) = mkpredicate_subtyping sig_st in
  (module struct include Lhol include Prst end)

(** Constructive propositional calculus *)
module type KPROPOSITIONAL = sig
  val top : T.sym
  val bot : T.sym
  val imp : T.sym
  val not : T.sym
  val conj : T.sym
  val disj : T.sym
  val ex : T.sym
end

let mkkpropositional (sig_st : Sig_state.t) : (module KPROPOSITIONAL) =
  let s = fsym sig_st in
  (module struct
    let top = s "top"
    let bot = s "bot"
    let not = s "not"
    let imp = s "imp"
    let conj = s "conj"
    let disj = s "disj"
    let ex = s "ex"
  end)

module type LHOL_PROPOSITIONAL = sig include LHOL include KPROPOSITIONAL end
let mklhol_propositional (sig_st: Sig_state.t) : (module LHOL_PROPOSITIONAL) =
  let (module Lhol) = mklhol sig_st
  and (module Kprop) = mkkpropositional sig_st in
  (module struct include Lhol include Kprop end)
