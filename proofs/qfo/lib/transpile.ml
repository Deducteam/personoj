(** Transpile terms from PVS-Cert to STT. Connectives are also transformed. *)

open Core
module T = Term
module LibT = LibTerm
module B = Bindlib
module E = Encodings

exception CannotTranslate of T.term
(** Raised when a non translatable symbol is found. *)

(** [translate_term ~ps ~pvs_c ~prop_c t] translates term [t]. Term
    [t] may contain symbols from [ps] and [pvs_c]. Connectives from
    [pvs_c] are translated by connectives of [prop_c]. *)
let translate_term ~(ps : E.predicate_subtyping) ~(pvs_c : E.connectives)
    ~(prop_c : E.connectives) (t : T.term) : T.term =
  let binary =
    [
      (pvs_c.implication, prop_c.implication)
    ; (pvs_c.conjunction, prop_c.conjunction)
    ; (pvs_c.disjunction, prop_c.disjunction)
    ]
  in
  let rec f (t : T.term) =
    match T.get_args t with
    | (Wild | TRef _ | Meta _ | TEnv _ | Plac _ | Patt _), _ ->
        assert false (* should not appear in typechecked terms *)
    (* Mapping constants *)
    | Symb i, [] when i == pvs_c.truth -> T.mk_Symb prop_c.truth
    | Symb i, [] when i == pvs_c.falsity -> T.mk_Symb prop_c.falsity
    (* Mapping unary operators *)
    | Symb i, [ e ] when i == pvs_c.negation ->
        T.add_args (T.mk_Symb prop_c.negation) [ f e ]
    (* Mapping binary operators *)
    | Symb i, [ l; r ] when List.(memq i (map fst binary)) ->
        let r =
          match T.unfold r with
          | Abst (_, b) ->
              if not (B.binder_constant b) then (
                Format.eprintf "Binder not constant";
                raise (CannotTranslate t));
              snd (B.unbind b)
          | _ -> assert false
        in
        let new_sym = List.assq i binary in
        T.add_args (T.mk_Symb new_sym) [ f l; f r ]
    (* Mapping quantifiers *)
    | Symb s, [ a; t ] when s == pvs_c.existential ->
        T.add_args (T.mk_Symb prop_c.existential) [ f a; f t ]
    | Symb s, [ a; t ] when s == pvs_c.universal ->
        T.add_args (T.mk_Symb prop_c.universal) [ f a; f t ]
    | Symb s, _ when s == ps.subset ->
        Format.eprintf
          "PVS-Cert pair, fst and snd cannot be translated to propositional \
           logic (yet)@.";
        raise (CannotTranslate t)
    | u, args ->
        let hd =
          match T.unfold u with
          | Abst (a, b) ->
              let x, b = B.unbind b in
              let b = f b in
              let b = B.bind_var x (T.lift b) in
              T.mk_Abst (f a, B.unbox b)
          | Prod (a, b) ->
              let x, b = B.unbind b in
              let b = f b in
              let b = B.bind_var x (T.lift b) in
              T.mk_Prod (f a, B.unbox b)
          | Vari _ | Symb _ -> u
          | _ -> assert false
        in
        T.add_args hd (List.map f args)
  in
  f t
