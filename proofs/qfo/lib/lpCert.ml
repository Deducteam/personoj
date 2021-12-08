(** Manipulating PVS-Cert encoded in Lambdapi. *)

open Core
module T = Term
module LibT = LibTerm
module B = Bindlib
module E = Encodings

(** [PropOfPcert(Pc)(DepConn)(Prop)] takes the encoding of PVS-Cert
    [Pc], the dependent logical connectives [DepConn] and non dependent
    ones [Prop] and provide a translation function from PVS-Cert to kind
    of first order (substitute dependent connectors when they can be,
    remove predicate subtyping). *)
module PropOfPcert (Pc : E.PCERT) (DepConn : E.CONNECTORS) (Prop : E.CONNECTORS) : sig
  exception CannotTranslate of T.term

  val f : T.term -> T.term
end = struct
  exception CannotTranslate of T.term

  (** Map dependent binary connectives to non dependent ones. *)
  let binary =
    [
      (Pc.implication, Prop.implication)
    ; (DepConn.implication, Prop.implication)
    ; (DepConn.conjunction, Prop.conjunction)
    ; (DepConn.disjunction, Prop.disjunction)
    ]

  let rec f (t : T.term) : T.term =
    match T.get_args t with
    | (Wild | TRef _ | Meta _ | TEnv _ | Plac _ | Patt _), _ ->
        assert false (* should not appear in typechecked terms *)
    (* Mapping constants *)
    | Symb i, [] when i == DepConn.truth -> T.mk_Symb Prop.truth
    | Symb i, [] when i == DepConn.falsity -> T.mk_Symb Prop.falsity
    (* Mapping unary operators *)
    | Symb i, [ e ] when i == DepConn.negation ->
        T.add_args (T.mk_Symb Prop.negation) [ f e ]
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
    | Symb s, _ when List.memq s Pc.symbols ->
        Format.eprintf
          "PVS-Cert pair, fst and snd cannot be translated to propositional \
           logic@.";
        raise (CannotTranslate t)
    | u, args ->
        let args = List.map f args in
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
        T.add_args hd args
end
