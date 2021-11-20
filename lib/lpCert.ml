(** Manipulating PVS-Cert encoded in Lambdapi. *)

open Core
module T = Term
module LibT = LibTerm
module B = Bindlib
module E = Encodings

module PropOfPcert (Pc : E.PCERT) (Prop : E.KPROPOSITIONAL) = struct
  exception CannotTranslate of T.term

  (** Could be replaced by proper symbols (just like implication). *)
  let bin_cons = [ ("∧", Prop.conj); ("∨", Prop.disj) ]

  let una_cons = [ ("¬", Prop.not) ]
  let nul_cons = [ ("true", Prop.top); ("false", Prop.bot) ]

  let rec f (t : T.term) : T.term =
    match T.get_args t with
    | (Wild | TRef _ | Meta _ | TEnv _ | Plac _ | Patt _), _ ->
        assert false (* should not appear in typechecked terms *)
    | Symb i, [ l; r ] when i == Pc.impd ->
        let r =
          match T.unfold r with
          | Abst (_, b) ->
              if not (B.binder_constant b) then (
                Format.eprintf "Binder not constant";
                raise (CannotTranslate t));
              snd (B.unbind b)
          | _ -> assert false
        in
        T.add_args (T.mk_Symb Prop.imp) [ f l; f r ]
    | Symb i, [ l; r ] when List.mem_assoc i.T.sym_name bin_cons ->
        let sym = T.mk_Symb (List.assoc i.T.sym_name bin_cons) in
        T.add_args sym [ f l; f r ]
    | Symb i, [ e ] when List.mem_assoc i.T.sym_name una_cons ->
        let sym = T.mk_Symb (List.assoc i.T.sym_name una_cons) in
        T.add_args sym [ f e ]
    | Symb s, _ when s == Pc.pair || s == Pc.fst || s == Pc.snd ->
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
