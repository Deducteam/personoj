(** Transpile terms from PVS-Cert to STT. Connectives are also transformed. *)

open Core
module T = Term
module LibT = LibTerm
module B = Bindlib
module E = Encodings

let ( let* ) = Result.bind
let return (x : 'a) : ('a, _) result = Ok x
let fail (x : 'a) (msg : string) : (_, 'a * string) result = Error (x, msg)

let list (l : ('a, 'b) Result.t list) : ('a list, 'b) Result.t =
  let f e lr =
    let* e_ok = e in
    let* lr_ok = lr in
    return @@ (e_ok :: lr_ok)
  in
  List.fold_right f l (return [])

(** [translate_term ~ps ~pvs_c ~prop_c t] translates term [t]. Term
    [t] may contain symbols from [ps] and [pvs_c]. Connectives from
    [pvs_c] are translated by connectives of [prop_c]. *)
let translate_term ~(ps : E.predicate_subtyping) ~(pvs_c : E.connectives)
    ~(prop_c : E.connectives) (t : T.term) : (T.term, T.term * string) Result.t
    =
  let binary =
    [
      (pvs_c.implication, prop_c.implication);
      (pvs_c.conjunction, prop_c.conjunction);
      (pvs_c.disjunction, prop_c.disjunction);
    ]
  in
  let rec f (t : T.term) =
    match T.get_args t with
    | (Wild | TRef _ | Meta _ | TEnv _ | Plac _ | Patt _), _ ->
        assert false (* should not appear in typechecked terms *)
    (* Mapping constants *)
    | Symb i, [] when i == pvs_c.truth -> return @@ T.mk_Symb prop_c.truth
    | Symb i, [] when i == pvs_c.falsity -> return @@ T.mk_Symb prop_c.falsity
    (* Mapping unary operators *)
    | Symb i, [ e ] when i == pvs_c.negation ->
        let* e = f e in
        return @@ T.add_args (T.mk_Symb prop_c.negation) [ e ]
    (* Mapping binary operators *)
    | Symb i, [ l; r ] when List.(memq i (map fst binary)) ->
        let* r =
          match T.unfold r with
          | Abst (_, b) ->
              if not (B.binder_constant b) then fail t "Binder not constant"
              else f @@ snd (B.unbind b)
          | _ -> assert false
        in
        let* l = f l in
        let new_sym = List.assq i binary in
        return @@ T.add_args (T.mk_Symb new_sym) [ l; r ]
    (* Mapping quantifiers *)
    | Symb s, [ a; t ] when s == pvs_c.existential ->
        let* a = f a in
        let* t = f t in
        return @@ T.add_args (T.mk_Symb prop_c.existential) [ a; t ]
    | Symb s, [ a; t ] when s == pvs_c.universal ->
        let* a = f a in
        let* t = f t in
        return @@ T.add_args (T.mk_Symb prop_c.universal) [ a; t ]
    | Symb s, _ when s == ps.subset ->
        fail t
          "PVS-Cert pair, fst and snd cannot be translated to propositional \
           logic (yet)@."
    | u, args ->
        let* hd =
          match T.unfold u with
          | Abst (a, b) ->
              let x, b = B.unbind b in
              let* b = f b in
              let b = B.bind_var x (T.lift b) in
              let* a = f a in
              return @@ T.mk_Abst (a, B.unbox b)
          | Prod (a, b) ->
              let x, b = B.unbind b in
              let* b = f b in
              let b = B.bind_var x (T.lift b) in
              let* a = f a in
              return @@ T.mk_Prod (a, B.unbox b)
          | Vari _ | Symb _ -> return u
          | _ -> fail u "Cannot handle"
        in
        let* l = list @@ List.map f args in
        return @@ T.add_args hd l
  in
  f t
