require open qfo.lhol;
symbol P: Prop;
symbol trivial: Prf (P ⇒ (λ _: Prf P, P));
