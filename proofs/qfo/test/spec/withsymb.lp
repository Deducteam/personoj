require open lpvs.lhol;
symbol P: Prop;
symbol trivial: Prf (P ⇒ (λ _: Prf P, P));
