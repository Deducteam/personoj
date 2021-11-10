  $ lpvs --lib-root encoding/ false.lp
  symbol false: ∀ prop (\p: prop. p);
  symbol true: ∀ prop (\p: prop. ⇒ p (\_: p. p));
  fof(false, ! [P] : P).
  fof(true, ! [P] : P => P).
