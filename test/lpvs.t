  $ lpvs --lib-root encoding/ false.lp
  symbol false: ∀ prop (\p: prop. p);
  symbol true: ∀ prop (\p: prop. ⇒ p (\_: p. p));
  fof(false, ! [p] : p).
  fof(true, ! [p] : p => p).
