  $ lpvs --lib-root encoding/ false.lp
  symbol false: ∀ prop (\p: prop. p);
  symbol true: ∀ prop (\p: prop. ⇒ p (\_: p. p));
  fof(false, ∀ prop (` [p] : p)).
  fof(true, ∀ prop (` [p] : ⇒ p (` [_] : p))).
