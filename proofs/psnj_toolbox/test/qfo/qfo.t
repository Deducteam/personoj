  $ psnj qfo -c . qfo.json < false.lp
  symbol false: all prop (λ p: El prop, p);
  symbol true: all prop (λ p: El prop, imp p p);

  $ echo 'symbol tt: ∀ {prop} (λ p: El prop, p ∧ (λ _, p));' | psnj qfo -c . qfo.json
  symbol tt: all prop (λ p: El prop, conj p p);

  $ echo 'symbol tt: ∃ {prop} (λ p: El prop, p);' | psnj qfo -c . qfo.json
  symbol tt: ex prop (λ p: El prop, p);

  $ psnj qfo -c . -e 'require open qfo.spec.withsymb;' qfo.json < withsymb_thms.lp
  symbol trivial: imp qfo.spec.withsymb.P qfo.spec.withsymb.P;
