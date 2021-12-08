  $ psnj-qfo --map-dir=qfo:encoding qfo.json < false.lp
  symbol false: @∀ prop (λ p: El prop, p);
  symbol true: @∀ prop (λ p: El prop, imp p p);

  $ echo 'symbol tt: ∀ {prop} (λ p: El prop, p ∧ (λ _, p));' | psnj-qfo --map-dir=qfo:encoding qfo.json
  symbol tt: @∀ prop (λ p: El prop, conj p p);

  $ psnj-qfo --map-dir=qfo:encoding --map-dir=spec:spec -e 'require open spec.withsymb;' qfo.json < withsymb_thms.lp
  symbol trivial: imp spec.withsymb.P spec.withsymb.P;
