  $ psnj-qfo --map-dir=lpvs:encoding qfo.json < false.lp
  symbol false: Prf (@∀ prop (λ p: El prop, p));
  symbol true: Prf (@∀ prop (λ p: El prop, imp p p));

  $ psnj-qfo --map-dir=lpvs:encoding --map-dir=spec:spec qfo.json < withsymb_thms.lp
  symbol trivial: Prf (imp spec.withsymb.P spec.withsymb.P);
