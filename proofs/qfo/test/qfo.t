  $ psnj-qfo --map-dir=lpvs:encoding qfo.json < false.lp
  symbol false: @∀ prop (λ p: El prop, p);
  symbol true: @∀ prop (λ p: El prop, imp p p);

  $ psnj-qfo --map-dir=lpvs:encoding --map-dir=spec:spec -e 'require open spec.withsymb;' qfo.json < withsymb_thms.lp
  symbol trivial: imp spec.withsymb.P spec.withsymb.P;
