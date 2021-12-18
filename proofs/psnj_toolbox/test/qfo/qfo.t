  $ echo 'symbol false : ∀ {prop} (λ p, p);' | psnj qfo -c . -l load.lp -m 'rule @∀ ↪ all;'
  symbol false: all prop (λ p: Prop, p);

  $ psnj qfo -c . -l load.lp --meta-load rules.lp < false.lp
  symbol false: all prop (λ p: El prop, p);
  symbol true: all prop (λ p: El prop, imp p p);

  $ echo 'symbol tt: ∀ {prop} (λ p: El prop, p ∧ (λ _, p));' | psnj qfo -c . -l load.lp --meta-load rules.lp
  symbol tt: all prop (λ p: El prop, conj p p);

  $ echo 'symbol tt: ∃ {prop} (λ p: El prop, p);' | psnj qfo -c . -l load.lp --meta-load rules.lp
  symbol tt: ex prop (λ p: El prop, p);

  $ psnj qfo -c . -l load.lp -e 'require open qfo.spec.withsymb;' --meta-load rules.lp < withsymb_thms.lp
  symbol trivial: imp qfo.spec.withsymb.P qfo.spec.withsymb.P;
