require open personoj.lhol personoj.nat personoj.extra.fixpoints personoj.logical;

symbol mul : Nat → Nat → Nat;
symbol pred : Nat → Nat;
symbol pred-lower: Prf (∀ {nat/o}(λ n, (lenat (pred n) n)));

// Definition of the factorial
symbol fac n ≔
  fix nat/o nat/o (λ x, x)
      (λ n fac,
       if (eqnat n zero)
          (λ _, zero)
          (λ _, (mul n (fac (pred n) (pred-lower n))))) n;
