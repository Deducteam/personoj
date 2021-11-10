symbol false : Prf (∀ {prop} (λ p: El prop, p));
symbol true : Prf (∀ {prop} (λ p: El prop, p ⇒ (λ _: Prf p, p)));
