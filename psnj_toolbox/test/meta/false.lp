symbol false : ∀ [prop] (λ p: El prop, p);
symbol true : ∀ [prop] (λ p: El prop, p ⇒ (λ _: Prf p, p));
