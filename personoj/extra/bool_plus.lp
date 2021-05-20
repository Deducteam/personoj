require open personoj.lhol personoj.tuple personoj.logical personoj.eqtup;

// It may be generalisable to dependent propositions
opaque
symbol and_intro:
  Prf
  (∀ {prop} (λ a,
   ∀ {prop} (λ b,
   a ⇒ (λ _, b ⇒ (λ _, a ∧ (λ _, b)))))) ≔
begin
  assume A B h0 h1 f;
  refine f h0 h1;
end;

opaque
symbol and_elim_1 a b (_: Prf (a ∧ (λ _, b))): Prf a ≔
begin
admitted;

symbol propositional_extensionality:
  Prf (∀ (λ p: El prop,
          (∀ (λ q: El prop, (iff p q) ⇒ (λ _, eq (cons p q))))));
