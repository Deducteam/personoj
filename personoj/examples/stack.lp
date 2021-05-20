require open personoj.lhol personoj.pvs_cert personoj.logical personoj.eqtup;
require open personoj.coercions;

// Get currified infix equality
require open personoj.extra.equality;

constant symbol stack {_: Set}: Set;
constant symbol empty {t: Set}: El (stack {t});

symbol nonempty_stack? {t: Set} (s: El (stack {t})) ≔ s ≠ empty {t};
symbol nonempty_stack {t: Set} ≔ psub (nonempty_stack? {t});

constant symbol push {t: Set}: El t → El (stack {t}) → El (nonempty_stack {t});
constant symbol pop {t: Set}: El (nonempty_stack {t}) → El (stack {t});
constant symbol top {t: Set}: El (nonempty_stack {t}) → El t;

symbol push_top_pop {t: Set}:
  Prf (∀ (λ s: El (stack {t}),
          nonempty_stack? {t} s ⇒ (λ nes, push (top s) (pop s) = s)))
begin
  // Solve TCCs
  refine nes;
  refine nes;
  refine nes;
end;

constant symbol pop_push {t: Set}:
  Prf (∀ (λ s: El (stack {t}), ∀ (λ x: El t, pop (push x s) = s)));

constant symbol top_push {t: Set}:
  Prf (∀ (λ s: El (stack {t}), ∀ (λ x: El t, top (push x s) = x)));

opaque symbol pop2push2 {t: Set}:
  Prf (∀ (λ s: El (stack {t}),
          ∀ (λ x: El t,
            (∀ (λ y: El t, pop (pop (push x (push y s))) = s))))) ≔
begin
  admit;
end;
