require open personoj.lhol personoj.pvs_cert personoj.logical;
require open personoj.coercions;
require open personoj.alt.eq;

notation = infix 2;
notation != infix 2;

constant symbol stack {_: Set}: Set;
constant symbol empty {t: Set}: El (stack {t});

symbol nonempty_stack? {t: Set} (s: El (stack {t})) ≔ s != empty {t};
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

// A demonstration on how to use extended quantifiers
require open personoj.alt.quantifiers;
require personoj.alt.SetVec as SVec;
require open personoj.extra.arity-tools;

constant symbol pop_push {t: Set}:
  Prf (∀* (SVec.vec two (stack {t}) t) (λ s x, pop (push x s) = s));

constant symbol top_push {t: Set}:
  Prf (∀* (SVec.vec two (stack {t}) t) (λ s x, top (push x s) = x));

opaque symbol pop2push2 {t: Set}:
  Prf (∀* (SVec.vec three (stack {t}) t t)
          (λ s x y, pop (pop (push x (push y s))) = s)) ≔
begin admit; end;
