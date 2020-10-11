require open personoj.encodings.lhol
require open personoj.encodings.prenex
require open personoj.encodings.bool_hol

// It may be generalisable to dependent propositions
theorem and_intro:
  Prf
  (∀ {bool} (λa,
   ∀ {bool} (λb,
   a ⊃ (λ_, b ⊃ (λ_, a ∧ (λ_, b))))))
proof
  assume A B h0 h1 f
  refine f h0 h1
qed

theorem and_elim_1 a b (_: Prf (a ∧ (λ_, b))): Prf a
proof
admit
