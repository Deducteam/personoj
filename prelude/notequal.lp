require open pvs_core booleans equalities

// symbol neq : ∀ T : Type, eta T ⇒ eta T ⇒ eta bool
symbol neq (T:Type) (x:eta T) (y:eta T) : eps (bnot (eq T x y))
