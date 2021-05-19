require open personoj.lhol
             personoj.pvs_cert
             personoj.logical
             personoj.equality;

constant symbol {|!Number!|}: Set;

constant symbol zero: El {|!Number!|};
constant symbol succ: El ({|!Number!|} ~> {|!Number!|});

builtin "0" ≔ zero;
builtin "+1" ≔ succ;

// In PVS, the axipms 1 ≠ 2, 1≠3, ... are built in
// Here we use the decidable equality
rule Prf (succ $n = succ $m) ↪ Prf ($n = $m)
with Prf (zero = succ _) ↪ Prf false
with Prf (zero = zero) ↪ Prf true;

// Define strings as list of numbers
constant symbol {|!String!|}: Set;
constant symbol str_empty: El {|!String!|};
constant symbol str_cons: El {|!Number!|} → El {|!String!|} → El {|!String!|};

symbol ∃ {T: Set} (P: El T → El prop) ≔ ¬ (∀ (λ x, ¬ (P x)));

symbol propositional_extensionality:
  Prf (∀ {prop} (λ p, (∀ {prop} (λ q, (iff p q) ⊃ (λ _, eq {prop} p q)))));
