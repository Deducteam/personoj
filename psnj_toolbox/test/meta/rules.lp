rule @∀ $a $e ↪ all $a $e;
rule @∃ $a $e ↪ ex $a $e;
rule $x ⇒ (λ _, $y[]) ↪ imp $x $y;
rule $x ∧ (λ _, $y[]) ↪ conj $x $y;
