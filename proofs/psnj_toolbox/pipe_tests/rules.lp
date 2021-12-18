rule true ↪ top;
rule false ↪ bot;
rule ¬ $x ↪ not $x;
rule $x ∧ (λ _, $y[]) ↪ conj $x $y;
rule $x ∨ (λ _, $y[]) ↪ disj $x $y;
rule $x ⇒ (λ _, $y[]) ↪ imp $x $y;
rule @∀ ↪ all;
rule @∃ ↪ ex;
