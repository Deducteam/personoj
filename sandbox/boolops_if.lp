require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.boolops_if

theorem trivial (P: η bool): ε (P ⊃ (λ_, P))
proof
  assume P
  refine λ_ _ f, f
qed
