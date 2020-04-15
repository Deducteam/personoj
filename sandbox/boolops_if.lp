require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.bool_pvs

theorem trivial (P: η bool): ε (P ⊃ (λ_, P))
proof
  assume P
  refine λ_ _ f, f
qed
