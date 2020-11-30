require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.pairs
require open personoj.encodings.logical

symbol peq {t: Set} (_: El (σ (μ t) (μ t))): Bool
definition pneq  {t} m ≔ ¬ (@peq t m)
