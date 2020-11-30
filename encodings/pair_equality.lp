require open personoj.encodings.lhol
require open personoj.encodings.pairs
require open personoj.encodings.logical

symbol peq {t: Set} (_: El (σ t t)): Bool
definition pneq  {t: Set} (m: El (σ t t)) ≔ ¬ (@peq t m)
