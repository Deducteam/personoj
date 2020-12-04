require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require personoj.encodings.tuple as T
require open personoj.encodings.logical

symbol peq {t: Set} (_: El (T.t (μ t) (μ t))): Bool
definition pneq {t} m ≔ ¬ (@peq t m)
