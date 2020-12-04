require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require personoj.encodings.tuple as T
require open personoj.encodings.logical

symbol eq {t: Set} (_: El (T.t (μ t) (μ t))): Bool
definition neq {t} m ≔ ¬ (@eq t m)
