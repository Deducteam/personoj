require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require personoj.encodings.prenex as P

symbol eq: P.χ (P.forallS (λT: ϕ {|set|}, P.∇ (T ~> T ~> bool)))
