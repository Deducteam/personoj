require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert
require personoj.encodings.lazy as L

symbol T: Set
symbol t: η T
symbol f: η (T ~> T)
symbol S: Set
symbol s: η S

set flag "print_implicits" on
definition qu_ko ≔ L.app (L.quote f) (L.quote s)
compute L.unquote qu_ko

definition qu_ok ≔ L.app (L.quote f) (L.quote t)
compute L.unquote qu_ok
