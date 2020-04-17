require open
  personoj.encodings.lhol
  personoj.encodings.pvs_cert
  personoj.encodings.if
  personoj.encodings.booleans
require personoj.encodings.deferred as Deferred
require open personoj.encodings.subtype

constant symbol T: Set
symbol t: η T
symbol f: η (T ~> T)
constant symbol S: Set
symbol s: η S

set flag "print_implicits" on
definition qu_ko ≔ Deferred.app (Deferred.quote f) (Deferred.quote s)
compute Deferred.unquote qu_ko

definition qu_ok ≔ Deferred.app (Deferred.quote f) (Deferred.quote t)
compute Deferred.unquote qu_ok

compute Deferred.quote {T ~> T} (λx:η T, f x)

symbol lor: Deferred.t bool → Deferred.t bool → Bool

compute let f' ≔ Deferred.quote f in
        let t' ≔ Deferred.quote t in
        let s' ≔ Deferred.quote s in
        let refl_t' ≔ Deferred.quote_p (S_Refl T) in
  lif T refl_t' refl_t' true (Deferred.app f' t') (Deferred.app f' s')
