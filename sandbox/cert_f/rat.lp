require open encodings.cert_f prelude.cert_f.booleans
prelude.cert_f.equalities prelude.cert_f.notequal
prelude.cert_f.naturalnumbers prelude.cert_f.int
require adlib.cert_f.nat as N

constant symbol rat : Univ Type
constant symbol zero : Term rat

symbol frac : Term nat ⇒ Term N.nznat ⇒ Term rat

symbol times : @Term Type rat ⇒ @Term Type rat ⇒ @Term Type rat

rule times (frac &a &b) (frac &c &d) →
  frac
   (N.times &a &c)
   (N.nznatc
    (N.times (fst N.not_zero &b) (fst N.not_zero &d))
    (N.prod_not_zero
     (fst N.not_zero &b)
     (fst N.not_zero &d)
     (snd N.not_zero &b)
     (snd N.not_zero &d)))


// rule times (frac &a &b) (frac &c &d) →
//   let bv  ≔ @fst nat N.not_zero &b in
//   let bpr ≔ @snd nat N.not_zero &b in
//   let dv  ≔ @fst nat N.not_zero &d in
//   let dpr ≔ @snd nat N.not_zero &d in
//   frac
//    (N.times &a &c)
//    (N.nznatc (N.times bv dv) (N.prod_not_zero bv dv bpr dpr))
