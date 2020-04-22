require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

// Dependent type
// FIXME: quantification on types must be restricted to a prenex form
set declared "ι"
symbol ι: Kind → TYPE
symbol dt_arr: Kind → Kind → Kind
set infix right 7 "*>" ≔ dt_arr
rule ι ({|set|} *> {|set|}) ↪ θ {|set|} → θ {|set|}
