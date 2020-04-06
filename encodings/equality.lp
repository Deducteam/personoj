require open personoj.encodings.lhol

symbol equal {T: Set}: η T ⇒ η T ⇒ Bool
set infix left 6 "=" ≔ equal
set builtin "T" ≔ η
set builtin "P" ≔ ε
set builtin "eq" ≔ equal
