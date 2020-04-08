require open personoj.encodings.lhol

symbol eq {T: Set}: η T ⇒ η T ⇒ Bool
set infix left 6 "=" ≔ eq
set builtin "eq" ≔ eq
