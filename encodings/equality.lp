require open personoj.encodings.lhol

symbol eq {T: Set}: El T → El T → Bool
set infix left 6 "=" ≔ eq
set builtin "eq" ≔ eq
