/// WIP
require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert

// Non dependent tuples
constant symbol tuple_t: Set → Set → Set
constant symbol tuple {t} {u}: El t → El u → El (tuple_t t u)
symbol p1 {t} {u}: El (tuple_t t u) → El t
symbol p2 {t} {u}: El (tuple_t t u) → El u
rule p1 (tuple $m _) ↪ $m
rule p2 (tuple _ $n) ↪ $n

constant symbol set_t: Set
constant symbol set_nil: El set_t
constant symbol set_cons: Set → El set_t → El set_t
// TODO rewrite rules that transform (t, u, v) ~> w in t ~> u ~> v ~> w

constant symbol TypeList: TYPE
constant symbol type_nil: TypeList
constant symbol type_cons: Set → TypeList → TypeList
symbol type_cdr: TypeList → TypeList
rule type_cdr (type_cons _ $tl) ↪ $tl
symbol type_car: TypeList → Set
rule type_car (type_cons $hd _) ↪ $hd

constant symbol Tuple: TypeList → TYPE
constant symbol tuple_nil: Tuple type_nil
constant symbol tuple_cons (t: Set) (_: El t) (tl: TypeList) (_: Tuple tl):
  Tuple (type_cons t tl)

symbol tuple_car (tl: TypeList): Tuple tl → El (type_car tl)
rule tuple_car (type_cons $t _) (tuple_cons $t $x _) ↪ $x
symbol tuple_cdr (tl: TypeList): Tuple tl → Tuple (type_cdr tl)
rule tuple_cdr _ (tuple_cons _ _ _ $tup) ↪ $tup
