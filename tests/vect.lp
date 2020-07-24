require open personoj.encodings.lhol
require open personoj.encodings.pvs_cert
require open personoj.encodings.deptype
require open personoj.encodings.prenex

symbol nat: θ {|set|}
symbol vect: ϕ (∀K (λ_, scheme_k (nat *> {|set|})))
assert vect: Set → El nat → Set
