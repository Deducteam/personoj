PREFIX = /usr/local/

bin:
	opam exec -- dune build

psnj: _build/install/default/bin/psnj
	ln -sf _build/install/default/bin/psnj .

install:
	cp -fL psnj ${PREFIX}/bin/psnj

.PHONY: bin
