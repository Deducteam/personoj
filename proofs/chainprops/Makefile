PREFIX ?= /usr/local/
DUNE ?= dune

all: psnj-chainprops psnj-chainprops.1

_build/default/chainprops.exe: chainprops.ml
	@${DUNE} build

psnj-chainprops: _build/default/chainprops.exe
	@ln -sf _build/default/chainprops.exe $@

tests:
	@${DUNE} runtest

psnj-chainprops.1: psnj-chainprops
	@./psnj-chainprops --help=groff > $@

man: psnj-chainprops.1

install: all
	cp -f psnj-chainprops ${PREFIX}/bin/
	cp -f psnj-chainprops.1 ${PREFIX}/man/man1/

.PHONY: install all man
