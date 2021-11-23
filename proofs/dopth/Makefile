DUNE ?= dune

EXE  = psnj-dopth
MAN1 = psnj-dopth.1

_build/default/dopth.exe: dopth.ml
	${DUNE} build

psnj-dopth: _build/default/dopth.exe
	@cp -f _build/default/dopth.exe $@

.PHONY: tests
tests: _build/default/dopth.exe
	@${DUNE} runtest

.include "../tools.mk"
