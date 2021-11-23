EXE = psnj-appaxiom
MAN1 = psnj-appaxiom.1

DUNE ?= dune

_build/default/appaxiom.exe: appaxiom.ml
	@${DUNE} build

psnj-appaxiom: _build/default/appaxiom.exe
	@cp -f _build/default/appaxiom.exe $@

psnj-appaxiom.1: psnj-appaxiom
	@./psnj-appaxiom --help=groff > $@

tests:
	@${DUNE} runtest

.include "../tools.mk"
