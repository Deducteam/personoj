EXE = psnj-chainprops
MAN1 = psnj-chainprops.1

DUNE ?= dune

_build/default/chainprops.exe: chainprops.ml
	@${DUNE} build

psnj-chainprops: _build/default/chainprops.exe
	@cp -f _build/default/chainprops.exe $@

psnj-chainprops.1: psnj-chainprops
	@./psnj-chainprops --help=groff > $@

tests:
	@${DUNE} runtest

.include "../tools.mk"
