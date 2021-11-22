all: psnj-chainprops

_build/default/chainprops.exe: chainprops.ml
	@dune build

psnj-chainprops: _build/default/chainprops.exe
	@ln -f _build/default/chainprops.exe psnj-chainprops

tests:
	@dune runtest

psnj-chainprops.1: psnj-chainprops
	@./psnj-chainprops --help=groff > psnj-chainprops.1

man: psnj-chainprops.1

.PHONY: man all
