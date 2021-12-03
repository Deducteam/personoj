PREFIX ?= /usr/local

_EXE = _build/default/bin/main.exe

all: psnj-qfo psnj-qfo.1

${_EXE}:
	@opam exec -- dune build
	
psnj-qfo: ${_EXE}
	@ln -sf ${_EXE} $@

psnj-qfo.1: psnj-qfo
	@./psnj-qfo --help=groff > $@

install: psnj-qfo psnj-qfo.1
	cp -Lf psnj-qfo ${PREFIX}/bin/
	cp -Lf psnj-qfo.1 ${PREFIX}/man/man1/

uninstall:
	rm -f ${PREFIX}/bin/psnj-qfo
	rm -f ${PREFIX}/man/man1/psnj-qfo.1

clean:
	rm -f psnj-qfo psnj-qfo.1
	opam exec -- dune clean

tests:
	@opam exec -- dune runtest
	@echo 'Success'

.PHONY: install uninstall clean tests
