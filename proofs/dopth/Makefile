DUNE ?= dune
PREFIX ?= /usr/local
all: psnj-dopth psnj-dopth.1

.include "../asciidoc.mk"

_build/default/dopth.exe: dopth.ml
	${DUNE} build

psnj-dopth: _build/default/dopth.exe
	@ln -f _build/default/dopth.exe $@

man: psnj-dopth.1

install: all
	@cp -f psnj-dopth ${PREFIX}/bin/
	@cp -f psnj-dopth.1 ${PREFIX}/man/man1/

.PHONY: man install all
