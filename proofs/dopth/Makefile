DUNE ?= dune
A2X  ?= a2x

all: ../bin/dopth

_build/default/dopth.exe: dopth.ml
	${DUNE} build

../bin/dopth: _build/default/dopth.exe
	cp -f _build/default/dopth.exe ../bin/dopth

man: dopth.asciidoc
	${A2X} -f manpage -d manpage dopth.asciidoc

.PHONY: ../bin/dopth
