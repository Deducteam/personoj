DUNE ?= dune

all: ../bin/dopth

_build/default/dopth.exe: dopth.ml
	${DUNE} build

../bin/dopth: _build/default/dopth.exe
	cp -f _build/default/dopth.exe ../bin/dopth
