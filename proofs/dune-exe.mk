# Dune targets for executables
# Include *before* tools.mk

DUNE     ?= dune
OPAM     ?= opam
ROOT     ?= # Name of the source file (without extension)
CMDLINER ?= # Set to something if cmdliner is used to generate documentation
EXE   = psnj-${ROOT}
MAN1  = psnj-${ROOT}.1

_build/default/${ROOT}.exe: ${ROOT}.ml
	@${OPAM} exec -- ${DUNE} build

psnj-${ROOT}: _build/default/${ROOT}.exe
	@cp -f _build/default/${ROOT}.exe $@

tests: _build/default/${ROOT}.exe
	@${OPAM} exec -- ${DUNE} runtest
	@echo 'Success'

.if ${CMDLINER}
${EXE}.1: ${EXE}
	@./${EXE} --help=groff > $@
.endif

clean:
	@${OPAM} exec -- ${DUNE} clean
	@rm -f ${EXE}
	@rm -f ${MAN1}

.PHONY: tests clean
