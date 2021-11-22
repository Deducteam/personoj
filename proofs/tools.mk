PREFIX ?= /usr/local
A2X    ?= a2x
EXE    ?=
MAN1   ?=
MAN5   ?=

.SUFFIXES: .1 .5 .asciidoc

.asciidoc.1:
	${A2X} -f manpage -d manpage $<

.asciidoc.5:
	${A2X} -f manpage -d manpage $<

install: ${EXE} ${MAN}
	cp -f ${EXE} ${PREFIX}/bin/
	cp -f ${MAN1} ${PREFIX}/man/man1/
	cp -f ${MAN5} ${PREFIX}/man/man5

uninstall:
.for e in ${EXE}
	rm -f ${PREFIX}/bin/$e
.endfor
.for m in ${MAN1}
	rm -f ${PREFIX}/man/man1/$m
.endfor
.for m in ${MAN5}
	rm -f ${PREFIX}/man/man5/$m
.endfor

man: ${MAN1} ${MAN5}

all: ${EXE} ${MAN1} ${MAN5}

.MAIN: all
.PHONY: man all install uninstall
