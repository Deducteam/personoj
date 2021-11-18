PREFIX ?= /usr/local

_EXE  = dopth
_EXE += psnj-split

all: bin/dopth man

bin/dopth:
	@(cd dopth && ${MAKE})

install: ${_EXE:S!^!bin/!} man
.for e in ${_EXE}
	cp bin/$e ${PREFIX}/bin/$e
	cp man/${e}.1 ${PREFIX}/man/man1/${e}.1
.endfor

uninstall:
.for e in ${_EXE}
	rm -f ${PREFIX}/bin/$e
	rm -f ${PREFIX}/man/man1/${e}.1
.endfor

man:
	@(cd man && ${MAKE})

.PHONY: man bin/dopth install uninstall
