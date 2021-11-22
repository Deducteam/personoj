PREFIX ?= /usr/local

_EXE = psnj-split

install: ${_EXE:S!^!bin/!} man
.for e in ${_EXE}
	cp bin/$e ${PREFIX}/bin/$e
	cp man/${e}.1 ${PREFIX}/man/man1/${e}.1
.endfor
	(cd dopth && eval $(opam env) && ${MAKE} install)
	(cd chainprops && eval $(opam env) && ${MAKE} install)

uninstall:
.for e in ${_EXE}
	rm -f ${PREFIX}/bin/$e
	rm -f ${PREFIX}/man/man1/${e}.1
.endfor
	@(cd dopth && ${MAKE} uninstall)
	@(cd chainprops && ${MAKE} uninstall)

man:
	@(cd man && ${MAKE})
	@(cd dopth && ${MAKE} man)
	@(cd chainprops && ${MAKE} man)

.PHONY: man install uninstall
