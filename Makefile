.PHONY: install
install:
	${MAKE} -C encoding install
	@sed 's:PVSDKPATH:${PWD}:' pvs-load.lisp | cat

.PHONY: encoding
encoding:
	${MAKE} -C encoding

.PHONY: tests
tests:
	${MAKE} -C tests

.PHONY: clean
clean:
	find . -type f -name "*.lpo" -exec rm -f {} +
