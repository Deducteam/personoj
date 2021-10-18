.PHONY: install
install:
	${MAKE} -C encoding install
	@echo "Add to ~/.pvs.lisp:"
	@echo "--------8<----------"
	cat tools/load-personoj.lisp
	@echo "(load-personoj)"
	@echo "--------8<----------"

.PHONY: encoding
encoding:
	${MAKE} -C encoding

.PHONY: tests
tests:
	${MAKE} -C tests

.PHONY: clean
clean:
	find . -type f -name "*.lpo" -exec rm -f {} +
