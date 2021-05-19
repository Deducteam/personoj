.PHONY: encoding
encoding:
	${MAKE} -C personoj

.PHONY: tests
tests:
	${MAKE} -C tests

.PHONY: clean
clean:
	find . -type f -name "*.lpo" -exec rm -f {} +
