ROOT     = pipe
CMDLINER = true

.PHONY: _tests
_tests:
	(cd examples && ${MAKE})

tests: _tests

.include "../dune-exe.mk"
.include "../tools.mk"
