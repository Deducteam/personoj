LP = lambdapi
LP_SRC  != find encodings prelude -type f -name "*.lp"
LP_OBJ = ${LP_SRC:S/.lp$/.lpo/}

.SUFFIXES: .lpo .lp

all: ${LP_OBJ}

.lp.lpo:
	${LP} check --gen-obj $<

.PHONY: install
install: ${LP_OBJ} lambdapi.pkg
	${LP} install lambdapi.pkg ${LP_OBJ} ${LP_SRC}

.PHONY: uninstall
uninstall:
	${LP} uninstall lambdapi.pkg

.PHONY: clean
clean:
	rm -f ${LP_OBJ}
