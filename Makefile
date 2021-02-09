LP = lambdapi
LP_FLAGS = -v0 -w
LP_SRC  != find encodings paper -type f -name "*.lp"
LP_OBJ = ${LP_SRC:S/.lp$/.lpo/}

.SUFFIXES: .lpo .lp

all: ${LP_OBJ}

.lp.lpo:
	${LP} check ${LP_FLAGS} --gen-obj $<

.PHONY: install
install: lambdapi.pkg
	${LP} install ${LP_FLAGS} lambdapi.pkg ${LP_SRC}

.PHONY: uninstall
uninstall:
	${LP} uninstall ${LP_FLAGS} lambdapi.pkg

.PHONY: clean
clean:
	rm -f ${LP_OBJ}
