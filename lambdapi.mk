LP       ?= lambdapi
LP_FLAGS ?= -v0 -w
LP_SRC   ?=
LP_OBJ   ?= ${LP_SRC:S/.lp$/.lpo/}

.SUFFIXES: .lpo .lp

all: ${LP_OBJ}

.lp.lpo:
	${LP} check ${LP_FLAGS} --gen-obj $<

.PHONY: install
install: lambdapi.pkg
	${LP} install ${LP_SRC} ${LP_OBJ}

.PHONY: uninstall
uninstall:
	${LP} uninstall

.PHONY: clean
clean:
	rm -f ${LP_OBJ}
