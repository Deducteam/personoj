# -*- mode: Makefile; tab-width: 4; -*-
# ex:ts=4 sw=4 filetype=make:

LP_SRC   ?=
LP_OBJ   ?= ${LP_SRC:S/.lp$/.lpo/}
LP_FLAGS ?= -v0 -w --gen-obj

all: ${LP_OBJ}

.SUFFIXES: .lp .lpo

.lp.lpo:
	${LP} check ${LP_FLAGS} ${.IMPSRC}

.PHONY: install
install: lambdapi.pkg ${LP_SRC} ${LP_OBJ}
	${LP} install ${LP_SRC} ${LP_OBJ}

.PHONY: uninstall
uninstall:
	${LP} uninstall

.PHONY: clean
clean:
	find . -name '*.lpo' -exec rm -f {} +
