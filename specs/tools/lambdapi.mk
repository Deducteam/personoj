LP       ?= lambdapi
LP_SRC   ?=

all: ${LP_OBJ}

.PHONY: clean
clean:
	rm -rf *.lpo *.lp *.lisp

.include "../../../mk/lambdapi.mk"
