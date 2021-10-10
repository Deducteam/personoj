#-*- mode: Makefile; tab-width: 4; -*-
# ex:ts=4 sw=4 filetype=make:

# lambdapi check command
LPC ?=

.SUFFIXES: .lp .lpo

.lp.lpo:
	${LPC} -v0 -w --gen-obj ${.IMPSRC}

.PHONY: clean
clean:
	rm -rf *.lp *.lpo *.lisp
