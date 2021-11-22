A2X ?= a2x

.SUFFIXES: .1 .asciidoc

.asciidoc.1:
	${A2X} -f manpage -d manpage $<
