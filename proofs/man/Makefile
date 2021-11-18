A2X ?= a2x

.SUFFIXES: .1 .asciidoc

all: dopth.1 psnj-split.1

.asciidoc.1:
	${A2X} -f manpage -d manpage $<

clobber:
	rm -f *.1

