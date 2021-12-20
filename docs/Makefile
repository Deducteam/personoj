A2X = a2x
all: main.html main.pdf

.SUFFIXES: .adoc .html .pdf

.adoc.html:
	${A2X} -f xhtml $<

.adoc.pdf:
	${A2X} -f pdf $<

.PHONY: clobber
clobber:
	rm -f *.html *.pdf
