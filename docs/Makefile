all: main.html

.SUFFIXES: .adoc .html

.adoc.html:
	asciidoc $<

.PHONY: clobber
clobber:
	rm -f *.html
