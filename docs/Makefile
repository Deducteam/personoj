all: main.html

.SUFFIXES: .adoc .html

.adoc.html:
	asciidoc -a mathjax $<

.PHONY: clobber
clobber:
	rm -f *.html
