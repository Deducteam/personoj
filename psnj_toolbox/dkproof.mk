DOPTH   ?= dopth
JQ      ?= jq
PERL    ?= perl
PROVEIT ?=

.SUFFIXES: .pvs .log .json .lp .dep

.pvs.log:
	${PROVEIT} --traces -l ${.IMPSRC}

.log.json:
	${PERL} -ne 'print if /^\{.*\}$$/' < ${.IMPSRC} > ${.TARGET}

.json.lp:
	${JQ} -r '"symbol {|" + .name + "!" + (.incr | tostring) + "|}: " + .dk + ";"' < ${.IMPSRC} > ${.TARGET}

.json.dep:
	${JQ} -r '(.name + "!" + (.incr | tostring)), .path' < ${.IMPSRC} | ${DOPTH} > \
	${.TARGET}

