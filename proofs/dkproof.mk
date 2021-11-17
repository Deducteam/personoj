KJSON   ?= keep-json
JSON2LP ?= json2lp
DOPTH   ?= dopth
JQ      ?= jq
PROVEIT ?=

.SUFFIXES: .pvs .log .json .lp .dep

.pvs.log:
	${PROVEIT} --traces -l ${.IMPSRC}

.log.json:
	${KJSON} < ${.IMPSRC} > ${.TARGET}

.json.lp:
	${JSON2LP} < ${.IMPSRC} > ${.TARGET}

.json.dep:
	${JQ} -r '(.name + "!" + (.incr | tostring)), .path' < ${.IMPSRC} | ${DOPTH} > \
	${.TARGET}

