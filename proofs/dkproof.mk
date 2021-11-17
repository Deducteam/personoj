KJSON   ?= keep-json
JSON2LP ?= json2lp
DOPTH   ?= dopth
JQ      ?= jq

.SUFFIXES: .log .json .lp .dep

.log.json:
	${KJSON} < ${.IMPSRC} > ${.TARGET}

.json.lp:
	${JSON2LP} < ${.IMPSRC} > ${.TARGET}

.json.dep:
	${JQ} -r '(.name + "!" + (.incr | tostring)), .path' < ${.IMPSRC} | ${DOPTH} > \
	${.TARGET}

