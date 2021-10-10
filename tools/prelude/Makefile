PVS2DK = ../pvs2dk.sh
LPC    = lambdapi check

THEORIES != grep -v -E '^-|\#' theories
PVSPATH   =

all: dummy ${THEORIES:C/$/.lpo/}

.PHONY: dummy
dummy:
	./mk_dummy.sh

translate: dummy ${THEORIES:C/$/.lp/}

# For each theory, translate it and apply a patch if it exists
.for th in ${THEORIES}
${th}.lp:
	PVSPATH=${PVSPATH} ${PVS2DK} -f ${PVSPATH}/lib/prelude.pvs -t ${.TARGET:R} \
-o ${PWD}/${.TARGET} > /dev/null
	-[ -r "${.TARGET}.patch" ] && patch < "${.TARGET}.patch"
	-[ -r "${.TARGET}.sh" ] && sh "${.TARGET}.sh"
.endfor

.include "../lambdapi.mk"
