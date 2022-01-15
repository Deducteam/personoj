PVS2DK    = ../scripts/pvs2dk.sh
THYSELECT = psnj thyselect
LP        = lambdapi

PVSPATH  ?=

_THEORIES != ${THYSELECT} -p < theories.json
_DUMMIES  != ${THYSELECT} -n < theories.json
# We cannot cat _THEORIES and _DUMMIES because order is important
_ALL      != ${THYSELECT} < theories.json

all: ${_ALL:C/$/.lpo/}
translate: ${_ALL:C/$/.lp/}

.for d in ${_DUMMIES}
${d}.lp:
	set -o noclobber && printf '// Dummy theory\n' > ${d}.lp
${d}.lisp:
	set -o noclobber && printf '(:context nil :decls nil)' > ${d}.lisp
.endfor

# For each theory, translate it and apply a patch if it exists
.for th in ${_THEORIES}
${th}.lp:
	PVSPATH=${PVSPATH} ${PVS2DK} -f ${PVSPATH}/lib/prelude.pvs -t ${.TARGET:R} \
-o ${PWD}/${.TARGET} > /dev/null
	-[ -r "${.TARGET}.sh" ] && sh "${.TARGET}.sh"
.endfor

clean-translation:
	rm -rf *.lp *.lisp

clean: clean-translation

.PHONY: clean-translation

.include "../mk/lambdapi.mk"
