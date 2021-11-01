PVS2DK = ../pvs2dk.sh
LP     = lambdapi

PVSPATH  ?=

_THEORIES != ../purify.pl -p < theories
_DUMMIES  != ../purify.pl -n < theories
# We cannot cat _THEORIES and _DUMMIES because order is important
_ALL      != ../purify.pl < theories

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
	-[ -r "${.TARGET}.patch" ] && patch < "${.TARGET}.patch"
	-[ -r "${.TARGET}.sh" ] && sh "${.TARGET}.sh"
.endfor

.include "../lambdapi.mk"
