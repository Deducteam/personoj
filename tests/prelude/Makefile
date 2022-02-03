PVSPATH  ?=
PVS       = ${PVSPATH}/pvs -raw
LISP     ?= sbcl --noinform
# PVS could be used for LISP, but the flags are different
LP        = lambdapi

_THEORIES != ${LISP} --load ../tools.lisp \
--eval '(theory-select "theories.json" :enabled)' --quit
_DUMMIES  != ${LISP} --load ../tools.lisp \
--eval '(theory-select "theories.json" :disabled)' --quit
# We cannot cat _THEORIES and _DUMMIES because order is important
_ALL      != ${LISP} --load ../tools.lisp \
--eval '(theory-select "theories.json" :all)' --quit

all: ${_ALL:C/$/.lpo/}
translate: ${_ALL:C/$/.lp/}

.for d in ${_DUMMIES}
${d}.lp:
	set -o noclobber && printf '// Dummy theory\n' > ${d}.lp
.endfor

# For each theory, translate it and apply a patch if it exists
.for th in ${_THEORIES}
${th}.lp:
	${PVS} -L ../pvs-tools.lisp \
-E "(prelude-export \"${th}\" \"${PWD}/${.TARGET}\" t)"
	-[ -r "${.TARGET}.sh" ] && sh "${.TARGET}.sh"
.endfor

clean-translation:
	rm -rf *.lp

clean: clean-translation

.PHONY: clean-translation

.include "../../mk/lambdapi.mk"
