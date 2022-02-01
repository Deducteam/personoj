PVSPATH  ?=
PVS       = ${PVSPATH}/pvs -raw
LP        = lambdapi

_THEORIES != ${PVS} -L export-module.lisp -E '(theory-select "theories.json" :enabled)' -E '(sb-ext:exit)'
_DUMMIES  != ${PVS} -L export-module.lisp -E '(theory-select "theories.json" :disabled)' -E '(sb-ext:exit)'
# We cannot cat _THEORIES and _DUMMIES because order is important
_ALL      != ${PVS} -L export-module.lisp -E '(theory-select "theories.json" :all)' -E '(sb-ext:exit)'

all: ${_ALL:C/$/.lpo/}
translate: ${_ALL:C/$/.lp/}

.for d in ${_DUMMIES}
${d}.lp:
	set -o noclobber && printf '// Dummy theory\n' > ${d}.lp
.endfor

# For each theory, translate it and apply a patch if it exists
.for th in ${_THEORIES}
${th}.lp:
	@${PVSPATH}/pvs -raw -L export-module.lisp \
-E "(prelude-export \"${th}\" \"${PWD}/${.TARGET}\" t)"
	-[ -r "${.TARGET}.sh" ] && sh "${.TARGET}.sh"
.endfor

clean-translation:
	rm -rf *.lp

clean: clean-translation

.PHONY: clean-translation

.include "../mk/lambdapi.mk"
