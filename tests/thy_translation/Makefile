PVSPATH ?=
PVS2DK   = ../../scripts/pvs2dk.sh

THEORIES =
THEORIES += naturals
THEORIES += simple
THEORIES += eqtype
THEORIES += decvar
THEORIES += import
THEORIES += call_import
THEORIES += prenex
THEORIES += thyp_val
THEORIES += thyp_type_val
THEORIES += mixedup
THEORIES += apply_theories
THEORIES += subtypethy
THEORIES += NatArray
THEORIES += vector
THEORIES += tuples
THEORIES += tup_patmatch
THEORIES += linear_form
THEORIES += tfrom
THEORIES += instantiate_subtype_formal
THEORIES += constant_param
THEORIES += exprastype
THEORIES += depsubtype
THEORIES += expandeddefs

all: ${THEORIES:C/$/.lp/}

.for th in ${THEORIES}
${th}.lp: simple.pvs
	PVSPATH=${PVSPATH} ${PVS2DK} -f simple.pvs -t ${.TARGET:R} \
-o ${PWD}/${.TARGET} > /dev/null
.endfor

.for th in ${THEORIES}
.PHONY: test-${th}
test-${th}: ${th}.lp
	cmp ${th}.lp.expected ${th}.lp
.endfor

tests: ${THEORIES:C/^/test-/}

clean:
	rm -f *.lp *.lisp

.PHONY: clean
