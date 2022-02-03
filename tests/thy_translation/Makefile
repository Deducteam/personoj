PVSPATH ?=
PVS      = ${PVSPATH}/pvs -raw

# We cannot internalise everything in lisp yet until the translation is "pure"
# (i.e. no variable is changed at the end of the translation)

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
THEORIES += letin

all: ${THEORIES:C/$/.lp/}

tests: ${THEORIES:C/^/test-/}

promote: ${THEORIES:C/^/promote-/}

.for th in ${THEORIES}
${th}.lp: simple.pvs
	@${PVS} -L test.lisp -E '(runtest "${.TARGET:R}")' -E '(uiop:quit)'
.endfor

.for th in ${THEORIES}
.PHONY: promote-${th}
promote-${th}: ${th}.lp
	@cp -f ${th}.lp ${th}.lp.expected
.endfor

clean:
	rm -f *.lp

.PHONY: clean
