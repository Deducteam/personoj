PVSPATH ?=
PVS2DK   = ${PVSPATH}/scripts/pvs2dk.sh
LPC      = lambdapi check

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
THEORIES += expandedde

all: ${THEORIES:C/$/.lp/}

.for th in ${THEORIES}
${th}.lp:
	PVSPATH=${PVSPATH} ${PVS2DK} -f simple.pvs -t ${.TARGET:R} \
-o simple/${.TARGET} > /dev/null
.endfor

.include "${PVSPATH}/lambdapi.mk"
