PVSPATH != realpath ../..
PVS2DK   = ../pvs2dk.sh
LPC      = lambdapi check

THEORIES =\
naturals \
simple \
eqtype \
decvar \
import \
call_import \
prenex \
thyp_val \
thyp_type_val \
mixedup \
apply_theories \
subtypethy \
NatArray \
vector \
tuples \
tup_patmatch \
linear_form \
tfrom \
instantiate_subtype_formal \
constant_param \
exprastype \
depsubtype \
expandeddefs

all: ${THEORIES:C/$/.lpo/}

translate: ${THEORIES:C/$/.lp/}

.for th in ${THEORIES}
${th}.lp:
	PVSPATH=${PVSPATH} ${PVS2DK} -f ${PVSPATH}/lib/simple.pvs -t ${.TARGET:R} \
-o ${PVSPATH}/translations/simple/${.TARGET} > /dev/null
.endfor

.include "../lambdapi.mk"
