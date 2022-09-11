PERSONOJ  =
PERSONOJ += personoj_lhol.dko
PERSONOJ += personoj_logical.dko
PERSONOJ += personoj_pvs_cert.dko
PERSONOJ += personoj_eq.dko
PERSONOJ += personoj_restrict.dko
PERSONOJ += personoj_coercions.dko
PERSONOJ += personoj_telescope.dko
PERSONOJ += personoj_extra_arity-tools.dko
PERSONOJ += personoj_nat.dko
PERSONOJ += personoj_cast.dko

all: ${PERSONOJ}

.SUFFIXES: .dk .dko

.dk.dko:
	dk check -e ${.IMPSRC}

.include ".depends"
