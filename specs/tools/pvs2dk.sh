#!/bin/sh

# usage: pvs2dk.sh [options]
#   -f p, --file p              translate theory from file p
#   -t h, --theory h            translate theory t
#   -o p, --output p            write translation to file p

set -euf

theory=""
file=""
output=""

HELP () {
	printf 'NAME: %s --- Translate a PVS specification to Dedukti

SYNOPSIS:

%s -f FILE -t THEORY -o OUTPUT

 Options:
   -f File containing the theory
   -t Name of the theory to translate
   -o Target file of the translation

DESCRIPTION:

Parse and proofcheck a theory in PVS to translate it
to Dedukti, an implementation of λΠ/R.

PVS filepaths are ALWAYS relative to the root of PVS. This restriction
is imposed by PVS (which uses a environment variable PVSPATH).

Theories are translated one at a time using the -f and -t options,
such as in pvs2dk --file=lib/prelude.pvs --theory=booleans.

Translated files can be type checked by Lambdapi (taken from the PATH). Files to
be typechecked may be edited.\n' "$0" "$0"
	exit 1
}

while getopts 'f:t:o:h' o; do
    case "$o" in
        f) file="$(realpath "${OPTARG}")" ;;
        t) theory="${OPTARG}" ;;
        o) output="${OPTARG}" ;;
				h) HELP ;;
				?) HELP ;;
    esac
done
output="$(realpath "$(dirname "${output}")")/$(basename "${output}")"

pvscmd="(prettyprint-dedukti \"${file}#${theory}\" \"${output}\")"
(cd "${PVSPATH:?'PVSPATH not set'}" || exit 1
 ./pvs -raw -E "${pvscmd}" --quit)
