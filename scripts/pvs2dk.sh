#!/bin/sh

# usage: pvs2dk.sh [options]
#   -f p, --file p              translate theory from file p
#   -t h, --theory h            translate theory t
#   -o p, --output p            write translation to file p

set -euf

theory=""
file=""
output=""

ansi () {
	printf "\e[${1}m${2}\e[0m"
}

HELP () {
	printf "NAME: pvs2dk --- Translate PVS specifications to Dedukti

SYNOPSIS:

$0 -f FILE -t THEORY -o OUTPUT

 Options:
   -f PVS file containing the theory to translate
   -t Name of the theory to translate
   -o Target file of the translation

DESCRIPTION:

Translate a PVS theory to lambdapi, an implementation of λΠ/R. It
translates only declarations, definitions and propositions, proofs are
not translated.

PVS filepaths are ALWAYS relative to the root of PVS. This restriction
is imposed by PVS (which uses a environment variable PVSPATH).

EXAMPLES

pvs2dk -f lib/prelude.pvs -t booleans -o booleans.lp
	Translate the theory $(ansi 4 booleans) in file $(ansi 4 booleans.pvs)
	to the file $(ansi 4 booleans.lp)
\n"
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
