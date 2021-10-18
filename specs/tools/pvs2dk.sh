#!/bin/ksh

# usage: pvs2dk.sh [options]
#   -f p, --file p              translate theory from file p
#   -t h, --theory h            translate theory t
#   -o p, --output p            write translation to file p
#   -v n, --verbose n           set verbosity level

set -eufo pipefail

theory=""
file=""
output=""
verbose=3

USAGE="[-author?Gabriel Hondet <gabriel.hondet@inria.fr>]"
USAGE+="[+NAME?pvs2dk.sh --- Translate a PVS specification to Dedukti]"
USAGE+="[+DESCRIPTION?Parse and proofcheck a theory in PVS to translate it
to Dedukti, an implementation of λΠ/R.

PVS filepaths are ALWAYS relative to the root of PVS. This restriction
is imposed by PVS (which uses a environment variable PVSPATH).

Theories are translated one at a time using the -f and -t options,
such as in pvs2dk --file=lib/prelude.pvs --theory=booleans.

Translated files can be type checked by Lambdapi (taken from the PATH). Files to
be typechecked may be edited.]"
USAGE+="[f:file?File containing the theory.]:[path]"
USAGE+="[t:theory?Name of the theory to translate.]:[theory]"
USAGE+="[o:output?Target file of the translation.]:[path]"
USAGE+="[v:verbose]#[verbose:=3?Verbosity level.]"
USAGE+=$'\n\n\n\n'
while getopts "${USAGE}" o; do
    case "$o" in
        f) file="$(realpath "${OPTARG}")" ;;
        t) theory="${OPTARG}" ;;
        o) output="${OPTARG}" ;;
        v) verbose="${OPTARG}"
    esac
done
output="$(realpath $(dirname ${output}))/$(basename ${output})"

pvscmd="(prettyprint-dedukti \"${file}#${theory}\" \"${output}\")"
(cd ${PVSPATH:?'PVSPATH not set'} || exit 1
 ./pvs -raw -E "${pvscmd}" --quit)
