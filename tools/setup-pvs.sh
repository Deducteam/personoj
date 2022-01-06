#!/bin/sh
set -eu

SCRIPT=$(realpath "$0")
DIR=$(dirname "$SCRIPT")
ROOT=$(realpath "${DIR}/..") # Root of personoj repo

sudo apt-get -q install bmake perl gcc git emacs-nox

## SBCL

url="https://downloads.sourceforge.net/project/sbcl/sbcl/1.4.16/sbcl-1.4.16-x86-64-linux-binary.tar.bz2"
(cd "${HOME}" || exit 1
 curl "$url" -L | tar jx
 ln -s sbcl* sbcl)

# PVS
(cd "$HOME" || exit 1
 git clone https://github.com/SRI-CSL/PVS.git -q
 cd PVS && git checkout pvs7.1)

(cd "${HOME}/PVS" || exit 1
 autoconf
 ./configure)

EMACS="$(command -v emacs)"
SBCLISP_HOME="${HOME}/sbcl"
PVSPATH="${HOME}/PVS"
export PVSPATH

for p in $(find "${ROOT}"/prelude/patches -name '*.diff' | sort); do
    patch "${PVSPATH}/lib/prelude.pvs" "$p"
done

(cd "${PVSPATH}"
 make EMACS="$EMACS" SBCLISP_HOME="$SBCLISP_HOME" >output 2>&1)

echo "$PVSPATH"
