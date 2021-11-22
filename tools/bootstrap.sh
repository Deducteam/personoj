#!/bin/sh
set -eu

SCRIPT=$(realpath "$0")
DIR=$(dirname "$SCRIPT")
ROOT=$(realpath "${DIR}/..") # Root of personoj repo

## Opam

yes | sudo apt-get -q install \
	zlib1g-dev libx11-dev libgmp-dev bubblewrap m4 gcc autoconf \
	make unzip pkg-config git rsync bmake gcc perl \
	emacs-nox
sudo sysctl kernel.unprivileged_userns_clone=1 
yes '/usr/local/bin' | sudo bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh) --version 2.1.0"

opam init --bare --no-setup -q

## SBCL

url="https://downloads.sourceforge.net/project/sbcl/sbcl/1.4.16/sbcl-1.4.16-x86-64-linux-binary.tar.bz2"
(cd "${HOME}" || exit 1
 curl "$url" -L | tar jx
 ln -s sbcl* sbcl)

gclone () {
    (cd "$HOME"
     git clone "$1"
     (cd "$2" || exit 1
      git checkout "$3"))
}

# Lambdapi

gclone https://github.com/gabrielhdt/lambdapi.git lambdapi 1a7031e4

(cd "${HOME}/lambdapi" || exit 1
 opam switch create . --locked --deps-only --yes
 eval "$(opam env)"
 make install
 why3 config detect)

# PVS

gclone https://github.com/SRI-CSL/PVS.git PVS pvs7.1

(cd "${HOME}/PVS" || exit 1
 autoconf
 ./configure)

 EMACS="$(command -v emacs)"
 SBCLISP_HOME="${HOME}/sbcl"
 PVSPATH="${HOME}/PVS"
 export PVSPATH

 for p in $(find "${ROOT}"/specs/tools/prelude_patches -name '*.diff' | sort); do
     patch "${PVSPATH}/lib/prelude.pvs" "$p"
 done

 (cd "${PVSPATH}"
  make EMACS="$EMACS" SBCLISP_HOME="$SBCLISP_HOME" >output 2>&1)
