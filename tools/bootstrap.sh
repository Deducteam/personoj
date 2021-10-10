#!/bin/sh
set -eu

curl -s 'http://www.lsv.fr/~hondet/pvs/lambdapi-pvs_1.0_all.deb' > lambdapi-pvs.deb 
yes | sudo apt-get -q install ./lambdapi-pvs.deb
yes | sudo apt-get install -q emacs # For PVS
sudo sysctl kernel.unprivileged_userns_clone=1 
yes '/usr/local/bin' | sudo bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh) --version 2.1.0"

url="https://downloads.sourceforge.net/project/sbcl/sbcl/1.4.16/sbcl-1.4.16-x86-64-linux-binary.tar.bz2"
(cd "${HOME}" || exit 1
 curl "$url" -L | tar jx
 ln -s sbcl* sbcl)

opam init --bare --no-setup -q

gclone () {
    (cd "$HOME"
     git clone "$1"
     (cd "$2" || exit 1
      git checkout "$3"))
}

gclone https://github.com/gabrielhdt/lambdapi.git lambdapi aae26f2d
gclone https://github.com/SRI-CSL/PVS.git PVS pvs7.1

(cd "${HOME}/lambdapi" || exit 1
 opam switch create . --locked --deps-only --yes
 eval "$(opam env)"
 make install
 why3 config detect)

(cd "${HOME}/PVS" || exit 1
 autoconf
 ./configure)

 EMACS="$(command -v emacs)"
 SBCLISP_HOME="${HOME}/sbcl"
 PVSPATH="${HOME}/PVS"
 export PVSPATH

 for p in $(find tools/prelude_patches -name '*.diff' | sort); do
     patch "${PVSPATH}/lib/prelude.pvs" "$p"
 done

 (cd "${PVSPATH}"
  make EMACS="$EMACS" SBCLISP_HOME="$SBCLISP_HOME" >output 2>&1)
