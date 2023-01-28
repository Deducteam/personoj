#!/bin/sh
#
# Install PVS 7.1 on a Debian-like intel x86_64 in ~. The script outputs the
# path to the PVS installation. PVS is then available as ~/pvs-7.1.0/pvs.
#
set -o nounset
set -o errexit
set -o xtrace

sudo apt install --yes emacs-nox bmake

(cd "$HOME" || exit 1
curl https://pvs.csl.sri.com/downloads/pvs7.1.0-ix86_64-Linux-sbclisp.tgz | tar -xz
cd pvs-7.1.0 || exit 1
sh install-sh)

echo "${HOME}/pvs-7.1.0"
