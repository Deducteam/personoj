#!/bin/sh
#
# Install PVS 8 on a Debian-like intel x86_64 in ~. The script outputs the
# path to the PVS installation. PVS is then available as ~/PVS/pvs.
#
set -o nounset
set -o errexit
set -o xtrace

sudo apt install --yes emacs-nox bmake

(cd "$HOME" || exit 1
git clone https://github.com/SRI-CSL/PVS
cd PVS || exit 1
./configure
make)

echo "${HOME}/PVS"
