#!/bin/sh
#
# Install PVS 7.1 on a Debian-like intel x86_64. The script outputs the path to
# the PVS installation.
#
set -eu

SCRIPT=$(realpath "$0")
DIR=$(dirname "$SCRIPT")

sudo apt install --yes emacs-nox bmake

(cd "$HOME" || exit 1
 curl https://pvs.csl.sri.com/downloads/pvs7.1.0-ix86_64-Linux-sbclisp.tgz | tar -xz
 cd pvs-7.1.0 || exit 1
 sh install-sh)

echo "${HOME}/pvs-7.1.0"
