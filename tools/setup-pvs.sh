#!/bin/sh
set -eu

SCRIPT=$(realpath "$0")
DIR=$(dirname "$SCRIPT")
ROOT=$(realpath "${DIR}/..") # Root of personoj repo

sudo apt install --yes emacs-nox bmake

(cd "$HOME" || exit 1
 curl http://pvs.csl.sri.com/downloads/pvs7.1.0-ix86_64-Linux-sbclisp.tgz | tar xz
 cd pvs-7.1.0 || exit 1
 sh install-sh)

PVSPATH="${HOME}/pvs-7.1.0"
echo "${HOME}/pvs-7.1.0"
