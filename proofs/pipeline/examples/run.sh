#!/bin/sh

set -eu
PROVEIT="${1:-}"
psnj-pipe --proveit=$PROVEIT --qfo=encoding/qfo.json hello.pvs encoding/ spec/
