#!/bin/sh

set -eu
PROVEIT="${1:-}"
psnj-pipe --proveit=$PROVEIT --qfo examples/encoding/qfo.json examples/hello.pvs
