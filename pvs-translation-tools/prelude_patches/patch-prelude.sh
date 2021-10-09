#!/bin/sh
set -eu
for p in $(find . -name '*.diff' | sort); do
	patch "${PVSPATH}/lib/prelude.pvs" "$p"
done
