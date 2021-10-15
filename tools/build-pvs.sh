#!/bin/sh
set -euf

# Build PVS using the system installed SBCL. Must be placed in $PVSPATH

PLATFORM="$(bin/pvs-platform)"

make -C src/utils/"${PLATFORM}"
make -C src/BDD/"${PLATFORM}"
(cd src/WS1S
 rm -rf mona
 ln -s mona-1.4 mona)
make -C src/WS1S/"${PLATFORM}"
sbcl --load src/make-pvs-parser.lisp
sbcl --eval "(defvar *pvs-path* \"$(pwd)\")" \
    --load src/make-pvs-methods.lisp
mkdir -p "bin/${PLATFORM}/runtime"
sbcl --eval '(load "pvs.system" :verbose t)' \
    --eval "(unwind-protect \
    (mk:operate-on-system :pvs :compile) \
    (save-lisp-and-die \"bin/${PLATFORM}/runtime/pvs-sbclisp\" \
    :toplevel (function startup-pvs) \
    :executable t))"
cp src/utils/"${PLATFORM}"/b64 bin/
cp src/BDD/"${PLATFORM}"/mu.so bin/"${PLATFORM}"/runtime
cp src/WS1S/"${PLATFORM}"/ws1s.so bin/"${PLATFORM}"/runtime
