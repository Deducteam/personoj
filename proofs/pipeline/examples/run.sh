#!/bin/sh
set -euf
opam exec -- psnj-pipe --qfo=encoding/qfo.json hello.log encoding/ spec/
