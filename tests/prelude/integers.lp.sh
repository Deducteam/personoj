#!/bin/sh

sed -i 's/\(symbol integer: Set ≔.*$\)/\1\nrule int.Int ↪ El integer;/' integers.lp
