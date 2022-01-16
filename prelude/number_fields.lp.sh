#!/bin/sh

sed -i 's/\(symbol numfield: Set ≔.*$\)/\1\nrule Nat ↪ El numfield;/' number_fields.lp
