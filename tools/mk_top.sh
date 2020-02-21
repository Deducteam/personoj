#!/bin/bash

## [./mk_top.sh d] creates a file [d/top.lp] importing all files in [d/]

out="top.lp"

( cd "$1" || exit
  rm "${out}"
  for f in *.lp; do
      p=$(printf '%s/' "$1" | tr -s '/' '.')
      printf 'require %s%s\n' "${p}" "${f%.lp}" >> "${out}"
  done )
