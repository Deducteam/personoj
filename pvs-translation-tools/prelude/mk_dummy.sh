#!/bin/ksh
set -euo pipefail -o noclobber

while read -r line; do
    # Remove comments from line
    nocom=$(print -f "$line" | sed -e 's/^\([^#]*\).*$/\1/' | tr -d '[:space:]')
    if (print -f "${nocom}" | grep -E -q '^-'); then
        fname="${nocom:1}"
        [ -r "${fname}.lp" ] || printf '// Dummy theory\n' > "${fname}.lp"
        [ -r "${fname}.lisp" ] || printf '(:context nil :decls nil)' > "${fname}.lisp"
    fi
done < 'theories'
