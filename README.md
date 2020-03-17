# PVS proofs

Encodings of PVS and manual translations of PVS prelude and others.
Use with `lambdapi`.

The work consists essentially in translating fragments of the PVS standard
library, also known as _Prelude_. The prelude is available
- as raw PVS,
  <https://raw.githubusercontent.com/SRI-CSL/PVS/master/lib/prelude.pvs>;
- as a documentation in pdf,
  <http://pvs.csl.sri.com/doc/pvs-prelude.pdf>;
- as html, <http://www.cs.rug.nl/~grl/ar06/prelude.html>.


## Requirements
- [`lambdapi`](https://github.com/gabrielhdt/lambdapi.git) on the `unif_hint`
  branch

## Structure
- `adlib` contains additional libraries not in the prelude
- `encodings` contains encodings of PVS into Dedukti, most of the work is done
  using `cert_f` encoding
- `prelude` contains parts of the PVS prelude
- `sandbox` contains miscellaneous experiments
- `tools` contains some additional scripts and utilities

Each directory contains a directory per encoding, so the logic file for the
prelude encoded with `cert_f` is under `prelude/cert_f/logic.lp`.

## Emacs abbreviations
The `.dir-locals.el` activates the `abbrev-mode` when opening a Dedukti file,
and calls a `set-local-abbrevs` function.

The
[`abbrev-mode`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html)
is used to expand some defined character sequences into longer words.
The typical example is to expand `btw` to `by the way`.
The function `set-local-abbrevs` is not built in emacs and 
allows to define abbreviation locally to a loaded file, 
otherwise, abbreviations would be saved and activated for all
Dedukti buffers. The function takes as argument a list of abbreviations.

The code of `set-local-abbrevs` comes from
<https://stackoverflow.com/questions/21836527/abbrev-local-to-document-in-emacs>
``` emacs-lisp
(defun set-local-abbrevs (abbrevs)
    "Add ABBREVS to `local-abbrev-table' and make it buffer local.
     ABBREVS should be a list of abbrevs as passed to `define-abbrev-table'.
     The `local-abbrev-table' will be replaced by a copy with the new 
     abbrevs added, so that it is not the same as the abbrev table used
     in other buffers with the same `major-mode'."
    (let* ((bufname (buffer-name))
           (prefix (substring (md5 bufname) 0 (length bufname)))
           (tblsym (intern (concat prefix "-abbrev-table"))))
      (set tblsym (copy-abbrev-table local-abbrev-table))
      (dolist (abbrev abbrevs)
          (define-abbrev (eval tblsym)
            (car abbrev)
            (cadr abbrev)
            (caddr abbrev)))
    (setq-local local-abbrev-table (eval tblsym))))
```

And we define the following abbreviations,

| Key sequence | Expansion |
|:-------------|----------:|
| subtype      |         ⊑ |
| upcast       |         ↑ |
| downcast     |         ↓ |
