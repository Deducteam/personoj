(in-package :common-lisp-user)

(defpackage dklog
  (:documentation "Some logging facilities for Dedukti.")
  (:use #:cl)
  (:export #:top #:expr #:type #:decl #:sign #:contexts)
  (:shadow #:expr #:type #:decl #:sign #:contexts))

(defpackage dksig
  (:documentation "Signatures for the export to Dedukti. They allow to remove
overloading from PVS' theories")
  (:use #:cl)
  (:import-from #:pvs #:aif #:mkstr #:symb #:append1 #:lrec)
  (:export
   :signature :make-signature :signature-theory :signature-context
   :find :add
   :open :dump)
  (:shadow :open :find))
