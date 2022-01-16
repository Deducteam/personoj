(in-package :common-lisp-user)

(defpackage dklog
  (:documentation "Some logging facilities for Dedukti.")
  (:use #:cl)
  (:export #:top #:expr #:type #:decl #:sign #:contexts)
  (:shadow #:expr #:type #:decl #:sign #:contexts))
