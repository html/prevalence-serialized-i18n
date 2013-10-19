;;;; package.lisp

(defpackage #:prevalence-serialized-i18n
  (:use #:cl #:c2mop #:weblocks-stores)
  (:export 
    #:translate #:translation
    #:value 
    #:active 
    #:*prevalence-serialized-i18n-store*)
  (:import-from :weblocks-utils #:first-by-values)
  (:shadowing-import-from :c2mop #:defclass #:defgeneric #:defmethod
                          #:standard-generic-function #:ensure-generic-function
                          #:standard-class #:typep #:subtypep #:standard-method)
  (:shadowing-import-from :weblocks-util #:asdf-system-directory))
