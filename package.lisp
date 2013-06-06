;;;; package.lisp

(defpackage #:prevalence-serialized-i18n
  (:use #:cl #:c2mop #:weblocks-stores)
  (:export 
    #:translate #:translation-string #:translation #:set-language #:*default-language* #:current-language 
    #:en-translation 
    #:ru-translation 
    #:uk-translation 
    #:translations-count 
    #:translation-string-active-p 
    #:value 
    #:active 
    #:*prevalence-serialized-i18n-store*)
  (:shadowing-import-from :c2mop #:defclass #:defgeneric #:defmethod
                          #:standard-generic-function #:ensure-generic-function
                          #:standard-class #:typep #:subtypep #:standard-method)
  (:shadowing-import-from :weblocks-util #:asdf-system-directory))

