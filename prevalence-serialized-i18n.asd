;;;; prevalence-serialized-i18n.asd

(asdf:defsystem #:prevalence-serialized-i18n
 :serial t
 :version "0.1.1"
 :description "Strings and their translations serialized with prevalence s-serialize"
 :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
 :license "LLGPL"
 :depends-on (#:cl-prevalence #:cl-ppcre #:closer-mop #:weblocks-stores #:weblocks-util #:weblocks-utils)
 :components 
 ((:file "package")
  (:file "internationalization-stuff" :depends-on ("package"))))

