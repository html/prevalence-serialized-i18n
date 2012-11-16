;;;; prevalence-serialized-i18n.asd

(asdf:defsystem #:prevalence-serialized-i18n
 :serial t
 :version "0.0.1"
 :description "Strings and their translations serialized with prevalence s-serialize"
 :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
 :license "LLGPL"
 :depends-on (#:cl-prevalence #:cl-ppcre #:closer-mop)
 :components 
 ((:file "package")
  (:file "internationalization-stuff" :depends-on ("package"))
  (:file "translation" :depends-on ("internationalization-stuff"))))

