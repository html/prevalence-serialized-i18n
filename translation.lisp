(in-package :prevalence-serialized-i18n)

(defclass translation-string ()
  ((id)
   (active :initform nil 
           :initarg :active 
           :accessor translation-string-active-p)
   (value :initform nil :initarg :value) 
   (time-last-used :initform (get-universal-time)) 
   (time-created :initform (get-universal-time)))
  (:documentation "Database class, contains source strings which should be translated"))

(defclass translation ()
  ((id)
   (translation-string 
     :type translation-string 
     :initarg :translation-string 
     :accessor translation-string)
   (value :initform nil 
          :initarg :value 
          :accessor value)
   (scope 
     :initform nil 
     :initarg :scope 
     :accessor translation-scope)
   (active :initform nil 
           :initarg :active 
           :accessor translation-active-p))
  (:documentation "Database class, contains translation variants for strings, referenced to translation-string class"))

(defmethod translations-count ((obj translation-string))
  (declare (special *translations*))
  (loop for i in *translations* if (= (slot-value (translation-string i) 'id) (slot-value obj 'id)) count i))

(defmacro define-lang-translation (lang)
  `(defmethod ,(intern (string-upcase (format nil "~A-TRANSLATION" lang))) ((obj translation-string))
     (let ((translation (first (find-translations-by-values :translation-string obj :scope (cons (list :lang ,(intern (string-upcase lang) "KEYWORD")) #'equal)))))
       (and translation (value translation)))))

(define-lang-translation en)
(define-lang-translation uk)
(define-lang-translation ru)
