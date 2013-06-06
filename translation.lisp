(in-package :prevalence-serialized-i18n)

(defmethod translations-count ((obj translation-string))
  (length (translation-string-translations obj)))

(defmethod translation-string-translations ((obj translation-string))
  (find-by-values 'translation :translation-string obj :store *prevalence-serialized-i18n-store*))

(defun langs-equal (item1 item2)
  (equal (getf item1 :lang) (getf item2 :lang)))

(defmacro define-lang-translation (lang)
  `(defmethod ,(intern (string-upcase (format nil "~A-TRANSLATION" lang))) ((obj translation-string))
     (let ((translation (first-by-values 'translation :translation-string obj :scope (cons (list :lang ,(intern (string-upcase lang) "KEYWORD")) #'langs-equal))))
       (and translation (value translation)))))

(defmacro define-lang-scope (lang)
  `(defmethod ,(intern (string-upcase (format nil "~A-SCOPE" lang))) ((obj translation-string))
     (let ((translation (first-by-values 'translation :translation-string obj :scope (cons (list :lang ,(intern (string-upcase lang) "KEYWORD")) #'langs-equal))))
       (and translation (translation-scope translation)))))

(define-lang-translation en)
(define-lang-translation uk)
(define-lang-translation ru)
(define-lang-scope en)
(define-lang-scope uk)
(define-lang-scope ru)
