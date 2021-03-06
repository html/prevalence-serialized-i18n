(in-package :prevalence-serialized-i18n)

(defclass translation ()
  ((id)
   (translation-string 
     :type string 
     :initarg :translation-string 
     :accessor translation-string)
   (value :initform nil 
          :initarg :value 
          :accessor value)
   (scope 
     :initform nil 
     :initarg :scope 
     :accessor translation-scope 
     :documentation "Translation string scope, contains 
                     :lang key - a language, 
                     :form key - can be :genitive :accusative etc.
                     :gender key - can be :masculine or :feminine or :neuter
                     :preceding-gender key - has values same as :gender key
                     :count key - has values depending on language, 
                        for Russian this is :one :few :many (it has 3 number forms)
                        for English this is :one :many (it has 2 number forms)
                     string/word form and other scope options useful for translation.")
   (active :initform nil 
           :initarg :active 
           :accessor translation-active-p)
   (time-created :initform (get-universal-time))
   (time-last-used :initform (get-universal-time)))
  (:documentation "Database class, contains translation values for different strings"))

(defstore *prevalence-serialized-i18n-store* :prevalence
          (merge-pathnames 
            (make-pathname :directory '(:relative "data"))
            (asdf-system-directory :prevalence-serialized-i18n)))

(weblocks-stores:open-stores)

(defun translation-scopes-equalp (item1 item2)
  (unless (= (length item1) (length item2))
    (return-from translation-scopes-equalp nil))

  (loop for (key value) on item1 by #'cddr do
        (unless (equal value (getf item2 key))
          (return-from translation-scopes-equalp nil)))
  t)

; Useful in cases when russian strings need to be translated
(defvar *yandex-translate-api-key* nil)
(defun translate-with-yandex (str &key (direction "ru-en"))
  (unless *yandex-translate-api-key* 
    (return-from translate-with-yandex str))

  (let* ((key *yandex-translate-api-key*)
         (url (format nil "https://translate.yandex.net/api/v1.5/tr.json/translate?key=~A&lang=~A&text=~A" key direction (arnesi:escape-as-uri str))))
    (second (assoc :text (json:decode-json-from-string 
                           (with-output-to-string (s)
                             (external-program:run "curl" (list "-s" url) :output s)))))))

(defun translate (string &rest scope)
  (when (zerop (length string))
    (return-from translate string))

  (let ((found-string (first-by-values 'translation 
                                       :store *prevalence-serialized-i18n-store*
                                       :translation-string (cons string #'string=)
                                       :scope (cons scope #'translation-scopes-equalp))))
    (if found-string 
      (progn 
        (setf (slot-value found-string 'time-last-used) (get-universal-time))
        (slot-value found-string 'value))
      (progn
        (weblocks-stores:persist-object 
          *prevalence-serialized-i18n-store* 
          (make-instance 'translation 
                         :translation-string string
                         :value (if (equal (weblocks::current-locale) :ru) string (translate-with-yandex string))
                         :scope scope
                         :active nil))
        string))))
