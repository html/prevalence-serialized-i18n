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

(defstore *prevalence-serialized-i18n-store* :prevalence
          (merge-pathnames 
            (make-pathname :directory '(:relative "data"))
            (asdf-system-directory :prevalence-serialized-i18n)))

(weblocks-stores:open-stores)

(defun find-by-values (class &rest args &key (test #'equal) order-by range &allow-other-keys)
  "Returns items of specified class. Filters passed as key arguments (key is slot name, value is value compared). 
   :test parameter is used to set default predicate for filters. You can also use (cons <filter-value <predicate>) instead of <filter-value> to override predicate for specific key."
  (setf args (alexandria:remove-from-plist args :order-by :range :store))
  (flet ((filter-by-values (object)
           (loop for key in args by #'cddr do 
                 (let ((value (getf args key))
                       (test-fun test))
                   (when (and (consp value) (functionp (cdr value)))
                     (setf test-fun (cdr value))
                     (setf value (car value)))
                   (unless (funcall test-fun value (slot-value object 
                                                           (intern (string  key) 
                                                                   (package-name (symbol-package (type-of object))))))
                     (return-from filter-by-values nil))))
           t))

    (find-persistent-objects *prevalence-serialized-i18n-store* class :filter #'filter-by-values :order-by order-by :range range)))

(defun first-by-values (&rest args)
  "Similar to 'find-by-values' but returns only first item"
  (first (apply #'find-by-values args)))

(defparameter *languages-supported* '(:en :ru :uk))
(defparameter *default-language* :ru)

(defun current-language ()
  *default-language*)

#+l(defun current-language ()
  (or (and (boundp 'hunchentoot:*session*) (%current-language)) *default-language*))

#+l(defun set-language (new-lang)
  (when (find (string-downcase new-lang) (mapcar #'string-downcase *languages-supported*) :test #'string=)
    (if (boundp 'hunchentoot:*session*)
      (setf (%current-language) (intern (string-upcase new-lang) "KEYWORD"))
      (setf *default-language* (intern (string-upcase new-lang) "KEYWORD")))
    
    t))

(defclass translated-string ()
  ((value 
     :type string 
     :initarg :value 
     :accessor translated-string-value)))

(defmethod print-object ((object translated-string) stream)
  (write-string (translated-string-value object) stream))

(defun log-translation-missing (string args)
  #+l(format t "Translation is missing for string /~A/ and scope ~A" string args)
  ; TODO: move this into firephp package
  #+l(when (and (find-package :firephp) (boundp 'hunchentoot:*reply*)) 
    (funcall (symbol-function (intern "FB" "FIREPHP")) "Translation is missing for string" string args))
  string)

(defun get-next-id-for (list)
  (loop for i from (1+ (length list)) do 
        (unless (find i list 
                      :test (lambda (i item)
                              (= i (slot-value item 'id))))
          (return-from get-next-id-for i))))

(defun get-translation-string-translation-for-lang (translation-string lang number-form)
  (let ((translations)
        (filtered-translations))
    ; First filtering by lang scope
    (setf filtered-translations 
          (loop for i in (translation-string-translations translation-string) 
                if (equal (getf (translation-scope i) :lang) lang)
                collect i))

    (setf translations filtered-translations)

    ; Second filtering by number form
    (setf filtered-translations 
          (loop for i in (translation-string-translations translation-string) 
                if (equal (getf (translation-scope i) :count) number-form)
                collect i))

    (first filtered-translations)))


(defun get-translated-string (string &rest args)
  (let* ((translation-string (or (first-by-values 'translation-string :value string)
                                 (persist-object 
                                   *prevalence-serialized-i18n-store*
                                   (make-instance 'translation-string :value string :active t))))
         (search-conditions (list :scope (and (getf args :lang) (list :lang (getf args :lang))) 
                                  :translation-string translation-string 
                                  :active t))
         (translation (or 
                        (get-translation-string-translation-for-lang translation-string (getf args :lang) (getf args :count))
                        (apply #'first-by-values 'translation search-conditions)
                        (progn
                          (setf (getf search-conditions :active) nil)
                          (or 
                            (apply #'first-by-values 'translation search-conditions)
                            (progn
                              (setf (getf search-conditions :scope) args)
                              (log-translation-missing string args)
                              (persist-object 
                                *prevalence-serialized-i18n-store*
                                (apply #'make-instance 
                                       (list* 'translation 
                                              (append search-conditions 
                                                      (if (equal 
                                                            (getf (getf search-conditions :scope) :lang)
                                                            (current-language)) 
                                                        (list :value string :active t)
                                                        (list :value "Untranslated" :active nil)))))))))))) 
    ;(setf (translation-scope translation) (list :lang (getf (translation-scope translation) :lang) :package (getf args :package)))
    (setf (slot-value translation-string 'time-last-used) (get-universal-time)) 
    (slot-value translation 'prevalence-serialized-i18n::value)))

; Internationalization
; Stolen from i18n
(defun read-lisp-string (input)
  (with-output-to-string (output)
    (loop
      (let ((char (read-char input)))
        (case char
          (#\\
           (setf char (read-char input)))
          (#\"
           (return)))
        (write-char char output)))))

(defun translate (string &rest args)
  (when (zerop (length string))
    (return-from translate string))

  (let* ((splitted-str (cl-ppcre:split "(\\$[^\\$]+\\$)" (get-translated-string string 
                                                                                :count (getf args :count)
                                                                                :lang (current-language) :package (getf args :package)) :with-registers-p t))
         (return-value 
           (format nil "~{~A~}" 
                   (prog1
                     (loop for i in splitted-str collect
                           (or 
                             (cl-ppcre:register-groups-bind (value)
                                                            ("\\$(.*)\\$" i)
                                                            (and value
                                                                 (let* ((key (read-from-string (string-upcase (format nil ":~A" value))))
                                                                        (value (getf args key)))
                                                                   (unless key 
                                                                     (error (format nil "Need key ~A for translation" key)))
                                                                   (unless value 
                                                                     (error (format nil "Need value for key ~A for translation" key)))
                                                                   (and 
                                                                     (progn 
                                                                       (remf args key)
                                                                       value)))))
                             i))
                     (when (> (length args) 2)
                       (error (format nil "Some keys do not correspond to their translate string value string \"~A\" args ~A" string args))))))
         (translated-string return-value))
    translated-string))

(defun load-data (&rest args)
  (let ((*translation-strings*)
        (*translations*))
    (loop for file-name in args do
          (with-open-file (file file-name :direction :input)
            (flet ((ids-comparator (item1 item2)
                     (= (slot-value item1 'id) (slot-value item2 'id))))
              (let ((state (s-serialization:make-serialization-state)))
                (loop for i in (s-serialization:deserialize-xml file state) do 
                  (pushnew i *translations* :test #'ids-comparator)
                  (pushnew (translation-string i) *translation-strings* :test #'ids-comparator))))))
    (loop for i in *translation-strings* do 
          (persist-object *prevalence-serialized-i18n-store* i))
    (loop for i in *translations* do 
          (persist-object *prevalence-serialized-i18n-store* i))))

#+l(set-dispatch-macro-character #\# #\l
                                 #'(lambda (stream char1 char2)
                                     (declare (ignore char1 char2))
                                     (let ((first-character (read-char stream)))
                                       (if (char= first-character #\")
                                         `(translate ,(read-lisp-string stream))
                                         (progn
                                           (unread-char first-character stream)
                                           `(translate ,@(read stream)))))))
