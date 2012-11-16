(in-package :prevalence-serialized-i18n)

(defvar *translated-table* nil)
(defvar *translation-strings* nil) ; List of instances of translation-string
(defvar *translations* nil) ; List of instances of translation

(defparameter *languages-supported* '(:en :ru :uk))
(defparameter *default-language* :ru)

#+l(defmacro %current-language ()
     `(webapp-session-value 'current-language weblocks::*session* (weblocks::find-app 'weblocks-strings-translation-app)))

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

(defun find-translation-string-in-array (string)
  (find string *translation-strings* 
        :test (lambda (item string)
                (string= item (slot-value string 'value)))))

(defun add-translation-string-to-array (translation-string)
  (setf (slot-value translation-string 'id) (get-next-id-for *translation-strings*))
  (push translation-string *translation-strings*)
  translation-string)

(defun get-next-id-for (list)
  (loop for i from (1+ (length list)) do 
        (unless (find i list 
                      :test (lambda (i item)
                              (= i (slot-value item 'id))))
          (return-from get-next-id-for i))))

(defun add-translation-to-array (translation)
  (setf (slot-value translation 'id) (get-next-id-for *translations*) )
  (push translation *translations*)
  translation)

(defun find-translations-by-values (&rest args &key (test #'equal) &allow-other-keys)
  (flet ((filter-by-values (object)
           (loop for key in args by #'cddr do 
                 (let ((value (getf args key))
                       (test-fun test))
                   (when (and (consp value) (functionp (cdr value)))
                     (setf test-fun (cdr value))
                     (setf value (car value)))
                   (unless (funcall test-fun value 
                                    (slot-value object 
                                                (intern (string  key) 
                                                        (package-name (symbol-package (type-of object))))))
                     (return-from filter-by-values nil))))
           t))

    (loop for i in *translations* if (filter-by-values i) collect i)))

(defun first-translation-by-values (&rest args)
  (first (apply #'find-translations-by-values args)))

(defun get-translated-string (string &rest args)
  (let* ((translation-string (or (find-translation-string-in-array string)
                                 (add-translation-string-to-array 
                                   (make-instance 'translation-string :value string :active t))))
         (search-conditions (list :scope (cons (and (getf args :lang) (list :lang (getf args :lang))) #'equal) 
                                  :translation-string translation-string 
                                  :active t))
         (translation (or 
                        (apply #'first-translation-by-values search-conditions) 
                        (progn
                          (setf (getf search-conditions :active) nil)
                          (or 
                            (apply #'first-translation-by-values search-conditions)
                            (progn
                              (setf (getf search-conditions :scope) args)
                              (log-translation-missing string args)
                              (add-translation-to-array 
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
  #+l(if (and (find-package :firephp) (boundp 'hunchentoot:*reply*)) 
    (funcall (symbol-function (intern "FB" "FIREPHP")) "Trying to translate" string args))

  (when (zerop (length string))
    (return-from translate string))

  (if (find string *translated-table* :test #'string=)
    string
    (let* ((splitted-str (cl-ppcre:split "(\\$[^\\$]+\\$)" (get-translated-string string :lang (current-language) :package (getf args :package)) :with-registers-p t))
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
                       (when args
                         (error (format nil "Some keys do not correspond to their translate string value string \"~A\" args ~A" string args))))))
           (translated-string return-value))
      (push translated-string *translated-table*)
      translated-string)))

(defun load-data (file-name)
  (setf *translation-strings* nil)
  (setf *translations* nil)

  (with-open-file (file file-name :direction :input)
    (flet ((ids-comparator (item1 item2)
             (= (slot-value item1 'id) (slot-value item2 'id))))
      (let ((state (s-serialization:make-serialization-state)))
        (loop for i in (s-serialization:deserialize-xml file state) do 
          (pushnew i *translations* :test #'ids-comparator)
          (pushnew (translation-string i) *translation-strings* :test #'ids-comparator))))))

(defun save-data (file-name)
  (declare (special *translations*))
  (with-open-file (file file-name :direction :output :if-does-not-exist :create)
    (let ((state (s-serialization:make-serialization-state)))
      (s-serialization:serialize-xml *translations* file state))))

#+l(set-dispatch-macro-character #\# #\l
                                 #'(lambda (stream char1 char2)
                                     (declare (ignore char1 char2))
                                     (let ((first-character (read-char stream)))
                                       (if (char= first-character #\")
                                         `(translate ,(read-lisp-string stream))
                                         (progn
                                           (unread-char first-character stream)
                                           `(translate ,@(read stream)))))))

#+l(push 
     (lambda(&rest args)
       (declare (special *translated-table*))
       (setf *translated-table* nil))
     (request-hook :application :post-render))
