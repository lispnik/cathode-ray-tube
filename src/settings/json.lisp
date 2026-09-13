;;;; src/settings/json.lisp -- just enough JSON for a profile.
;;;;
;;;; WHY NOT A LIBRARY.  com.inuoe.jzon was the dependency here and it does not
;;;; compile on ECL:
;;;;
;;;;     in file jzon.lisp, at (DEFUN NATIVE-NAMESTRING ...)
;;;;     * (OS-UNIX-P) is not a legal function name.
;;;;
;;;; That is a bug in its ECL support rather than anything we did, but it takes
;;;; the whole portable half of this program down with it -- and the portable
;;;; half exists precisely so that it loads where Objective-C does not.
;;;;
;;;; What is actually needed is small enough not to be worth a platform: a
;;;; cool-retro-term profile is a FLAT object whose values are numbers, strings
;;;; and booleans, with no arrays and no nesting.  So this reads and writes that
;;;; and refuses anything else, rather than pretending to be a JSON library.
;;;; If a future profile format grows structure, this will say so loudly instead
;;;; of half-reading it.

(in-package #:cathode-ray-tube.settings)

;;; Writing ------------------------------------------------------------------------

(defun write-json-string (string stream)
  (write-char #\" stream)
  (loop for character across string
        do (case character
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (t (if (< (char-code character) #x20)
                    (format stream "\\u~4,'0x" (char-code character))
                    (write-char character stream)))))
  (write-char #\" stream))

(defun write-json-number (number stream)
  "A number the way JSON wants it, and the way Storage.qml writes it.

An integer prints without a decimal point; everything else prints with as few
digits as say it exactly, which for the four-decimal values a profile holds
means 0.55 rather than 0.55000001192092896.  ~F rather than PRIN1 because CL
prints a single-float as 0.55 and a double as 0.55d0, and `d0' is not JSON."
  (if (integerp number)
      (format stream "~D" number)
      (let ((rounded (/ (fround (* (rational number) 10000)) 10000)))
        (if (= rounded (truncate rounded))
            (format stream "~D" (truncate rounded))
            (format stream "~A"
                    (string-right-trim "0" (format nil "~,4F" rounded)))))))

(defun write-json (alist &key (stream nil) (pretty t))
  "ALIST as a flat JSON object.  Values may be numbers, strings, or T/NIL.

NIL is FALSE, not null and not the empty list -- this writes profiles, where
the only booleans are blinkingCursor and the only NIL that can arrive is one."
  (let ((out (or stream (make-string-output-stream))))
    (write-string "{" out)
    (loop for (key . value) in alist
          for first = t then nil
          do (unless first (write-string "," out))
             (when pretty (format out "~%    "))
             (write-json-string (string key) out)
             (write-string ": " out)
             (etypecase value
               (string (write-json-string value out))
               (real (write-json-number value out))
               (boolean (write-string (if value "true" "false") out))))
    (when pretty (format out "~%"))
    (write-string "}" out)
    (unless stream (get-output-stream-string out))))

;;; Reading ------------------------------------------------------------------------

(define-condition json-error (error)
  ((message :initarg :message :reader json-error-message)
   (position :initarg :position :initform nil :reader json-error-position))
  (:report (lambda (condition stream)
             (format stream "~A~@[ at character ~D~]"
                     (json-error-message condition)
                     (json-error-position condition)))))

(defun skip-whitespace (string index)
  (loop while (and (< index (length string))
                   (member (char string index) '(#\Space #\Tab #\Newline #\Return)))
        do (incf index))
  index)

(defun expect-char (string index character)
  (let ((index (skip-whitespace string index)))
    (unless (and (< index (length string)) (char= (char string index) character))
      (error 'json-error :message (format nil "expected ~C" character)
                         :position index))
    (1+ index)))

(defun read-json-string (string index)
  "(values STRING NEXT-INDEX), INDEX being at the opening quote."
  (let ((index (expect-char string index #\"))
        (out (make-string-output-stream)))
    (loop
      (when (>= index (length string))
        (error 'json-error :message "unterminated string" :position index))
      (let ((character (char string index)))
        (cond
          ((char= character #\") (return (values (get-output-stream-string out)
                                                 (1+ index))))
          ((char= character #\\)
           (incf index)
           (let ((escape (char string index)))
             (write-char (case escape
                           (#\n #\Newline) (#\r #\Return) (#\t #\Tab)
                           (#\b #\Backspace) (#\f #\Page)
                           (#\u (let ((code (parse-integer string :start (1+ index)
                                                                  :end (+ index 5)
                                                                  :radix 16)))
                                  (incf index 4)
                                  (code-char code)))
                           (t escape))
                         out)
             (incf index)))
          (t (write-char character out) (incf index)))))))

(defun read-json-value (string index)
  (let ((index (skip-whitespace string index)))
    (when (>= index (length string))
      (error 'json-error :message "value expected" :position index))
    (let ((character (char string index)))
      (cond
        ((char= character #\") (read-json-string string index))
        ((and (<= (+ index 4) (length string))
              (string= "true" string :start2 index :end2 (+ index 4)))
         (values t (+ index 4)))
        ((and (<= (+ index 5) (length string))
              (string= "false" string :start2 index :end2 (+ index 5)))
         (values nil (+ index 5)))
        ((and (<= (+ index 4) (length string))
              (string= "null" string :start2 index :end2 (+ index 4)))
         (values nil (+ index 4)))
        ((or (digit-char-p character) (char= character #\-))
         (let ((end index))
           (loop while (and (< end (length string))
                            (or (digit-char-p (char string end))
                                (find (char string end) ".-+eE")))
                 do (incf end))
           (let ((text (subseq string index end)))
             (values (if (find-if (lambda (c) (find c ".eE")) text)
                         (let ((*read-default-float-format* 'double-float))
                           ;; READ-FROM-STRING on a token already known to be
                           ;; numeric: the characters were checked above, so
                           ;; nothing else can be evaluated here.
                           (read-from-string text))
                         (parse-integer text))
                     end))))
        ;; Structure this does not support, and says so rather than guessing.
        ((or (char= character #\{) (char= character #\[))
         (error 'json-error
                :message "nested objects and arrays are not supported here"
                :position index))
        (t (error 'json-error :message (format nil "unexpected ~C" character)
                              :position index))))))

(defun read-json (string)
  "A flat JSON object as an alist of (KEY-STRING . VALUE).  Signals JSON-ERROR."
  (let ((index (expect-char string 0 #\{))
        (result '()))
    (setf index (skip-whitespace string index))
    (when (and (< index (length string)) (char= (char string index) #\}))
      (return-from read-json '()))
    (loop
      (multiple-value-bind (key next) (read-json-string string index)
        (setf index (expect-char string next #\:))
        (multiple-value-bind (value next) (read-json-value string index)
          (push (cons key value) result)
          (setf index (skip-whitespace string next))))
      (cond
        ((>= index (length string))
         (error 'json-error :message "unterminated object" :position index))
        ((char= (char string index) #\,) (incf index))
        ((char= (char string index) #\}) (return (nreverse result)))
        (t (error 'json-error
                  :message (format nil "expected , or } but found ~C"
                                   (char string index))
                  :position index))))))
