;;;; src/metal/uniforms.lisp -- uniform blocks, laid out once.
;;;;
;;;; A shader's `constant' struct and the Lisp that fills it are two
;;;; descriptions of the same bytes, and nothing checks that they agree.  Get an
;;;; offset wrong and there is no error: a float lands in the neighbouring
;;;; field, the picture is subtly wrong, and the thing that looks wrong is never
;;;; the thing that is.
;;;;
;;;; So the layout is computed HERE, from a declaration, using Metal's rules --
;;;; float4 aligns to 16, float2 to 8, float to 4, and the struct to its widest
;;;; member.  The shader declares its fields in the same order, and
;;;; tests/metal-tests.lisp writes every field from Lisp and reads it back
;;;; through a compute kernel, which is the only way to know the two agree.

(in-package #:cathode-ray-tube.metal)

(defparameter +uniform-types+
  '((:float  . (4 . 4))
    (:float2 . (8 . 8))
    (:float3 . (16 . 16))     ; a float3 occupies float4's space, as in C
    (:float4 . (16 . 16))
    (:int    . (4 . 4))
    (:uint   . (4 . 4)))
  "TYPE -> (SIZE . ALIGNMENT), in bytes.")

(defun uniform-type-size (type) (car (cdr (assoc type +uniform-types+))))
(defun uniform-type-alignment (type) (cdr (cdr (assoc type +uniform-types+))))

(defun compute-uniform-layout (fields)
  "(NAME TYPE)... -> (values ALIST TOTAL-SIZE), ALIST being (NAME TYPE OFFSET)."
  (let ((offset 0) (alignment 4) (layout '()))
    (dolist (field fields)
      (destructuring-bind (name type) field
        (let ((size (or (uniform-type-size type)
                        (error "Unknown uniform type ~S." type)))
              (align (uniform-type-alignment type)))
          (setf offset (* align (ceiling offset align))
                alignment (max alignment align))
          (push (list name type offset) layout)
          (incf offset size))))
    ;; The struct's own size is rounded up to its alignment, which matters
    ;; because setFragmentBytes: is told a length and Metal reads that many.
    (values (nreverse layout) (* alignment (ceiling offset alignment)))))

(defmacro define-uniform-block (name &body fields)
  "Declare a uniform block, generating its size and field offsets.

    (define-uniform-block dynamic-uniforms
      (:font-color :float4)
      (:time :float))

gives +DYNAMIC-UNIFORMS-SIZE+, DYNAMIC-UNIFORMS-LAYOUT, and makes the block
usable with WITH-UNIFORMS and UREF."
  (multiple-value-bind (layout size) (compute-uniform-layout fields)
    `(progn
       (defparameter ,(intern (format nil "+~A-SIZE+" name)) ,size)
       (defparameter ,(intern (format nil "~A-LAYOUT" name)) ',layout)
       (setf (gethash ',name *uniform-blocks*) (list ',layout ,size))
       ',name)))

(defvar *uniform-blocks* (make-hash-table :test 'eq))

(defun uniform-block-layout (name)
  (or (first (gethash name *uniform-blocks*))
      (error "No uniform block named ~S." name)))

(defun uniform-block-size (name)
  (or (second (gethash name *uniform-blocks*))
      (error "No uniform block named ~S." name)))

(defun uniform-offset (block field)
  (let ((entry (assoc field (uniform-block-layout block))))
    (unless entry
      (error "Uniform block ~S has no field ~S." block field))
    (values (third entry) (second entry))))

(defun (setf uref) (value pointer block field)
  "Write VALUE into POINTER at FIELD's offset.

A scalar for :FLOAT, and a list or vector of the right length for the vector
types -- which is checked, because a two-element list handed to a :FLOAT4 field
would otherwise leave the last two components holding whatever was there
before."
  (multiple-value-bind (offset type) (uniform-offset block field)
    (flet ((put (index number)
             (setf (cffi:mem-ref pointer :float (+ offset (* 4 index)))
                   (float number 1.0))))
      (ecase type
        ((:float) (put 0 value))
        ((:int :uint)
         (setf (cffi:mem-ref pointer :int32 offset) (round value)))
        ((:float2 :float3 :float4)
         (let* ((count (ecase type (:float2 2) (:float3 3) (:float4 4)))
                (values (coerce value 'vector)))
           (unless (= (length values) count)
             (error "~S is a ~S and wants ~D components, got ~D: ~S"
                    field type count (length values) value))
           (dotimes (i count) (put i (aref values i)))))))
    value))

(defun uref (pointer block field)
  (multiple-value-bind (offset type) (uniform-offset block field)
    (flet ((get-float (index)
             (cffi:mem-ref pointer :float (+ offset (* 4 index)))))
      (ecase type
        ((:float) (get-float 0))
        ((:int :uint) (cffi:mem-ref pointer :int32 offset))
        ((:float2) (list (get-float 0) (get-float 1)))
        ((:float3) (list (get-float 0) (get-float 1) (get-float 2)))
        ((:float4) (list (get-float 0) (get-float 1) (get-float 2) (get-float 3)))))))

(defmacro with-uniforms ((accessor block) &body body)
  "Stack-allocate BLOCK's bytes, zeroed, and bind ACCESSOR to reach them.

    (with-uniforms (u dynamic-uniforms)
      (setf (u :time) 1.0)
      (bind-uniforms encoder (u) 'dynamic-uniforms 0))

(U :FIELD) reads, (SETF (U :FIELD) x) writes, and (U) with no argument is the
pointer itself, for handing to BIND-UNIFORMS.

ACCESSOR IS THE CALLER'S SYMBOL and that is the point.  A MACROLET binds a
symbol, not a name: one defined here would be CRT.METAL::U, while a caller in
another package writes its own U and gets `the function (SETF U) is undefined'
-- an error that names the symptom, mentions no macro, and sends you looking for
a missing setf function that was never the problem.

Zeroed matters too: a field the caller forgets to set is 0 rather than whatever
was on the stack, which makes an omission a reliable black screen instead of an
intermittent one."
  (let ((size (gensym "SIZE"))
        (pointer (gensym "UNIFORMS")))
    `(let ((,size (uniform-block-size ',block)))
       (cffi:with-foreign-object (,pointer :uint8 ,size)
         (dotimes (i ,size) (setf (cffi:mem-aref ,pointer :uint8 i) 0))
         ;; Built with LIST rather than a nested backquote: three levels deep is
         ;; where this stops being readable and starts being wrong.
         (macrolet ((,accessor (&optional field)
                      (if field
                          (list 'uref ',pointer '',block field)
                          ',pointer)))
           ,@body)))))

(defun bind-uniforms (encoder pointer block index &key (stage :fragment))
  (let ((size (uniform-block-size block)))
    (ecase stage
      (:fragment (bind-fragment-bytes encoder pointer size index))
      (:vertex (bind-vertex-bytes encoder pointer size index)))))
