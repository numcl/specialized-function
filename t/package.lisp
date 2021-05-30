#|
  This file is a part of specialized-function project.
  Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :specialized-function.test
  (:use :cl
        :specialized-function
        :fiveam
        :trivia :alexandria :iterate :lisp-namespace))
(in-package :specialized-function.test)



(def-suite :specialized-function)
(in-suite :specialized-function)

;; run test with (run! test-name) 

(defparameter *a* (make-array 5 :element-type 'fixnum))
(defparameter *b* (make-array '(5 5) :element-type 'fixnum))
(defparameter *a1* (make-array 5 :element-type 'fixnum :displaced-to *a*))
(defparameter *b1* (make-array 25 :element-type 'fixnum :displaced-to *b*))
(defparameter *b2* (make-array '(5 5) :element-type 'fixnum :displaced-to *b*))

(defparameter *c* (make-array 5 :element-type 'single-float))
(defparameter *d* (make-array '(5 5) :element-type 'single-float))
(defparameter *c1* (make-array 5 :element-type 'single-float :displaced-to *c*))
(defparameter *d1* (make-array 25 :element-type 'single-float :displaced-to *d*))
(defparameter *d2* (make-array '(5 5) :element-type 'single-float :displaced-to *d*))

;; *a* and *b* don't need to be distinguished due to the rank difference
;; same for *c* and *d*

(test (specialized-function :compile-at :run-time)
  (defun fn (a b)
    (print (specialized-function::widetag a))
    (print (specialized-function::widetag b))
    (unwind-protect-case ()
        (specializing (a b) ()
          (declare (optimize))
          (print (type-of a))
          (print (type-of b)))
      (:normal
       (print :ok))
      (:abort
       (print :ng))))

  ;; base case 
  (finishes
    (fn *a* *a*))
  (signals error
    (fn *a* *b*))
  (signals error
    (fn *b* *b*))
  (signals error
    (fn *b* *a*))

  (finishes
    (fn *a* *c*))                       ; should dispatch to the different
                                        ; functions because rank is same but
                                        ; element-type is different
  (finishes
    (fn *a* *c*))
  (finishes
    (fn *c* *c*))
  (finishes
    (fn *c* *a*))

  (finishes (fn *a* *a1*))              ; both should pass because they have the same rank
  (finishes (fn *a* *b1*))              ; (could be dispatched to different functions due to
                                        ; displacement

  (signals error
    (fn *a* *d*))                       ; should error due to rank difference
  (signals error
    (fn *d* *d*))
  (signals error
    (fn *d* *a*))

  (finishes                             ; should dispatch to different function
    (fn *a* *c1*))                      ; (fixnum/single-float arrays, same rank)
  (finishes
    (fn *c1* *c1*))
  (finishes
    (fn *c1* *a*))
  (finishes
    (fn *d1* *c1*))
  (finishes
    (fn *c1* *c1*))
  (finishes
    (fn *c1* *d1*))
  (finishes
    (fn *d1* *d1*)))

(define-condition compiled () ())

(test (lowtag-dispatch :compile-at :run-time)
  (defun fn (a b)
    (let ((flag nil))
      (handler-bind ((compiled
                      (lambda (c)
                        (setf flag t))))
        (specializing (a b) ()
          (macrolet ((m ()
                       (signal 'compiled)))
            (m))
          :ok))
      flag))

  ;; base case
  (is-true (fn 0 0) "the first call should cause compilation")
  (iter (repeat 10)
        (for a = (random most-positive-fixnum))
        (for b = (random most-positive-fixnum))
        (is-false (fn a b) "the later calls should not cause compilation. args:~%~A~%~A" a b))

  (is-true (fn nil nil)
           "the first call should cause compilation")
  (is-false (fn nil nil)
            "the later calls should not cause compilation")
  (iter (repeat 10)
        (for a = (cons (random most-positive-fixnum)
                       (random most-positive-fixnum)))
        (for b = (cons (random most-positive-fixnum)
                       (random most-positive-fixnum)))
        (for c = (cons (random most-positive-fixnum)
                       (random most-positive-fixnum)))
        (for d = (cons (random most-positive-fixnum)
                       (random most-positive-fixnum)))
        (if (first-iteration-p)
            (progn
              (is-true (fn a nil) "the first call should cause compilation")
              (is-true (fn nil b) "the first call should cause compilation")
              (is-true (fn c d)   "the first call should cause compilation"))
            (progn
              (is-false (fn a nil) "the later calls should not cause compilation. args:~%~A~%~A" a nil)
              (is-false (fn nil b) "the later calls should not cause compilation. args:~%~A~%~A" nil b)
              (is-false (fn c d)   "the later calls should not cause compilation. args:~%~A~%~A" c d)))))





#+sbcl
(flet ((pair (name)
         (when-let ((s (find-symbol name :sb-vm)))
           (when (boundp s)
             (cons (symbol-value s) s)))))
  (defparameter *widetags*
    (iter (for name in
               '("BIGNUM-WIDETAG"
                 "CHARACTER-WIDETAG"
                 "CLOSURE-WIDETAG"
                 "CODE-HEADER-WIDETAG"
                 "COMPLEX-ARRAY-WIDETAG"                ; this is an array
                 "COMPLEX-BASE-STRING-WIDETAG"          ; this is an array
                 "COMPLEX-BIT-VECTOR-WIDETAG"           ; this is an array
                 "COMPLEX-CHARACTER-STRING-WIDETAG"     ; this is an array
                 "COMPLEX-DOUBLE-FLOAT-WIDETAG"         ; this is a complex number
                 "COMPLEX-SINGLE-FLOAT-WIDETAG"         ; this is a complex number
                 "COMPLEX-VECTOR-NIL-WIDETAG"           ; this is an array          2021/05/30 removed in sbcl-2.0.10 https://github.com/sbcl/sbcl/commit/8a2df46b46e3feb201c7f19096018a3f4b1c7d8a
                 "COMPLEX-VECTOR-WIDETAG"               ; this is an array
                 "COMPLEX-WIDETAG"                      ; this is a complex number
                 "DOUBLE-FLOAT-WIDETAG"
                 "FDEFN-WIDETAG"
                 "FILLER-WIDETAG"
                 "FUNCALLABLE-INSTANCE-WIDETAG"
                 "INSTANCE-WIDETAG"
                 "N-WIDETAG-BITS"
                 "NO-TLS-VALUE-MARKER-WIDETAG"
                 "RATIO-WIDETAG"
                 "SAP-WIDETAG"
                 "SIMD-PACK-256-WIDETAG"
                 "SIMD-PACK-WIDETAG"
                 "SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT-WIDETAG"
                 "SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT-WIDETAG"
                 "SIMPLE-ARRAY-DOUBLE-FLOAT-WIDETAG"
                 "SIMPLE-ARRAY-FIXNUM-WIDETAG"
                 "SIMPLE-ARRAY-NIL-WIDETAG"
                 "SIMPLE-ARRAY-SIGNED-BYTE-16-WIDETAG"
                 "SIMPLE-ARRAY-SIGNED-BYTE-32-WIDETAG"
                 "SIMPLE-ARRAY-SIGNED-BYTE-64-WIDETAG"
                 "SIMPLE-ARRAY-SIGNED-BYTE-8-WIDETAG"
                 "SIMPLE-ARRAY-SINGLE-FLOAT-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-BYTE-15-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-BYTE-16-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-BYTE-2-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-BYTE-31-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-BYTE-32-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-BYTE-4-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-BYTE-63-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-BYTE-64-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-BYTE-7-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-BYTE-8-WIDETAG"
                 "SIMPLE-ARRAY-UNSIGNED-FIXNUM-WIDETAG"
                 "SIMPLE-ARRAY-WIDETAG"
                 "SIMPLE-BASE-STRING-WIDETAG"
                 "SIMPLE-BIT-VECTOR-WIDETAG"
                 "SIMPLE-CHARACTER-STRING-WIDETAG"
                 "SIMPLE-FUN-WIDETAG"
                 "SIMPLE-VECTOR-WIDETAG"
                 "SINGLE-FLOAT-WIDETAG"
                 "SYMBOL-WIDETAG"
                 "UNBOUND-MARKER-WIDETAG"
                 "VALUE-CELL-WIDETAG"
                 "WEAK-POINTER-WIDETAG"))
      (for pair = (pair name))
      (when pair
        (collecting pair into pairs))
      (finally
       (return (sort pairs #'< :key #'car)))))
  (format t "~{~a~%~}" *widetags*))

#+sbcl
(progn
  (format t "~&~4a : ~4a ~35a ~4a ~35a ~4a"
          'arg
          'tag
          'name
          'saet
          'name
          'specialized-tag)
  (dolist (a `(*a* *b* *a1* *b1* *b2*
                   *c* *d* *c1* *d1* *d2*))
    (let ((v (symbol-value a)))
      (format t "~&~4a : ~4a ~35a ~4a ~35a ~4a"
              a
              (sb-kernel:widetag-of v)
              (cdr (assoc (sb-kernel:widetag-of v) *widetags*))
              (sb-kernel:array-underlying-widetag v)
              (cdr (assoc (sb-kernel:array-underlying-widetag v) *widetags*))
              (specialized-function::widetag v)))))
