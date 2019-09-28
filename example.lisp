
(in-package :specialized-function)

(defun *2 (x)
  (specializing (x) (:verbose t)
    (* 2 x)))

(print (*2 5))
(print (*2 5))
(print (*2 5.0))
(print (*2 5.0d0))


(defun add (x y)
  (specializing (x y) (:verbose t)
    (+ x y)))

(print (add 2 5))
(print (add 2 5.0))
(print (add 2.0 5.0))
(print (add 2.0d0 5.0))
(print (add 2.0d0 5.0d0))

(defun addp (a x y)
  (specializing (x y) (:verbose t)
    (print a)
    (+ x y)))

(print (addp "a" 2 5))
(print (addp "a" 2 5.0))
(print (addp "a" 2.0 5.0))
(print (addp "a" 2.0d0 5.0))
(print (addp "a" 2.0d0 5.0d0))

;; bad example, sum could be out of fixnum range for fixnum, ended up calling generic-+
(defun sum (a)
  (specializing (a) (:verbose t)
    (declare (optimize (speed 3)))
    (let ((sum (aref a 0)))
      (loop for i from 1 below (length a)
         do (incf sum (aref a i)))
      sum)))

;; (disassemble #'last-specialized-function)

;; better example, sum is specialized; when sum gets out of fixnum range,
;; it becomes a runtime error
(defun sum (a)
  (let ((sum (aref a 0)))
    (specializing (a sum) (:verbose t)
      (declare (optimize (speed 3)))
      (loop for i from 1 below (length a)
         do (incf sum (aref a i)))
      sum)))

;; (disassemble #'last-specialized-function)

;; There is no way to tell the compiler that the type of SUM is same as the type of the array!
;; To truly solve this issue, you need the ability to derive the type of a variable from another variable.
;; This is not available within common lisp, but if you hack LET/LET* it is possible.
;; See https://github.com/numcl/numcl/blob/master/src/0specialops.lisp

(print (sum #(1 1 1)))
(print (sum (make-array 1000 :element-type 'fixnum :initial-element 1)))
(print (sum (make-array 1000 :element-type 'single-float :initial-element 1.0)))
(print (sum (make-array 1000 :element-type 'double-float :initial-element 1.0d0)))

;; (disassemble #'last-specialized-function)

(defun dot-original (a b c)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop
     for ai across a
     for bi across b
     for i fixnum from 0
     do (incf c (* ai bi)))
  c)

(defun dot-specialized (a b c)
  (specializing (a b c) (:verbose t)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop
     for ai across a
     for bi across b
     for i fixnum from 0
     do (incf c (* ai bi)))
    c))

;; does not have the dispatch overhead
(defun dot-handtuned (a b c)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           ((simple-array single-float 1) a b)
           (single-float c))
  (loop
     for ai across a
     for bi across b
     for i fixnum from 0
     do (incf c (* ai bi)))
  c)

(defun gc ()
  #+sbcl
  (sb-ext:gc)
  #+ccl
  (ccl:gc))

(defun benchmark ()
  (let ((a (make-array 100000 :element-type 'single-float :initial-element 2.0))
        (b (make-array 100000 :element-type 'single-float :initial-element 2.0)))
    (format t "~%~@{~a~^~%~}"
            (machine-type)
            (machine-version)
            (machine-instance)
            (software-type)
            (software-version)
            (lisp-implementation-type)
            (lisp-implementation-version))
    (gc)
    (time (dot-original a b 0.0))
    (gc)
    (time (dot-original a b 0.0))
    (gc)
    (time (loop repeat 1000 do (dot-original a b 0.0)))
    (format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (gc)
    (time (dot-specialized a b 0.0))
    (gc)
    (time (dot-specialized a b 0.0))
    (gc)
    (time (loop repeat 1000 do (dot-specialized a b 0.0)))
    (format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (gc)
    (time (dot-handtuned a b 0.0))
    (gc)
    (time (dot-handtuned a b 0.0))
    (gc)
    (time (loop repeat 1000 do (dot-handtuned a b 0.0)))))

(benchmark)

#|
Example statistics on:

  X86-64
  Intel(R) Core(TM) i5-6300U CPU @ 2.40GHz
  masataro-ThinkPad-T460
  Linux
  4.4.0-146-generic
  SBCL
  1.5.2

Evaluation took:
  0.003 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  6,966,642 processor cycles
  0 bytes consed
  
Evaluation took:
  0.003 seconds of real time
  0.004000 seconds of total run time (0.004000 user, 0.000000 system)
  133.33% CPU
  5,979,642 processor cycles
  0 bytes consed
  
Evaluation took:
  1.832 seconds of real time
  1.832000 seconds of total run time (1.832000 user, 0.000000 system)
  100.00% CPU
  4,574,515,126 processor cycles
  0 bytes consed
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Specializing (A B C) to (SIMPLE-ARRAY SINGLE-FLOAT 1) (SIMPLE-ARRAY
;                                                        SINGLE-FLOAT
;                                                        1) SHORT-FLOAT
Evaluation took:
  0.003 seconds of real time
  0.004000 seconds of total run time (0.004000 user, 0.000000 system)
  133.33% CPU
  21 lambdas converted
  7,954,610 processor cycles
  654,304 bytes consed
  
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  529,310 processor cycles
  0 bytes consed
  
Evaluation took:
  0.208 seconds of real time
  0.212000 seconds of total run time (0.212000 user, 0.000000 system)
  101.92% CPU
  518,996,152 processor cycles
  0 bytes consed
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  435,056 processor cycles
  0 bytes consed
  
Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  575,544 processor cycles
  0 bytes consed
  
Evaluation took:
  0.205 seconds of real time
  0.204000 seconds of total run time (0.204000 user, 0.000000 system)
  99.51% CPU
  511,326,866 processor cycles
  0 bytes consed

|#
