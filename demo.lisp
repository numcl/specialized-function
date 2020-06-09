
(in-package :specialized-function)

(declaim (inline dot-original dot-specialized dot-handtuned))

;; untyped version
(defun dot-original (a b c)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop
     for ai across a
     for bi across b
     for i fixnum from 0
     do (incf c (* ai bi)))
  c)

;; specialized-function
(defun dot-specialized (a b c)
  (specializing (a b c) (:verbose t)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop
       for ai across a
       for bi across b
       for i fixnum from 0
       do (incf c (* ai bi)))
    c))

;; hand-tuned : does not have the dispatch overhead
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
  (ccl:gc)
  #+ecl
  (si:gc))

(defun benchmark ()
  (let ((a (make-array 100000 :element-type 'single-float :initial-element 2.0))
        (b (make-array 100000 :element-type 'single-float :initial-element 2.0)))
    ;; iterate over 10000 element arrays
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
    (time (loop repeat 1000 do (dot-original a b 0.0)))
    (format t ";; Super slow!!! untyped version")
    (format t "~%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (gc)
    (time (dot-specialized a b 0.0))
    (format t ";; ^^^^^^^^^^^ The first invocation is slow due to compilation~%")
    (gc)
    (time (dot-specialized a b 0.0))
    (format t ";; ^^^^^^^^^^^ The second+ invocations are fast~%")
    (gc)
    ;; dispatch happens 1000 times
    (time (loop repeat 1000 do (dot-specialized a b 0.0)))
    (format t "~%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (gc)
    (time (dot-handtuned a b 0.0))
    (gc)
    (time (loop repeat 1000 do (dot-handtuned a b 0.0)))
    (format t "~%;; ^^^^^^^^^^^ The difference is negligeble --- dispatch overhead is small enough!~%")))

(benchmark)


(format t "~%;; can see the last function that was compiled")
(disassemble 'last-specialized-function)
;; Size: 192 bytes

(format t "~%;; The code is almost identical, achieving the same performance")
(disassemble 'dot-handtuned)
;; Size: 196 bytes

