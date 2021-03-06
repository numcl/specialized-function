#|

This file is a part of NUMCL project.
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

NUMCL is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation,either version 3 of the License, or (at your option) any
later version.

NUMCL is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
NUMCL.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :specialized-function)

(setf (documentation 'last-specialized-function 'function)
      "The symbol is fbound to the last function that was compiled by SPECILIZING1 macro, for debugging/tuning purpose.")


(defparameter +table-size+ 0 "the size of the table.")
(defparameter *base-types* (make-array 64 :adjustable t :fill-pointer 0))

(defun array-element-types ()
  (stable-sort (remove-duplicates
                (mapcar #'upgraded-array-element-type
                        (concatenate
                         'list
                         (map-product #'list
                                      '(unsigned-byte signed-byte)
                                      (iota 128 :start 1))
                         *base-types*))
                :test #'type=)
               #'subtypep))

;; this is commented out because it increases the compilation time significantly
#+(or)
(declaim (inline widetag))
(defvar *rebuild-widetag* nil)
(defun rebuild-widetag () (eval (widetag-lambda)))
(defun register-base-type (type &optional (*rebuild-widetag* *rebuild-widetag*))
  (unless (find type *base-types* :test 'alexandria:type=)
    (vector-push-extend type *base-types* (max 1 (length *base-types*))))
  (when *rebuild-widetag*
    (rebuild-widetag)))

(defun widetag-lambda ()
  (let ((count -1)
        (array-count (length (array-element-types))))
    (values
     `(progn
        (defun widetag (x)
          "This function returns an integer based on the type of the object."
          (declare (optimize (speed 2) (debug 0)))
          (labels ((rec (x)
                     (etypecase x
                       ,@(mapcar (lambda (type) `((simple-array ,type) ,(incf count)))
                                 (array-element-types))
                       (array
                        ;; access the underlying 1D simple array.
                        ;; purpose: to reduce the function size.
                        #+sbcl
                        ,(progn
                           (incf count array-count)
                           `(+ ,array-count (the (integer 0 (,array-count))
                                                 (sb-kernel:with-array-data ((v x) (s) (e))
                                                   (declare (ignore s e))
                                                   (rec v)))))
                        #-sbcl
                        (etypecase x
                          ,@(mapcar (lambda (type) `((array ,type) ,(incf count)))
                                    (array-element-types))))

                       ;; initial implementation: just enumerate typecase
                       #+(or)
                       ,@(map 'list (lambda (type) `(,type ,(incf count)))
                              *base-types*)

                       ;; second implementation: optimize for widetags
                       ;; #+(or)
                       ,@`((fixnum
                            ;; fixnum tends to be detected by lowtags: both sbcl and ccl.
                            ;; so let's detect it first.
                            ,(incf count))
                           (number
                            ;; on sbcl, widetags for the similar types are grouped
                            ;; together and can be tested in one check. I expect the
                            ;; same for CCL.
                            (etypecase x
                              (real
                               (etypecase x
                                 (rational
                                  (etypecase x
                                    (integer
                                     (etypecase x
                                       ,@(map 'list (lambda (type) `(,type ,(incf count)))
                                              (remove 'fixnum (remove-if-not #'integer-subtype-p *base-types*)))))
                                    (ratio ,(incf count))))
                                 (float
                                  (etypecase x
                                    ,@(map 'list (lambda (type) `(,type ,(incf count)))
                                           (remove-if-not #'float-subtype-p *base-types*))))))
                              (complex
                               (etypecase x
                                 ,@(map 'list (lambda (type) `(,type ,(incf count)))
                                        (remove-if-not #'complex-type-p *base-types*))))))
                           ,@(map 'list (lambda (type) `(,type ,(incf count)))
                                  (remove-if #'number-subtype-p *base-types*))))))
            (rec x)))
        (defparameter +table-size+ ,(incf count))))))

;; benchmark

(defparameter *benchmark-size*
  #+sbcl 1000000
  #+ccl 10000
  #-(or sbcl ccl) 10000)

(defun benchmark ()
  (rebuild-widetag)
  (let ((array
         (vector 100
                 (1+ most-positive-fixnum)
                 1/2
                 0.0s0
                 0.0f0
                 0.0d0
                 0.0l0
                 #C(0.0s0 0.0s0)
                 #C(0.0f0 0.0f0)
                 #C(0.0d0 0.0d0)
                 #C(0.0l0 0.0l0)
                 #C(1 1)
                 #C(1/2 1/2)
                 #\c
                 'a
                 nil
                 (cons 1 2)
                 (lambda (x) x)
                 (make-array 1 :element-type t)
                 (make-array 1 :element-type 'fixnum)
                 (make-array 1 :element-type 'single-float)
                 (make-array 1 :element-type 'double-float)
                 (make-array 1 :element-type 'bit)
                 (make-array 1 :element-type 'character)
                 (make-array 1 :element-type t             :displaced-to (make-array 1 :element-type t))
                 (make-array 1 :element-type 'fixnum       :displaced-to (make-array 1 :element-type 'fixnum))
                 (make-array 1 :element-type 'single-float :displaced-to (make-array 1 :element-type 'single-float))
                 (make-array 1 :element-type 'double-float :displaced-to (make-array 1 :element-type 'double-float))
                 (make-array 1 :element-type 'bit          :displaced-to (make-array 1 :element-type 'bit))
                 (make-array 1 :element-type 'character    :displaced-to (make-array 1 :element-type 'character))
                 (make-array '(2 2) :element-type t)
                 (make-array '(2 2) :element-type 'fixnum)
                 (make-array '(2 2) :element-type 'single-float)
                 (make-array '(2 2) :element-type 'double-float)
                 (make-array '(2 2) :element-type 'bit)
                 (make-array '(2 2) :element-type 'character)
                 (make-array '(2 2) :element-type t             :displaced-to (make-array '(2 2) :element-type t))
                 (make-array '(2 2) :element-type 'fixnum       :displaced-to (make-array '(2 2) :element-type 'fixnum))
                 (make-array '(2 2) :element-type 'single-float :displaced-to (make-array '(2 2) :element-type 'single-float))
                 (make-array '(2 2) :element-type 'double-float :displaced-to (make-array '(2 2) :element-type 'double-float))
                 (make-array '(2 2) :element-type 'bit          :displaced-to (make-array '(2 2) :element-type 'bit))
                 (make-array '(2 2) :element-type 'character    :displaced-to (make-array '(2 2) :element-type 'character)))))
    (loop repeat 5
       do
         (time
          (loop repeat (the fixnum *benchmark-size*) do
               (loop for e across array do
                    (widetag e)))))))


(setf *rebuild-widetag* nil)
;; base number types
(register-base-type 'fixnum)
#+64-bit
(register-base-type '(unsigned-byte 64))
#+32-bit
(register-base-type '(unsigned-byte 32))
#+64-bit
(register-base-type '(signed-byte 64))
#+32-bit
(register-base-type '(signed-byte 32))
(register-base-type 'bignum)
(register-base-type 'ratio)
(register-base-type 'short-float)
(register-base-type 'single-float)
(register-base-type 'double-float)
(register-base-type 'long-float)

;; due to the bug in sbcl, below are not the realistic set of types
;; (register-base-type `(complex ,(upgraded-complex-part-type 'fixnum))) ; could be (complex t)
;; (register-base-type `(complex ,(upgraded-complex-part-type '(unsigned-byte 64))))
;; (register-base-type `(complex ,(upgraded-complex-part-type '(unsigned-byte 32))))
;; (register-base-type `(complex ,(upgraded-complex-part-type '(signed-byte 64))))
;; (register-base-type `(complex ,(upgraded-complex-part-type '(signed-byte 32))))
;; (register-base-type `(complex ,(upgraded-complex-part-type 'bignum)))
;; (register-base-type `(complex ,(upgraded-complex-part-type 'ratio)))
(register-base-type `(complex ,(upgraded-complex-part-type 'short-float)))
(register-base-type `(complex ,(upgraded-complex-part-type 'single-float)))
(register-base-type `(complex ,(upgraded-complex-part-type 'double-float)))
(register-base-type `(complex ,(upgraded-complex-part-type 'long-float)))
(register-base-type `(complex ,(upgraded-complex-part-type 'real)))

(register-base-type 'base-char)
(register-base-type 'extended-char)
(register-base-type 'function)
(register-base-type 'cons)
(register-base-type 'symbol)
(register-base-type 'structure-object)
(register-base-type 'standard-object)
(register-base-type t
                    t)                 ;rebuild widetags
(setf *rebuild-widetag* t)

(declaim (inline upgraded-object-type))
(defun upgraded-object-type (x)
  "Takes an object and returns a reasonable type declaration for the object.
Analogous to upgraded-array-element-type, but works on an object."
  (etypecase x
    ;; aggregate all numbers within fixnum range.
    ;; For example, (type-of 5) may return any valid supertype e.g. FIXNUM, (integer 5 5), (integer 0 5).
    ;; Same goes with bignums.
    (fixnum       'fixnum)
    #+64-bit
    ((unsigned-byte 64) '(unsigned-byte 64))
    #+32-bit
    ((unsigned-byte 32) '(unsigned-byte 32))
    #+64-bit
    ((signed-byte 64) '(signed-byte 64))
    #+32-bit
    ((signed-byte 32) '(signed-byte 32))
    (bignum       'bignum)
    (ratio        'ratio)
    (short-float  'short-float)
    (single-float 'single-float)
    (double-float 'double-float)
    (long-float   'long-float)

    ((complex fixnum)       '(complex fixnum))
    ((complex bignum)       '(complex bignum))
    ((complex ratio)        '(complex ratio))
    ((complex short-float)  '(complex short-float))
    ((complex single-float) '(complex single-float))
    ((complex double-float) '(complex double-float))
    ((complex long-float)   '(complex long-float))
    (complex 'complex)

    (base-char     'base-char)
    (extended-char 'extended-char)
    
    ;; Arrays: Ignore dimensions, but keep the rank.
    ;; Rationale:
    ;; It is common to feed arrays of the different element types to the same algorithm.
    ;; It needs to be recompiled for each element type, though.
    ;; 
    ;; It is also common to feed arrays of the different sizes to the same algorithm.
    ;; It does not need to be recompiled, as it only reduces the termination check.
    ;; 
    ;; It is uncommon to feed arrays of the different ranks to the same algorithm.
    ;; If it really happens, the number of loops should be altered.
    (simple-array
     `(simple-array ,(array-element-type x)
                    ,(array-rank x)))
    (array
     `(array        ,(array-element-type x)
                    ,(array-rank x)))
    ;; Aggregate all structure/standard/funcallable object classes.
    ;; don't specialize on classes; no use, and also could be potentially wrong.
    ;; for example, the specific instance given in the first invocation could be a subclass.
    (structure-object   'structure-object) 
    (standard-object    'standard-object)
    (function           'function)
    ;; Aggregate all subtypes of symbols: keyword, null
    (symbol 'symbol)
    ;; Aggregate all subtypes of cons: (cons fixnum fixnum) etc.
    (cons   'cons)
    (t (error "cannot confidently aggregate the type from the instance ~a!
type-of says ~a but there could be supertypes that are compatible to this function"
              x
              (type-of x)))))


(defun specialized-function-form (vars lexvars lexvars-types decl-and-body vals)
  (multiple-value-bind (body decls) (parse-body decl-and-body)
    `(lambda (,@vars ,@lexvars)
       (declare (ignorable ,@lexvars))
       ,@(mapcar (lambda (var val)
                   `(declare (type ,(upgraded-object-type val) ,var)))
                 vars
                 vals)
       ,@(mappend (lambda (var type)
                    (when type
                      `((declare (type ,(cdr type) ,var)))))
                  lexvars
                  lexvars-types)
       ,@decls
       #-sbcl
       ,@(mapcar (lambda (var val)
                   `(check-type ,var ,(upgraded-object-type val)))
                 vars
                 vals)
       ,@body)))

#+(or)
(print (specialized-function-form 'x nil '((* 2 X)) 5))

;; debug macro
(defmacro in-compile-time ((&optional env) &body body)
  "macro for debugging"
  (with-gensyms (macro)
    `(macrolet ((,macro (&environment ,env)
                  ,@body))
       (,macro))))



(declaim (inline table))
(defun table (size)
  (make-array size
              :element-type '(or null function)
              :initial-element nil))

(deftype table (size) `(simple-array t (,size)))

(defmacro specializing (args (&key verbose time) &body decl-and-body &environment env)
  (assert (every #'symbolp args))
  (assert (typep verbose 'boolean))
  (with-gensyms (table)
    
    (let* ((lexvars (set-difference (find-lexical-variables env) args))
           (lexvars-types (mapcar (lambda (var) (assoc 'type (nth-value 2 (cltl2:variable-information var env))))
                                  lexvars))
           (widetags (make-gensym-list (length args))))
      
      `(let ((,table (load-time-value (table ,+table-size+)))
             ,@(mapcar (lambda (var arg)
                         `(,var (widetag ,arg)))
                       widetags
                       args))
         (declare (optimize (speed 3) (debug 0) (safety 0)))

         ,@(iter (for (v . rest) on widetags)
                 (for compile-and-set =
                      `(setf (symbol-function 'last-specialized-function)
                             (compile nil
                                      (specialized-function-form
                                       ',args ',lexvars ',lexvars-types ',decl-and-body (list ,@args)))))
                 (for default =
                      (cond
                        (rest `(table ,+table-size+))
                        (verbose
                         `(locally
                              (declare (optimize (debug 1) (speed 1) (safety 1)))
                            (format t "~&~<; ~@;Specializing ~a to ~{~a~^ ~}~:>"
                                    (list ',args (mapcar #'upgraded-object-type (list ,@args))))
                            ,compile-and-set))
                        (t
                         compile-and-set)))
                 
                 (collecting
                  `(setf ,table
                         (or (aref (the (table ,+table-size+) ,table) ,v)
                             (setf (aref (the (table ,+table-size+) ,table) ,v)
                                   ,default)))))
         (,(if time 'time 'progn)
         (funcall (the
                   (function ,(mapcar (constantly t) (append args lexvars)) *)
                   ,table)
                  ,@args
                  ,@lexvars))))))
