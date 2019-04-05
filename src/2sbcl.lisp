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

#|

function WIDETAG returns (unsigned-byte 9), which is one bit longer than
SB-VM:N-WIDETAG-BITS which is 8. The reason is that it encodes an additional information
of whether the array is complex or not.

|# 

(define-constant +widetag-bits+ SB-VM:N-WIDETAG-BITS) ; 8
(define-constant +table-size+ (expt 2 (1+ +widetag-bits+)))

(declaim (inline widetag))
(declaim (ftype (function (t) (unsigned-byte #.(1+ +widetag-bits+))) widetag))
(defun widetag (x)
  "This function returns something more than the simple widetags in SBCL.
When the array is a complex array with array-header (multidimensional or non-simple),
it obtains the value of SB-KERNEL:ARRAY-UNDERLYING-WIDETAG,
instead of directly returning the widetag for the complex array.
If the array is non-simple, it then sets the 9th bit. "
  ;; note: ranks are supposed to be fixed, thus the different ranks should
  ;; result in a type-error due to the type declaration.  Thus there is no need
  ;; to distinguish simple vectors and simple multidimensional arrays here.
  ;;
  ;; However, we do want to distinguish whether this is a simple array or a
  ;; non-simple array, because it affects the data-array access.
  ;; simple multidimensional arrays need a single %array-data,
  ;; while non-simple multidimensional arrays need more %array-data recursions.
  (etypecase x
    (simple-array
     ;; this way it can dispatch based on the
     ;; widetag of the storage-array, which contains
     ;; the element-type information.
     ;; In contrast, for arrays with array-headers
     ;; e.g. multidimentional or displaced arrays,
     ;; have a widetag of SB-VM:COMPLEX-ARRAY-WIDETAG (if it is multidimentional)
     ;; or SB-VM:COMPLEX-VECTOR-WIDETAG (if it is single-dimensional)
     ;; and therefore it is impossible to dispatch based on the element-type.
     (sb-kernel:array-underlying-widetag x))
    (array
     (logior (ash 1 +widetag-bits+)
             (sb-kernel:array-underlying-widetag x)))
    ;; 
    ;; lowered the following tests, since they tend not to be performance sensitive
    ;; and tests are linear
    ;; 
    ;; types covered by lowtags
    ;; I am assuming that these values are never returned by
    ;; array-underlying-widetag nor widetag-of
    (fixnum   0)
    (cons     1)                        ; note : nil is not a cons, treated as a symbol
    (function 2)
    (t
     (sb-kernel:widetag-of x))))

;; cf.
;; SB-KERNEL:**PRIMITIVE-OBJECT-LAYOUTS**
#+(or)
(iter (for x in-vector SB-KERNEL:**PRIMITIVE-OBJECT-LAYOUTS**)
      (for i from 0)
      (when (zerop (mod i 8))
        (Terpri))
      (print x))

(defun find-lexical-variables (env)
  (mapcar #'car
          (sb-c::lexenv-vars
           (sb-c::coerce-to-lexenv env))))

(declaim (inline table))
(defun table ()
  (make-array +table-size+
              :element-type '(or null function)
              :initial-element nil))

(deftype table () `(simple-array t (,+table-size+)))

#+(or)
(defun fn ()
  (let ((x (load-time-value (cons 0 0))))
    (incf (cdr x))
    (prog1 x
      (setf x 2)
      (print x))))

(defmacro specializing (args (&key verbose) &body decl-and-body &environment env)
  (assert (every #'symbolp args))
  (assert (typep verbose 'boolean))
  (with-gensyms (table)
    
    (let ((lexvars (set-difference (find-lexical-variables env) args))
          (widetags (make-gensym-list (length args))))
      
      `(let ((,table (load-time-value (table)))
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
                                       ',args ',lexvars ',decl-and-body (list ,@args)))))
                 (for default =
                      (cond
                        (rest `(table))
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
                         (or (aref (sb-ext:truly-the table ,table) ,v)
                             (setf (aref (sb-ext:truly-the table ,table) ,v)
                                   ,default)))))
         
         (funcall (sb-ext:truly-the
                   (function ,(mapcar (constantly t) (append args lexvars))
                             (values &optional))
                   ,table)
                  ,@args
                  ,@lexvars)))))


