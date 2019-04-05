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

;; See https://ccl.clozure.com/ftp/pub/mcldoc/mcldoc.pdf for the description of typecode in CCL
;;

;; An object's \emph{typecode} is the value of its lisptag if that value is other
;; than ppc::tag-misc and the value of the uvector's header's subtag otherwise.
;; Determining an object's typecode therefore takes about 5 PPC instructions:

;; note : lisptag is 2bits

;; A header (immheader or nodeheader) can't appear anywhere where a boxed value
;; can (other than as the first word of a uvector.) There are no lisp primitives
;; or compiler operations that return headers as values or cause them to appear
;; in boxed machine registers. A uvector's header is a 32-bit value whose upper
;; 24 bits denote the uvector's length (in elements) and whose low 8 bits (called
;; the \emph{subtag}) denote the uvector's type. (Uvectors can therefore contain
;; at most (2\textasciicircum{}24)-1 elements.)

;; note : subtag is 8 bits
;; therefore, typecode is 8 bits

(declaim (inline widetag))
(defun widetag (x)
  (ccl::typecode x))

(define-constant +widetag-bits+ 8)
(define-constant +table-size+ (expt 2 +widetag-bits+))

;; see ccl:variable-information for details

(defun find-lexical-variables (env)
  (let ((vars (ccl::lexenv.variables env)))
    (when (listp vars)
      (append (iter (for v in vars)
                    (for bits = (ccl::var-bits v))
                    (when (and (typep bits 'integer)
			       (not (logbitp ccl::$vbittemporary bits))
			       ;; (not (logbitp ccl::$vbitignoreunuused bits))
			       ;; (not (logbitp ccl::$vbitignore bits))
			       ;; (not (logbitp ccl::$vbitspecial bits))
                               )
                      (collect (ccl::var-name v))))
              (find-lexical-variables (ccl::lexenv.parent-env env))))))

#+(or)
(defun fn (x y)
  (declare (fixnum x))
  (in-compile-time (env)
    (print (find-lexical-variables env))
    nil)
  (print x)
  (print y))

(declaim (inline table))
(defun table ()
  (make-array (expt 2 +widetag-bits+)
              :element-type '(or null function)
              :initial-element nil))

(deftype table () `(simple-array t (,+table-size+)))

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
                         (or (aref (the table ,table) ,v)
                             (setf (aref (the table ,table) ,v)
                                   ,default)))))
         
         (funcall (the
                   (function ,(mapcar (constantly t) (append args lexvars))
                             (values &optional))
                   ,table)
                  ,@args
                  ,@lexvars)))))
