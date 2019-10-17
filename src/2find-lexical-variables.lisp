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

#+sbcl
(defun find-lexical-variables (env)
  (remove-if
   (lambda (v)
     (multiple-value-match (trivial-cltl2:variable-information v env)
       (((not :lexical) _ _)
        t)
       ((_ _ (alist ('ignore . var)))
        var)))
   (remove-duplicates
    (mapcar #'car
            (sb-c::lexenv-vars
             (sb-c::coerce-to-lexenv env))))))


;; see ccl:variable-information for details

#+ccl
(defun find-lexical-variables (env)
  (labels ((rec (env)
             (let ((vars (ccl::lexenv.variables env)))
               (when (listp vars)
                 (union (iter (for v in vars)
                              (for bits = (ccl::var-bits v))
                              (when (and (typep bits 'integer)
			                 (not (logbitp ccl::$vbittemporary bits))
			                 ;; (not (logbitp ccl::$vbitignoreunuused bits))
			                 (not (logbitp ccl::$vbitignore bits))
			                 (not (logbitp ccl::$vbitspecial bits))
                                         )
                                (collect (ccl::var-name v))))
                        (rec (ccl::lexenv.parent-env env)))))))
    (remove-duplicates (rec env))))

#+(or)
(defun fn (x y)
  (declare (fixnum x))
  (in-compile-time (env)
    (print (find-lexical-variables env))
    nil)
  (print x)
  (print y))

#+lispworks
(defun find-lexical-variables (env)
  (let ((list nil))
    (sys:map-environment env :variable
                         #'(lambda (name kind info)
                             (declare (ignore info))
                             (when (eq kind :lexical)
                               (push name list))))
    (delete-duplicates (nreverse list))))

#-(or sbcl ccl lispworks)
(defun find-lexical-variables (env)
  nil)
