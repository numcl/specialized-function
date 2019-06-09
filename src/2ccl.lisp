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

