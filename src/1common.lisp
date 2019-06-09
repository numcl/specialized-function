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


(declaim (inline specialized-function-form))
(defun specialized-function-form (vars lexvars decl-and-body vals)
  `(lambda (,@vars ,@lexvars)
     ,@(mapcar (lambda (var val)
                 `(declare (type ,(upgraded-object-type val) ,var)))
               vars
               vals)
     ,@decl-and-body))

#+(or)
(print (specialized-function-form 'x nil '((* 2 X)) 5))

;; debug macro
(defmacro in-compile-time ((&optional env) &body body)
  "macro for debugging"
  (with-gensyms (macro)
    `(macrolet ((,macro (&environment ,env)
                  ,@body))
       (,macro))))
