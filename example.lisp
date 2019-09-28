
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

;; Example : type information outside specializing macro is propagated appropriately

(defun test-lexical-typedecl ()
  (let ((c -1)
        (a (make-array 15 :element-type 'fixnum)))
    (declare (fixnum c))
    (specializing (a) ()
      (dotimes (i 15)
        (setf (aref a i) (incf c))))
    a))

(print (test-lexical-typedecl))

(disassemble #'last-specialized-function)

;; before adding the lexical information :

; disassembly for (LAMBDA (A C))
; Size: 98 bytes. Origin: #x5331CCBE                          ; (LAMBDA (A C))
; CBE:       498B5D10         MOV RBX, [R13+16]               ; thread.binding-stack-pointer
; CC2:       48895DF8         MOV [RBP-8], RBX
; CC6:       31DB             XOR EBX, EBX
; CC8:       EB3B             JMP L1
; CCA:       660F1F440000     NOP
; CD0: L0:   48895DE8         MOV [RBP-24], RBX
; CD4:       BF02000000       MOV EDI, 2
; CD9:       488BD6           MOV RDX, RSI
; CDC:       FF1425B0001052   CALL QWORD PTR [#x521000B0]     ; GENERIC-+
; CE3:       488B5DE8         MOV RBX, [RBP-24]
; CE7:       4C8B4DF0         MOV R9, [RBP-16]
; CEB:       488BF2           MOV RSI, RDX
; CEE:       F6C201           TEST DL, 1
; CF1:       7523             JNE L2
; CF3:       4D8B41F9         MOV R8, [R9-7]
; CF7:       4939D8           CMP R8, RBX
; CFA:       761F             JBE L3
; CFC:       4989549901       MOV [R9+RBX*4+1], RDX
; D01:       4883C302         ADD RBX, 2
; D05: L1:   4883FB1E         CMP RBX, 30
; D09:       7CC5             JL L0
; D0B:       BA17001050       MOV EDX, #x50100017             ; NIL
; D10:       488BE5           MOV RSP, RBP
; D13:       F8               CLC
; D14:       5D               POP RBP
; D15:       C3               RET
; D16: L2:   CC47             BREAK 71                        ; OBJECT-NOT-FIXNUM-ERROR
; D18:       08               BYTE #X08                       ; RDX
; D19:       CC0F             BREAK 15                        ; Invalid argument count trap
; D1B: L3:   CC1E             BREAK 30                        ; INVALID-ARRAY-INDEX-ERROR
; D1D:       24               BYTE #X24                       ; R9
; D1E:       21               BYTE #X21                       ; R8
; D1F:       0D               BYTE #X0D                       ; RBX

;; after adding the lexical information :

; disassembly for (LAMBDA (A C))
; Size: 82 bytes. Origin: #x53315252                          ; (LAMBDA (A C))
; 52:       498B4510         MOV RAX, [R13+16]                ; thread.binding-stack-pointer
; 56:       488945F8         MOV [RBP-8], RAX
; 5A:       31DB             XOR EBX, EBX
; 5C:       EB2B             JMP L1
; 5E:       6690             NOP
; 60: L0:   498BF1           MOV RSI, R9
; 63:       48D1FE           SAR RSI, 1
; 66:       48FFC6           INC RSI
; 69:       488BC6           MOV RAX, RSI
; 6C:       48D1E0           SHL RAX, 1
; 6F:       7029             JO L2
; 71:       48D1E6           SHL RSI, 1
; 74:       4C8BCE           MOV R9, RSI
; 77:       498B40F9         MOV RAX, [R8-7]
; 7B:       4839D8           CMP RAX, RBX
; 7E:       761F             JBE L3
; 80:       4989749801       MOV [R8+RBX*4+1], RSI
; 85:       4883C302         ADD RBX, 2
; 89: L1:   4883FB1E         CMP RBX, 30
; 8D:       7CD1             JL L0
; 8F:       BA17001050       MOV EDX, #x50100017              ; NIL
; 94:       488BE5           MOV RSP, RBP
; 97:       F8               CLC
; 98:       5D               POP RBP
; 99:       C3               RET
; 9A: L2:   CC47             BREAK 71                         ; OBJECT-NOT-FIXNUM-ERROR
; 9C:       1A               BYTE #X1A                        ; RSI
; 9D:       CC0F             BREAK 15                         ; Invalid argument count trap
; 9F: L3:   CC1E             BREAK 30                         ; INVALID-ARRAY-INDEX-ERROR
; A1:       20               BYTE #X20                        ; R8
; A2:       01               BYTE #X01                        ; RAX
; A3:       0D               BYTE #X0D                        ; RBX
