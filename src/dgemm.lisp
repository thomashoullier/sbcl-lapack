;;;; double-float matrix multiplication
(in-package :sbcl-lapack)

(declaim (inline dgemm_))
;; The low-level binding.
(define-alien-routine ("dgemm_" dgemm_) void
  (transa c-string)
  (transb c-string)
  (m int :copy)
  (n int :copy)
  (k int :copy)
  (alpha double :copy)
  (a (* double))
  (lda int :copy)
  (b (* double))
  (ldb int :copy)
  (beta double :copy)
  (c (* double))
  (ldc int :copy))

(defun dgemm (a-in b-in
              &key ((:transa transa) "n") ((:transb transb) "n")
                ((:alpha alpha) 1d0) ((:beta beta) 0d0) ((:c c-in) nil))
  "Double-float matrices multiplication: alpha * A . B + beta * C
   a, b and c are native common lisp 2D arrays of double-float elements.
   The rows and columns are multiplied as usual.
   transa and transb operate a transposition on a and b respectively if
   the letters 't' or 'c' are passed."
  (let ((a (aops:each-index* 'double-float (i j) (aref a-in j i)))
        (b (aops:each-index* 'double-float (i j) (aref b-in j i)))
        (c) (notb T)
        (a0 (array-dimension a-in 0))
        (a1 (array-dimension a-in 1))
        (b0 (array-dimension b-in 0))
        (b1 (array-dimension b-in 1))
        (m 0) (n 0) (k 0))
    ;; m, n, k determination as a function of transa and transb
    (cond ((equalp "n" transa)
           (setf m a0))
          ((or (equalp "t" transa) (equalp "c" transa))
           (setf m a1))
          (t (error "transa invalid: ~A" transa)))
    (cond ((equalp "n" transb)
           (setf k a1
                 n b1))
          ((or (equalp "t" transb) (equalp "c" transb))
           (setf n b0
                 k a0
                 notb nil))
          (t (error "transb invalid: ~A" transb)))
    ;; User input checking
    (when (/= k (if notb b0 b1)) (error "Matrix dimension mismatch: A and B"))
    (when (and c-in (or (/= m (array-dimension c-in 0))
                        (/= n (array-dimension c-in 1))))
      (error "Matrix dimension mismatch: C"))
    (if c-in
        (setf c (aops:each-index* 'double-float (i j) (aref c-in j i)))
        (setf c (make-array (list m n) :element-type 'double-float)))
    (sb-sys:with-pinned-objects (a b c)
      (dgemm_ transa transb m n k
              alpha
              (double-pointer a) a0
              (double-pointer b) b0
              beta
              (double-pointer c) (array-dimension c 1)))
    (aops:each-index* 'double-float (i j) (aref c j i))))
