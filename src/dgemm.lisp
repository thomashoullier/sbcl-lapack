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

(defun dgemm (a b c)
  "Double-float matrices multiplication: C = A . B
   a, b and c are native common lisp 2D arrays of double-float elements.
   The rows and columns are multiplied as usual.
   The result is stored in c."
  (let ((a0 (array-dimension a 0))
        (a1 (array-dimension a 1))
        (b1 (array-dimension b 1)))
    ;; Note SBCL matrices are tranpositions of FORTRAN matrices, so
    ;; we feed the matrices in reverse order to avoid transposition.
    (sb-sys:with-pinned-objects (a b c)
      (dgemm_ "n" "n" b1 a0 a1 1d0
              (double-pointer b) b1 (double-pointer a) a1 0d0
              (double-pointer c) b1))))
