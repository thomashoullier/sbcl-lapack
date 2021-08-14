;;;; DGETRF: LU factorization of a general M-by-N matrix A using
;;;;         partial pivoting with row interchanges.
(in-package :sbcl-lapack)

(define-alien-routine ("dgetrf_" dgetrf_) void
  (m int :copy) ;number of rows in A
  (n int :copy) ;number of columns in A
  (a (* double)) ;matrix A
  (lda int :copy) ;leading dimension of A (number of rows)
  (ipiv (* int)) ;stores the permutation matrix of the pivot, output only.
  (info (* int))) ;exit code: 0 if successful.

(defun dgetrf (a-in)
  "Compute the LUP decomposition of matrix A.
   Return:
   * A: Storing LU in the same matrix, omitting the unit diagonal in L.
   * ipiv: permutation vector representing matrix P.
   A = P.L.U"
  (let* (;; Copy and transpose the input matrix A.
         (m (array-dimension a-in 0))
         (n (array-dimension a-in 1))
         (a (aops:each-index* 'double-float (i j) (aref a-in j i)))
         (lda m)
         (ipiv (make-array (min m n) :element-type '(signed-byte 32)))
         (info (make-array 1 :element-type '(signed-byte 32)
                             :initial-element -1)))
    (sb-sys:with-pinned-objects (m n a lda ipiv info)
      (dgetrf_ m n (double-pointer a) lda (int-pointer ipiv)
               (int-pointer info)))
    ;; Catch errors
    (when (/= 0 (aref info 0))
      (error "dgetrf failed with info=~A" (aref info 0)))
    ;; Return transposed matrix and ipiv.
    (values (aops:each-index* 'double-float (i j) (aref a j i))
            ipiv)))
