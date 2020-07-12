;;;; DGESV: Solve a linear system A.X = B with LU decomposition.
(in-package :sbcl-lapack)

(define-alien-routine ("dgesv_" dgesv_) void
  (n int :copy) ;number of linear equations
  (nrhs int :copy) ;number of right hand sides
  (a (* double)) ;matrix A
  (lda int :copy) ;leading dimension of A (number of rows)
  (ipiv (* int)) ;stores the permutation matrix of the pivot, output only.
  (b (* double)) ;matrix B
  (ldb int :copy) ;leading dimension of B.
  (info (* int))) ;exit code: 0 if successful.

(defun dgesv (a-in b-in)
  "Compute the solutions X to the linear system of equations
   A.X = B with B the right hand sides. Uses LU decomposition.
   Return X. Signals an error if the operation cannot be done (eg. degeneracy)."
  (let* (;; We have to copy and transpose the input matrices.
         (a (aops:each-index* 'double-float (i j) (aref a-in j i)))
         (b (aops:each-index* 'double-float (i j) (aref b-in j i)))
         (n (array-dimension a-in 1))
         (nrhs (array-dimension b-in 1))
         (lda (array-dimension a-in 0))
         (ipiv (make-array n :element-type '(signed-byte 32)))
         (ldb (array-dimension b-in 0))
         (info (make-array 1 :element-type '(signed-byte 32)
                             :initial-element -1)))
    (when (/= lda ldb)
      (error "A has ~A rows when B has ~A !" lda ldb))
    (when (/= n ldb)
      (error "A has ~A columns when B has ~A rows!" n ldb))
    ;; Note: a stores the LU decomposition and ipiv the transposition
    ;; matrix, ignored.
    (sb-sys:with-pinned-objects (a b ipiv info)
      (dgesv_ n nrhs (double-pointer a) lda (int-pointer ipiv)
              (double-pointer b) ldb (int-pointer info)))
    (when (/= 0 (aref info 0))
      (error "dgesv failed with info=~A" (aref info 0)))
    (values (aops:each-index* 'double-float (i j) (aref b j i)))))
