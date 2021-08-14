(defpackage :sbcl-lapack
  (:documentation "BLAS/LAPACK bindings for SBCL.")
  (:use :cl :sb-alien)
  (:export #:dgemm
           #:dgesv
           #:dgetrf))
