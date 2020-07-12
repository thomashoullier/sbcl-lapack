;;;; Shared definitions and helpers.
(in-package :sbcl-lapack)

;;; Loading shared libraries
(load-shared-object "libblas.so") ;BLAS
(load-shared-object "liblapack.so") ;LAPACK

(defun double-pointer (array)
  "Matrices must be passed with (double-pointer matrix)
   to the alien routines."
  (sap-alien (sb-sys:vector-sap (sb-ext:array-storage-vector array))
             (* double)))

(defun int-pointer (array)
  (sap-alien (sb-sys:vector-sap (sb-ext:array-storage-vector array))
             (* int)))
