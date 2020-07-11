;;;; Shared definitions and helpers.
(in-package :sbcl-lapack)

;;; Loading libblas
(load-shared-object "libblas.so.3")

(defun double-pointer (array)
  "Matrices must be passed with (double-pointer matrix)
   to the alien routines."
  (sap-alien (sb-sys:vector-sap (sb-ext:array-storage-vector array))
             (* double)))
