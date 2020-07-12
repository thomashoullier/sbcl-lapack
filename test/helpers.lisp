(in-package :sbcl-lapack/test)

;; Maximum acceptable error between two elements of compared matrices.
(defparameter *eps* (* 10 double-float-epsilon))

(defun double-mat (contents)
  "Shorthand for double-float matrices with initial-contents 'contents."
  (let ((nrows (list-length contents))
        (ncols (list-length (car contents))))
    (make-array (list nrows ncols) :element-type 'double-float
                                   :initial-contents contents)))

(defun compare-matrices (a b eps)
  "Compare two matrices of the same size element by element
   and check they are identical numerically with accuracy eps.
   T is matrices are identical, nil otherwise."
  (let ((a-dim (array-dimensions a))
        (b-dim (array-dimensions b)))
    (when (not (equal a-dim b-dim)) (return-from compare-matrices nil))
    (aops:each (lambda (x) (when (> (abs x) eps) (return-from
                                                  compare-matrices nil)))
               (aops:each #'- (aops:flatten a) (aops:flatten b)))
    T))
