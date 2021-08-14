(in-package :sbcl-lapack/test)

(deftest matrix-multiplication
  (testing "dgemm"
    ;; Test #1
    (let ((a (double-mat '((1d0 1d0) (1d0 2d0))))
          (b (double-mat '((1d0 1d0) (1d0 3d0))))
          (valid-c (double-mat '((2d0 4d0) (3d0 7d0)))))
      (ok (compare-matrices (sbcl-lapack:dgemm a b) valid-c *eps*) "Test #1"))
    ;; Test #2
    (let ((a (double-mat '((1d0 1d0 3d0) (1d0 2d0 3d0))))
          (b (double-mat '((1d0 1d0) (1d0 3d0) (2d0 2d0))))
          (transa "t")
          (transb "t")
          (alpha 0.5d0)
          (beta 1.5d0)
          (c (double-mat '((2d0 -1d0 2d0) (1d0 -2d0 0d0) (5d0 1d0 -3d0))))
          (valid-c (double-mat
                    '((4d0 0.5d0 5d0) (3d0 0.5d0 3d0) (10.5d0 7.5d0 1.5d0)))))
      (ok (compare-matrices (sbcl-lapack:dgemm a b :transa transa :transb transb
                                                   :alpha alpha :beta beta :c c)
                            valid-c *eps*) "Test #2"))
    ;; Error #1: Non-compatible dimensions
    (let ((a (double-mat '((1d0 1d0 3d0 0d0) (1d0 2d0 3d0 1d0))))
          (b (double-mat '((1d0 1d0) (1d0 3d0) (2d0 2d0)))))
      (ok (signals (sbcl-lapack:dgemm a b))
          "Error #1: Non-compatible dimensions"))
    ;; Error #2: C has wrong dimensions
    (let ((a (double-mat '((1d0 1d0 3d0) (1d0 2d0 3d0))))
          (b (double-mat '((1d0 1d0) (1d0 3d0) (2d0 2d0))))
          (transa "t")
          (transb "t")
          (alpha 0.5d0)
          (beta 1.5d0)
          (c (double-mat
              '((2d0 -1d0 2d0 4d0) (1d0 -2d0 0d0 1d0) (5d0 1d0 -3d0 0d0)))))
      (ok (signals (sbcl-lapack:dgemm a b :transa transa :transb transb
                                          :alpha alpha :beta beta :c c))
          "Error #2: C dimensions"))))

(deftest solving
  (testing "dgesv"
    ;; Test #1
    (let ((a (double-mat '((1d0 2d0) (3d0 4d0))))
          (b (double-mat '((4d0) (2d0))))
          (valid-x (double-mat '((-6d0) (5d0)))))
      (ok (compare-matrices (sbcl-lapack:dgesv a b) valid-x *eps*) "Test #1"))
    ;; Test #2
    (let ((a (double-mat '((1d0 2d0 3d0) (1d0 -1d0 1d0) (2d0 3d0 -1d0))))
          (b (double-mat '((5d0 -1d0) (3d0 -2d0) (1d0 2d0))))
          (valid-x
            (double-mat
             '((1.4736842105263157d0 -0.5263157894736841d0)
               (-0.21052631578947362d0 0.7894736842105263d0)
               (1.3157894736842106d0 -0.6842105263157895d0)))))
      (ok (compare-matrices (sbcl-lapack:dgesv a b) valid-x *eps*) "Test #2"))
    ;; Test #3
    (let ((a (double-mat '((3d0))))
          (b (double-mat '((1d0))))
          (valid-x (double-mat (list (list (/ 1d0 3d0))))))
      (ok (compare-matrices (sbcl-lapack:dgesv a b) valid-x *eps*)
          "Test #3: Matrix of size 1."))
    ;; Error #1: Degenerate
    (let ((a (double-mat '((1d0 2d0 3d0) (1d0 -1d0 1d0) (1d0 2d0 3d0))))
          (b (double-mat '((5d0 -1d0) (3d0 -2d0) (1d0 2d0)))))
      (ok (signals (sbcl-lapack:dgesv a b)) "Error #1: Degenerate"))
    ;; Error #2: Underdetermined
    (let ((a (double-mat '((1d0 2d0 3d0) (1d0 -1d0 1d0) (0d0 0d0 0d0))))
          (b (double-mat '((5d0 -1d0) (3d0 -2d0) (1d0 2d0)))))
      (ok (signals (sbcl-lapack:dgesv a b)) "Error #2: Underdetermined"))
    ;; Error #3: A does not have the correct number of rows.
    (let ((a (double-mat '((1d0 2d0 3d0) (1d0 -1d0 1d0))))
          (b (double-mat '((5d0 -1d0) (3d0 -2d0) (1d0 2d0)))))
      (ok (signals (sbcl-lapack:dgesv a b))
          "Error #3: A non-valid number of rows."))
    ;; Error #4: A does not have the correct number of columns.
    (let ((a (double-mat '((1d0 3d0) (1d0 -1d0) (4d0 5d0))))
          (b (double-mat '((5d0 -1d0) (3d0 -2d0) (1d0 2d0)))))
      (ok (signals (sbcl-lapack:dgesv a b))
          "Error #4: A non-valid number of columns.")))
  (testing "dgetrf"
    ;; Test #1
    (let ((a (double-mat '((2d0 0d0 2d0 0.6d0) (3d0 3d0 4d0 -2d0)
                           (5d0 5d0 4d0 2d0) (-1d0 -2d0 3.4d0 -1d0))))
          (lu-val (double-mat '((5d0 5d0 4d0 2d0)
                                (0.4d0 -2d0 0.4d0 -0.2d0)
                                (-0.2d0 0.5d0 4d0 -0.5d0)
                                (0.6d0 0d0 0.4d0 -3d0))))
          (ipiv-val #(3 3 4 4))
          (lu) (ipiv))
      (multiple-value-setq (lu ipiv) (sbcl-lapack:dgetrf a))
      (ok (and (compare-matrices lu lu-val *eps*)
               (equalp ipiv ipiv-val)) "Test #1"))))
