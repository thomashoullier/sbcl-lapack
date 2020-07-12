(defpackage :sbcl-lapack/test
  (:documentation "Rove test suite for sbcl-lapack.")
  (:use :cl :rove))

(in-package :sbcl-lapack/test)

(deftest matrix-multiplication
  (testing "dgemm"
    ;; Test #1
    (let ((a (make-array '(2 2) :element-type 'double-float
                                :initial-contents '((1d0 1d0) (1d0 2d0))))
          (b (make-array '(2 2) :element-type 'double-float
                                :initial-contents '((1d0 1d0) (1d0 3d0))))
          (c)
          (valid-c (make-array
                    '(2 2) :element-type 'double-float
                    :initial-contents '((2d0 4d0) (3d0 7d0)))))
      (setf c (sbcl-lapack:dgemm a b))
      (ok (equalp c valid-c) "Test #1"))
    ;; Test #2
    (let ((a (make-array
              '(2 3) :element-type 'double-float
              :initial-contents '((1d0 1d0 3d0) (1d0 2d0 3d0))))
          (b (make-array
              '(3 2) :element-type 'double-float
              :initial-contents '((1d0 1d0) (1d0 3d0) (2d0 2d0))))
          (c)
          (valid-c (make-array
                    '(2 2) :element-type 'double-float
                    :initial-contents '((8d0 10d0) (9d0 13d0)))))
      (setf c (sbcl-lapack:dgemm a b))
      (ok (equalp c valid-c) "Test #2"))
    ;; Error #1: Non-compatible dimensions
    (let ((a (make-array
              '(2 4) :element-type 'double-float
              :initial-contents '((1d0 1d0 3d0 0d0) (1d0 2d0 3d0 1d0))))
          (b (make-array
              '(3 2) :element-type 'double-float
              :initial-contents '((1d0 1d0) (1d0 3d0) (2d0 2d0))))
          (c)
          (valid-c (make-array
                    '(2 2) :element-type 'double-float
                    :initial-contents '((8d0 10d0) (9d0 13d0)))))
      (ok (signals (setf c (sbcl-lapack:dgemm a b)))
          "Error #1: Non-compatible dimensions"))))

(deftest solving
  (testing "dgesv"
    ;; Test #1
    (let ((a (make-array
              '(2 2) :element-type 'double-float
              :initial-contents '((1d0 2d0) (3d0 4d0))))
          (b (make-array
              '(2 1) :element-type 'double-float
              :initial-contents '((4d0) (2d0))))
          (x)
          (valid-x (make-array
                    '(2 1) :element-type 'double-float
                    :initial-contents '((-6d0) (5d0)))))
      (setf x (sbcl-lapack:dgesv a b))
      (ok (equalp valid-x x) "Test #1"))
    ;; Test #2
    (let ((a (make-array
              '(3 3) :element-type 'double-float
              :initial-contents '((1d0 2d0 3d0) (1d0 -1d0 1d0) (2d0 3d0 -1d0))))
          (b (make-array
              '(3 2) :element-type 'double-float
              :initial-contents '((5d0 -1d0) (3d0 -2d0) (1d0 2d0))))
          (x)
          (valid-x
            (make-array
             '(3 2) :element-type 'double-float
             :initial-contents
             '((1.4736842105263157d0 -0.5263157894736841d0)
               (-0.21052631578947362d0 0.7894736842105263d0)
               (1.3157894736842106d0 -0.6842105263157895d0)))))
      (setf x (sbcl-lapack:dgesv a b))
      (ok (equalp valid-x x) "Test #2"))
    ;; Error #1: Degenerate
    (let ((a (make-array
              '(3 3) :element-type 'double-float
              :initial-contents '((1d0 2d0 3d0) (1d0 -1d0 1d0) (1d0 2d0 3d0))))
          (b (make-array
              '(3 2) :element-type 'double-float
              :initial-contents '((5d0 -1d0) (3d0 -2d0) (1d0 2d0))))
          (x))
      (ok (signals (setf x (sbcl-lapack:dgesv a b)))
          "Error #1: Degenerate"))
    ;; Error #2: Underdetermined
    (let ((a (make-array
              '(3 3) :element-type 'double-float
              :initial-contents '((1d0 2d0 3d0) (1d0 -1d0 1d0) (0d0 0d0 0d0))))
          (b (make-array
              '(3 2) :element-type 'double-float
              :initial-contents '((5d0 -1d0) (3d0 -2d0) (1d0 2d0))))
          (x))
      (ok (signals (setf x (sbcl-lapack:dgesv a b)))
          "Error #2: Underdetermined"))
    ;; Error #3: A does not have the correct number of rows.
    (let ((a (make-array
              '(2 3) :element-type 'double-float
              :initial-contents '((1d0 2d0 3d0) (1d0 -1d0 1d0))))
          (b (make-array
              '(3 2) :element-type 'double-float
              :initial-contents '((5d0 -1d0) (3d0 -2d0) (1d0 2d0))))
          (x))
      (ok (signals (setf x (sbcl-lapack:dgesv a b)))
          "Error #3: A non-valid number of rows."))
    ;; Error #4: A does not have the correct number of columns.
    (let ((a (make-array
              '(3 2) :element-type 'double-float
              :initial-contents '((1d0 3d0) (1d0 -1d0) (4d0 5d0))))
          (b (make-array
              '(3 2) :element-type 'double-float
              :initial-contents '((5d0 -1d0) (3d0 -2d0) (1d0 2d0))))
          (x))
      (ok (signals (setf x (sbcl-lapack:dgesv a b)))
          "Error #4: A non-valid number of columns."))))
