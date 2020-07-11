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
      (ok (equalp c valid-c) "Test #2"))))
