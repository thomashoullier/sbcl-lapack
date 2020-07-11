(defsystem sbcl-lapack
  :name "sbcl-lapack"
  :version "0.1"
  :author "Thomas HOULLIER"
  :description "Bindings to LAPACK/BLAS."
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "shared" :depends-on ("package"))
                 (:file "dgemm" :depends-on ("package" "shared")))))
  :in-order-to ((test-op (test-op "sbcl-lapack/test"))))

(defsystem sbcl-lapack/test
  :name "sbcl-lapack/test"
  :version "0.1"
  :author "Thomas HOULLIER"
  :description "Rove test suite for sbcl-lapack."
  :depends-on ("sbcl-lapack" "rove")
  :components
  ((:module "test"
    :components ((:file "rove-suite"))))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
