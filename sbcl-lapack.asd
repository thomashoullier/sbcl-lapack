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
  ;; :in-order-to ((test-op (test-op "stl-io/test")))
  )
