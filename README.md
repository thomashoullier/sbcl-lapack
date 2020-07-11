# SBCL BLAS/LAPACK bindings
This is a set of (very few) SBCL bindings for BLAS/LAPACK. I just add whatever
I need manually. This should be mostly for tests, so as not to make libraries
reliant on SBCL-specific code.

## Functions

### List
* `dgemm`

### Description

**dgemm** a b => c

Perform [dgemm](https://www.netlib.org/lapack/explore-html/d1/d54/group__double__blas__level3_gaeda3cbd99c8fb834a60a6412878226e1.html#gaeda3cbd99c8fb834a60a6412878226e1),
double-float matrix multiplication: $C = A B$.

* *a*, *b*: Native 2D Common Lisp matrices of `double-float` elements.
        The rows and columns are multiplied as expected in matrix
        multiplication.
* *c*: Result matrix.

## Dependencies
LAPACK and BLAS should be installed on the system.

`sbcl-lapack/test`:
* [rove](https://github.com/fukamachi/rove)

## Run tests

```common-lisp
(asdf:test-system "sbcl-lapack")
```

## Caveats
* No user input checks of any kind.
* The results are put in matrices calling `make-array` each time. Performance
  could be marginally improved.

## References
* https://www.netlib.org/lapack/explore-html/index.html
  Documentation for Lapack routines.
