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
double-float matrix multiplication: $C = A B$. (Actual dgemm has more
features than just matrix multiplication).

* *a*, *b*: Native 2D Common Lisp matrices of `double-float` elements.
        The rows and columns are multiplied as expected in matrix
        multiplication.
* *c*: Result matrix.

**dgesv** a b => x

Perform [dgesv](https://www.netlib.org/lapack/explore-html/d7/d3b/group__double_g_esolve_ga5ee879032a8365897c3ba91e3dc8d512.html#ga5ee879032a8365897c3ba91e3dc8d512).
Solve the linear system of equations for x: $Ax = B$ using LU decomposition.
x is the matrix of solution corresponding to the right-hand sides in B.
An error is emitted if the operation was impossible: *eg.* when A is degenerate.

* *a*, *b*: Native 2D matrices of `double-float` elements.
            *a* is necessarily square. *b* can have as many columns
            as right-hand sides.
* *x*: The sets of solutions, in columns, same indices as right-hand
       sides in *b*.

## Dependencies
LAPACK and BLAS should be installed on the system.

`sbcl-lapack/test`:
* [rove](https://github.com/fukamachi/rove)

## Run tests

```common-lisp
(asdf:test-system "sbcl-lapack")
```

## Caveats
* The results are put in matrices with copying. Performance could be
  marginally improved.

## References
* https://www.netlib.org/lapack/explore-html/index.html
  Documentation for Lapack routines.
