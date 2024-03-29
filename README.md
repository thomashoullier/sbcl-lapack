# SBCL BLAS/LAPACK bindings
This is a set of (very few) SBCL bindings for BLAS/LAPACK. I just add whatever
I need manually. This should be mostly for tests, so as not to make libraries
reliant on SBCL-specific code.

## Functions

### List
* `dgemm`
* `dgesv`

### Description

**dgemm** a b &key transa transb alpha beta c => x

Perform [dgemm](https://www.netlib.org/lapack/explore-html/d1/d54/group__double__blas__level3_gaeda3cbd99c8fb834a60a6412878226e1.html#gaeda3cbd99c8fb834a60a6412878226e1),
double-float matrix multiplication: $X = \alpha \cdot A.B + \beta \cdot C$.

* *a*, *b*, *c*: Native 2D Common Lisp matrices of `double-float` elements.
        The rows and columns are multiplied as expected in matrix
        multiplication.
* *transa*, *transb*: Case-insensitive. Can be `"n"` for no
  transposition to apply. `"t"` or `"c"` to apply a transposition.
* *alpha*, *beta*: `double-float` scalars.
* *x*: Result matrix.

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

**dgetrf** a => lu ipiv

Perform [dgetrf](https://www.netlib.org/lapack/explore-html/dd/d9a/group__double_g_ecomputational_ga0019443faea08275ca60a734d0593e60.html#ga0019443faea08275ca60a734d0593e60).
LUP decomposition of matrix A. Returns L and U stored in a single matrix lu with
the unit elements of L omitted. ipiv is the row swap vector to apply in
sequence to lu in order to find A. We retrieve A with A = P.L.U.
Error whenever A is (exactly) singular.

* *a*: Native 2D matrices of `double-float` elements.
       *a* is necessarily square and non-singular.
* *lu*: L and U matrices stored as a single square matrix.
* *ipiv*: Row permutations sequence.

## Dependencies
LAPACK and BLAS should be installed on the system.

`sbcl-lapack`:
* [array-operations](https://github.com/bendudson/array-operations)

`sbcl-lapack/test`:
* [rove](https://github.com/fukamachi/rove)

## Run tests

```common-lisp
(asdf:test-system "sbcl-lapack")
```

## Choices
As far as possible, I try to:
* Use functional calls.
* Hide the return codes, turn them into errors and warnings.

## Caveats
* The results are put in matrices with copying. Performance could be
  marginally improved.
* It is possible to make the system crash by providing wrong user inputs.
  I try to catch the most common mistakes though.

## References
* https://www.netlib.org/lapack/explore-html/index.html
  Documentation for LAPACK/BLAS routines.
