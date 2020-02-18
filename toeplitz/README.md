
# TOEPLITZ: Solution of Toeplitz linear systems

TOEPLITZ is a FORTRAN90 library which solves a variety of Toeplitz and circulant linear systems.

The package can also handle circulant Toeplitz systems, and some other more complicated but related forms.

The TOEPLITZ package was written in the early 1980's by a joint working group of American and Soviet mathematicians.

The original, true, correct version of TOEPLITZ is available in the TOEPLITZ subdirectory of the NETLIB web site.

## Related Data and Programs:

BLAS1, a FORTRAN90 library which contains the level 1 Basic Linear Algebra Subprograms.

LINPACK, a FORTRAN90 library which carries out the factorization and solution of linear systems of a variety of types.

LINPLUS, a FORTRAN90 library which includes some routines for manipulating Toeplitz matrices.

TEST_MAT, a FORTRAN90 library which includes some routines which define sample Toeplitz matrices.

TOEPLITZ_CHOLESKY, a FORTRAN90 library which computes the Cholesky factorization of a nonnegative definite symmetric Toeplitz matrix.

## Reference:
- Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov, Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz, The TOEPLITZ Package User's Guide Argonne National Laboratory ANL-83-16, 1983.
- Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart, LINPACK User's Guide, SIAM, 1979, ISBN13: 978-0-898711-72-1.
- Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh, Basic Linear Algebra Subprograms for Fortran Usage, Algorithm 539, ACM Transactions on Mathematical Software, Volume 5, Number 3, September 1979, pages 308-323.

## Source Code:
- toeplitz.f90, the source code.
- toeplitz.sh, commands to compile the source code.

## Examples and Tests:

- toeplitz_prb.f90, a sample problem.
- toeplitz_prb.sh, commands to compile, link and run the sample problem.
- toeplitz_prb_output.txt, sample problem output.

## List of Routines:

- C4_ABS1 computes the L1 absolute value of a complex number.
- C4_SWAP swaps two complex values.
- CAXPY adds a constant times one vector to another.
- CBTO_SL solves the complex block Toeplitz linear system A * X = B.
- CCCC_SL solves the complex double column circulant system A * X = B.
- CCCG_SL solves the complex CCG linear system A * X = B.
- CCC_SL solves the complex column circulant system A * X = B.
- CCCT_SL solves the complex CCT linear system A * X = B..
- CCG_SL solves the complex CG linear system A * X = B.
- CCI_MXV multiplies a complex circulant matrix times a vector.
- CCI_PRINT prints a complex circulant matrix.
- CCI_PRINT_SOME prints some of a complex circulant matrix.
- CCI_RANDOM randomizes a complex circulant matrix.
- CCI_SL solves the complex circulant system A * X = B.
- CCTG_SL solves the complex CTG linear system A * X = B.
- CCT_SL solves the complex CT linear system A * X = B.
- CDOTC forms the dot product of two vectors, conjugating the first.
- CGEFA factors a complex matrix by gaussian elimination.
- CGESL solves the complex general system A * X = B.
- CSALW Fourier transforms the rows of a complex rectangular matrix.
- CSCAL scales a complex vector by a constant.
- CSSCAL scales a complex vector by a real constant.
- CTG_SL solves a linear system involving a complex TG matrix.
- CTG_SL1 solves a CTG linear system.
- CTO_MXV multiplies a complex Toeplitz matrix times a vector.
- CTO_PRINT prints a complex Toeplitz matrix.
- CTO_PRINT_SOME prints some of a complex Toeplitz matrix.
- CTO_RANDOM randomizes a complex Toeplitz matrix.
- CTO_SL solves the complex Toeplitz system A * X = B.
- CTO_VXM multiplies a vector times a complex Toeplitz matrix.
- CTRDI computes the determinant and inverse of a complex triangular matrix.
- CVEC_INDICATOR sets a complex vector to the indicator vector.
- CVEC_PRINT prints a complex vector, with an optional title.
- CVEC_PRINT_SOME prints some of a complex vector.
- CVEC_RANDOM returns a random complex vector in a given range.
- ICAMAX finds the index of element having maximum absolute value.
- ISAMAX finds the index of the vector element of maximum absolute value.
- R4_RANDOM returns a random real in a given range.
- R4VEC_INDICATOR sets a real vector to the indicator vector.
- R4VEC_PRINT prints a real vector.
- R4VEC_PRINT_SOME prints "some" of a real vector.
- R4VEC_RANDOM returns a random real vector in a given range.
- SAMAX returns the maximum absolute value of the entries in a vector.
- SAXPY adds a constant times one vector to another.
- SBTO_MXV computes the real block Toeplitz matrix product A * X = B.
- SBTO_PRINT prints a block Toeplitz matrix.
- SBTO_PRINT_SOME prints some of a block Toeplitz matrix.
- SBTO_SL solves the real block Toeplitz linear system A * X = B.
- SBTO_TO_SGE converts a block Toeplitz matrix to a Linpack General matrix.
- SBTO_VXM computes the real block Toeplitz matrix product X * A = B.
- SCC_QR computes the QR factorization of a real M by L column circulant matrix.
- SCNRM2 computes the unitary norm of a complex vector.
- SDOT forms the dot product of two vectors.
- SGEFA factors a real matrix.
- SGESL solves a real general linear system A * X = B.
- SNRM2 computes the Euclidean norm of a vector.
- SSCAL scales a vector by a constant.
- STO_MXV multiplies a Toeplitz matrix times a vector.
- STO_PRINT prints a Toeplitz matrix.
- STO_PRINT_SOME prints some of a Toeplitz matrix.
- STO_RANDOM randomizes a Toeplitz matrix.
- STO_SL solves the real Toeplitz system A * X = B.
- STO_VXM multiplies a vector times a Toeplitz matrix.
- STRDI computes the determinant and inverse of a real triangular matrix.
- TIMESTAMP prints the current YMDHMS date as a time stamp.

You can go up one level to the FORTRAN90 source codes.

Last revised on 13 November 2006.
