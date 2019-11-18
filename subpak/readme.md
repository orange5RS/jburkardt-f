# SUBPAK: A Utility Library
SUBPAK is a FORTRAN90 library which contains a number of utility routines.

## Licensing:
The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.

## Languages:
SUBPAK is available in a C version and a C++ version and a FORTRAN77 version and a FORTRAN90 version and a MATLAB version.

## Related Data and Programs:
- C4LIB, a FORTRAN90 library which implements certain elementary functions for "C4" or single precision complex variables;
- C8LIB, a FORTRAN90 library which implements certain elementary functions for "C8" or double precision complex variables;
- CELL, a FORTRAN90 library which defines a cell array, a generalization of an array which can compactly store and retrieve vector or matrix data of varying size, such as the rows of a triangular matrix.
- I4LIB, a FORTRAN90 library which contains many utility routines, using "I4" or "single precision integer" arithmetic.
- I8LIB, a FORTRAN90 library which contains many utility routines, using "I8" or "double precision integer" arithmetic.
- INDEX, a FORTRAN90 library which converts a multidimensional vector index to a one-dimensional vector index; it can handle zero and one based indexing schemes, as well as column major and row major conventions.
- R16LIB, a FORTRAN90 library which contains many utility routines, using "R16" or "quadruple precision real" arithmetic.
- R4LIB, a FORTRAN90 library which contains many utility routines, using "R4" or "single precision real" arithmetic.
- R8LIB, a FORTRAN90 library which contains many utility routines, using "R8" or "double precision real" arithmetic.

## Reference:
1. Milton Abramowitz, Irene Stegun,
   Handbook of Mathematical Functions,
   National Bureau of Standards, 1964,
   ISBN: 0-486-61272-4,
   LC: QA47.A34.

2. Book Industry Study Group,
   The Evolution in Product Identification: Sunrise 2005 and the ISBN-13,
   http://www.bisg.org/docs/The_Evolution_in_Product_ID.pdf

3. Thomas Cormen, Charles Leiserson, Ronald Rivest,
   Introduction to Algorithms,
   MIT Press, 2001,
   ISBN: 0262032937,
   LC: QA76.C662.
4. Donald Kreher, Douglas Simpson,
   Combinatorial Algorithms,
   CRC Press, 1998,
   ISBN: 0-8493-3988-X,
   LC: QA164.K73.
5. Peter Lewis, Allen Goodman, James Miller,
   A Pseudo-Random Number Generator for the System/360,
   IBM Systems Journal,
   Volume 8, 1969, pages 136-143.
6. MIL-STD 1753,
   Fortran, DoD Supplement to American National Standard X3.9-1978,
   November, 1978.
7. Albert Nijenhuis, Herbert Wilf,
   Combinatorial Algorithms for Computers and Calculators,
   Second Edition,
   Academic Press, 1978,
   ISBN: 0-12-519260-6,
   LC: QA164.N54.
8. Branch Rickey,
   Goodby to Some Old Baseball Ideas,
   Life Magazine,
   2 August 1954.
9. Alan Schwarz,
   Looking Beyond the Batting Average,
   The New York Times,
   Sunday, 1 August 2004.

## Source Code:
- subpak.f90, the source code;
- subpak.sh, commands to compile the source code;

## Examples and Tests:
- subpak_prb.f90, a sample calling program;
- subpak_prb.sh, commands to compile, link and run the sample calling program;
- subpak_prb_output.txt, the output file.

## List of Routines:
- ANGLE_SHIFT shifts angle ALPHA to lie between BETA and BETA+2PI.
- ANGLE_SHIFT_DEG shifts angle ALPHA to lie between BETA and BETA+360.
- ANGLE_TO_RGB returns a color on the perimeter of the color hexagon.
- AXIS_LIMITS returns "nice" axis limits for a plot.
- BAR_CHECK computes the check digit for a barcode.
- BAR_CODE constructs the 113 character barcode from 11 digits.
- BAR_DIGIT_CODE_LEFT returns the 7 character left bar code for a digit.
- BAR_DIGIT_CODE_RIGHT returns the 7 character right bar code for a digit.
- BMI_ENGLISH computes the body mass index given English measurements.
- BMI_METRIC computes the body mass index given metric measurements.
- CH_IS_DIGIT is TRUE if C is a decimal digit.
- DEGREES_TO_RADIANS converts an angle measure from degrees to radians.
- E_CONSTANT returns the value of E.
- EULER_CONSTANT returns the value of the Euler-Mascheroni constant.
- FAC_DIV divides two quantities represented as prime factors.
- FAC_GCD finds the GCD of two products of prime factors.
- FAC_LCM finds the LCM of two products of prime factors.
- FAC_MUL multiplies two quantities represented as prime factors.
- FAC_PRINT prints a product of prime factors.
- FAC_TO_I4 converts a product of prime factors into an integer.
- FAC_TO_RAT converts a prime factorization into a rational value.
- FEET_TO_METERS converts a measurement in feet to meters.
- GAUSS_SUM evaluates a function that is the sum of Gaussians.
- GET_SEED returns a seed for the random number generator.
- GET_UNIT returns a free FORTRAN unit number.
- GRID1 finds grid points between X1 and X2 in N dimensions.
- GRID1N finds the I-th grid point between X1 and X2 in N dimensions.
- GRID2 computes grid points between X1 and X2 in N dimensions.
- GRID2N computes one grid point between X1 and X2 in N dimensions.
- GRID3 computes a grid on the parallelogram set by X1, X2 and X3 in N space.
- GRID3N computes a parallelogram grid on 3 points in N dimensions.
- GRID4 computes a grid on the parallelogram set by X1, X2 and X3 in N space.
- GRID4N computes a single point on a parallelogram grid in N space.
- I4_MODP returns the nonnegative remainder of I4 division.
- I4_SIGN evaluates the sign of an I4.
- I4_SWAP swaps two I4's.
- I4_TO_DIGITS_DECIMAL determines the last N decimal digits of an I4.
- I4_TO_FAC converts an I4 into a product of prime factors.
- I4_TO_ISBN converts an I4 to an ISBN digit.
- I4_UNIFORM returns a scaled pseudorandom I4.
- I4INT_TO_R8INT maps an I4INT to an R8INT.
- I4VEC_INDICATOR sets an I4VEC to the indicator vector.
- I4VEC_MIN computes the minimum element of an I4VEC.
- I4VEC_PERMUTE permutes an I4VEC in place.
- I4VEC_PRINT prints an I4VEC.
- I4VEC_UNIFORM returns a scaled pseudorandom I4VEC.
- IJ_NEXT returns the next matrix index.
- IJ_NEXT_GT returns the next matrix index, with the constraint that I < J.
- INDEX_BOX2_NEXT_2D produces indices on the surface of a box in 2D.
- INDEX_BOX2_NEXT_3D produces indices on the surface of a box in 3D.
- INDEX1_COL indexes a 1D vector by columns.
- INDEX1_ROW indexes a 1D vector by rows.
- INDEX2_COL indexes a 2D array by columns.
- INDEX2_ROW indexes a 2D array by row.
- INDEX3_COL indexes a 3D array by columns.
- INDEX3_ROW indexes a 3D array by rows.
- INDEX4_COL indexes a 4D array by columns.
- INDEX4_ROW indexes a 4D array by rows.
- INDEXN_COL indexes an ND array by columns.
- INDEXN_ROW indexes an ND array by rows.
- ISBN_CHECK checks an ISBN code.
- ISBN_FILL fills in a missing digit in an ISBN code.
- ISBN_TO_I4 converts an ISBN character into an integer.
- ISET2_COMPARE compares two I2 sets.
- ISET2_INDEX_INSERT_UNIQUE inserts unique values into an indexed sorted list.
- ISET2_INDEX_SEARCH searches for an I2 set value in an indexed sorted list.
- LCM_12N computes the least common multiple of the integers 1 through N.
- LMAT_PRINT prints an LMAT.
- LMAT_PRINT_SOME prints some of an LMAT.
- LMAT_TRANSPOSE_PRINT prints an LMAT, transposed.
- LMAT_TRANSPOSE_PRINT_SOME prints some of an LMAT, transposed.
- LUHN_CHECK computes the Luhn checksum for a string of digits.
- LVEC_PRINT prints an LVEC.
- LVEC_PRINT_SOME prints "some" of an LVEC.
- PAUSE_INPUT waits until an input character is entered.
- PERM_CHECK checks that a vector represents a permutation.
- PERM_CYCLE analyzes a permutation.
- PERM_FREE reports the number of unused items in a partial permutation.
- PERM_INVERSE inverts a permutation "in place".
- PERM_NEXT computes all of the permutations on N objects, one at a time.
- PERM_PRINT prints a permutation.
- PERM_UNIFORM selects a random permutation of N objects.
- POUNDS_TO_KILOGRAMS converts a measurement in pounds to kilograms.
- PRIME returns any of the first PRIME_MAX prime numbers.
- PRIME_GE returns the smallest prime greater than or equal to N.
- PRIMER computes the prime numbers up to a given limit.
- R8_LOG_10 returns the logarithm base 10 of an R8.
- R8_UNIFORM returns a scaled pseudorandom R8.
- R8_UNIFORM_01 returns a unit pseudorandom R8.
- R8MAT_PRINT prints an R8MAT.
- R8MAT_PRINT_SOME prints some of an R8MAT.
- R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
- R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
- R8POLY_DEGREE returns the degree of a polynomial.
- R8POLY_PRINT prints out a polynomial.
- R8VEC_INDICATOR sets an R8VEC to the indicator vector.
- R8VEC_MEAN returns the mean of an R8VEC.
- R8VEC_PRINT prints an R8VEC.
- R8VEC_VARIANCE returns the variance of an R8VEC.
- RADIANS_TO_DEGREES converts an angle measure from radians to degrees.
- RANDOM_INITIALIZE initializes the FORTRAN90 random number seed.
- RAT_FACTOR factors a rational value into a product of prime factors.
- RICKEY evaluates Branch Rickey's baseball index.
- ROOTS_TO_I4POLY converts polynomial roots to polynomial coefficients.
- ROOTS_TO_R8POLY converts polynomial roots to polynomial coefficients.
- SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
- TIMESTAMP prints the current YMDHMS date as a time stamp.
- TUPLE_NEXT2 computes the next element of an integer tuple space.
- TVEC_EVEN computes evenly spaced angles between 0 and 2*PI.
- TVEC_EVEN2 computes evenly spaced angles between 0 and 2*PI.
- TVEC_EVEN3 computes evenly spaced angles between 0 and 2*PI.
- TVEC_EVEN_BRACKET computes evenly spaced angles between THETA1 and THETA2.
- TVEC_EVEN_BRACKET2 computes evenly spaced angles from THETA1 to THETA2.
- TVEC_EVEN_BRACKET3 computes evenly spaced angles from THETA1 to THETA2.
- UPC_CHECK_DIGIT returns the check digit of a UPC.
- VERSINE_PULSE adds a versine pulse to a constant.

You can go up one level to the FORTRAN90 source codes.

Last revised on 02 November 2011. 
