
# I4LIB: A Single Precision Integer Arithmetic Utility Library
I4LIB is a FORTRAN90 library which contains many utility routines for "I4" or "single precision integer" arithmetic.

## Licensing:
The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.

## Languages:
I4LIB is available in a C version and a C++ version and a FORTRAN77 version and a FORTRAN90 version and a MATLAB version.

## Related Data and Programs:
- C4LIB, a FORTRAN90 library which implements certain elementary functions for "C4" or single precision complex variables;
- C8LIB, a FORTRAN90 library which implements certain elementary functions for "C8" or double precision complex variables;
- I8LIB, a FORTRAN90 library which contains many utility routines, using "I8" or "double precision integer" arithmetic.
- R16LIB, a FORTRAN90 library which contains many utility routines, using "R16" or "quadruple precision real" arithmetic.
- R4LIB, a FORTRAN90 library which contains many utility routines, using "R4" or "single precision real" arithmetic.
- R8LIB, a FORTRAN90 library which contains many utility routines, using "R8" or "double precision real" arithmetic.
- SUBPAK, a FORTRAN90 library which contains many utility routines;

## Reference:
1. Milton Abramowitz, Irene Stegun,
   Handbook of Mathematical Functions,
   National Bureau of Standards, 1964,
   ISBN: 0-486-61272-4,
   LC: QA47.A34.
2. Thomas Cormen, Charles Leiserson, Ronald Rivest,
   Introduction to Algorithms,
   MIT Press, 2001,
   ISBN: 0262032937,
   LC: QA76.C662.
3. Albert Nijenhuis, Herbert Wilf,
   Combinatorial Algorithms for Computers and Calculators,
   Second Edition,
   Academic Press, 1978,
   ISBN: 0-12-519260-6,
   LC: QA164.N54.

## Source Code:
- i4lib.f90, the source code;
- i4lib.sh, commands to compile the source code;

## Examples and Tests:
- i4lib_prb.f90, a sample calling program;
- i4lib_prb.sh, commands to compile, link and run the sample calling program;
- i4lib_prb_output.txt, the output file.

## List of Routines:
- I4_BIT_HI1 returns the position of the high 1 bit base 2 in an I4.
- I4_BIT_LO0 returns the position of the low 0 bit base 2 in an I4.
- I4_BIT_LO1 returns the position of the low 1 bit base 2 in an I4.
- I4_BIT_REVERSE reverses the bits in an I4.
- I4_CHARACTERISTIC gives the characteristic for an I4.
- I4_CHOOSE computes the binomial coefficient C(N,K) as an I4.
- I4_DIV_ROUNDED computes the rounded result of I4 division.
- I4_DIVP returns the smallest multiple of J greater than or equal to I.
- I4_EVEN returns TRUE if an I4 is even.
- I4_FACTORIAL computes the factorial of N.
- I4_GCD finds the greatest common divisor of two I4's.
- I4_GCDB finds the greatest common divisor of the form K**N of two I4's.
- I4_HUGE returns a "huge" I4.
- I4_HUGE_NORMALIZER returns the "normalizer" for I4_HUGE.
- I4_IS_POWER_OF_2 reports whether an I4 is a power of 2.
- I4_IS_PRIME reports whether an I4 is prime.
- I4_LCM computes the least common multiple of two I4's.
- I4_LOG_10 returns the integer part of the logarithm base 10 of an I4.
- I4_LOG_2 returns the integer part of the logarithm base 2 of an I4.
- I4_LOG_I4 returns the logarithm of an I4 to an I4 base.
- I4_LOG_R8 returns the logarithm of an I4 to an R8 base.
- I4_MANT computes the "mantissa" of a double precision number.
- I4_MODDIV breaks an I4 into a multiple of a divisor and remainder.
- I4_MODP returns the nonnegative remainder of I4 division.
- I4_MOP returns the I-th power of -1 as an I4 value.
- I4_ODD returns TRUE if an I4 is odd.
- I4_POWER returns the integer power of an I4.
- I4_SIGN evaluates the sign of an I4.
- I4_SWAP swaps two I4's.
- I4_SWAP3 swaps three I4's.
- I4_TO_ANGLE maps I4's to points on a circle.
- I4_TO_DIGITS_DECIMAL determines the last N decimal digits of an I4.
- I4_TO_FAC converts an I4 into a product of prime factors.
- I4_TO_HALTON computes one element of a leaped Halton subsequence.
- I4_TO_ISBN converts an I4 to an ISBN digit.
- I4_UNIFORM returns a scaled pseudorandom I4.
- I4_UNSWAP3 unswaps three I4's.
- I4_WALSH_1D evaluates the Walsh function.
- I4_WIDTH returns the "width" of an I4.
- I4_WRAP forces an I4 to lie between given limits by wrapping.
- I4_XOR calculates the exclusive OR of two I4's.
- I43MAT_FLIP_COLS swaps the columns of an I43MAT.
- I43MAT_FLIP_ROWS swaps the rows of an I43MAT.
- I4COL_COMPARE compares columns I and J of an I4COL.
- I4COL_FIND searches an I4COL for a particular column value.
- I4COL_FIND_ITEM searches an I4COL for a given scalar value.
- I4COL_FIND_PAIR_WRAP searches an I4COL for a pair of items.
- I4COL_FIRST_INDEX indexes the first occurrence of values in an I4COL.
- I4COL_SORT_A ascending sorts an I4COL.
- I4COL_SORT_D descending sorts an I4COL.
- I4COL_SORT2_A ascending sorts the elements of each column of an I4COL.
- I4COL_SORT2_D descending sorts elements of each column of an I4COL.
- I4COL_SORTED_SINGLETON_COUNT counts singletons in an I4COL.
- I4COL_SORTED_UNIQUE keeps unique elements in a sorted I4COL.
- I4COL_SORTED_UNIQUE_COUNT counts unique elements in an I4COL.
- I4COL_SWAP swaps columns J1 and J2 of an I4COL.
- I4COL_UNIQUE_INDEX indexes the unique occurrence of values in an I4COL.
- I4I4_SORT_A ascending sorts a pair of integers.
- I4I4I4_SORT_A ascending sorts a triple of integers.
- I4INT_TO_R4INT maps an I4INT to an R4INT.
- I4INT_TO_R8INT maps an I4INT to an R8INT.
- I4LIST_PRINT prints an I4LIST.
- I4MAT_BORDER_ADD adds a "border" to an I4MAT.
- I4MAT_BORDER_CUT cuts the "border" of an I4MAT.
- I4MAT_ELIM carries out exact Gauss elimination on an I4MAT.
- I4MAT_FLIP_COLS swaps the columns of an I4MAT.
- I4MAT_FLIP_ROWS swaps the rows of an I4MAT.
- I4MAT_HISTOGRAM computes a histogram of the elements of an I4MAT.
- I4MAT_INDICATOR sets up an "indicator" I4MAT.
- I4MAT_L1_INVERSE inverts a unit lower triangular I4MAT.
- I4MAT_MAX returns the maximum of an I4MAT.
- I4MAT_MAX_INDEX returns the location of the maximum of an I4MAT.
- I4MAT_MIN returns the minimum of an I4MAT.
- I4MAT_MIN_INDEX returns the location of the minimum of an I4MAT.
- I4MAT_MM multiplies two I4MAT's.
- I4MAT_PERM permutes the rows and columns of a square I4MAT.
- I4MAT_PERM_UNIFORM selects a random permutation of an I4MAT.
- I4MAT_PERM2 permutes the rows and columns of a rectangular I4MAT.
- I4MAT_PERM2_UNIFORM selects a random permutation of an I4MAT.
- I4MAT_PRINT prints an I4MAT.
- I4MAT_PRINT_SOME prints some of an I4MAT.
- I4MAT_RED divides out common factors in a row or column of an I4MAT.
- I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
- I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
- I4MAT_U1_INVERSE inverts a unit upper triangular I4MAT.
- I4MAT_UNIFORM returns a scaled pseudorandom I4MAT.
- I4ROW_COMPARE compares two rows of an I4ROW.
- I4ROW_FIND_ITEM searches the rows of an I4ROW for a given value.
- I4ROW_FIND_PAIR_WRAP searches rows of an I4ROW for a pair of items.
- I4ROW_MAX returns the maximums of the rows of an I4ROW.
- I4ROW_MEAN returns the means of the rows of an I4ROW.
- I4ROW_MIN returns the minimums of the rows of an I4ROW.
- I4ROW_SORT_A ascending sorts the rows of an I4ROW.
- I4ROW_SORT_D descending sorts the rows of an I4ROW.
- I4ROW_SORT2_D descending sorts the elements of each row of an I4ROW.
- I4ROW_SORTED_UNIQUE keeps unique elements in an I4ROW.
- I4ROW_SORTED_UNIQUE_COUNT counts unique elements in an I4ROW.
- I4ROW_SUM returns the sums of the rows of an I4ROW.
- I4ROW_SWAP swaps two rows of an I4ROW.
- I4ROW_VARIANCE returns the variances of an I4ROW.
- I4VEC_ADD computes C = A + B for I4VEC's.
- I4VEC_AMAX returns the largest magnitude in an I4VEC.
- I4VEC_AMAX_INDEX returns the index of the largest magnitude in an I4VEC.
- I4VEC_AMIN returns the smallest magnitude in an I4VEC.
- I4VEC_AMIN_INDEX returns the index of the smallest magnitude in an I4VEC.
- I4VEC_AMINZ returns the smallest nonzero magnitude in an I4VEC.
- I4VEC_AMINZ_INDEX returns the smallest nonzero magnitude in an I4VEC.
- I4VEC_ANY_LT: ( any ( A < B ) ) for I4VEC's.
- I4VEC_ASCEND_SUB computes the longest ascending subsequence of an I4VEC.
- I4VEC_ASCENDS determines if an I4VEC is (weakly) ascending.
- I4VEC_AXPY adds a scaled multiple of one I4VEC to another.
- I4VEC_BRACKET searches a sorted I4VEC for successive brackets of a value.
- I4VEC_COMPARE compares two I4VEC's.
- I4VEC_COPY copies an I4VEC.
- I4VEC_CUM computes the cumulutive sum of the entries of an I4VEC.
- I4VEC_DESCENDS determines if an I4VEC is decreasing.
- I4VEC_DIRECT_PRODUCT creates a direct product of I4VEC's.
- I4VEC_DIRECT_PRODUCT2 creates a direct product of I4VEC's.
- I4VEC_EVEN_ALL is TRUE if all entries of an I4VEC are even.
- I4VEC_EVEN_ANY is TRUE if any entry of an I4VEC is even.
- I4VEC_FIRST_INDEX indexes the first occurrence of values in an I4VEC.
- I4VEC_FRAC searches for the K-th smallest element in an I4VEC.
- I4VEC_GCD returns the greatest common divisor of an I4VEC.
- I4VEC_HEAP_A reorders an I4VEC into an ascending heap.
- I4VEC_HEAP_D reorders an I4VEC into an descending heap.
- I4VEC_HEAP_D_EXTRACT extracts the maximum value from a heap descending I4VEC.
- I4VEC_HEAP_D_INSERT inserts a new I4 into a heap descending I4VEC.
- I4VEC_HEAP_D_MAX returns the maximum value in a heap descending I4VEC.
- I4VEC_HISTOGRAM computes a histogram of the elements of an I4VEC.
- I4VEC_INDEX returns the first location of a given value in an I4VEC.
- I4VEC_INDEX_DELETE_ALL deletes a value in an indexed sorted I4VEC.
- I4VEC_INDEX_DELETE_DUPES deletes duplicates from an indexed sorted I4VEC.
- I4VEC_INDEX_DELETE_ONE deletes one copy of I4 from an indexed sorted I4VEC.
- I4VEC_INDEX_INSERT inserts an I4 into an indexed sorted I4VEC.
- I4VEC_INDEX_INSERT_UNIQUE inserts a unique I4 into an indexed sorted I4VEC.
- I4VEC_INDEX_ORDER sorts an I4VEC using an index vector.
- I4VEC_INDEX_SEARCH searches for an I4 in an indexed sorted I4VEC.
- I4VEC_INDEX_SORT_UNIQUE creates a sorted unique index for an I4VEC.
- I4VEC_INDEXED_HEAP_D creates a descending heap from an indexed I4VEC.
- I4VEC_INDEXED_HEAP_D_EXTRACT: extract from heap descending indexed I4VEC.
- I4VEC_INDEXED_HEAP_D_INSERT: insert value into heap descending indexed I4VEC.
- I4VEC_INDEXED_HEAP_D_MAX: maximum value in heap descending indexed I4VEC.
- I4VEC_INDICATOR sets an I4VEC to the indicator vector.
- I4VEC_INSERT inserts a value into an I4VEC.
- I4VEC_LCM returns the least common multiple of an I4VEC.
- I4VEC_MASK_PRINT prints a masked I4VEC.
- I4VEC_MAX computes the maximum element of an I4VEC.
- I4VEC_MAX_INDEX computes the index of a maximum element of an I4VEC.
- I4VEC_MAX_INDEX_LAST returns the last maximal element location in an I4VEC
- I4VEC_MEAN returns the mean of an I4VEC.
- I4VEC_MEDIAN returns the median of an unsorted I4VEC.
- I4VEC_MERGE_A merges two ascending sorted I4VEC.
- I4VEC_MIN computes the minimum element of an I4VEC.
- I4VEC_MIN_INDEX computes the index of the minimum element of an I4VEC.
- I4VEC_NONZERO_COUNT counts the nonzero entries in an I4VEC.
- I4VEC_NONZERO_FIRST left-shifts all nonzeros in an I4VEC.
- I4VEC_ODD_ALL is TRUE if all entries of an I4VEC are odd.
- I4VEC_ODD_ANY is TRUE if any entry of an I4VEC is odd.
- I4VEC_ORDER_TYPE determines if I4VEC is (non)strictly ascending/descending.
- I4VEC_PAIRWISE_PRIME checks whether an I4VEC's entries are pairwise prime.
- I4VEC_PART partitions an integer NVAL into N nearly equal parts.
- I4VEC_PART_QUICK_A reorders an I4VEC as part of a quick sort.
- I4VEC_PERMUTE permutes an I4VEC in place.
- I4VEC_PERMUTE_UNIFORM randomly permutes an I4VEC.
- I4VEC_PRINT prints an I4VEC.
- I4VEC_PRINT_SOME prints "some" of an I4VEC.
- I4VEC_PRODUCT returns the product of the entries of an I4VEC.
- I4VEC_RED divides out common factors in an I4VEC.
- I4VEC_REVERSE reverses the elements of an I4VEC.
- I4VEC_ROTATE rotates an I4VEC in place.
- I4VEC_RUN_COUNT counts runs of equal values in an I4VEC.
- I4VEC_SEARCH_BINARY_A searches an ascending sorted I4VEC for a value.
- I4VEC_SEARCH_BINARY_D searches a descending sorted I4VEC for a value.
- I4VEC_SORT_BUBBLE_A ascending sorts an I4VEC using bubble sort.
- I4VEC_SORT_BUBBLE_D descending sorts an I4VEC using bubble sort.
- I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
- I4VEC_SORT_HEAP_D descending sorts an I4VEC using heap sort.
- I4VEC_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an I4VEC.
- I4VEC_SORT_HEAP_INDEX_D does an indexed heap descending sort of an I4VEC.
- I4VEC_SORT_INSERT_A uses an ascending insertion sort on an I4VEC.
- I4VEC_SORT_INSERT_D uses a descending insertion sort on an I4VEC.
- I4VEC_SORT_QUICK_A ascending sorts an I4VEC using quick sort.
- I4VEC_SORT_SHELL_A ascending sorts an I4VEC using Shell's sort.
- I4VEC_SORTED_UNDEX returns unique sorted indexes for a sorted I4VEC.
- I4VEC_SORTED_UNIQUE finds the unique elements in a sorted I4VEC.
- I4VEC_SORTED_UNIQUE_COUNT counts the unique elements in a sorted I4VEC.
- I4VEC_SORTED_UNIQUE_HIST histograms the unique elements of a sorted I4VEC.
- I4VEC_SPLIT "splits" an unsorted I4VEC based on a splitting value.
- I4VEC_STD returns the standard deviation of an I4VEC.
- I4VEC_SUM returns the sum of the entries of an I4VEC.
- I4VEC_SWAP swaps the entries of two I4VEC's.
- I4VEC_TRANSPOSE_PRINT prints an I4VEC "transposed".
- I4VEC_UNDEX returns unique sorted indexes for an I4VEC.
- I4VEC_UNIFORM returns a scaled pseudorandom I4VEC.
- I4VEC_UNIQUE_COUNT counts the unique elements in an unsorted I4VEC.
- I4VEC_UNIQUE_INDEX indexes the unique occurrence of values in an I4VEC.
- I4VEC_VALUE_INDEX indexes entries equal to a given value in an I4VEC.
- I4VEC_VALUE_NUM counts entries equal to a given value in an I4VEC.
- I4VEC_VARIANCE returns the variance of an I4VEC.
- I4VEC_WIDTH returns the "width" of an I4VEC.
- I4VEC_ZERO sets the entries of an I4VEC to 0.
- I4VEC2_COMPARE compares entries of an I4VEC2.
- I4VEC2_PRINT prints a pair of integer vectors.
- I4VEC2_SORT_A ascending sorts a vector of pairs of integers.
- I4VEC2_SORT_D descending sorts a vector of pairs of integers.
- I4VEC2_SORTED_UNIQUE keeps the unique elements in a sorted I4VEC2.
- PERM_CHECK checks that a vector represents a permutation.
- PERM_CYCLE analyzes a permutation.
- PERM_UNIFORM selects a random permutation of N objects.
- PRIME returns any of the first PRIME_MAX prime numbers.
- SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
- TIMESTAMP prints the current YMDHMS date as a time stamp.

You can go up one level to the FORTRAN90 source codes.

Last revised on 17 August 2010. 
