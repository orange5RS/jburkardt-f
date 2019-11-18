
# I8LIB: A Double Precision Integer Arithmetic Utility Library

**I8LIB** is a FORTRAN90 library which contains a number of utility routines for "I8" or "double precision integer" arithmetic.

## Licensing:
The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.

## Languages:
**I8LIB** is available in a C version and a C++ version and a FORTRAN77 version and a FORTRAN90 version.

## Related Data and Programs:
- *C4LIB*, a FORTRAN90 library which implements certain elementary functions for "C4" or single precision complex variables;
- *C8LIB*, a FORTRAN90 library which implements certain elementary functions for "C8" or double precision complex variables;
- *I4LIB*, a FORTRAN90 library which contains many utility routines, using "I4" or "single precision integer" arithmetic.
- *R16LIB*, a FORTRAN90 library which contains many utility routines, using "R16" or "quadruple precision real" arithmetic.
- *R4LIB*, a FORTRAN90 library which contains many utility routines, using "R4" or "single precision real" arithmetic.
- *R8LIB*, a FORTRAN90 library which contains many utility routines, using "R8" or "double precision real" arithmetic.
- *SUBPAK*, a FORTRAN90 library which contains many utility routines;

## Source Code:
- i8lib.f90, the source code;
- i8lib.sh, commands to compile the source code;

## Examples and Tests:
- i8lib_prb.f90, a sample calling program;
- i8lib_prb.sh, commands to compile, link and run the sample calling program;
- i8lib_prb_output.txt, the output file.

## List of Routines:
- I8_BIT_HI1 returns the position of the high 1 bit base 2 in an I8.
- I8_BIT_LO0 returns the position of the low 0 bit base 2 in an I8.
- I8_CHOOSE computes the binomial coefficient C(N,K) as an I8.
- I8_HUGE returns a "huge" I8.
- I8_HUGE_NORMALIZER returns the "normalizer" for I8_HUGE.
- I8_UNIFORM returns a scaled pseudorandom I8.
- I8_XOR calculates the exclusive OR of two I8's.
- R8I8_UNIFORM returns a scaled pseudorandom R8 using an I8 seed.
- R8I8_UNIFORM_01 returns a unit pseudorandom R8 using an I8 seed.
- TIMESTAMP prints the current YMDHMS date as a time stamp.

You can go up one level to the FORTRAN90 source codes.

Last revised on 27 June 2010.
