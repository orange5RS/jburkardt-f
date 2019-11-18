
# NORMAL
Normal Random Number Generators

NORMAL is a FORTRAN90 library which returns a sequence of normally distributed pseudorandom numbers.

NORMAL is based on two simple ideas:

- the use of a fairly simple uniform pseudorandom number generator, which can be implemented in software;
- the use of the Box-Muller transformation to convert pairs of uniformly distributed random values to pairs of normally distributed random values.

Using these ideas, it is not too hard to generate normal sequences of real or complex values, of single or double precision. These values can be generated as single quantities, vectors or matrices. An associated seed actually determines the sequence. Varying the seed will result in producing a different sequence.

The fundamental underlying random number generator used here is based on a simple, old, and limited linear congruential random number generator originally used in the IBM System 360.

This library makes it possible to compare certain computations that use normal random numbers, written in C, C++, FORTRAN77, FORTRAN90 or MATLAB.

## Licensing:

The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.

## Languages:

NORMAL is available in a C version and a C++ version and a FORTRAN77 version and a FORTRAN90 version and a MATLAB version

## Related Data and Programs:
- ASA183, a FORTRAN90 library which implements the Wichman-Hill pseudorandom number generator.
- CVT, a FORTRAN90 library which computes elements of a Centroidal Voronoi Tessellation.
- F90_RANDOM, FORTRAN90 programs which illustrate the use of Fortran's random number generator routines.
- FAURE, a FORTRAN90 library which computes elements of a Faure quasirandom sequence.
- GRID, a FORTRAN90 library which computes elements of a grid dataset.
- HALTON, a FORTRAN90 library which computes elements of a Halton quasirandom sequence.
- HAMMERSLEY, a FORTRAN90 library which computes elements of a Hammersley quasirandom sequence.
- HEX_GRID, a FORTRAN90 library which computes elements of a hexagonal grid dataset.
- HEX_GRID_ANGLE, a FORTRAN90 library which computes elements of an angled hexagonal grid dataset.
- IEEE_UNIFORM, a FORTRAN90 library which tries to uniformly sample the discrete set of values that represent the legal IEEE real numbers;
- IHS, a FORTRAN90 library which computes elements of an improved distributed Latin hypercube dataset.
- LATIN_CENTER, a FORTRAN90 library which computes elements of a Latin Hypercube dataset, choosing center points.
- LATIN_EDGE, a FORTRAN90 library which computes elements of a Latin Hypercube dataset, choosing edge points.
- LATIN_RANDOM, a FORTRAN90 library which computes elements of a Latin Hypercube dataset, choosing points at random.
- LCVT, a FORTRAN90 library which computes a latinized Centroidal Voronoi Tessellation.
- NIEDERREITER2, a FORTRAN90 library which computes elements of a Niederreiter quasirandom sequence with base 2.
- NORMAL_DATASET, a FORTRAN90 program which generates a dataset of multivariate normal pseudorandom values and write them to a file.
- SOBOL, a FORTRAN90 library which computes elements of a Sobol quasirandom sequence.
- SUBPAK, a FORTRAN90 library which includes a routine random_initialize that can be used to try to initialize the seed for the FORTRAN90 random number generator.
- UNIFORM, a FORTRAN90 library which computes elements of a uniformly distributed pseudorandom sequence.
- VAN_DER_CORPUT, a FORTRAN90 library which computes elements of a van der Corput quasirandom sequence.

## Reference:

1. Paul Bratley, Bennett Fox, Linus Schrage,
   A Guide to Simulation,
   Second Edition,
   Springer, 1987,
   ISBN: 0387964673,
   LC: QA76.9.C65.B73.

2. Bennett Fox,
   Algorithm 647: Implementation and Relative Efficiency of Quasirandom Sequence Generators,
   ACM Transactions on Mathematical Software,
   Volume 12, Number 4, December 1986, pages 362-376.

3. Donald Knuth,
   The Art of Computer Programming,
   Volume 2, Seminumerical Algorithms,
   Third Edition,
   Addison Wesley, 1997,
   ISBN: 0201896842,
   LC: QA76.6.K64.

4. Pierre LEcuyer,
   Random Number Generation,
   in Handbook of Simulation,
   edited by Jerry Banks,
   Wiley, 1998,
   ISBN: 0471134031,
   LC: T57.62.H37.

5. Peter Lewis, Allen Goodman, James Miller,
   A Pseudo-Random Number Generator for the System/360,
   IBM Systems Journal,
   Volume 8, 1969, pages 136-143.

## Source Code:
- normal.f90, the source code.
- normal.sh, commands to compile the source code.

## Examples and Tests:
- normal_prb.f90, a sample calling program.
- normal_prb.sh, commands to compile, link and run the sample calling program.
- normal_prb_output.txt, the output file.

# List of Routines:
- C4_NORMAL_01 returns a unit pseudonormal C4.
- C8_NORMAL_01 returns a unit pseudonormal C8.
- I4_HUGE returns a "huge" I4.
- I4_NORMAL returns a scaled pseudonormal I4.
- I8_NORMAL returns a scaled pseudonormal I8.
- R4_NORMAL returns a scaled pseudonormal R4.
- R4_NORMAL_01 returns a unit pseudonormal R4.
- R4_UNIFORM_01 returns a unit pseudorandom R4.
- R4VEC_NORMAL returns a scaled pseudonormal R4VEC.
- R4VEC_UNIFORM_01 returns a unit pseudorandom R4VEC.
- R8_NORMAL returns a scaled pseudonormal R8.
- R8_NORMAL_01 returns a unit pseudonormal R8.
- R8_UNIFORM_01 returns a unit pseudorandom R8.
- R8MAT_NORMAL returns a scaled pseudonormal R8MAT.
- R8MAT_NORMAL_01 returns a unit pseudonormal R8MAT.
- R8VEC_NORMAL returns a scaled pseudonormal R8VEC.
- R8VEC_NORMAL_01 returns a unit pseudonormal R8VEC.
- R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
- TIMESTAMP prints the current YMDHMS date as a time stamp.

You can go up one level to the FORTRAN90 source codes.

Last revised on 02 February 2008. 
