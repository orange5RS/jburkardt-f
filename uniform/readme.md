
# UNIFORM
Uniform Random Number Generation

UNIFORM is a FORTRAN90 library which returns a sequence of uniformly distributed pseudorandom numbers.

The fundamental underlying random number generator in UNIFORM is based on a simple, old, and limited linear congruential random number generator originally used in the IBM System 360. If you want state of the art random number generation, look elsewhere!

FORTRAN90 already has the random_number function, which can return pseudorandom numbers rapidly, in bulk, and generally with less correlation than UNIFORM provides.

However, this library makes it possible to compare certain computations that use uniform random numbers, written in C, C++, FORTRAN77, FORTRAN90, Mathematica or MATLAB.

Various types of random data can be computed. The routine names are chosen to indicate the corresponding type:

- C4, complex ( kind = 4 )
- C8, complex ( kind = 8 )
- CH, character
- I4, integer ( kind = 4 )
- I8, integer ( kind = 8 )
- L, logical
- R4, real ( kind = 4 )
- R8, real ( kind = 8 )

In some cases, a one dimensional vector or two dimensional array of values is to be generated, and part of the name will therefore include:

- VEC, vector;
- MAT, a matrix of data;

The underlying random numbers are generally defined over some unit interval or region. Routines are available which return these "unit" values, while other routines allow the user to specify limits between which the unit values are rescaled. If a routine returns unit values, its name will include a special indicator:

- 01, the data that is being returned lies in a unit interval or region;

The random number generator embodied here is not very sophisticated. It will not have the best properties of distribution, noncorrelation and long period. It is not the purpose of this library to achieve such worthy goals. This is simply a reasonably portable library that can be implemented in various languages, on various machines, and for which it is possible, for instance, to regard the output as a function of the seed, and moreover, to work directly with the sequence of seeds, if necessary.


## Licensing:

The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.


## Languages:

UNIFORM is available in a C version and a C++ version and a FORTRAN77 version and a FORTRAN90 version and a Mathematica version and a MATLAB version


## Related Data and Programs:

- ASA183, a FORTRAN90 library which implements the Wichman-Hill pseudorandom number generator.
- CVT, a FORTRAN90 library which computes elements of a Centroidal Voronoi Tessellation.
- FAURE, a FORTRAN90 library which computes elements of a Faure quasirandom sequence.
- F90_RANDOM, FORTRAN90 programs which illustrate the use of Fortran's random number generator routines.
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
- LATTICE_RULE, a FORTRAN90 library which approximates multidimensional integrals using lattice rules.
- LCVT, a FORTRAN90 library which computes a latinized Centroidal Voronoi Tessellation.
- NIEDERREITER2, a FORTRAN90 library which computes elements of a Niederreiter quasirandom sequence with base 2.
- NORMAL, a FORTRAN90 library which computes elements of a sequence of pseudorandom normally distributed values.
- RANDLC, a FORTRAN90 library which generates a sequence of pseudorandom numbers, used by the NAS Benchmark programs.
- RANDOM_MPI, a FORTRAN90 program which demonstrates one way to generate the same sequence of random numbers for both sequential execution and parallel execution under MPI.
- SOBOL, a FORTRAN90 library which computes elements of a Sobol quasirandom sequence.
- UNIFORM_DATASET, a FORTRAN90 program which generates a dataset of uniform pseudorandom values and writes them to a file.
- VAN_DER_CORPUT, a FORTRAN90 library which computes elements of a van der Corput quasirandom sequence.
- ZIGGURAT, a FORTRAN90 program which generates points from a uniform, normal or exponential distribution, using the ziggurat method.


## Reference:

- Paul Bratley, Bennett Fox, Linus Schrage, A Guide to Simulation, Second Edition, Springer, 1987, ISBN: 0387964673, LC: QA76.9.C65.B73.
- Bennett Fox, Algorithm 647: Implementation and Relative Efficiency of Quasirandom Sequence Generators, ACM Transactions on Mathematical Software, Volume 12, Number 4, December 1986, pages 362-376.
- Donald Knuth, The Art of Computer Programming, Volume 2, Seminumerical Algorithms, Third Edition, Addison Wesley, 1997, ISBN: 0201896842, LC: QA76.6.K64.
- Pierre LEcuyer, Random Number Generation, in Handbook of Simulation, edited by Jerry Banks, Wiley, 1998, ISBN: 0471134031, LC: T57.62.H37.
- Peter Lewis, Allen Goodman, James Miller, A Pseudo-Random Number Generator for the System/360, IBM Systems Journal, Volume 8, Number 2, 1969, pages 136-143.
- Stephen Park, Keith Miller, Random Number Generators: Good Ones are Hard to Find, Communications of the ACM, Volume 31, Number 10, October 1988, pages 1192-1201.
- Eric Weisstein, CRC Concise Encyclopedia of Mathematics, CRC Press, 2002, Second edition, ISBN: 1584883472, LC: QA5.W45.
- Barry Wilkinson, Michael Allen, Parallel Programming: Techniques and Applications Using Networked Workstations and Parallel Computers, Prentice Hall, ISBN: 0-13-140563-2, LC: QA76.642.W54.


## Source Code:

- uniform.f90, the source code.
- uniform.sh, commands to compile the source code.


## Examples and Tests:

- uniform_prb.f90, a sample calling program.
- uniform_prb.sh, commands to compile, link and run the sample calling program.
- uniform_prb_output.txt, the output file.


## List of Routines:

- C4_UNIFORM_01 returns a unit pseudorandom C4.
- C4MAT_UNIFORM_01 returns a unit pseudorandom C4MAT.
- C4VEC_UNIFORM_01 returns a unit pseudorandom C4VEC.
- C8_UNIFORM_01 returns a unit pseudorandom C8.
- C8MAT_UNIFORM_01 returns a unit pseudorandom C8MAT.
- C8VEC_UNIFORM_01 returns a unit pseudorandom C8VEC.
- CH_UNIFORM returns a scaled pseudorandom CH.
- CONGRUENCE solves a congruence of the form ( A * X = C ) mod B.
- GET_SEED returns a seed for the random number generator.
- I4_GCD finds the greatest common divisor of two I4's.
- I4_HUGE returns a "huge" I4.
- I4_SEED_ADVANCE "advances" the seed.
- I4_UNIFORM returns a scaled pseudorandom I4.
- I4_UNIFORM_0I returns a pseudorandom I4.
- I4MAT_UNIFORM returns a scaled pseudorandom I4MAT.
- I4VEC_UNIFORM returns a scaled pseudorandom I4VEC.
- I8_UNIFORM returns a scaled pseudorandom I8.
- L_UNIFORM returns a pseudorandom L.
- LCRG_ANBN computes the "N-th power" of a linear congruential generator.
- LCRG_EVALUATE evaluates an LCRG, y = ( A * x + B ) mod C.
- LCRG_SEED computes the N-th seed of a linear congruential generator.
- LMAT_UNIFORM returns a pseudorandom LMAT.
- LVEC_UNIFORM returns a pseudorandom LVEC.
- POWER_MOD computes ( A^N ) mod M.
- R4_UNIFORM returns a scaled pseudorandom R4.
- R4_UNIFORM_01 returns a unit pseudorandom R4.
- R4MAT_UNIFORM returns a scaled pseudorandom R4MAT.
- R4MAT_UNIFORM_01 returns a unit pseudorandom R4MAT.
- R4VEC_UNIFORM returns a scaled pseudorandom R4VEC.
- R4VEC_UNIFORM_01 returns a unit pseudorandom R4VEC.
- R8_UNIFORM returns a scaled pseudorandom R8.
- R8_UNIFORM_01 returns a unit pseudorandom R8.
- R8COL_UNIFORM fills an R8COL with scaled pseudorandom numbers.
- R8I8_UNIFORM returns a scaled pseudorandom R8 using an I8 seed.
- R8I8_UNIFORM_01 returns a unit pseudorandom R8 using an I8 seed.
- R8MAT_UNIFORM returns a scaled pseudorandom R8MAT.
- R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
- R8ROW_UNIFORM fills an R8ROW with scaled pseudorandom numbers.
- R8VEC_UNIFORM returns a scaled pseudorandom R8VEC.
- R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
- TIMESTAMP prints the current YMDHMS date as a time stamp.

You can go up one level to the FORTRAN90 source codes.

Last revised on 15 January 2012. 
