# GEOMPACK: Delaunay triangulation
GEOMPACK is a FORTRAN90 library which computes the Delaunay triangulation of a set of points in the plane, by Barry Joe.

## Licensing:
The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.

## Languages:
GEOMPACK is available in a C version and a C++ version and a FORTRAN77 version and a FORTRAN90 version and a MATLAB version.

## Related Data and Programs:
- DELAUNAY_TREE_2D, a C++ program which computes the Delaunay triangulation of a 2D dataset;
- DUTCH, a FORTRAN90 library which carries out tasks in computational geometry.
- GEOMETRY, a FORTRAN90 library which performs geometric calculations in 2, 3 and N dimensional space.
- GEOMPACK2, a more extensive FORTRAN90 library, from which GEOMPACK was extracted.
- GEOMPACK3, a FORTRAN90 library which handles 3D geometric problems.
- SWEEP2, a C program which computes Voronoi Diagrams or Delaunay triangulations, by Steve Fortune;
- SPHERE_DELAUNAY, a FORTRAN90 program which computes and plots the Delaunay triangulation of points on the unit sphere.
- TABLE_DELAUNAY, a FORTRAN90 program which reads a file of point coordinates in the TABLE format and writes out the Delaunay triangulation.
- TRIANGULATION_DISPLAY_OPENGL, a C++ program which reads files defining a triangulation and displays an image using OpenGL.
- TRIANGULATION_TRIANGLE_NEIGHBORS, a FORTRAN90 program which reads data defining a triangulation, determines the neighboring triangles of each triangle, and writes that information to a file.

## Author:
Original FORTRAN77 version by Barry Joe; FORTRAN90 version by John Burkardt.

## Reference:
- Franz Aurenhammer, Voronoi diagrams - a study of a fundamental geometric data structure, ACM Computing Surveys, Volume 23, Number 3, pages 345-405, September 1991.
- Barry Joe, GEOMPACK - a software package for the generation of meshes using geometric algorithms, Advances in Engineering Software, Volume 13, pages 325-331, 1991.

## Source Code:
- geompack.f90, the source code.
- geompack.sh, commands to compile the source code.

## Examples and Tests:
- geompack_prb.f90, a sample calling program.
- geompack_prb.sh, commands to compile and run the sample program.
- geompack_prb_output.txt, the output file.
- triangulation_plot.png, a PNG image of a triangulation.

## List of Routines:
- ALPHA_MEASURE determines the triangulated pointset quality measure ALPHA.
- ANGLE_RAD_2D returns the angle swept out between two rays in 2D.
- ARC_COSINE computes the arc cosine function, with argument truncation.
- DIAEDG chooses a diagonal edge.
- DTRIS2 constructs a Delaunay triangulation of 2D vertices.
- GET_UNIT returns a free FORTRAN unit number.
- I4_MODP returns the nonnegative remainder of integer ( kind = 4 ) division.
- I4_SWAP swaps two I4's.
- I4_WRAP forces an I4 to lie between given limits by wrapping.
- I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
- I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
- I4VEC_HEAP_D reorders an I4VEC into an descending heap.
- I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
- I4VEC_SORTED_UNIQUE finds the unique elements in a sorted I4VEC.
- LRLINE determines if a point is left of, right or, or on a directed line.
- PERM_CHECK checks that a vector represents a permutation.
- PERM_INV inverts a permutation "in place".
- POINTS_DELAUNAY_NAIVE_2D is a naive Delaunay triangulation scheme.
- POINTS_HULL_2D computes the convex hull of a set of points in 2D.
- QUAD_CONVEX_RANDOM returns a random convex quadrilateral.
- R82VEC_PART_QUICK_A reorders an R82VEC as part of a quick sort.
- R82VEC_PERMUTE permutes an R82VEC in place.
- R82VEC_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an R82VEC.
- R82VEC_SORT_QUICK_A ascending sorts an R82VEC using quick sort.
- R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
- R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
- R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
- R8VEC_EQ is true if two R8VEC's are equal.
- R8VEC_GT == ( A1 > A2 ) for R8VEC's.
- R8VEC_LT == ( A1 < A2 ) for R8VEC's.
- R8VEC_PRINT prints an R8VEC.
- R8VEC_SWAP swaps the entries of two R8VEC's.
- SWAPEC swaps diagonal edges until all triangles are Delaunay.
- TIMESTAMP prints the current YMDHMS date as a time stamp.
- TRIANGLE_CIRCUMCENTER_2D computes the circumcenter of a triangle in 2D.
- TRIANGULATION_ORDER3_PLOT plots a 3-node triangulation of a set of nodes.
- TRIANGULATION_ORDER3_PRINT prints information about a Delaunay triangulation.
- VBEDG determines which boundary edges are visible to a point.

You can go up one level to the FORTRAN90 source codes.

Last revised on 25 June 2009.
