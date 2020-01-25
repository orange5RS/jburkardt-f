# DUTCH: Computational Geometry

DUTCH is a FORTRAN90 library which implements some of the computational geometry routines from the reference.


## Licensing:

The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.


## Related Data and Programs:

- [GEOMETRY](https://github.com/orange5RS/jburkardt-f/tree/master/geometry), a FORTRAN90 library which contains many geometrical algorithms.
- [GEOMPACK](https://github.com/orange5RS/jburkardt-f/tree/master/geompack), a FORTRAN90 library which contain routines for Voronoi diagrams and Delaunay triangulations.


## Reference:

1. Marc de Berg, Marc van Kreveld, Mark Overmars, Otfried Schwarzkopf, Computational Geometry, Springer, 2000.
2. Thomas Cormen, Charles Leiserson, Ronald Rivest, Introduction to Algorithms, MIT Press.
3. Albert Nijenhuis, Herbert Wilf, Combinatorial Algorithms, Academic Press, 1978, second edition, ISBN 0-12-519260-6.
4. Joseph O'Rourke, Computational Geometry, Cambridge University Press, Second Edition, 1998. http://www.cs.uu.nl/geobook/.


## Source Code:

- dutch.f90, the source code.
- dutch.sh, commands to compile the source code.


## Examples and Tests:

- dutch_prb.f90, a sample problem.
- dutch_prb.sh, commands to compile, link and run the sample problem.
- dutch_prb_output.txt, the output file.


## List of Routines:

- ANGLE_DEG_2D returns the angle swept out between two rays in 2D.
- ANGLE_RAD_2D returns the angle in radians swept out between two rays in 2D.
- CIRCLE_DIA2IMP_2D converts a diameter to an implicit circle in 2D.
- CIRCLE_EXP2IMP_2D converts a circle from explicit to implicit form in 2D.
- CIRCLE_IMP_CONTAINS_POINT_2D determines if an implicit circle contains a point in 2D.
- CROSS0_2D finds the cross product of (P1-P0) and (P2-P0) in 2D.
- I4_MODP returns the nonnegative remainder of integer division.
- I4_SWAP swaps two integer values.
- I4_UNIFORM returns a pseudorandom I4.
- I4_WRAP forces an integer to lie between given limits by wrapping.
- I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
- I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
- I4VEC_FRAC searches for the K-th smallest element in an N-vector.
- I4VEC_HEAP_A reorders an array of integers into an ascending heap.
- I4VEC_HEAP_D reorders an array of integers into an descending heap.
- I4VEC_HEAP_D_EXTRACT extracts the maximum value from a descending heap.
- I4VEC_HEAP_D_INSERT inserts a new value into a descending heap.
- I4VEC_HEAP_D_MAX returns the maximum value in a descending heap of integers.
- I4VEC_INDICATOR sets an integer vector to the indicator vector.
- I4VEC_MEDIAN returns the median of an unsorted integer vector.
- I4VEC_POP pops an integer vector off of a stack.
- I4VEC_PRINT prints an integer vector.
- I4VEC_PUSH pushes an integer vector onto a stack.
- I4VEC_SORT_HEAP_D descending sorts an integer array using heap sort.
- I4VEC_SPLIT_UNSORT "splits" an unsorted I4VEC based on a splitting value.
- IJ_NEXT returns the next matrix index.
- IJ_NEXT_GT returns the next matrix index, with the constraint that I < J.
- LINE_EXP2IMP_2D converts an explicit line to implicit form in 2D.
- LINE_EXP_POINT_DIST_2D: distance ( explicit line, point ) in 2D.
- LINE_EXP_POINT_DIST_SIGNED_2D: signed distance ( explicit line, point ) in 2D.
- LINE_SEG_CONTAINS_POINT_2D reports if a line segment contains a point in 2D.
- LINE_SEG_VEC_INT_2D computes intersections of a set of line segments.
- LINES_EXP_INT_2D determines where two explicit lines intersect in 2D.
- LINES_IMP_INT_2D determines where two implicit lines intersect in 2D.
- LINES_SEG_DIST_2D computes the distance of two line segments in 2D.
- LINES_SEG_INT_1D computes the intersection of two line segments in 1D.
- LINES_SEG_INT_2D computes the intersection of two line segments in 2D.
- PERM_PRINT prints a permutation.
- PERM_RANDOM returns a random permutation.
- POINTS_CONVEX_HULL_CUBIC_2D computes the convex hull of 2D points.
- POINTS_CONVEX_HULL_NLOGH_2D computes the convex hull of 2D points.
- POINTS_CONVEX_HULL_NLOGN_2D computes the convex hull of 2D points.
- POINTS_MINIDISC1_2D finds the smallest circle through Q containing points P.
- POINTS_MINIDISC2_2D finds the smallest circle through Q1 and Q2 containing points P.
- POINTS_MINIDISC_2D finds the smallest circle containing points P.
- POLY_TRIANGULATE_2D returns a triangulation of a polygon.
- POLY_REORDER_NODES reorders nodes of a polygon so node 1 is leftest lowest.
- POLYCON_MINKOWSKI_SUM_LINEAR computes the Minkowski sum of two convex polygons.
- POLYCON_MINKOWSKI_SUM_N2LOGN2 Minkowski sums two convex polygons.
- R4_UNIFORM_01 returns a unit pseudorandom R4.
- R8_SWAP swaps two R8's.
- R82VEC_PART_QUICK_A reorders a R82VEC as part of a quick sort.
- R82VEC_SORT_QUICK_A ascending sorts a R82VEC using quick sort.
- R8MAT_PRINT prints an R8MAT.
- R8MAT_SOLVE uses Gauss-Jordan elimination to solve an N by N linear system.
- R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
- R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
- R8MAT2_INVERSE inverts a 2 by 2 real matrix using Cramer's rule.
- R8VEC_EQ is true if two R8VEC's are equal.
- R8VEC_GT == ( A1 > A2 ) for R8VEC's.
- R8VEC_LT == ( A1 < A2 ) for R8VEC's.
- R8VEC_SWAP swaps the entries of two R8VECs.
- R8VEC2_COMPARE compares elements of an R8VEC2.
- R8VEC2_PRINT prints a pair of real vectors.
- R8VEC2_SORT_A ascending sorts a vector of pairs of integers.
- RADIANS_TO_DEGREES converts an angle from radians to degrees.
- RECT_INT_2D computes the intersection of two rectangles in 2D.
- SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
- TIMESTAMP prints the current YMDHMS date as a time stamp.
- TRIANGLE_CONTAINS_POINT_2D finds if a point is inside a triangle in 2D.
- TRIANGULATE_TRICOLOR three-colors the nodes of a triangulated polygon.
- TRIANGULATE_COLOR_PUSH pushes a side of a colored triangle onto the stack.
- TRIANGULATE_COLOR_POP pops a side of a colored triangle from the stack.
- TRIANGULATE_COMMON_EDGE seeks the other triangle that shares an edge.
- TRIANGULATION_BOUNDARY_COUNT returns the number of boundary edges.

You can go up one level to the FORTRAN90 source codes.

Last revised on 02 January 2011. 
