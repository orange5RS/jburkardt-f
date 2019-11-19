# TRIPACK: Constrained Delaunay Triangulation
TRIPACK is a FORTRAN90 library which computes the Delaunay triangulation of a set of points in the plane.

TRIPACK has the unusual option of allowing the user to specify constraint curves to be included in the triangulation.

TRIPACK is primarily a FORTRAN90 "translation" of the original FORTRAN77 program written by Robert Renka, and published in the ACM Transactions on Mathematical Software.

TRIPACK is ACM TOMS algorithm 751. The text of the original FORTRAN77 program is available online through ACM: http://www.acm.org/pubs/calgo or NETLIB: http://www.netlib.org/toms/index.html.

Specifically, the directory http://www.netlib.org/toms/751 contains the original, true, correct version of ACM TOMS Algorithm 751.

## Languages:
- TRIPACK is available in a FORTRAN77 version and a FORTRAN90 version.
Related Data and Programs:
- DELAUNAY_LMAP_2D, a FORTRAN90 program which computes the Delaunay triangulation of points in the plane subject to a linear mapping.
- GEOMPACK, a FORTRAN90 library which can compute Delaunay triangulations Voronoi diagrams and other information, written by Barry Joe.
- STRIPACK, a FORTRAN90 library which computes the Delaunay triangulation or Voronoi diagram of points on a sphere.
- TABLE_DELAUNAY, a FORTRAN90 program which reads a file of point coordinates in the TABLE format and writes out the Delaunay triangulation.
- TRIANGULATION, a FORTRAN90 library which performs various operations on order 3 ("linear") or order 6 ("quadratic") triangulations.
- TRIANGULATION_PLOT, a FORTRAN90 program which makes a PostScript image of a triangulation of points.
- TRIANGULATION_TRIANGLE_NEIGHBORS, a FORTRAN90 program which reads data defining a triangulation, determines the neighboring triangles of each triangle, and writes that information to a file.

## Author:
- Robert Renka

## Reference:
1. Franz Aurenhammer,
   Voronoi diagrams - a study of a fundamental geometric data structure,
   ACM Computing Surveys,
   Volume 23, pages 345-405, September 1991.
2. Robert Renka,
   Algorithm 751: TRIPACK, A Constrained Two-Dimensional Delaunay Triangulation Package,
   ACM Transactions on Mathematical Software,
   Volume 22, Number 1, 1996.
3. Brian Wichmann, David Hill,
   An Efficient and Portable Pseudo-random Number Generator,
   Applied Statistics,
   Volume 31, Number 2, 1982, pages 188-190.

## Source Code:
- tripack.f90, the source code.
- tripack.sh, commands to compile the source code.

## Examples and Tests:
- tripack_prb.f90, a sample problem.
- tripack_prb.sh, commands to compile, link and run the sample problem.
- tripack_prb_output.txt, sample problem output.
- tripack_prb.png, a PNG image of the triangulation.

## List of Routines:
- ADDCST adds constraint curves to a Delaunay triangulation.
- ADDNOD adds a node to a triangulation.
- AREAP computes the signed area of a polygonal curve.
- BDYADD adds a boundary node to a triangulation.
- BNODES returns a list of the boundary nodes.
- CIRCUM determines the circumcenter (and more) of a triangle.
- CRTRI determines if a triangle lies in a constraint region.
- DELARC deletes a boundary arc from a triangulation.
- DELNB deletes a neighbor from an adjacency list.
- DELNOD deletes a node from a triangulation.
- EDGE swaps arcs to force two nodes to be adjacent.
- GETNP sets the next nearest node to a given node.
- INDXCC returns the index of an exterior constraint curve.
- INSERT inserts K as a neighbor of N1.
- INTADD adds an interior point to a triangulation.
- INTSEC determines if two line segments intersect.
- JRAND returns a uniformly distributed random integer between 1 and N.
- LEFT determines whether a node is to the left of a line.
- LSTPTR returns the index of NB in the adjacency list for N0.
- NBCNT returns the number of neighbors of a node.
- NEARND finds the nearest triangulation node to a point.
- OPTIM optimizes the quadrilateral portion of a triangulation.
- STORE forces its argument to be stored.
- SWAP adjusts a triangulation by swapping a diagonal arc.
- SWPTST applies the circumcircle test to a quadrilateral.
- TRFIND locates a point relative to a triangulation.
- TRLIST converts a triangulation to triangle list form.
- TRLPRT prints the triangles in a triangulation.
- TRMESH triangulates a set of points in the plane.
- TRMSHR triangulates logically rectangular data.
- TRMTST tests a data structure representing a Delaunay triangulation.
- TRPLOT plots a triangulation in an EPS file.
- TRPRNT prints information about a planar triangulation.

You can go up one level to the FORTRAN90 source codes.

Last revised on 15 December 2005. 
