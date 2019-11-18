
# GEOMPACK2: Voronoi diagrams, Delaunay triangulations

GEOMPACK2 is a FORTRAN90 library which carries out certain geometric computations, including the Voronoi diagram, and the Delaunay triangulation of a set of points in the plane.

Note that DTRIS2 or RTRIS2, the fundamental routine for constructing the Delaunay triangulation, alters the input coordinate data by sorting it. This has caused me so many problems that I finally wrote a modified version of DTRIS2/RTRIS2 that undoes the sorting before return. In all other programs that use DTRIS2/RTRIS2, I use the modified version, but I have left the original here in this package.

## Related Data and Programs:
- DUTCH, a FORTRAN90 library which carries out tasks in computational geometry.
- GEOMETRY, a FORTRAN90 library which performs geometric calculations in 2, 3 and N dimensional space.
- GEOMPACK, a FORTRAN90 library which is a subset of GEOMPACK2, and which is available in several other computer languages.
- GEOMPACK3, a FORTRAN90 library which handles 3D geometric problems.
- TRIANGULATION_DISPLAY_OPENGL, a C++ program which reads files defining a triangulation and displays an image using OpenGL.
- TRIANGULATION_TRIANGLE_NEIGHBORS, a FORTRAN90 program which reads data defining a triangulation, determines the neighboring triangles of each triangle, and writes that information to a file.

## Author:
Original FORTRAN77 version by Barry Joe,
FORTRAN90 version by John Burkardt.

## Reference:
1. Franz Aurenhammer,
   Voronoi diagrams - a study of a fundamental geometric data structure,
   ACM Computing Surveys,
   Volume 23, pages 345-405, September 1991.

2. Barry Joe,
   GEOMPACK - a software package for the generation of meshes using geometric algorithms,
   Advances in Engineering Software,
   Volume 13, pages 325-331, 1991.

## Source Code:
- geompack2.f90, the source code.
- geompack2.sh, commands to compile the source code.

## Examples and Tests:
- geompack2_prb.f90, a sample problem.
- geompack2_prb.sh, commands to compile, link and run the sample problem.
- geompack2_prb_output.txt, the output file.
- test06_triangulation_plot.png, a PNG image of a Delaunay triangulation.

- annulus.in, an annulus.
- cmos.in, 30 nodes
- convex.in, a convex polygon with 6 nodes.
- holeatt.in. attached hole in square, 12 nodes.
- intfac.in, interfaces in region, 90 nodes.
- lsup.in, the outline of Lake Superior;
- notch.in, notch in a rectangle.
- ptpg.in.
- refcor.in, refined corner of a square.
- refint.in, refinement in interior of square.
- refsid.in, refinement in the side of a square.
- shr1.in.
- shr2.in.
- shr3.in.
- shr4.in.
- simple.in, a simple polygon.
- vpol1.in.
- vpol2.in.
- vpol3.in.
- vpol4.in.
- vpol5.in.
- vpol6.in.

## List of Routines:
- ANGLE computes the interior angle at a vertex defined by 3 points.
- AREAPG computes twice the signed area of a simple polygon.
- AREATR computes twice the signed area of a triangle.
- BEDGMV generates boundary edge mesh vertices.
- BNSRT2 bin sorts N points in 2D into increasing bin order.
- CMCIRC determines whether a point lies within a circle through 3 points.
- CVDEC2 decomposes a polygonal region into convex polygons.
- CVDTRI converts boundary triangles to Delaunay triangles.
- DEGREES_TO_RADIANS converts an angle from degrees to radians.
- DELAUNAY_PRINT prints out information defining a Delaunay triangulation.
- DHPSRT sorts points into lexicographic order using heap sort
- DIAEDG triangulates 4 points using the circumcircle criterion.
- DIAM2 finds the diameter of a convex polygon.
- DLESS determine whether P is lexicographically less than Q.
- DSFTDW sifts A(*,MAP(L)) down a heap of size U.
- DSMCPR initializes the polygonal decomposition data structure.
- DSMDF2 sets up a data structure for a heuristic mesh distribution.
- DSPGDC initializes the polygonal decomposition data structure.
- DTRIS2 constructs a Delaunay triangulation of 2D vertices.
- DTRIW2 constructs an incremental Delaunay triangulation in 2D.
- EDGHT searches a hash table for a record in EDGE containing key (A,B).
- EQDIS2 further subdivides convex polygons for mesh equidistribution.
- FNDSEP finds separators to resolve a reflex vertex.
- FNDTRI finds two triangles containing a given edge.
- GET_UNIT returns a free FORTRAN unit number.
- GTIME gets the current CPU time in seconds.
- HOLVRT determines top and bottom vertices of holes in polygonal regions.
- I4_MODP returns the nonnegative remainder of integer division.
- I4_SWAP swaps two integer values.
- I4_WRAP forces an integer to lie between given limits by wrapping.
- IHPSRT uses heapsort on integer points in K-dimension.
- ILESS determines whether a K-dimensional point P is lexically less than Q.
- I4MAT_PRINT prints an I4MAT.
- INSED2 inserts an edge into the head and polygon vertex lists.
- INSVR2 inserts a point into the vertex coordinate and polygon vertex lists.
- INTPG integrates the mesh distribution function in a convex polygon.
- INTTRI generates triangles inside a convex polygon.
- ISFTDW sifts A(*,MAP(L)) down a heap of size U.
- I4VEC_INDICATOR sets an integer vector to the indicator vector.
- JNHOLE joins a hole boundary to the boundary of a polygon.
- LOP applies the local optimization procedure to two triangles.
- LRLINE determines if a point is left of, right or, or on a directed line.
- LUFAC computes the LU factorization of a matrix.
- LUSOL solves a linear system with an LU factored matrix.
- MDF2 evaluates the heuristic mesh distribution function at (X,Y).
- MFDEC2 subdivides polygons to decrease mesh distribution variation.
- MINANG determines the minimum of the boundary angles for a separator.
- MMASEP chooses the best of four separators by the max-min angle criterion.
- MTREDG sets fields for a triangle as needed by routine TMERGE.
- PRIME returns a prime greater than a given integer K.
- PRMDF2 preprocesses a mesh distribution function evaluation.
- PTPOLG determines if a point is in, on or outside a polygon.
- R8MAT_PRINT prints an R8MAT.
- RADIANS_TO_DEGREES converts an angle from radians to degrees.
- RANDPT generates N random K-dimensional points from the uniform distribution.
- RESVRT resolves a reflex vertex of a simply connected polygon.
- ROTIAR rotates the elements of an integer array.
- ROTIPG rotates the vertex indices of a simple polygon.
- ROTPG rotates a convex polygon.
- SEPMDF splits a polygon according to the mesh distribution function.
- SEPSHP splits a convex polygon according to shape.
- SFDWMF sifts PSI(INDP(L)) down a heap.
- SFUPMF sifts PSI(INDP(R)) up a heap.
- SHRNK2 shrinks a convex polygon.
- SPDEC2 decomposes a polygonal region with holes into simple polygons.
- SWAPEC swaps diagonal edges until all triangles are Delaunay.
- TIMESTAMP prints the current YMDHMS date as a time stamp.
- TMERGE forms triangles near the boundary by merging vertex chains.
- TRIANGULATION_PLOT_EPS plots a triangulation of a pointset.
- TRINBR determines the neighboring triangles of every triangle.
- TRIPR2 generates triangles inside each convex polygon of a decomposition.
- TRISIZ smooths the mean mesh distribution function.
- TRPOLG generates a Delaunay triangular mesh inside a convex polygon.
- UMDF2 is a dummy mesh distribution function.
- URAND is a uniform random number generator.
- VBEDG determines visible boundary edges of a 2D triangulation.
- VISPOL computes the visibility polygon.
- VISVRT determines a list of visible vertices.
- VORNBR determines the Voronoi neighbors of an eyepoint.
- VPLEFT is called by routine VISPOL for the LEFT operation (OPER = 1).
- VPRGHT is called by routine VISPOL for the RIGHT operation (OPER = 2).
- VPSCNA is called by routine VISPOL for the SCANA operation (OPER = 3).
- VPSCNB is called by routine VISPOL for the SCANB operation (OPER = 4).
- VPSCNC is called by routine VISPOL for the SCANC operation (OPER = 5).
- VPSCND is called by routine VISPOL for the SCAND operation (OPER = 6).
- WALKT2 searches for a triangle containing a point.
- WIDTH2 finds the minimum breadth of a convex polygon.
- XEDGE determines if an edge intersects another edge or ray.
- XLINE finds the intersection of lines parallel to two other lines.

You can go up one level to the FORTRAN90 source codes.

Last revised on 12 November 2006. 
