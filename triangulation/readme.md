
# TRIANGULATION
Triangulation of 2D data

TRIANGULATION is a FORTRAN90 library which computes a triangulation of a set of points in 2D, and carries out various other related operations on triangulations of order 3 or 6.

The mesh is the collection of triangles. Each triangle is termed an "element". The points used to define the shape of the triangle (the corners, and sometimes a few more points) are called the "nodes".


## Routines are available to:

- evaluate "quality measures" for the mesh;
- create a "node neighbor array" for each node;
- create an "element neighbor array" for each element;
- estimate the integral of a function over the region covered by the mesh;
- plot the nodes and elements of a mesh;
- determine the parts of the mesh that lie on the boundary;
- sample points at random from the region covered by the mesh;
- search a mesh to determine which element contains a point.

Since triangulations are often used to define a finite element mesh, which in turn defines a sparse matrix, there are routines available which can define the sparse compressed column arrays needed for a sparse matrix associated with a mesh of order 3 or 6. The special case of the Taylor-Hood mixed element is also handled, which is essentially an order 6 grid counted twice and an order 3 grid that only uses the vertices of the order 6 grid.


## Licensing:

The computer code and data files described and made available on this web page are distributed under the GNU LGPL license.


## Languages:

TRIANGULATION is available in a C version and a C++ version and a FORTRAN77 version and a FORTRAN90 version and a MATLAB version.


## Related Data and Programs:

- CVT_TRIANGULATION, a FORTRAN90 program which uses routines from the test_triangulation library to create a CVT-based triangularization.
- DISTMESH, a MATLAB program which takes the definition of a 2D region, and fills it up with a set of nodes, and triangulates those nodes to make a triangulation of the region. The region may be nonconvex and may include holes; the user may request a specific density for the nodes, and may require certain points to be in the set of nodes.
- HEX_GRID_TRIANGULATE, a FORTRAN90 program which computes points on a hexagonal grid inside a number of test regions.
- MESH_BANDWIDTH, a FORTRAN90 program which returns the geometric bandwidth associated with a mesh of elements of any order and in a space of arbitrary dimension.
- TABLE_DELAUNAY, a FORTRAN90 program which triangulates a set of nodes whose coordinates are stored in a file.
- TEST_TRIANGULATION, a FORTRAN90 library which sets up a number of triangulation test problems.
- TRIANGLE, a C program which computes a triangulation of a geometric region.
- TRIANGULATION_BOUNDARY_NODES, a FORTRAN90 program which reads data defining a triangulation, determines which nodes lie on the boundary, and writes their coordinates to a file.
- TRIANGULATION_CORNER, a FORTRAN90 program which patches triangulations so that no triangle has two sides on the boundary.
- TRIANGULATION_DELAUNAY_DISCREPANCY, a FORTRAN90 program which measures the amount by which a triangulation fails the local Delaunay test;
- TRIANGULATION_DISPLAY_OPENGL, a C++ program which reads files defining a triangulation and displays an image using Open GL.
- TRIANGULATION_HISTOGRAM, a FORTRAN90 program which computes histograms of data over a triangulation.
- TRIANGULATION_L2Q, a FORTRAN90 program which reads data defining a 3-node triangulation and generates midside nodes and writes out the corresponding 6-node triangulation.
- TRIANGULATION_MASK, a FORTRAN90 program which takes an existing triangulation and deletes triangles and their corresponding nodes as requested by the user.
- TRIANGULATION_ORDER3, a directory which contains a description and examples of order 3 triangulations.
- TRIANGULATION_ORDER6, a directory which contains a description and examples of order 6 triangulations.
- TRIANGULATION_ORIENT, a FORTRAN90 program which reads data defining a triangulation, makes sure that every triangle has positive orientation, and if not, writes a corrected triangle file.
- TRIANGULATION_PLOT, a FORTRAN90 program which reads data defining a triangulation and creates a PostScript image of the nodes and triangles.
- TRIANGULATION_Q2L, a FORTRAN90 program which reads data defining a 6-node triangulation, and subdivides each triangle into 4 3-node triangles, writing the resulting triangulation to a file.
- TRIANGULATION_QUAD, a FORTRAN90 program which estimates the integral of a function over a triangulated region.
- TRIANGULATION_QUALITY, a FORTRAN90 program which reads data defining a triangulation and computes a number of quality measures.
- TRIANGULATION_RCM, a FORTRAN90 program which reads data defining a triangulation, determines an ordering of the nodes that will reduce the bandwidth of the adjacency matrix, and writes the new triangulation information to a file.
- TRIANGULATION_REFINE, a FORTRAN90 program which reads data defining a triangulation, replaces each triangle by four congruent smaller ones, and writes the new triangulation information to a file.
- TRIANGULATION_TRIANGLE_NEIGHBORS, a FORTRAN90 program which reads data defining a triangulation, determines the neighboring triangles of each triangle, and writes that information to a file.


## Reference:

- Franz Aurenhammer, Voronoi diagrams - a study of a fundamental geometric data structure, ACM Computing Surveys, Volume 23, Number 3, September 1991, pages 345-405. Paul Bratley, Bennett Fox, Linus Schrage, A Guide to Simulation, Second Edition, Springer, 1987, ISBN: 0387964673, LC: QA76.9.C65.B73.
- Marc deBerg, Marc Krevald, Mark Overmars, Otfried Schwarzkopf, Computational Geometry, Springer, 2000, ISBN: 3-540-65620-0, LC: QA448.D38.C65.
- Barry Joe, GEOMPACK - a software package for the generation of meshes using geometric algorithms, Advances in Engineering Software, Volume 13, Number 5, 1991, pages 325-331.
- Albert Nijenhuis, Herbert Wilf, Combinatorial Algorithms for Computers and Calculators, Second Edition, Academic Press, 1978, ISBN: 0-12-519260-6, LC: QA164.N54.
- Atsuyuki Okabe, Barry Boots, Kokichi Sugihara, Sung Nok Chiu, Spatial Tessellations: Concepts and Applications of Voronoi Diagrams, Second Edition, Wiley, 2000, ISBN: 0-471-98635-6, LC: QA278.2.O36.
- Joseph ORourke, Computational Geometry, Second Edition, Cambridge, 1998, ISBN: 0521649765, LC: QA448.D38.
- Per-Olof Persson, Gilbert Strang, A Simple Mesh Generator in MATLAB, SIAM Review, Volume 46, Number 2, June 2004, pages 329-345.


## Source Code:

- triangulation.f90, the source code.
- triangulation.sh, commands to compile the source code.


## Examples and Tests:

- triangulation_prb.f90, a sample calling program.
- triangulation_prb.sh, commands to compile and run the sample program.
- triangulation_prb_output.txt, the output from a run of the sample program.
- ns_triangulation.png, a PNG image of a "tiny" triangulation used for the Navier-Stokes examples.
- triangulation_order3_plot.png, a PNG image of a triangulation.
- triangulation_order3_plot2.png, a PNG image of a triangulation.
- triangulation_order6_plot.png, a PNG image of a triangulation.


## List of Routines:
- ALPHA_MEASURE determines the triangulated pointset quality measure ALPHA.
- ANGLE_RAD_2D returns the angle swept out between two rays in 2D.
- ARC_COSINE computes the arc cosine function, with argument truncation.
- AREA_MEASURE determines the area ratio quality measure.
- BANDWIDTH determines the bandwidth associated with a finite element mesh.
- DELAUNAY_SWAP_TEST performs the Delaunay swap test.
- DIAEDG chooses a diagonal edge.
- GET_SEED returns a seed for the random number generator.
- GET_UNIT returns a free FORTRAN unit number.
- I4_MODP returns the nonnegative remainder of I4 division.
- I4_SIGN evaluates the sign of an I4.
- I4_SWAP switches two I4's.
- I4_UNIFORM returns a scaled pseudorandom I4.
- I4_WRAP forces an I4 to lie between given limits by wrapping.
- I4COL_COMPARE compares columns I and J of an I4COL.
- I4COL_SORT_A ascending sorts an I4COL.
- I4COL_SORTED_UNIQUE_COUNT counts unique elements in an I4COL.
- I4COL_SWAP swaps columns I and J of an I4COL.
- I4I4_SORT_A ascending sorts a pair of integers.
- I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
- I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
- I4VEC_HEAP_D reorders an I4VEC into a descending heap.
- I4VEC_INDICATOR sets an I4VEC to the indicator vector.
- I4VEC_MIN computes the minimum element of an I4VEC.
- I4VEC_PRINT prints an I4VEC.
- I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
- I4VEC_SORTED_UNIQUE gets the unique elements in a sorted I4VEC.
- I4VEC2_COMPARE compares pairs of integers stored in two vectors.
- I4VEC2_SORT_A ascending sorts a vector of pairs of integers.
- I4VEC2_SORTED_UNIQUE gets the unique elements in a sorted I4VEC2.
- LRLINE determines if a point is left of, right or, or on a directed line.
- LVEC_PRINT prints an LVEC.
- NODE_MERGE detects nodes that should be merged.
- NS_ADJ_COL_SET sets the COL array in a Navier Stokes triangulation.
- NS_ADJ_COUNT counts adjacencies in a Navier Stokes triangulation.
- NS_ADJ_INSERT inserts an adjacency into a compressed column adjacency matrix.
- NS_ADJ_ROW_SET sets the Navier Stokes sparse compressed column row indices.
- PERM_CHECK checks that a vector represents a permutation.
- PERM_INVERSE inverts a permutation "in place".
- POINTS_DELAUNAY_NAIVE_2D is a naive Delaunay triangulation scheme.
- POINTS_HULL_2D computes the convex hull of 2D points.
- POINTS_POINT_NEAR_NAIVE_ND finds the nearest point to a given point in ND.
- Q_MEASURE determines the triangulated pointset quality measure Q.
- QUAD_CONVEX_RANDOM returns a random convex quadrilateral.
- R8_UNIFORM_01 returns a unit pseudorandom R8.
- R82VEC_PERMUTE permutes an R82VEC in place.
- R82VEC_SORT_HEAP_INDEX_A ascending index heaps an R82VEC.
- R8MAT_PRINT prints an R8MAT.
- R8MAT_PRINT_SOME prints some of an R8MAT.
- R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
- R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
- R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
- R8TRIS2 constructs a Delaunay triangulation of 2D vertices.
- R8VEC_BRACKET searches a sorted R8VEC for successive brackets of a value.
- R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
- SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
- SWAPEC swaps diagonal edges until all triangles are Delaunay.
- TIMESTAMP prints the current YMDHMS date as a time stamp.
- TRIANGLE_ANGLES_2D computes the angles of a triangle in 2D.
- TRIANGLE_AREA_2D computes the area of a triangle in 2D.
- TRIANGLE_CIRCUMCENTER_2D computes the circumcenter of a triangle in 2D.
- TRIANGLE_ORDER3_PHYSICAL_TO_REFERENCE maps T3 physical to reference points.
- TRIANGLE_ORDER3_REFERENCE_TO_PHYSICAL maps T3 reference to physical points.
- TRIANGLE_ORDER6_PHYSICAL_TO_REFERENCE maps T6 physical to reference points.
- TRIANGLE_ORDER6_REFERENCE_TO_PHYSICAL maps T6 reference to physical points.
- TRIANGLE_REFERENCE_SAMPLE returns random points in the reference triangle.
- TRIANGLE_SAMPLE returns random points in a triangle.
- TRIANGULATION_DELAUNAY_DISCREPANCY_COMPUTE reports if a triangulation is Delaunay.
- TRIANGULATION_NEIGHBOR_TRIANGLES determines triangle neighbors.
- TRIANGULATION_NODE_ORDER determines the order of nodes in a triangulation.
- TRIANGULATION_ORDER3_ADJ_COUNT counts adjacencies in a triangulation.
- TRIANGULATION_ORDER3_ADJ_SET sets adjacencies in a triangulation.
- TRIANGULATION_ORDER3_ADJ_SET2 sets adjacencies in a triangulation.
- TRIANGULATION_ORDER3_BOUNDARY_EDGE_COUNT counts the boundary edges.
- TRIANGULATION_ORDER3_BOUNDARY_EDGE_COUNT_EULER counts boundary edges.
- TRIANGULATION_ORDER3_BOUNDARY_NODE indicates which nodes are on the boundary.
- TRIANGULATION_ORDER3_CHECK makes some simple checks on a triangulation.
- TRIANGULATION_ORDER3_EDGE_CHECK checks the edges of a triangulation.
- TRIANGULATION_ORDER3_EXAMPLE1 sets up a sample triangulation.
- TRIANGULATION_ORDER3_EXAMPLE1_SIZE sets sizes for a sample triangulation.
- TRIANGULATION_ORDER3_EXAMPLE2 returns an example triangulation.
- TRIANGULATION_ORDER3_EXAMPLE2_SIZE returns the size of an example.
- TRIANGULATION_ORDER3_NEIGHBOR determines a neighbor of a given triangle.
- TRIANGULATION_ORDER3_NEIGHBOR_NODES determines triangulation neighbor nodes.
- TRIANGULATION_ORDER3_NEIGHBOR_NODES_PRINT prints a node neighbor array.
- TRIANGULATION_ORDER3_PLOT plots a 3-node triangulation of a set of nodes.
- TRIANGULATION_ORDER3_PRINT prints information about a triangulation.
- TRIANGULATION_ORDER3_QUAD approximates an integral over a triangulation.
- TRIANGULATION_ORDER3_REFINE_COMPUTE computes a refined order 3 triangulation.
- TRIANGULATION_ORDER3_REFINE_SIZE sizes a refined order 3 triangulation.
- TRIANGULATION_ORDER3_SAMPLE returns random points in a triangulation.
- TRIANGULATION_ORDER4_PLOT plots a 4-node triangulation of a pointset.
- TRIANGULATION_ORDER6_ADJ_COUNT counts adjacencies in a triangulation.
- TRIANGULATION_ORDER6_ADJ_SET sets adjacencies in a triangulation.
- TRIANGULATION_ORDER6_BOUNDARY_EDGE_COUNT counts the boundary edges.
- TRIANGULATION_ORDER6_BOUNDARY_EDGE_COUNT_EULER counts boundary edges.
- TRIANGULATION_ORDER6_BOUNDARY_NODE indicates which nodes are on the boundary.
- TRIANGULATION_ORDER6_EXAMPLE1 sets up a sample triangulation.
- TRIANGULATION_ORDER6_EXAMPLE1_SIZE sets sizes for a sample triangulation.
- TRIANGULATION_ORDER6_EXAMPLE2 returns an example triangulation.
- TRIANGULATION_ORDER6_EXAMPLE2_SIZE returns the size of an example.
- TRIANGULATION_ORDER6_NEIGHBOR determines a neighbor of a given triangle.
- TRIANGULATION_ORDER6_PLOT plots a 6-node triangulation of a set of nodes.
- TRIANGULATION_ORDER6_PRINT prints out information defining a triangulation.
- TRIANGULATION_ORDER6_REFINE_COMPUTE computes a refined order 6 triangulation.
- TRIANGULATION_ORDER6_REFINE_SIZE sizes a refined order 6 triangulation.
- TRIANGULATION_ORDER6_TO_ORDER3 linearizes a quadratic triangulation.
- TRIANGULATION_ORDER6_VERTEX_COUNT counts the vertex nodes in a triangulation.
- TRIANGULATION_SEARCH_DELAUNAY searches a Delaunay triangulation for a point.
- TRIANGULATION_SEARCH_NAIVE naively searches a triangulation.
- VBEDG determines which boundary edges are visible to a point.
- VORONOI_POLYGON_AREA computes the area of a Voronoi polygon.
- VORONOI_POLYGON_CENTROID computes the centroid of a Voronoi polygon.
- VORONOI_POLYGON_VERTICES computes the vertices of a Voronoi polygon.

You can go up one level to the FORTRAN90 source codes.
Last revised 30 December 2010. 