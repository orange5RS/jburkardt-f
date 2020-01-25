

module     jburk_triangulation_voronoi_
implicit none

   interface        voronoi_polygon_area
   module procedure voronoi_polygon_area
   end interface    voronoi_polygon_area
   public           voronoi_polygon_area

   interface        voronoi_polygon_centroid
   module procedure voronoi_polygon_centroid
   end interface    voronoi_polygon_centroid
   public           voronoi_polygon_centroid

   interface        voronoi_polygon_vertices
   module procedure voronoi_polygon_vertices
   end interface    voronoi_polygon_vertices
   public           voronoi_polygon_vertices

contains


!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   VORONOI_POLYGON_AREA computes the area of a Voronoi polygon.
!> @date    2005-10-17
!> @see     Atsuyuki Okabe, Barry Boots, Kokichi Sugihara, Sung Nok Chiu, 
!!          Spatial Tessellations: Concepts and Applications of Voronoi Diagrams, Second Edition,
!!          Wiley, 2000, page 485.
!----------------------------------------------------------------------
subroutine voronoi_polygon_area ( node, neighbor_num, neighbor_index, &
  node_num, node_xy, area )

!*****************************************************************************80
!
!! VORONOI_POLYGON_AREA computes the area of a Voronoi polygon.
!
!  Discussion:
!
!    It is assumed that the Voronoi polygon is finite!  Every Voronoi
!    diagram includes some regions which are infinite, and for those,
!    this formula is not appropriate.
!
!    The routine is given the indices of the nodes that are neighbors of a
!    given "center" node.  A node is a neighbor of the center node if the
!    Voronoi polygons of the two nodes share an edge.  The triangles of the
!    Delaunay triangulation are formed from successive pairs of these neighbor
!    nodes along with the center node.
!
!    The assumption that the polygon is a Voronoi polygon is
!    used to determine the location of the boundaries of the polygon,
!    which are the perpendicular bisectors of the lines connecting
!    the center point to each of its neighbors.
!
!    The finiteness assumption is employed in part in the
!    assumption that the polygon is bounded by the finite
!    line segments from point 1 to 2, 2 to 3, ...,
!    M-1 to M, and M to 1, where M is the number of neighbors.
!
!    It is assumed that this subroutine is being called by a
!    process which has computed the Voronoi diagram of a large
!    set of nodes, so the arrays X and Y are dimensioned by
!    NODE_NUM, which may be much greater than the number of neighbor
!    nodes.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Atsuyuki Okabe, Barry Boots, Kokichi Sugihara, Sung Nok Chiu,
!    Spatial Tessellations: Concepts and Applications of Voronoi Diagrams,
!    Second Edition,
!    Wiley, 2000, page 485.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE, the index of the node whose Voronoi
!    polygon is to be measured.  1 <= NODE <= NODE_NUM.
!
!    Input, integer ( kind = 4 ) NEIGHBOR_NUM, the number of neighbor nodes of
!    the given node.
!
!    Input, integer ( kind = 4 ) NEIGHBOR_INDEX(NEIGHBOR_NUM), the indices
!    of the neighbor nodes (used to access X and Y).  The neighbor
!    nodes should be listed in the (counter-clockwise) order in
!    which they occur as one circles the center node.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) AREA, the area of the Voronoi polygon.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2
  integer ( kind = 4 ) neighbor_num
  integer ( kind = 4 ) node_num

  real ( kind = 8 ) a
  real ( kind = 8 ) area
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) neighbor_index(neighbor_num)
  integer ( kind = 4 ) node
  real ( kind = 8 ) node_xy(dim_num,node_num)
  real ( kind = 8 ) pc(dim_num)
  real ( kind = 8 ) pi(dim_num)
  real ( kind = 8 ) pj(dim_num)
  real ( kind = 8 ) ui(dim_num)
  real ( kind = 8 ) uj(dim_num)

  area = 0.0D+0

  if ( node < 1 .or. node_num < node ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VORONOI_POLYGON_AREA - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of input parameter NODE.'
    stop
  end if

  pc(1:dim_num) = node_xy(1:dim_num,node)

  i = neighbor_num

  pi(1:dim_num) = node_xy(1:dim_num,i)

  j = 1
  j = neighbor_index(j)

  pj(1:dim_num) = node_xy(1:dim_num,j)

  a = ( pi(1)**2 + pi(2)**2 - pc(1)**2 - pc(2)**2 )
  b = ( pj(1)**2 + pj(2)**2 - pc(1)**2 - pc(2)**2 )
  c = 2.0D+0 * ( ( pi(1) - pc(1) ) * ( pj(2) - pc(2) ) &
                - ( pj(1) - pc(1) ) * ( pi(2) - pc(2) ) )
  uj(1) = ( a * ( pj(2) - pc(2) ) - b * ( pi(2) - pc(2) )  ) / c
  uj(2) = ( a * ( pj(1) - pc(1) ) - b * ( pi(1) - pc(1) )  ) / c

  do i = 1, neighbor_num

    pi(1:dim_num) = pj(1:dim_num)

    ui(1:dim_num) = uj(1:dim_num)

    j = i + 1
    if ( neighbor_num < j ) then
      j = 1
    end if

    j = neighbor_index(j)

    pj(1:dim_num) = node_xy(1:dim_num,j)

    a = ( pi(1)**2 + pi(2)**2 - pc(1)**2 - pc(2)**2 )
    b = ( pj(1)**2 + pj(2)**2 - pc(1)**2 - pc(2)**2 )
    c = 2.0D+0 * ( ( pi(1) - pc(1) ) * ( pj(2) - pc(2) ) &
                  - ( pj(1) - pc(1) ) * ( pi(2) - pc(2) ) )
    uj(1) = ( a * ( pj(2) - pc(2) ) - b * ( pi(2) - pc(2) )  ) / c
    uj(2) = ( a * ( pj(1) - pc(1) ) - b * ( pi(1) - pc(1) )  ) / c

    area = area + uj(1) * ui(2) - ui(1) * uj(2)

  end do

  area = 0.5D+0 * area

  return
end subroutine voronoi_polygon_area



!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   VORONOI_POLYGON_CENTROID computes the centroid of a Voronoi polygon.
!> @date    2005-10-17
!> @see     Atsuyuki Okabe, Barry Boots, Kokichi Sugihara, Sung Nok Chiu, 
!!          Spatial Tessellations: Concepts and Applications of Voronoi Diagrams, Second Edition,
!!          Wiley, 2000, page 485.
!----------------------------------------------------------------------
subroutine voronoi_polygon_centroid ( node, neighbor_num, neighbor_index, &
  node_num, node_xy, centroid )

!*****************************************************************************80
!
!! VORONOI_POLYGON_CENTROID computes the centroid of a Voronoi polygon.
!
!  Discussion:
!
!    It is assumed that the Voronoi polygon is finite!  Every Voronoi
!    diagram includes some regions which are infinite, and for those,
!    this formula is not appropriate.
!
!    The routine is given the indices of the nodes that are neighbors of a
!    given "center" node.  A node is a neighbor of the center node if the
!    Voronoi polygons of the two nodes share an edge.  The triangles of the
!    Delaunay triangulation are formed from successive pairs of these neighbor
!    nodes along with the center node.
!
!    The assumption that the polygon is a Voronoi polygon is
!    used to determine the location of the boundaries of the polygon,
!    which are the perpendicular bisectors of the lines connecting
!    the center point to each of its neighbors.
!
!    The finiteness assumption is employed in part in the
!    assumption that the polygon is bounded by the finite
!    line segments from point 1 to 2, 2 to 3, ...,
!    M-1 to M, and M to 1, where M is the number of neighbors.
!
!    It is assumed that this subroutine is being called by a
!    process which has computed the Voronoi diagram of a large
!    set of nodes, so the arrays X and Y are dimensioned by
!    NODE_NUM, which may be much greater than the number of neighbor
!    nodes.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Atsuyuki Okabe, Barry Boots, Kokichi Sugihara, Sung Nok Chiu,
!    Spatial Tessellations: Concepts and Applications of Voronoi Diagrams,
!    Second Edition,
!    Wiley, 2000, page 490.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE, the index of the node whose Voronoi
!    polygon is to be analyzed.  1 <= NODE <= NODE_NUM.
!
!    Input, integer ( kind = 4 ) NEIGHBOR_NUM, the number of neighbor nodes of
!    the given node.
!
!    Input, integer ( kind = 4 ) NEIGHBOR_INDEX(NEIGHBOR_NUM), the indices
!    of the neighbor nodes.  These indices are used to access the
!    X and Y arrays.  The neighbor nodes should be listed in the
!    (counter-clockwise) order in which they occur as one circles
!    the center node.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the total number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) CENTROID(2), the coordinates of the centroid
!    of the Voronoi polygon of node NODE.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2
  integer ( kind = 4 ) neighbor_num
  integer ( kind = 4 ) node_num

  real ( kind = 8 ) a
  real ( kind = 8 ) area
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) centroid(dim_num)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) neighbor_index(neighbor_num)
  integer ( kind = 4 ) node
  real ( kind = 8 ) node_xy(dim_num,node_num)
  real ( kind = 8 ) pc(dim_num)
  real ( kind = 8 ) pi(dim_num)
  real ( kind = 8 ) pj(dim_num)
  real ( kind = 8 ) ui(dim_num)
  real ( kind = 8 ) uj(dim_num)

  centroid(1:dim_num) = 0.0D+0

  if ( node < 1 .or. node_num < node ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VORONOI_POLYGON_CENTROID - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of input parameter NODE.'
    stop
  end if

  pc(1:dim_num) = node_xy(1:dim_num,node)

  i = neighbor_num
  i = neighbor_index(i)

  pi(1:dim_num) = node_xy(1:dim_num,i)

  j = 1
  j = neighbor_index(j)

  pj(1:dim_num) = node_xy(1:dim_num,j)

  a = ( pi(1) * pi(1) + pi(2) * pi(2) - pc(1) * pc(1) - pc(2) * pc(2) )
  b = ( pj(1) * pj(1) + pj(2) * pj(2) - pc(1) * pc(1) - pc(2) * pc(2) )
  c = 2.0D+0 * ( ( pi(1) - pc(1) ) * ( pj(2) - pc(2) ) &
                - ( pj(1) - pc(1) ) * ( pi(2) - pc(2) ) )
  uj(1) = ( a * ( pj(2) - pc(2) ) - b * ( pi(2) - pc(2) )  ) / c
  uj(2) = ( a * ( pj(1) - pc(1) ) - b * ( pi(1) - pc(1) )  ) / c

  do i = 1, neighbor_num

    pi(1:dim_num) = pj(1:dim_num)
    ui(1:dim_num) = uj(1:dim_num)

    j = i + 1
    if ( neighbor_num < j ) then
      j = 1
    end if

    pj(1:dim_num) = node_xy(1:dim_num,j)

    a = ( pi(1) * pi(1) + pi(2) * pi(2) - pc(1) * pc(1) - pc(2) * pc(2) )
    b = ( pj(1) * pj(1) + pj(2) * pj(2) - pc(1) * pc(1) - pc(2) * pc(2) )
    c = 2.0D+0 * ( ( pi(1) - pc(1) ) * ( pj(2) - pc(2) ) &
                  - ( pj(1) - pc(1) ) * ( pi(2) - pc(2) ) )
    uj(1) = ( a * ( pj(2) - pc(2) ) - b * ( pi(2) - pc(2) )  ) / c
    uj(2) = ( a * ( pj(1) - pc(1) ) - b * ( pi(1) - pc(1) )  ) / c

    centroid(1) = centroid(1) + ( ui(2) - uj(2) ) &
      * ( ( uj(1) + ui(1) )**2 - uj(1) * ui(1) )
    centroid(2) = centroid(2) + ( ui(1) - uj(1) ) &
      * ( ( uj(2) + ui(2) )**2 - uj(2) * ui(2) )

  end do

  call voronoi_polygon_area ( node, neighbor_num, neighbor_index, &
    node_num, node_xy, area )

  centroid(1:dim_num) = centroid(1:dim_num) / ( 6.0D+0 * area )

  return
end subroutine voronoi_polygon_centroid



!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   VORONOI_POLYGON_VERTICES computes the vertices of a Voronoi polygon.
!> @date    2005-10-17
!> @see     Atsuyuki Okabe, Barry Boots, Kokichi Sugihara, Sung Nok Chiu, 
!!          Spatial Tessellations: Concepts and Applications of Voronoi Diagrams, Second Edition,
!!          Wiley, 2000, page 485.
!> @see     function triangle_circumcenter_2d ( )
!----------------------------------------------------------------------
subroutine voronoi_polygon_vertices ( node, neighbor_num, neighbor_index, &
  node_num, node_xy, v )

!*****************************************************************************80
!
!! VORONOI_POLYGON_VERTICES computes the vertices of a Voronoi polygon.
!
!  Discussion:
!
!    This routine is only appropriate for Voronoi polygons that are finite.
!
!    The routine is given the indices of the nodes that are neighbors of a
!    given "center" node.  A node is a neighbor of the center node if the
!    Voronoi polygons of the two nodes share an edge.  The triangles of the
!    Delaunay triangulation are formed from successive pairs of these neighbor
!    nodes along with the center node.
!
!    Given only the neighbor node information, it is possible to determine
!    the location of the vertices of the polygonal Voronoi region by computing
!    the circumcenters of the Delaunay triangles.
!
!    It is assumed that this subroutine is being called by a process which has
!    computed the Voronoi diagram of a large set of nodes, so the arrays X and
!    Y are dimensioned by NODE_NUM, which may be much greater than the number
!    of neighbor nodes.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Atsuyuki Okabe, Barry Boots, Kokichi Sugihara, Sung Nok Chiu,
!    Spatial Tessellations: Concepts and Applications of Voronoi Diagrams,
!    Second Edition,
!    Wiley, 2000.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE, the index of the node whose Voronoi
!    polygon is to be analyzed.  1 <= NODE <= NODE_NUM.
!
!    Input, integer ( kind = 4 ) NEIGHBOR_NUM, the number of neighbor nodes of
!    the given node.
!
!    Input, integer ( kind = 4 ) NEIGHBOR_INDEX(NEIGHBOR_NUM), the indices
!    of the neighbor nodes.  These indices are used to access the
!    X and Y arrays.  The neighbor nodes should be listed in the
!    (counter-clockwise) order in which they occur as one circles
!    the center node.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) V(2,NEIGHBOR_NUM), the coordinates of
!    the vertices of the Voronoi polygon around node NODE.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2
  integer ( kind = 4 ) neighbor_num
  integer ( kind = 4 ) node_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) neighbor_index(neighbor_num)
  integer ( kind = 4 ) node
  real ( kind = 8 ) node_xy(dim_num,node_num)
  real ( kind = 8 ) t(dim_num,3)
  real ( kind = 8 ) v(dim_num,neighbor_num)

  if ( node < 1 .or. node_num < node ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VORONOI_POLYGON_VERTICES - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of input parameter NODE.'
    stop
  end if

  t(1:dim_num,1) = node_xy(1:dim_num,node)

  ip1 = neighbor_index(1)
  t(1:dim_num,3) = node_xy(1:dim_num,ip1)

  do i = 1, neighbor_num

    t(1:dim_num,2) = t(1:dim_num,3)

    ip1 = i + 1
    if ( neighbor_num < ip1 ) then
      ip1 = 1
    end if

    ip1 = neighbor_index(ip1)
    t(1:dim_num,3) = node_xy(1:dim_num,ip1)

    call triangle_circumcenter_2d ( t, v(1:dim_num,i) )

  end do

  return
end subroutine voronoi_polygon_vertices

end module jburk_triangulation_voronoi_
