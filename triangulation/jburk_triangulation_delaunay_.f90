

module jburk_triangulation_delaunay_
implicit none

interface        delaunay_swap_test
module procedure delaunay_swap_test
end interface    delaunay_swap_test
public           delaunay_swap_test

interface        swapec
module procedure swapec
end interface    swapec
public           swapec

contains



subroutine delaunay_swap_test ( xy, swap )

!*****************************************************************************80
!
!! DELAUNAY_SWAP_TEST performs the Delaunay swap test.
!
!  Discussion:
!
!    The current triangles are formed by nodes (1,2,3) and (1,3,4).
!    if a swap is recommended, the new triangles will be (1,2,4) and (2,3,4).
!
!      4     ?     4
!     / \         /|\
!    1---3  ==>  1 | 3
!     \ /         \|/
!      2           2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Graham Carey,
!    Computational Grids:
!    Generation, Adaptation and Solution Strategies,
!    Taylor and Francis, 1997,
!    ISBN13: 978-1560326359,
!    LC: QA377.C32.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XY(2,4), the coordinates of four points.
!
!    Output, logical SWAP, is TRUE if the triangles (1,2,4) and (2,3,4)
!    are to replace triangles (1,2,3) and (1,3,4).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  logical              swap
  real ( kind = 8 ) x13
  real ( kind = 8 ) x14
  real ( kind = 8 ) x23
  real ( kind = 8 ) x24
  real ( kind = 8 ) xy(2,4)
  real ( kind = 8 ) y13
  real ( kind = 8 ) y14
  real ( kind = 8 ) y23
  real ( kind = 8 ) y24

  x13 = xy(1,1) - xy(1,3)
  x14 = xy(1,1) - xy(1,4)
  x23 = xy(1,2) - xy(1,3)
  x24 = xy(1,2) - xy(1,4)

  y13 = xy(2,1) - xy(2,3)
  y14 = xy(2,1) - xy(2,4)
  y23 = xy(2,2) - xy(2,3)
  y24 = xy(2,2) - xy(2,4)

  a = x13 * x23 + y13 * y23
  b = x24 * y14 - x14 * y24
  c = x23 * y13 - x13 * y23
  d = x24 * x14 + y14 * y24
!
!  The reference gives two initial tests before the
!  main one.  However, there seems to be an error
!  in at least one of these tests.  Since they are
!  intended to avoid error in borderline cases, but
!  instead cause real error in common cases, they are
!  omitted for now.
!
! if ( 0.0D+00 <= a .and. 0.0D+00 <= d ) then
!   swap = .true.
! else if ( a < d .and. d < 0.0D+00 ) then
!   swap = .true.
!  else if...

  if ( a * b < c * d ) then
    swap = .true.
  else
    swap = .false.
  end if

  return
end





subroutine swapec ( i, top, btri, bedg, node_num, node_xy, element_num, &
  element_node, element_neighbor, stack, ierr )

!*****************************************************************************80
!
!! SWAPEC swaps diagonal edges until all triangles are Delaunay.
!
!  Discussion:
!
!    The routine swaps diagonal edges in a 2D triangulation, based on
!    the empty circumcircle criterion, until all triangles are Delaunay,
!    given that I is the index of the new vertex added to the triangulation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 July 2001
!
!  Author:
!
!    Original FORTRAN77 version by Barry Joe.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Barry Joe,
!    GEOMPACK - a software package for the generation of meshes
!    using geometric algorithms,
!    Advances in Engineering Software,
!    Volume 13, pages 325-331, 1991.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of the new vertex.
!
!    Input/output, integer ( kind = 4 ) TOP, the index of the top of the stack.
!    On output, TOP is zero.
!
!    Input/output, integer ( kind = 4 ) BTRI, BEDG; on input, if positive, are
!    the triangle and edge indices of a boundary edge whose updated indices
!    must be recorded.  On output, these may be updated because of swaps.
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of triangles.
!
!    Input/output, integer ( kind = 4 ) ELEMENT_NODE(3,ELEMENT_NUM), the
!    triangle incidence list.  May be updated on output because of swaps.
!
!    Input/output, integer ( kind = 4 ) ELEMENT_NEIGHBOR(3,ELEMENT_NUM), the
!    triangle neighbor list; negative values are used for links of the
!    counter-clockwise linked list of boundary edges;  May be updated on output
!    because of swaps.
!
!      LINK = -(3*I + J-1) where I, J = triangle, edge index.
!
!    Workspace, integer STACK(MAXST); on input, entries 1 through TOP
!    contain the indices of initial triangles (involving vertex I)
!    put in stack; the edges opposite I should be in interior;  entries
!    TOP+1 through MAXST are used as a stack.
!
!    Output, integer ( kind = 4 ) IERR is set to 8 for abnormal return.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2
  integer ( kind = 4 ) node_num
  integer ( kind = 4 ) element_num

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ) bedg
  integer ( kind = 4 ) btri
  integer ( kind = 4 ) c
  integer ( kind = 4 ) diaedg
  integer ( kind = 4 ) e
  integer ( kind = 4 ) ee
  integer ( kind = 4 ) em1
  integer ( kind = 4 ) ep1
  integer ( kind = 4 ) f
  integer ( kind = 4 ) fm1
  integer ( kind = 4 ) fp1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierr
  integer ( kind = 4 ) i4_wrap
  integer ( kind = 4 ) l
  real ( kind = 8 ) node_xy(dim_num,node_num)
  integer ( kind = 4 ) r
  integer ( kind = 4 ) s
  integer ( kind = 4 ) stack(node_num)
  integer ( kind = 4 ) swap
  integer ( kind = 4 ) t
  integer ( kind = 4 ) top
  integer ( kind = 4 ) element_node(3,element_num)
  integer ( kind = 4 ) element_neighbor(3,element_num)
  integer ( kind = 4 ) tt
  integer ( kind = 4 ) u
  real ( kind = 8 ) x
  real ( kind = 8 ) y
!
!  Determine whether triangles in stack are Delaunay, and swap
!  diagonal edge of convex quadrilateral if not.
!
  x = node_xy(1,i)
  y = node_xy(2,i)

  do

    if ( top <= 0 ) then
      exit
    end if

    t = stack(top)
    top = top - 1

    if ( element_node(1,t) == i ) then
      e = 2
      b = element_node(3,t)
    else if ( element_node(2,t) == i ) then
      e = 3
      b = element_node(1,t)
    else
      e = 1
      b = element_node(2,t)
    end if

    a = element_node(e,t)
    u = element_neighbor(e,t)

    if ( element_neighbor(1,u) == t ) then
      f = 1
      c = element_node(3,u)
    else if ( element_neighbor(2,u) == t ) then
      f = 2
      c = element_node(1,u)
    else
      f = 3
      c = element_node(2,u)
    end if

    swap = diaedg ( x, y, node_xy(1,a), node_xy(2,a), node_xy(1,c), &
      node_xy(2,c), node_xy(1,b), node_xy(2,b) )

    if ( swap == 1 ) then

      em1 = e - 1
      em1 = i4_wrap ( em1, 1, 3 )
      ep1 = e + 1
      ep1 = i4_wrap ( ep1, 1, 3 )
      fm1 = f - 1
      fm1 = i4_wrap ( fm1, 1, 3 )
      fp1 = f + 1
      fp1 = i4_wrap ( fp1, 1, 3 )

      element_node(ep1,t) = c
      element_node(fp1,u) = i

      r = element_neighbor(ep1,t)
      s = element_neighbor(fp1,u)

      element_neighbor(ep1,t) = u
      element_neighbor(fp1,u) = t
      element_neighbor(e,t) = s
      element_neighbor(f,u) = r

      if ( 0 < element_neighbor(fm1,u) ) then
        top = top + 1
        stack(top) = u
      end if

      if ( 0 < s ) then

        if ( element_neighbor(1,s) == u ) then
          element_neighbor(1,s) = t
        else if ( element_neighbor(2,s) == u ) then
          element_neighbor(2,s) = t
        else
          element_neighbor(3,s) = t
        end if

        top = top + 1

        if ( node_num < top ) then
          ierr = 8
          return
        end if

        stack(top) = t

      else

        if ( u == btri .and. fp1 == bedg ) then
          btri = t
          bedg = e
        end if

        l = - ( 3 * t + e - 1 )
        tt = t
        ee = em1

        do while ( 0 < element_neighbor(ee,tt) )

          tt = element_neighbor(ee,tt)

          if ( element_node(1,tt) == a ) then
            ee = 3
          else if ( element_node(2,tt) == a ) then
            ee = 1
          else
            ee = 2
          end if

        end do

        element_neighbor(ee,tt) = l

      end if

      if ( 0 < r ) then

        if ( element_neighbor(1,r) == t ) then
          element_neighbor(1,r) = u
        else if ( element_neighbor(2,r) == t ) then
          element_neighbor(2,r) = u
        else
          element_neighbor(3,r) = u
        end if

      else

        if ( t == btri .and. ep1 == bedg ) then
          btri = u
          bedg = f
        end if

        l = - ( 3 * u + f - 1 )
        tt = u
        ee = fm1

        do while ( 0 < element_neighbor(ee,tt) )

          tt = element_neighbor(ee,tt)

          if ( element_node(1,tt) == b ) then
            ee = 3
          else if ( element_node(2,tt) == b ) then
            ee = 1
          else
            ee = 2
          end if

        end do

        element_neighbor(ee,tt) = l

      end if

    end if

  end do

  return
end


end module jburk_triangulation_delaunay_
