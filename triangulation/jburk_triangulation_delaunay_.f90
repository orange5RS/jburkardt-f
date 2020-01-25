

module jburk_triangulation_delaunay_
implicit none

interface        delaunay_swap_test
module procedure delaunay_swap_test
end interface    delaunay_swap_test
public           delaunay_swap_test

interface        r8tris2
module procedure r8tris2
end interface    r8tris2
public           r8tris2

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



subroutine r8tris2 ( node_num, node_xy, element_num, element_node, &
  element_neighbor )

!*****************************************************************************80
!
!! R8TRIS2 constructs a Delaunay triangulation of 2D vertices.
!
!  Discussion:
!
!    The routine constructs the Delaunay triangulation of a set of 2D vertices
!    using an incremental approach and diagonal edge swaps.  Vertices are
!    first sorted in lexicographically increasing (X,Y) order, and
!    then are inserted one at a time from outside the convex hull.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2001
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
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input/output, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates
!    of the nodes.  On output, the vertices have been sorted into
!    dictionary order.
!
!    Output, integer ( kind = 4 ) ELEMENT_NUM, the number of triangles in the
!    triangulation;  ELEMENT_NUM is equal to 2*NODE_NUM - NB - 2, where NB is
!    the number of boundary vertices.
!
!    Output, integer ( kind = 4 ) ELEMENT_NODE(3,ELEMENT_NUM), the nodes that
!    make up each triangle.  The elements are indices of P.  The vertices of
!    the triangles are in counter clockwise order.
!
!    Output, integer ( kind = 4 ) ELEMENT_NEIGHBOR(3,ELEMENT_NUM), the
!    triangle neighbor list.  Positive elements are indices of TIL; negative
!    elements are used for links of a counter clockwise linked list of boundary
!    edges;  LINK = -(3*I + J-1) where I, J = triangle, edge index;
!    ELEMENT_NEIGHBOR(J,I) refers to the neighbor along edge from vertex J
!    to J+1 (mod 3).
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2
  integer ( kind = 4 ) node_num

  real ( kind = 8 ) cmax
  integer ( kind = 4 ) e
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierr
  integer ( kind = 4 ) indx(node_num)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) ledg
  integer ( kind = 4 ) lr
  integer ( kind = 4 ) lrline
  integer ( kind = 4 ) ltri
  integer ( kind = 4 ) m
  integer ( kind = 4 ) m1
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) n
  real ( kind = 8 ) node_xy(dim_num,node_num)
  integer ( kind = 4 ) redg
  integer ( kind = 4 ) rtri
  integer ( kind = 4 ) stack(node_num)
  integer ( kind = 4 ) t
  real ( kind = 8 ) tol
  integer ( kind = 4 ) top
  integer ( kind = 4 ) element_neighbor(3,node_num*2)
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_node(3,node_num*2)

  tol = 100.0D+00 * epsilon ( tol )

  ierr = 0
!
!  Sort the vertices by increasing (x,y).
!
  call r82vec_sort_heap_index_a ( node_num, node_xy, indx )

  call r82vec_permute ( node_num, indx, node_xy )
!
!  Make sure that the data nodes are "reasonably" distinct.
!
  m1 = 1

  do i = 2, node_num

    m = m1
    m1 = i

    k = 0

    do j = 1, dim_num

      cmax = max ( abs ( node_xy(j,m) ), abs ( node_xy(j,m1) ) )

      if ( tol * ( cmax + 1.0D+00 ) &
           < abs ( node_xy(j,m) - node_xy(j,m1) ) ) then
        k = j
        exit
      end if

    end do

    if ( k == 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
      write ( *, '(a,i8)' ) '  Fails for point number I = ', i
      write ( *, '(a,i8)' ) '  M = ', m
      write ( *, '(a,i8)' ) '  M1 = ', m1
      write ( *, '(a,2g14.6)' ) '  NODE_XY(M)  = ', node_xy(1:dim_num,m)
      write ( *, '(a,2g14.6)' ) '  NODE_XY(M1) = ', node_xy(1:dim_num,m1)
      ierr = 224
      stop
    end if

  end do
!
!  Starting from nodes M1 and M2, search for a third point M that
!  makes a "healthy" triangle (M1,M2,M)
!
  m1 = 1
  m2 = 2
  j = 3

  do

    if ( node_num < j ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
      ierr = 225
      stop
    end if

    m = j

    lr = lrline ( node_xy(1,m), node_xy(2,m), node_xy(1,m1), &
      node_xy(2,m1), node_xy(1,m2), node_xy(2,m2), 0.0D+00 )

    if ( lr /= 0 ) then
      exit
    end if

    j = j + 1

  end do
!
!  Set up the triangle information for (M1,M2,M), and for any other
!  triangles you created because points were collinear with M1, M2.
!
  element_num = j - 2

  if ( lr == -1 ) then

    element_node(1,1) = m1
    element_node(2,1) = m2
    element_node(3,1) = m
    element_neighbor(3,1) = -3

    do i = 2, element_num

      m1 = m2
      m2 = i+1

      element_node(1,i) = m1
      element_node(2,i) = m2
      element_node(3,i) = m

      element_neighbor(1,i-1) = -3 * i
      element_neighbor(2,i-1) = i
      element_neighbor(3,i) = i - 1

    end do

    element_neighbor(1,element_num) = -3 * element_num - 1
    element_neighbor(2,element_num) = -5
    ledg = 2
    ltri = element_num

  else

    element_node(1,1) = m2
    element_node(2,1) = m1
    element_node(3,1) = m

    element_neighbor(1,1) = -4

    do i = 2, element_num

      m1 = m2
      m2 = i+1

      element_node(1,i) = m2
      element_node(2,i) = m1
      element_node(3,i) = m

      element_neighbor(3,i-1) = i
      element_neighbor(1,i) = -3 * i - 3
      element_neighbor(2,i) = i - 1

    end do

    element_neighbor(3,element_num) = -3 * element_num
    element_neighbor(2,1) = -3 * element_num - 2
    ledg = 2
    ltri = 1

  end if
!
!  Insert the vertices one at a time from outside the convex hull,
!  determine visible boundary edges, and apply diagonal edge swaps until
!  Delaunay triangulation of vertices (so far) is obtained.
!
  top = 0

  do i = j+1, node_num

    m = i
    m1 = element_node(ledg,ltri)

    if ( ledg <= 2 ) then
      m2 = element_node(ledg+1,ltri)
    else
      m2 = element_node(1,ltri)
    end if

    lr = lrline ( node_xy(1,m), node_xy(2,m), node_xy(1,m1), &
      node_xy(2,m1), node_xy(1,m2), node_xy(2,m2), 0.0D+00 )

    if ( 0 < lr ) then
      rtri = ltri
      redg = ledg
      ltri = 0
    else
      l = -element_neighbor(ledg,ltri)
      rtri = l / 3
      redg = mod ( l, 3 ) + 1
    end if

    call vbedg ( node_xy(1,m), node_xy(2,m), node_num, node_xy, element_num, &
      element_node, element_neighbor, ltri, ledg, rtri, redg )

    n = element_num + 1
    l = -element_neighbor(ledg,ltri)

    do

      t = l / 3
      e = mod ( l, 3 ) + 1
      l = -element_neighbor(e,t)
      m2 = element_node(e,t)

      if ( e <= 2 ) then
        m1 = element_node(e+1,t)
      else
        m1 = element_node(1,t)
      end if

      element_num = element_num + 1
      element_neighbor(e,t) = element_num

      element_node(1,element_num) = m1
      element_node(2,element_num) = m2
      element_node(3,element_num) = m

      element_neighbor(1,element_num) = t
      element_neighbor(2,element_num) = element_num - 1
      element_neighbor(3,element_num) = element_num + 1

      top = top + 1

      if ( node_num < top ) then
        ierr = 8
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
        write ( *, '(a)' ) '  Stack overflow.'
        stop
      end if

      stack(top) = element_num

      if ( t == rtri .and. e == redg ) then
        exit
      end if

    end do

    element_neighbor(ledg,ltri) = -3 * n - 1
    element_neighbor(2,n) = -3 * element_num - 2
    element_neighbor(3,element_num) = -l

    ltri = n
    ledg = 2

    call swapec ( m, top, ltri, ledg, node_num, node_xy, element_num, &
      element_node, element_neighbor, stack, ierr )

    if ( ierr /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8TRIS2 - Fatal error!'
      write ( *, '(a)' ) '  Error return from SWAPEC.'
      stop
    end if

  end do
!
!  Now account for the sorting that we did.
!
  do i = 1, 3
    do j = 1, element_num
      element_node(i,j) = indx ( element_node(i,j) )
    end do
  end do

  call perm_inverse ( node_num, indx )

  call r82vec_permute ( node_num, indx, node_xy )

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
