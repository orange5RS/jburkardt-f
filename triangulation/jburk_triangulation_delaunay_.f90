

module jburk_triangulation_delaunay_
implicit none

interface        delaunay_swap_test
module procedure delaunay_swap_test
end interface    delaunay_swap_test
public           delaunay_swap_test

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


end module jburk_triangulation_delaunay_
