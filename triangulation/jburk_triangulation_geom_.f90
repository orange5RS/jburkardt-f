

module     jburk_triangulation_geom_
implicit none

interface        alpha_measure
module procedure alpha_measure
end interface    alpha_measure
public           alpha_measure

interface        angle_rad_2d
module procedure angle_rad_2d
end interface    angle_rad_2d
public           angle_rad_2d

interface        arc_cosine
module procedure arc_cosine
end interface    arc_cosine
public           arc_cosine

interface        area_measure
module procedure area_measure
end interface    area_measure
public           area_measure

interface        diaedg
module procedure diaedg
end interface    diaedg
public           diaedg

contains

subroutine alpha_measure ( n, z, element_order, element_num, element_node, &
  alpha_min, alpha_ave, alpha_area )

!*****************************************************************************80
!
!! ALPHA_MEASURE determines the triangulated pointset quality measure ALPHA.
!
!  Discusion:
!
!    The ALPHA measure evaluates the uniformity of the shapes of the triangles
!    defined by a triangulated pointset.
!
!    We compute the minimum angle among all the triangles in the triangulated
!    dataset and divide by the maximum possible value (which, in degrees,
!    is 60).  The best possible value is 1, and the worst 0.  A good
!    triangulation should have an ALPHA score close to 1.
!
!    The code has been modified to 'allow' 6-node triangulations.
!    However, no effort is made to actually process the midside nodes.
!    Only information from the vertices is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) Z(2,N), the points.
!
!    Input, integer ( kind = 4 ) ELEMENT_ORDER, the order of the triangles.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of triangles.
!
!    Input, integer ( kind = 4 ) ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM),
!    the triangulation.
!
!    Output, real ( kind = 8 ) ALPHA_MIN, the minimum value of ALPHA over all
!    triangles.
!
!    Output, real ( kind = 8 ) ALPHA_AVE, the value of ALPHA averaged over
!    all triangles.
!
!    Output, real ( kind = 8 ) ALPHA_AREA, the value of ALPHA averaged over
!    all triangles and weighted by area.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order

  real ( kind = 8 ) a_angle
  integer ( kind = 4 ) a_index
  real ( kind = 8 ) a_x
  real ( kind = 8 ) a_y
  real ( kind = 8 ) ab_len
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_area
  real ( kind = 8 ) alpha_ave
  real ( kind = 8 ) alpha_min
  real ( kind = 8 ) arc_cosine
  real ( kind = 8 ) area
  real ( kind = 8 ) area_total
  real ( kind = 8 ) b_angle
  integer ( kind = 4 ) b_index
  real ( kind = 8 ) b_x
  real ( kind = 8 ) b_y
  real ( kind = 8 ) bc_len
  real ( kind = 8 ) c_angle
  integer ( kind = 4 ) c_index
  real ( kind = 8 ) c_x
  real ( kind = 8 ) c_y
  real ( kind = 8 ) ca_len
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  integer ( kind = 4 ) triangle
  integer ( kind = 4 ) element_node(element_order,element_num)
  real ( kind = 8 ) z(2,n)

  alpha_min = huge ( alpha )
  alpha_ave = 0.0D+00
  alpha_area = 0.0D+00
  area_total = 0.0D+00

  do triangle = 1, element_num

    a_index = element_node(1,triangle)
    b_index = element_node(2,triangle)
    c_index = element_node(3,triangle)

    a_x = z(1,a_index)
    a_y = z(2,a_index)
    b_x = z(1,b_index)
    b_y = z(2,b_index)
    c_x = z(1,c_index)
    c_y = z(2,c_index)

    area = 0.5D+00 * abs ( a_x * ( b_y - c_y ) &
                         + b_x * ( c_y - a_y ) &
                         + c_x * ( a_y - b_y ) )

    ab_len = sqrt ( ( a_x - b_x )**2 + ( a_y - b_y )**2 )
    bc_len = sqrt ( ( b_x - c_x )**2 + ( b_y - c_y )**2 )
    ca_len = sqrt ( ( c_x - a_x )**2 + ( c_y - a_y )**2 )
!
!  Take care of a ridiculous special case.
!
    if ( ab_len == 0.0D+00 .and. &
         bc_len == 0.0D+00 .and. &
         ca_len == 0.0D+00 ) then

      a_angle = 2.0D+00 * pi / 3.0D+00
      b_angle = 2.0D+00 * pi / 3.0D+00
      c_angle = 2.0D+00 * pi / 3.0D+00

    else

      if ( ca_len == 0.0D+00 .or. ab_len == 0.0D+00 ) then
        a_angle = pi
      else
        a_angle = arc_cosine ( ( ca_len**2 + ab_len**2 - bc_len**2 ) &
          / ( 2.0D+00 * ca_len * ab_len ) )
      end if

      if ( ab_len == 0.0D+00 .or. bc_len == 0.0D+00 ) then
        b_angle = pi
      else
        b_angle = arc_cosine ( ( ab_len**2 + bc_len**2 - ca_len**2 ) &
          / ( 2.0D+00 * ab_len * bc_len ) )
      end if

      if ( bc_len == 0.0D+00 .or. ca_len == 0.0D+00 ) then
        c_angle = pi
      else
        c_angle = arc_cosine ( ( bc_len**2 + ca_len**2 - ab_len**2 ) &
          / ( 2.0D+00 * bc_len * ca_len ) )
      end if

    end if

    alpha_min = min ( alpha_min, a_angle )
    alpha_min = min ( alpha_min, b_angle )
    alpha_min = min ( alpha_min, c_angle )

    alpha_ave = alpha_ave + alpha_min

    alpha_area = alpha_area + area * alpha_min

    area_total = area_total + area

  end do

  alpha_ave = alpha_ave / real ( element_num, kind = 8 )
  alpha_area = alpha_area / area_total
!
!  Normalize angles from [0,pi/3] degrees into qualities in [0,1].
!
  alpha_min = alpha_min * 3.0D+00 / pi
  alpha_ave = alpha_ave * 3.0D+00 / pi
  alpha_area = alpha_area * 3.0D+00 / pi

  return
end



function angle_rad_2d ( p1, p2, p3 )

!*****************************************************************************80
!
!! ANGLE_RAD_2D returns the angle swept out between two rays in 2D.
!
!  Discussion:
!
!    Except for the zero angle case, it should be true that
!
!      ANGLE_RAD_2D ( P1, P2, P3 ) + ANGLE_RAD_2D ( P3, P2, P1 ) = 2 * PI
!
!        P1
!        /
!       /
!      /
!     /
!    P2--------->P3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P1(2), P2(2), P3(2), define the rays
!    P1 - P2 and P3 - P2 which define the angle.
!
!    Output, real ( kind = 8 ) ANGLE_RAD_2D, the angle swept out by the rays,
!    in radians.  0 <= ANGLE_RAD_2D < 2 * PI.  If either ray has zero
!    length, then ANGLE_RAD_2D is set to 0.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) angle_rad_2d
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) p(dim_num)
  real ( kind = 8 ) p1(dim_num)
  real ( kind = 8 ) p2(dim_num)
  real ( kind = 8 ) p3(dim_num)

  p(1) = ( p3(1) - p2(1) ) * ( p1(1) - p2(1) ) &
       + ( p3(2) - p2(2) ) * ( p1(2) - p2(2) )


  p(2) = ( p3(1) - p2(1) ) * ( p1(2) - p2(2) ) &
       - ( p3(2) - p2(2) ) * ( p1(1) - p2(1) )

  if ( p(1) == 0.0D+00 .and. p(2) == 0.0D+00 ) then
    angle_rad_2d = 0.0D+00
    return
  end if

  angle_rad_2d = atan2 ( p(2), p(1) )

  if ( angle_rad_2d < 0.0D+00 ) then
    angle_rad_2d = angle_rad_2d + 2.0D+00 * pi
  end if

  return
end



function arc_cosine ( c )

!*****************************************************************************80
!
!! ARC_COSINE computes the arc cosine function, with argument truncation.
!
!  Discussion:
!
!    If you call your system ACOS routine with an input argument that is
!    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant
!    surprise (I did).
!
!    This routine simply truncates arguments outside the range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) C, the argument.
!
!    Output, real ( kind = 8 ) ARC_COSINE, an angle whose cosine is C.
!
  implicit none

  real ( kind = 8 ) arc_cosine
  real ( kind = 8 ) c
  real ( kind = 8 ) c2

  c2 = c
  c2 = max ( c2, -1.0D+00 )
  c2 = min ( c2, +1.0D+00 )

  arc_cosine = acos ( c2 )

  return
end



subroutine area_measure ( n, z, element_order, element_num, element_node, &
  area_min, area_max, area_ratio, area_ave, area_std )

!*****************************************************************************80
!
!! AREA_MEASURE determines the area ratio quality measure.
!
!  Discusion:
!
!    This measure computes the area of every triangle, and returns
!    the ratio of the minimum to the maximum triangle.  A value of
!    1 is "perfect", indicating that all triangles have the same area.
!    A value of 0 is the worst possible result.
!
!    The code has been modified to 'allow' 6-node triangulations.
!    However, no effort is made to actually process the midside nodes.
!    Only information from the vertices is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) Z(2,N), the points.
!
!    Input, integer ( kind = 4 ) ELEMENT_ORDER, the order of the triangles.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of triangles.
!
!    Input, integer ( kind = 4 ) ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM),
!    the triangulation.
!
!    Output, real ( kind = 8 ) AREA_MIN, AREA_MAX, the minimum and maximum
!    areas.
!
!    Output, real ( kind = 8 ) AREA_RATIO, the ratio of the minimum to the
!    maximum area.
!
!    Output, real ( kind = 8 ) AREA_AVE, the average area.
!
!    Output, real ( kind = 8 ) AREA_STD, the standard deviation of the areas.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order

  real ( kind = 8 ) area
  real ( kind = 8 ) area_ave
  real ( kind = 8 ) area_max
  real ( kind = 8 ) area_min
  real ( kind = 8 ) area_ratio
  real ( kind = 8 ) area_std
  integer ( kind = 4 ) triangle
  integer ( kind = 4 ) element_node(element_order,element_num)
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) z(2,n)

  area_max = 0.0D+00
  area_min = huge ( area_min )
  area_ave = 0.0

  do triangle = 1, element_num

    x1 = z(1,element_node(1,triangle))
    y1 = z(2,element_node(1,triangle))
    x2 = z(1,element_node(2,triangle))
    y2 = z(2,element_node(2,triangle))
    x3 = z(1,element_node(3,triangle))
    y3 = z(2,element_node(3,triangle))

    area = 0.5D+00 * abs ( x1 * ( y2 - y3 ) &
                         + x2 * ( y3 - y1 ) &
                         + x3 * ( y1 - y2 ) )

    area_min = min ( area_min, area )
    area_max = max ( area_max, area )

    area_ave = area_ave + area

  end do

  area_ave = area_ave / real ( element_num, kind = 8 )

  area_std = 0.0D+00
  do triangle = 1, element_num

    x1 = z(1,element_node(1,triangle))
    y1 = z(2,element_node(1,triangle))
    x2 = z(1,element_node(2,triangle))
    y2 = z(2,element_node(2,triangle))
    x3 = z(1,element_node(3,triangle))
    y3 = z(2,element_node(3,triangle))

    area = 0.5D+00 * abs ( x1 * ( y2 - y3 ) &
                         + x2 * ( y3 - y1 ) &
                         + x3 * ( y1 - y2 ) )

    area_std = area_std + ( area - area_ave )**2
  end do
  area_std = sqrt ( area_std / real ( element_num, kind = 8 ) )

  if ( 0.0D+00 < area_max ) then
    area_ratio = area_min / area_max
  else
    area_ratio = 0.0D+00
  end if

  return
end



function diaedg ( x0, y0, x1, y1, x2, y2, x3, y3 )

!*****************************************************************************80
!
!! DIAEDG chooses a diagonal edge.
!
!  Discussion:
!
!    The routine determines whether 0--2 or 1--3 is the diagonal edge
!    that should be chosen, based on the circumcircle criterion, where
!    (X0,Y0), (X1,Y1), (X2,Y2), (X3,Y3) are the vertices of a simple
!    quadrilateral in counterclockwise order.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2001
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
!    Input, real ( kind = 8 ) X0, Y0, X1, Y1, X2, Y2, X3, Y3, the
!    coordinates of the vertices of a quadrilateral, given in
!    counterclockwise order.
!
!    Output, integer ( kind = 4 ) DIAEDG, chooses a diagonal:
!    +1, if diagonal edge 02 is chosen;
!    -1, if diagonal edge 13 is chosen;
!     0, if the four vertices are cocircular.
!
  implicit none

  real ( kind = 8 ) ca
  real ( kind = 8 ) cb
  integer ( kind = 4 ) diaedg
  real ( kind = 8 ) dx10
  real ( kind = 8 ) dx12
  real ( kind = 8 ) dx30
  real ( kind = 8 ) dx32
  real ( kind = 8 ) dy10
  real ( kind = 8 ) dy12
  real ( kind = 8 ) dy30
  real ( kind = 8 ) dy32
  real ( kind = 8 ) s
  real ( kind = 8 ) tol
  real ( kind = 8 ) tola
  real ( kind = 8 ) tolb
  real ( kind = 8 ) x0
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3

  tol = 100.0D+00 * epsilon ( tol )

  dx10 = x1 - x0
  dy10 = y1 - y0
  dx12 = x1 - x2
  dy12 = y1 - y2
  dx30 = x3 - x0
  dy30 = y3 - y0
  dx32 = x3 - x2
  dy32 = y3 - y2

  tola = tol * max ( abs ( dx10 ), abs ( dy10 ), abs ( dx30 ), abs ( dy30 ) )
  tolb = tol * max ( abs ( dx12 ), abs ( dy12 ), abs ( dx32 ), abs ( dy32 ) )

  ca = dx10 * dx30 + dy10 * dy30
  cb = dx12 * dx32 + dy12 * dy32

  if ( tola < ca .and. tolb < cb ) then

    diaedg = - 1

  else if ( ca < - tola .and. cb < - tolb ) then

    diaedg = 1

  else

    tola = max ( tola, tolb )

    s = ( dx10 * dy30 - dx30 * dy10 ) * cb &
      + ( dx32 * dy12 - dx12 * dy32 ) * ca

    if ( tola < s ) then
      diaedg = - 1
    else if ( s < - tola ) then
      diaedg = 1
    else
      diaedg = 0
    end if

  end if

  return
end


end module jburk_triangulation_geom_
