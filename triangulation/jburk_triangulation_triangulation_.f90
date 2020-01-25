

module jburk_triangulation_triangulation_
implicit none



contains



subroutine element_order3_physical_to_reference ( t, n, phy, ref )

!*****************************************************************************80
!
!! ELEMENT_ORDER3_PHYSICAL_TO_REFERENCE maps T3 physical to reference points.
!
!  Discussion:
!
!    Given the vertices of an order 3 physical triangle and a point
!    (X,Y) in the physical triangle, the routine computes the value
!    of the corresponding image point (XSI,ETA) in reference space.
!
!    This routine is also appropriate for an order 4 triangle, assuming
!    that the fourth node is always the centroid of the triangle.
!
!    This routine may be appropriate for an order 6
!    triangle, if the mapping between reference and physical space
!    is linear.  This implies, in particular, that the sides of the
!    image triangle are straight and that the "midside" nodes in the
!    physical triangle are halfway along the sides of
!    the physical triangle.
!
!  Reference Element T3:
!
!    |
!    1  3
!    |  |\
!    |  | \
!    S  |  \
!    |  |   \
!    |  |    \
!    0  1-----2
!    |
!    +--0--R--1-->
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the X and Y coordinates
!    of the vertices.  The vertices are assumed to be the images of
!    (0,0), (1,0) and (0,1) respectively.
!
!    Input, integer ( kind = 4 ) N, the number of points to transform.
!
!    Input, real ( kind = 8 ) PHY(2,N), the coordinates of physical points
!    to be transformed.
!
!    Output, real ( kind = 8 ) REF(2,N), the coordinates of the corresponding
!    points in the reference space.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) phy(2,n)
  real ( kind = 8 ) ref(2,n)
  real ( kind = 8 ) t(2,3)

  ref(1,1:n) = ( ( t(2,3) - t(2,1) ) * ( phy(1,1:n) - t(1,1) )   &
               - ( t(1,3) - t(1,1) ) * ( phy(2,1:n) - t(2,1) ) ) &
             / ( ( t(2,3) - t(2,1) ) * ( t(1,2)     - t(1,1) )   &
               - ( t(1,3) - t(1,1) ) * ( t(2,2)     - t(2,1) ) )

  ref(2,1:n) = ( ( t(1,2) - t(1,1) ) * ( phy(2,1:n) - t(2,1) )   &
               - ( t(2,2) - t(2,1) ) * ( phy(1,1:n) - t(1,1) ) ) &
             / ( ( t(2,3) - t(2,1) ) * ( t(1,2)     - t(1,1) )   &
               - ( t(1,3) - t(1,1) ) * ( t(2,2)     - t(2,1) ) )

  return
end


subroutine element_order3_reference_to_physical ( t, n, ref, phy )

!*****************************************************************************80
!
!! ELEMENT_ORDER3_REFERENCE_TO_PHYSICAL maps T3 reference to physical points.
!
!  Discussion:
!
!    Given the vertices of an order 3 physical triangle and a point
!    (XSI,ETA) in the reference triangle, the routine computes the value
!    of the corresponding image point (X,Y) in physical space.
!
!    This routine is also appropriate for an order 4 triangle,
!    as long as the fourth node is the centroid of the triangle.
!
!    This routine may also be appropriate for an order 6
!    triangle, if the mapping between reference and physical space
!    is linear.  This implies, in particular, that the sides of the
!    image triangle are straight and that the "midside" nodes in the
!    physical triangle are halfway along the sides of
!    the physical triangle.
!
!  Reference Element T3:
!
!    |
!    1  3
!    |  |\
!    |  | \
!    S  |  \
!    |  |   \
!    |  |    \
!    0  1-----2
!    |
!    +--0--R--1-->
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the coordinates of the vertices.
!    The vertices are assumed to be the images of (0,0), (1,0) and
!    (0,1) respectively.
!
!    Input, integer ( kind = 4 ) N, the number of points to transform.
!
!    Input, real ( kind = 8 ) REF(2,N), points in the reference triangle.
!
!    Output, real ( kind = 8 ) PHY(2,N), corresponding points in the
!    physical triangle.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) phy(2,n)
  real ( kind = 8 ) ref(2,n)
  real ( kind = 8 ) t(2,3)

  do i = 1, 2
    phy(i,1:n) = t(i,1) * ( 1.0D+00 - ref(1,1:n) - ref(2,1:n) ) &
               + t(i,2) *             ref(1,1:n)                &
               + t(i,3) *                          ref(2,1:n)
  end do

  return
end






subroutine element_order6_physical_to_reference ( t, n, phy, ref )

!*****************************************************************************80
!
!! ELEMENT_ORDER6_PHYSICAL_TO_REFERENCE maps T6  physical to reference points.
!
!  Discussion:
!
!    Given the vertices of an order 6 physical triangle and a point
!    (X,Y) in the physical triangle, the routine computes the value
!    of the corresponding image point (R,S) in reference space.
!
!    The mapping from (R,S) to (X,Y) has the form:
!
!      X(R,S) = A1 * R * R + B1 * R * S + C1 * S * S
!             + D1 * R     + E1 * S     + F1
!
!      Y(R,S) = A2 * R * R + B2 * R * S + C2 * S * S
!             + D2 * R     + E2 * S     + F2
!
!  Reference Element T3:
!
!    |
!    1  3
!    |  |\
!    |  | \
!    S  6  5
!    |  |   \
!    |  |    \
!    0  1--4--2
!    |
!    +--0--R--1-->
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,6), the coordinates of the vertices.
!    The vertices are assumed to be the images of (0,0), (1,0), (0,1),
!    (1/2,0), (1/2,1/2) and (0,1/2), in that order.
!
!    Input, integer ( kind = 4 ) N, the number of points to transform.
!
!    Input, real ( kind = 8 ) PHY(2,N), the coordinates of points in the
!    physical space.
!
!    Output, real ( kind = 8 ) REF(2,N), the coordinates of the corresponding
!    points in the reference space.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) b(2)
  real ( kind = 8 ) c(2)
  real ( kind = 8 ) d(2)
  real ( kind = 8 ) det
  real ( kind = 8 ) dx(2)
  real ( kind = 8 ) e(2)
  real ( kind = 8 ) f(2)
  real ( kind = 8 ) fun(2)
  real ( kind = 8 ) fun_norm
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it
  integer ( kind = 4 ) j
  real ( kind = 8 ) jac(2,2)
  integer ( kind = 4 ), parameter :: it_max = 10
  real ( kind = 8 ), parameter :: it_tol = 0.000001D+00
  real ( kind = 8 ) phy(2,n)
  real ( kind = 8 ) ref(2,n)
  real ( kind = 8 ) t(2,6)
!
!  Set iteration parameters.
!
  do i = 1, 2

    a(i) =   2.0D+00 * t(i,1) + 2.0D+00 * t(i,2)                    &
           - 4.0D+00 * t(i,4)

    b(i) =   4.0D+00 * t(i,1)                                       &
           - 4.0D+00 * t(i,4) + 4.0D+00 * t(i,5) - 4.0D+00 * t(i,6)

    c(i) =   2.0D+00 * t(i,1)                    + 2.0D+00 * t(i,3) &
                                                 - 4.0D+00 * t(i,6)

    d(i) = - 3.0D+00 * t(i,1) -           t(i,2)                    &
           + 4.0D+00 * t(i,4)

    e(i) = - 3.0D+00 * t(i,1)                    -           t(i,3) &
                                                 + 4.0D+00 * t(i,6)
    f(i) =             t(i,1)

  end do
!
!  Initialize the points by inverting the linear map.
!
  call element_order3_physical_to_reference ( t(1:2,1:3), n, phy, ref )
!
!  Carry out the Newton iteration.
!
  do j = 1, n

    do it = 1, it_max

      fun(1:2) = a(1:2) * ref(1,j) * ref(1,j) &
               + b(1:2) * ref(1,j) * ref(2,j) &
               + c(1:2) * ref(2,j) * ref(2,j) &
               + d(1:2) * ref(1,j) &
               + e(1:2) * ref(2,j) &
               + f(1:2) &
               - phy(1:2,j)

      fun_norm = sqrt ( fun(1) * fun(1) + fun(2) * fun(2) )

      if ( fun_norm <= it_tol ) then
        exit
      end if

      jac(1:2,1) = 2.0D+00 * a(1:2) * ref(1,j) &
                 +           b(1:2) * ref(2,j) + d(1:2)

      jac(1:2,2) =           b(1:2) * ref(1,j) &
                 + 2.0D+00 * c(1:2) * ref(2,j) + e(1:2)

      det = jac(1,1) * jac(2,2) - jac(1,2) * jac(2,1)

      if ( det == 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) &
          'ELEMENT_ORDER6_PHYSICAL_TO_REFERENCE - Fatal error!'
        write ( *, '(a)' ) '  The jacobian of the mapping is singular.'
      end if

      dx(1) = (  jac(2,2) * fun(1) - jac(1,2) * fun(2) ) / det
      dx(2) = ( -jac(2,1) * fun(1) + jac(1,1) * fun(2) ) / det

      ref(1:2,j) = ref(1:2,j) - dx(1:2)

    end do

  end do

  return
end



subroutine element_order6_reference_to_physical ( t, n, ref, phy )

!*****************************************************************************80
!
!! ELEMENT_ORDER6_REFERENCE_TO_PHYSICAL maps T6 reference to physical points.
!
!  Discussion:
!
!    Given the vertices of an order 6 physical triangle and a point
!    (XSI,ETA) in the reference triangle, the routine computes the value
!    of the corresponding image point (X,Y) in physical space.
!
!    The mapping from (XSI,ETA) to (X,Y) has the form:
!
!      X(ETA,XSI) = A1 * XSI**2 + B1 * XSI*ETA + C1 * ETA**2
!                 + D1 * XSI    + E1 * ETA     + F1
!
!      Y(ETA,XSI) = A2 * XSI**2 + B2 * XSI*ETA + C2 * ETA**2
!                 + D2 * XSI    + E2 * ETA     + F2
!
!  Reference Element T6:
!
!    |
!    1  3
!    |  |\
!    |  | \
!    S  6  5
!    |  |   \
!    |  |    \
!    0  1--4--2
!    |
!    +--0--R--1-->
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,6), the coordinates of the vertices.
!    The vertices are assumed to be the images of (0,0), (1,0),
!    (0,1),(1/2,0), (1/2,1/2) and (0,1/2) respectively.
!
!    Input, integer ( kind = 4 ) N, the number of points to transform.
!
!    Input, real ( kind = 8 ) REF(2,N), points in the reference triangle.
!
!    Output, real ( kind = 8 ) PHY(2,N), corresponding points in the
!    physical triangle.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) b(2)
  real ( kind = 8 ) c(2)
  real ( kind = 8 ) d(2)
  real ( kind = 8 ) e(2)
  real ( kind = 8 ) f(2)
  integer ( kind = 4 ) i
  real ( kind = 8 ) phy(2,n)
  real ( kind = 8 ) ref(2,n)
  real ( kind = 8 ) t(2,6)

  do i = 1, 2

    a(i) =   2.0D+00 * t(i,1) + 2.0D+00 * t(i,2)                    &
           - 4.0D+00 * t(i,4)

    b(i) =   4.0D+00 * t(i,1)                                       &
           - 4.0D+00 * t(i,4) + 4.0D+00 * t(i,5) - 4.0D+00 * t(i,6)

    c(i) =   2.0D+00 * t(i,1)                    + 2.0D+00 * t(i,3) &
                                                 - 4.0D+00 * t(i,6)

    d(i) = - 3.0D+00 * t(i,1) -           t(i,2)                    &
           + 4.0D+00 * t(i,4)

    e(i) = - 3.0D+00 * t(i,1)                    -           t(i,3) &
                                                 + 4.0D+00 * t(i,6)
    f(i) =             t(i,1)

  end do

  do i = 1, 2
    phy(i,1:n) = a(i) * ref(1,1:n) * ref(1,1:n) &
               + b(i) * ref(1,1:n) * ref(2,1:n) &
               + c(i) * ref(2,1:n) * ref(2,1:n) &
               + d(i) * ref(1,1:n) &
               + e(i) * ref(2,1:n) &
               + f(i)
  end do

  return
end



subroutine triangle_reference_sample ( n, seed, p )

!*****************************************************************************80
!
!! TRIANGLE_REFERENCE_SAMPLE returns random points in the reference triangle.
!
!  Diagram:
!
!       3
!    s  |\
!    i  | \
!    d  |  \
!    e  |   \  side 2
!       |    \
!    3  |     \
!       |      \
!       1-------2
!
!         side 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points to generate.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) P(2,N), random points in the triangle.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2
  integer ( kind = 4 ) n

  real ( kind = 8 ) alpha
  real ( kind = 8 ) beta
  integer ( kind = 4 ) j
  real ( kind = 8 ) p(dim_num,n)
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  do j = 1, n

    r = r8_uniform_01 ( seed )
!
!  Interpret R as a percentage of the triangle's area.
!
!  Imagine a line L, parallel to side 1, so that the area between
!  vertex 1 and line L is R percent of the full triangle's area.
!
!  The line L will intersect sides 2 and 3 at a fraction
!  ALPHA = SQRT ( R ) of the distance from vertex 1 to vertices 2 and 3.
!
    alpha = sqrt ( r )
!
!  Now choose, uniformly at random, a point on the line L.
!
    beta = r8_uniform_01 ( seed )

    p(1,j) = ( 1.0D+00 - beta ) * alpha
    p(2,j) =             beta   * alpha

  end do

  return
end



subroutine triangle_sample ( t, n, seed, p )

!*****************************************************************************80
!
!! TRIANGLE_SAMPLE returns random points in a triangle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T(2,3), the triangle vertices.
!
!    Input, integer ( kind = 4 ) N, the number of points to generate.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) P(2,N), random points in the triangle.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2
  integer ( kind = 4 ) n

  real ( kind = 8 ) alpha(n)
  integer ( kind = 4 ) dim
  real ( kind = 8 ) p(dim_num,n)
  real ( kind = 8 ) p12(dim_num,n)
  real ( kind = 8 ) p13(dim_num,n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t(dim_num,3)
!
!  For comparison between F90, C++ and MATLAB codes, call R8VEC_UNIFORM_01.
!
  call r8vec_uniform_01 ( n, seed, alpha )
!
!  Interpret R as a percentage of the triangle's area.
!
!  Imagine a line L, parallel to side 1, so that the area between
!  vertex 1 and line L is R percent of the full triangle's area.
!
!  The line L will intersect sides 2 and 3 at a fraction
!  ALPHA = SQRT ( R ) of the distance from vertex 1 to vertices 2 and 3.
!
  alpha(1:n) = sqrt ( alpha(1:n) )
!
!  Determine the coordinates of the points on sides 2 and 3 intersected
!  by line L.
!
  do dim = 1, dim_num

    p12(dim,1:n) = ( 1.0D+00 - alpha(1:n) ) * t(dim,1) &
                             + alpha(1:n)   * t(dim,2)

    p13(dim,1:n) = ( 1.0D+00 - alpha(1:n) ) * t(dim,1) &
                             + alpha(1:n)   * t(dim,3)

  end do
!
!  Now choose, uniformly at random, a point on the line L.
!
  call r8vec_uniform_01 ( n, seed, alpha )

  do dim = 1, dim_num

    p(dim,1:n) = ( 1.0D+00 - alpha(1:n) ) * p12(dim,1:n) &
                           + alpha(1:n)   * p13(dim,1:n)

  end do

  return
end



function triangulation_area ( node_num, node_xy, element_order, &
  element_num, element_node )

!*****************************************************************************80
!
!! TRIANGULATION_AREA computes the area of a triangulation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Input, integer ( kind = 4 ) ELEMENT_ORDER, the order of the triangles.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of triangles.
!
!    Input, integer ( kind = 4 ) ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM),
!    the nodes making up each triangle.
!
!    Output, real ( kind = 8 ) TRIANGULATION_AREA, the area.
!
  implicit none

  integer ( kind = 4 ) node_num
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order

  integer ( kind = 4 ) element
  real ( kind = 8 ) element_area
  integer ( kind = 4 ) element_node(element_order,element_num)
  real ( kind = 8 ) element_xy(2,3)
  real ( kind = 8 ) node_xy(2,node_num)
  real ( kind = 8 ) triangulation_area
  real ( kind = 8 ) value

  value = 0.0D+00

  do element = 1, element_num

    element_xy(1:2,1:3) = node_xy(1:2,element_node(1:3,element))

    call triangle_area_2d ( element_xy, element_area )

    value = value + element_area

  end do

  triangulation_area = value

  return
end



subroutine triangulation_areas ( node_num, node_xy, element_order, &
  element_num, element_node, triangle_area, triangulation_area )

!*****************************************************************************80
!
!! TRIANGULATION_AREAS computes triangle and triangulation areas.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NODE_NUM, the number of nodes in the
!    triangulation.
!
!    Input, real ( kind = 8 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Input, integer ( kind = 4 ) ELEMENT_ORDER, the order of triangles in
!    the triangulation.
!
!    Input, integer ( kind = 4 ) ELEMENT_NUM, the number of triangles in
!    the triangulation.
!
!    Input, integer ( kind = 4 ) ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM),
!    the nodes making up each triangle.
!
!    Output, real ( kind = 8 ) TRIANGLE_AREA(ELEMENT_NUM), the area of
!    the triangles.
!
!    Output, real ( kind = 8 ) TRIANGULATION_AREA, the area of
!    the triangulation.
!
  implicit none

  integer ( kind = 4 ) node_num
  integer ( kind = 4 ) element_num
  integer ( kind = 4 ) element_order

  real ( kind = 8 ) node_xy(2,node_num)
  integer ( kind = 4 ) triangle
  real ( kind = 8 ) triangle_area(element_num)
  integer ( kind = 4 ) element_node(element_order,element_num)
  real ( kind = 8 ) triangle_xy(2,3)
  real ( kind = 8 ) triangulation_area

  triangulation_area = 0.0D+00

  do triangle = 1, element_num

    triangle_xy(1:2,1:3) = node_xy(1:2,element_node(1:3,triangle))

    call triangle_area_2d ( triangle_xy, triangle_area(triangle) )

    triangulation_area = triangulation_area + triangle_area(triangle)

  end do

  return
end





end module jburk_triangulation_triangulation_
