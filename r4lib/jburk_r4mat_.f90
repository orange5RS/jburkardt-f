module     jburk_r4mat_
use, intrinsic :: iso_fortran_env
implicit none

contains

subroutine r4mat_border_add ( m, n, table, table2 )

!*****************************************************************************80
!
!! R4MAT_BORDER_ADD adds a "border" to an R4MAT.
!
!  Discussion:
!
!    We suppose the input data gives values of a quantity on nodes
!    in the interior of a 2D grid, and we wish to create a new table
!    with additional positions for the nodes that would be on the
!    border of the 2D grid.
!
!                  0 0 0 0 0 0
!      * * * *     0 * * * * 0
!      * * * * --> 0 * * * * 0
!      * * * *     0 * * * * 0
!                  0 0 0 0 0 0
!
!    The illustration suggests the situation in which a 3 by 4 array
!    is input, and a 5 by 6 array is to be output.
!
!    The old data is shifted to its correct positions in the new array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the spatial dimension.
!
!    Input, integer (kind=4) :: N, the number of points.
!
!    Input, real (kind=4) :: TABLE(M,N), the table data.
!
!    Output, real (kind=4) :: TABLE2(M+2,N+2), the augmented table data.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: table(m,n)
  real (kind=4) :: table2(m+2,n+2)

  table2(1,1:n+2) = 0.0E+00
  table2(m+2,1:n+2) = 0.0E+00
  table2(2:m+1,1) = 0.0E+00
  table2(2:m+1,n+2) = 0.0E+00

  table2(2:m+1,2:n+1) = table(1:m,1:n)

  return
end
function r4r4_compare ( x1, y1, x2, y2 )

!*****************************************************************************80
!
!! R4R4_COMPARE compares two R4R4's.
!
!  Discussion:
!
!    An R4R4 is simply a pair of R4 values, stored separately.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X1, Y1, the first vector.
!
!    Input, real (kind=4) :: X2, Y2, the second vector.
!
!    Output, integer (kind=4) :: R4R4_COMPARE:
!    -1, (X1,Y1) < (X2,Y2);
!     0, (X1,Y1) = (X2,Y2);
!    +1, (X1,Y1) > (X2,Y2).
!
  implicit none

  integer (kind=4) :: compare
  integer (kind=4) :: r4r4_compare
  real (kind=4) :: x1
  real (kind=4) :: x2
  real (kind=4) :: y1
  real (kind=4) :: y2

  if ( x1 < x2 ) then
    compare = -1
  else if ( x2 < x1 ) then
    compare = +1
  else if ( y1 < y2 ) then
    compare = -1
  else if ( y2 < y1 ) then
    compare = +1
  else
    compare = 0
  end if

  r4r4_compare = compare

  return
end
subroutine r4r4_print ( a1, a2, title )

!*****************************************************************************80
!
!! R4R4_PRINT prints an R4R4.
!
!  Discussion:
!
!    An R4R4 is simply a pair of R4R4's, stored separately.
!
!    A format is used which suggests a coordinate pair:
!
!  Example:
!
!    Center : ( 1.23, 7.45 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A1, A2, the coordinates of the vector.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  real      ( kind = 4 ) a1
  real      ( kind = 4 ) a2
  character ( len = * )  title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '( 2x, a, a4, g14.6, a1, g14.6, a1 )' ) &
      trim ( title ), ' : (', a1, ',', a2, ')'
  else
    write ( *, '( 2x, a1, g14.6, a1, g14.6, a1 )' ) '(', a1, ',', a2, ')'
  end if

  return
end
function r4r4r4_compare ( x1, y1, z1, x2, y2, z2 )

!*****************************************************************************80
!
!! R4R4R4_COMPARE compares two R4R4R4's.
!
!  Discussion:
!
!    An R4R4R4 is simply 3 R4 values, stored as scalars.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X1, Y1, Z1, the first vector.
!
!    Input, real (kind=4) :: X2, Y2, Z2, the second vector.
!
!    Output, integer (kind=4) :: R4R4R4_COMPARE:
!    -1, (X1,Y1,Z1) < (X2,Y2,Z2);
!     0, (X1,Y1,Z1) = (X2,Y2,Z2);
!    +1, (X1,Y1,Z1) > (X2,Y2,Z2).
!
  implicit none

  integer (kind=4) :: compare
  integer (kind=4) :: r4r4r4_compare
  real (kind=4) :: x1
  real (kind=4) :: x2
  real (kind=4) :: y1
  real (kind=4) :: y2
  real (kind=4) :: z1
  real (kind=4) :: z2

  if ( x1 < x2 ) then
    compare = -1
  else if ( x2 < x1 ) then
    compare = +1
  else if ( y1 < y2 ) then
    compare = -1
  else if ( y2 < y1 ) then
    compare = +1
  else if ( z1 < z2 ) then
    compare = -1
  else if ( z2 < z1 ) then
    compare = +1
  else
    compare = 0
  end if

  r4r4r4_compare = compare

  return
end
subroutine r4r4r4vec_index_insert_unique ( n_max, n, x, y, z, indx, &
  xval, yval, zval, ival, ierror )

!*****************************************************************************80
!
!! R4R4R4VEC_INDEX_INSERT_UNIQUE inserts unique R4R4R in an indexed sorted list.
!
!  Discussion:
!
!    An R4R4R4VEC is set of N R4R4R4 items.
!
!    An R4R4R4 is simply 3 R4 values, stored as scalars.
!
!    If the input value does not occur in the current list, it is added,
!    and N, X, Y, Z and INDX are updated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N_MAX, the maximum size of the list.
!
!    Input/output, integer (kind=4) :: N, the size of the list.
!
!    Input/output, real (kind=4) :: X(N), Y(N), Z(N), the R4R4R4 vector.
!
!    Input/output, integer (kind=4) :: INDX(N), the sort index of the list.
!
!    Input, real (kind=4) :: XVAL, YVAL, ZVAL, the value to be inserted
!    if it is not already in the list.
!
!    Output, integer (kind=4) :: IVAL, the index in X, Y, Z corresponding
!    to the value XVAL, YVAL, ZVAL.
!
!    Output, integer (kind=4) :: IERROR, 0 for no error, 1 if an error
!    occurred.
!
  implicit none

  integer (kind=4) :: n_max

  integer (kind=4) :: equal
  integer (kind=4) :: ierror
  integer (kind=4) :: indx(n_max)
  integer (kind=4) :: ival
  integer (kind=4) :: less
  integer (kind=4) :: more
  integer (kind=4) :: n
  real (kind=4) :: x(n_max)
  real (kind=4) :: xval
  real (kind=4) :: y(n_max)
  real (kind=4) :: yval
  real (kind=4) :: z(n_max)
  real (kind=4) :: zval

  ierror = 0

  if ( n <= 0 ) then

    if ( n_max <= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4R4R4VEC_INDEX_INSERT_UNIQUE - Fatal error!'
      write ( *, '(a)' ) '  Not enough space to store new data.'
      return
    end if

    n = 1
    x(1) = xval
    y(1) = yval
    z(1) = zval
    indx(1) = 1
    ival = 1
    return

  end if
!
!  Does ( XVAL, YVAL, ZVAL ) already occur in ( X, Y, Z)?
!
  call r4r4r4vec_index_search ( n, x, y, z, indx, xval, yval, zval, &
    less, equal, more )

  if ( equal == 0 ) then

    if ( n_max <= n ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4R4R4VEC_INDEX_INSERT_UNIQUE - Fatal error!'
      write ( *, '(a)' ) '  Not enough space to store new data.'
      return
    end if

    x(n+1) = xval
    y(n+1) = yval
    z(n+1) = zval
    ival = n + 1
    indx(n+1:more+1:-1) = indx(n:more:-1)
    indx(more) = n + 1
    n = n + 1

  else

    ival = indx(equal)

  end if

  return
end
subroutine r4r4r4vec_index_search ( n, x, y, z, indx, xval, yval, &
  zval, less, equal, more )

!*****************************************************************************80
!
!! R4R4R4VEC_INDEX_SEARCH searches for R4R4R4 value in an indexed sorted list.
!
!  Discussion:
!
!    An R4R4R4VEC is set of N R4R4R4 items.
!
!    An R4R4R4 is simply 3 R4 values, stored as scalars.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the size of the list.
!
!    Input, real (kind=4) :: X(N), Y(N), Z(N), the list.
!
!    Input, integer (kind=4) :: INDX(N), the sort index of the list.
!
!    Input, real (kind=4) :: XVAL, YVAL, ZVAL, the value to be sought.
!
!    Output, integer (kind=4) :: LESS, EQUAL, MORE, the indexes in INDX of the
!    entries of X that are just less than, equal to, and just greater
!    than XVAL.  If XVAL does not occur in X, then EQUAL is zero.
!    If XVAL is the minimum entry of X, then LESS is 0.  If XVAL
!    is the greatest entry of X, then MORE is N+1.
!
  implicit none

  integer (kind=4) :: n

  integer (kind=4) :: compare
  integer (kind=4) :: r4r4r4_compare
  integer (kind=4) :: equal
  integer (kind=4) :: hi
  integer (kind=4) :: indx(n)
  integer (kind=4) :: less
  integer (kind=4) :: lo
  integer (kind=4) :: mid
  integer (kind=4) :: more
  real (kind=4) :: x(n)
  real (kind=4) :: xhi
  real (kind=4) :: xlo
  real (kind=4) :: xmid
  real (kind=4) :: xval
  real (kind=4) :: y(n)
  real (kind=4) :: yhi
  real (kind=4) :: ylo
  real (kind=4) :: ymid
  real (kind=4) :: yval
  real (kind=4) :: z(n)
  real (kind=4) :: zhi
  real (kind=4) :: zlo
  real (kind=4) :: zmid
  real (kind=4) :: zval

  if ( n <= 0 ) then
    less = 0
    equal = 0
    more = 0
    return
  end if

  lo = 1
  hi = n

  xlo = x(indx(lo))
  ylo = y(indx(lo))
  zlo = z(indx(lo))

  xhi = x(indx(hi))
  yhi = y(indx(hi))
  zhi = z(indx(hi))

  compare = r4r4r4_compare ( xval, yval, zval, xlo, ylo, zlo )

  if ( compare == -1 ) then
    less = 0
    equal = 0
    more = 1
    return
  else if ( compare == 0 ) then
    less = 0
    equal = 1
    more = 2
    return
  end if

  compare = r4r4r4_compare ( xval, yval, zval, xhi, yhi, zhi )

  if ( compare == 1 ) then
    less = n
    equal = 0
    more = n + 1
    return
  else if ( compare == 0 ) then
    less = n - 1
    equal = n
    more = n + 1
    return
  end if

  do

    if ( lo + 1 == hi ) then
      less = lo
      equal = 0
      more = hi
      return
    end if

    mid = ( lo + hi ) / 2
    xmid = x(indx(mid))
    ymid = y(indx(mid))
    zmid = z(indx(mid))

    compare = r4r4r4_compare ( xval, yval, zval, xmid, ymid, zmid )

    if ( compare == 0 ) then
      equal = mid
      less = mid - 1
      more = mid + 1
      return
    else if ( compare == -1 ) then
      hi = mid
    else if ( compare == +1 ) then
      lo = mid
    end if

  end do

  return
end
subroutine r4r4vec_index_insert_unique ( n_max, n, x, y, indx, xval, yval, &
  ival, ierror )

!*****************************************************************************80
!
!! R4R4VEC_INDEX_INSERT_UNIQUE inserts a unique R4R4 in an indexed sorted list.
!
!  Discussion:
!
!    An R4R4VEC is set of N R4R4 items.
!
!    An R4R4 is simply 2 R4 values, stored as scalars.
!
!    If the input value does not occur in the current list, it is added,
!    and N, X, Y and INDX are updated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N_MAX, the maximum size of the list.
!
!    Input/output, integer (kind=4) :: N, the size of the list.
!
!    Input/output, real (kind=4) :: X(N), Y(N), the list of R4R4 vectors.
!
!    Input/output, integer (kind=4) :: INDX(N), the sort index of the list.
!
!    Input, real (kind=4) :: XVAL, YVAL, the value to be inserted if it is
!    not already in the list.
!
!    Output, integer (kind=4) :: IVAL, the index in X, Y corresponding to the
!    value XVAL, YVAL.
!
!    Output, integer (kind=4) :: IERROR, 0 for no error, 1 if an
!    error occurred.
!
  implicit none

  integer (kind=4) :: n_max

  integer (kind=4) :: equal
  integer (kind=4) :: ierror
  integer (kind=4) :: indx(n_max)
  integer (kind=4) :: ival
  integer (kind=4) :: less
  integer (kind=4) :: more
  integer (kind=4) :: n
  real (kind=4) :: x(n_max)
  real (kind=4) :: xval
  real (kind=4) :: y(n_max)
  real (kind=4) :: yval

  ierror = 0

  if ( n <= 0 ) then

    if ( n_max <= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4R4VEC_INDEX_INSERT_UNIQUE - Fatal error!'
      write ( *, '(a)' ) '  Not enough space to store new data.'
      return
    end if

    n = 1
    x(1) = xval
    y(1) = yval
    indx(1) = 1
    ival = 1
    return

  end if
!
!  Does ( XVAL, YVAL ) already occur in ( X, Y )?
!
  call r4r4vec_index_search ( n, x, y, indx, xval, yval, less, equal, more )

  if ( equal == 0 ) then

    if ( n_max <= n ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4R4VEC_INDEX_INSERT_UNIQUE - Fatal error!'
      write ( *, '(a)' ) '  Not enough space to store new data.'
      return
    end if

    x(n+1) = xval
    y(n+1) = yval
    ival = n + 1
    indx(n+1:more+1:-1) = indx(n:more:-1)
    indx(more) = n + 1
    n = n + 1

  else

    ival = indx(equal)

  end if

  return
end
subroutine r4r4vec_index_search ( n, x, y, indx, xval, yval, less, equal, &
  more )

!*****************************************************************************80
!
!! R4R4VEC_INDEX_SEARCH searches for an R4R4 in an indexed sorted list.
!
!  Discussion:
!
!    An R4R4VEC is set of N R4R4 items.
!
!    An R4R4 is simply 2 R4 values, stored as scalars.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the size of the current list.
!
!    Input, real (kind=4) :: X(N), Y(N), the list.
!
!    Input, integer (kind=4) :: INDX(N), the sort index of the list.
!
!    Input, real (kind=4) :: XVAL, YVAL, the value to be sought.
!
!    Output, integer (kind=4) :: LESS, EQUAL, MORE, the indexes in INDX of the
!    entries of X that are just less than, equal to, and just greater
!    than XVAL.  If XVAL does not occur in X, then EQUAL is zero.
!    If XVAL is the minimum entry of X, then LESS is 0.  If XVAL
!    is the greatest entry of X, then MORE is N+1.
!
  implicit none

  integer (kind=4) :: n

  integer (kind=4) :: compare
  integer (kind=4) :: r4r4_compare
  integer (kind=4) :: equal
  integer (kind=4) :: hi
  integer (kind=4) :: indx(n)
  integer (kind=4) :: less
  integer (kind=4) :: lo
  integer (kind=4) :: mid
  integer (kind=4) :: more
  real (kind=4) :: x(n)
  real (kind=4) :: xhi
  real (kind=4) :: xlo
  real (kind=4) :: xmid
  real (kind=4) :: xval
  real (kind=4) :: y(n)
  real (kind=4) :: yhi
  real (kind=4) :: ylo
  real (kind=4) :: ymid
  real (kind=4) :: yval

  if ( n <= 0 ) then
    less = 0
    equal = 0
    more = 0
    return
  end if

  lo = 1
  hi = n

  xlo = x(indx(lo))
  ylo = y(indx(lo))

  xhi = x(indx(hi))
  yhi = y(indx(hi))

  compare = r4r4_compare ( xval, yval, xlo, ylo )

  if ( compare == -1 ) then
    less = 0
    equal = 0
    more = 1
    return
  else if ( compare == 0 ) then
    less = 0
    equal = 1
    more = 2
    return
  end if

  compare = r4r4_compare ( xval, yval, xhi, yhi )

  if ( compare == 1 ) then
    less = n
    equal = 0
    more = n + 1
    return
  else if ( compare == 0 ) then
    less = n - 1
    equal = n
    more = n + 1
    return
  end if

  do

    if ( lo + 1 == hi ) then
      less = lo
      equal = 0
      more = hi
      return
    end if

    mid = ( lo + hi ) / 2
    xmid = x(indx(mid))
    ymid = y(indx(mid))

    compare = r4r4_compare ( xval, yval, xmid, ymid )

    if ( compare == 0 ) then
      equal = mid
      less = mid - 1
      more = mid + 1
      return
    else if ( compare == -1 ) then
      hi = mid
    else if ( compare == +1 ) then
      lo = mid
    end if

  end do

  return
end
subroutine r4int_to_r4int ( rmin, rmax, r, r2min, r2max, r2 )

!*****************************************************************************80
!
!! R4INT_TO_R4INT maps one R4INT to another.
!
!  Discussion:
!
!    The formula used is
!
!      R2 := R2MIN + ( R2MAX - R2MIN ) * ( R - RMIN ) / ( RMAX - RMIN )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: RMIN, RMAX, the first range.
!
!    Input, real (kind=4) :: R, the number to be converted.
!
!    Input, real (kind=4) :: R2MAX, R2MIN, the second range.
!
!    Output, real (kind=4) :: R2, the corresponding value in
!    the range [R2MIN,R2MAX].
!
  implicit none

  real (kind=4) :: r
  real (kind=4) :: rmax
  real (kind=4) :: rmin
  real (kind=4) :: r2
  real (kind=4) :: r2max
  real (kind=4) :: r2min

  if ( rmax == rmin ) then

    r2 = ( r2max + r2min ) / 2.0E+00

  else

    r2 = ( ( ( rmax - r        ) * r2min   &
           + (        r - rmin ) * r2max ) &
           / ( rmax     - rmin ) )

  end if

  return
end
subroutine r4int_to_i4int ( rmin, rmax, r, imin, imax, i )

!*****************************************************************************80
!
!! R4INT_TO_I4INT maps an R4INT to an integer interval.
!
!  Discussion:
!
!    The formula used is
!
!      I := IMIN + ( IMAX - IMIN ) * ( R - RMIN ) / ( RMAX - RMIN )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: RMIN, RMAX, the range.
!
!    Input, real (kind=4) :: R, the number to be converted.
!
!    Input, integer (kind=4) :: IMAX, IMIN, the integer range.
!
!    Output, integer (kind=4) :: I, the corresponding value in the
!    range [IMIN,IMAX].
!
  implicit none

  integer (kind=4) :: i
  integer (kind=4) :: imax
  integer (kind=4) :: imin
  real (kind=4) :: r
  real (kind=4) :: rmax
  real (kind=4) :: rmin

  if ( rmax == rmin ) then

    i = ( imax + imin ) / 2

  else

    i = nint ( &
      ( ( rmax - r        ) * real ( imin, kind = 4 )   &
      + (        r - rmin ) * real ( imax, kind = 4 ) ) &
      / ( rmax     - rmin ) )

  end if

  return
end
subroutine r4mat_border_cut ( m, n, table, table2 )

!*****************************************************************************80
!
!! R4MAT_BORDER_CUT cuts the "border" of an R4MAT.
!
!  Discussion:
!
!    We suppose the input data gives values of a quantity on nodes
!    on a 2D grid, and we wish to create a new table corresponding only
!    to those nodes in the interior of the 2D grid.
!
!      0 0 0 0 0 0
!      0 * * * * 0    * * * *
!      0 * * * * 0 -> * * * *
!      0 * * * * 0    * * * *
!      0 0 0 0 0 0
!
!    The illustration suggests the situation in which a 5 by 6 array
!    is input, and a 3 by 4 array is to be output.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the spatial dimension.
!
!    Input, integer (kind=4) :: N, the number of points.
!
!    Input, real (kind=4) :: TABLE(M,N), the table data.
!
!    Output, real (kind=4) :: TABLE2(M-2,N-2), the new table data.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: table(m,n)
  real (kind=4) :: table2(m-2,n-2)

  if ( m <= 2 .or. n <= 2 ) then
    return
  end if

  table2(1:m-2,1:n-2) = table(2:m-1,2:n-1)

  return
end
subroutine r4mat_cholesky_factor ( n, a, c )

!*****************************************************************************80
!
!! R4MAT_CHOLESKY_FACTOR computes the Cholesky factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is a lower triangular matrix L such that:
!
!      A = L * L'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns of
!    the matrix A.
!
!    Input, real (kind=4) :: A(N,N), the N by N matrix.
!
!    Output, real (kind=4) :: C(N,N), the N by N lower triangular
!    Cholesky factor.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: c(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: sum2

  c(1:n,1:n) = a(1:n,1:n)

  do j = 1, n

    c(1:j-1,j) = 0.0E+00

    do i = j, n

      sum2 = c(j,i) - dot_product ( c(j,1:j-1), c(i,1:j-1) )

      if ( i == j ) then
        if ( sum2 <= 0.0E+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R4MAT_CHOLESKY_FACTOR - Fatal error!'
          write ( *, '(a)' ) '  Matrix is not positive definite.'
          stop
        else
          c(i,j) = sqrt ( sum2 )
        end if
      else
        if ( c(j,j) /= 0.0E+00 ) then
          c(i,j) = sum2 / c(j,j)
        else
          c(i,j) = 0.0E+00
        end if
      end if

    end do

  end do

  return
end
subroutine r4mat_cholesky_solve ( n, a, b, x )

!*****************************************************************************80
!
!! R4MAT_CHOLESKY_SOLVE solves a Cholesky factored linear system A * x = b.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns of
!    the matrix A.
!
!    Input, real (kind=4) :: A(N,N), the N by N Cholesky factor of the
!    system matrix.
!
!    Input, real (kind=4) :: B(N), the right hand side of the linear system.
!
!    Output, real (kind=4) :: X(N), the solution of the linear system.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: b(n)
  real (kind=4) :: x(n)
!
!  Solve L * y = b.
!
  call r4mat_l_solve ( n, a, b, x )
!
!  Solve L' * x = y.
!
  call r4mat_lt_solve ( n, a, x, x )

  return
end
subroutine r4mat_choresky_factor ( n, a, c )

!*****************************************************************************80
!
!! R4MAT_CHORESKY_FACTOR computes the "Choresky" factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is an upper triangular matrix R such that:
!
!      A = R * R'
!
!    Note that the usual Cholesky factor is a LOWER triangular matrix L
!    such that
!
!      A = L * L'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns of
!    the matrix A.
!
!    Input, real (kind=4) :: A(N,N), the N by N matrix.
!
!    Output, real (kind=4) :: C(N,N), the N by N upper triangular
!    "Choresky" factor.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: c(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: sum2

  c(n:1:-1,n:1:-1) = a(1:n,1:n)

  do j = 1, n

    c(1:j-1,j) = 0.0E+00

    do i = j, n

      sum2 = c(j,i) - dot_product ( c(j,1:j-1), c(i,1:j-1) )

      if ( i == j ) then
        if ( sum2 <= 0.0E+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R4MAT_CHORESKY_FACTOR - Fatal error!'
          write ( *, '(a)' ) '  Matrix is not positive definite.'
          stop
        else
          c(i,j) = sqrt ( sum2 )
        end if
      else
        if ( c(j,j) /= 0.0E+00 ) then
          c(i,j) = sum2 / c(j,j)
        else
          c(i,j) = 0.0E+00
        end if
      end if

    end do

  end do

  c(n:1:-1,n:1:-1) = c(1:n,1:n)

  return
end
subroutine r4mat_copy ( m, n, a, b )

!*****************************************************************************80
!
!! R4MAT_COPY copies an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the order of the matrix.
!
!    Input, real (kind=4) :: A(M,N), the matrix to be copied.
!
!    Output, real (kind=4) :: B(M,N), a copy of the matrix.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  real (kind=4) :: b(m,n)

  b(1:m,1:n) = a(1:m,1:n)

  return
end
subroutine r4mat_det ( n, a, det )

!*****************************************************************************80
!
!! R4MAT_DET computes the determinant of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    Original FORTRAN77 version by Helmut Spaeth.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Helmut Spaeth,
!    Cluster Analysis Algorithms
!    for Data Reduction and Classification of Objects,
!    Ellis Horwood, 1980, page 125-127.
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix.
!
!    Input, real (kind=4) :: A(N,N), the matrix whose determinant is desired.
!
!    Output, real (kind=4) :: DET, the determinant of the matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: b(n,n)
  real (kind=4) :: det
  integer (kind=4) :: j
  integer (kind=4) :: k
  integer (kind=4) :: m
  integer (kind=4) :: piv(1)
  real (kind=4) :: t

  b(1:n,1:n) = a(1:n,1:n)

  det = 1.0E+00

  do k = 1, n

    piv = maxloc ( abs ( b(k:n,k) ) )

    m = piv(1) + k - 1

    if ( m /= k ) then
      det = - det
      t      = b(m,k)
      b(m,k) = b(k,k)
      b(k,k) = t
    end if

    det = det * b(k,k)

    if ( b(k,k) /= 0.0E+00 ) then

      b(k+1:n,k) = -b(k+1:n,k) / b(k,k)

      do j = k + 1, n
        if ( m /= k ) then
          t      = b(m,j)
          b(m,j) = b(k,j)
          b(k,j) = t
        end if
        b(k+1:n,j) = b(k+1:n,j) + b(k+1:n,k) * b(k,j)
      end do

    end if

  end do

  return
end
function r4mat_det_2d ( a )

!*****************************************************************************80
!
!! R4MAT_DET_2D computes the determinant of a 2 by 2 R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The formula for the determinant of a 2 by 2 matrix is
!
!      a11 * a22 - a12 * a21.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A(2,2), the matrix whose determinant is desired.
!
!    Output, real (kind=4) :: R4MAT_DET_2D, the determinant of the matrix.
!
  implicit none

  real (kind=4) :: a(2,2)
  real (kind=4) :: r4mat_det_2d

  r4mat_det_2d = a(1,1) * a(2,2) - a(1,2) * a(2,1)

  return
end
function r4mat_det_3d ( a )

!*****************************************************************************80
!
!! R4MAT_DET_3D computes the determinant of a 3 by 3 R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The formula for the determinant of a 3 by 3 matrix is
!
!        a11 * a22 * a33 - a11 * a23 * a32
!      + a12 * a23 * a31 - a12 * a21 * a33
!      + a13 * a21 * a32 - a13 * a22 * a31
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A(3,3), the matrix whose determinant is desired.
!
!    Output, real (kind=4) :: R4MAT_DET_3D, the determinant of the matrix.
!
  implicit none

  real (kind=4) :: a(3,3)
  real (kind=4) :: r4mat_det_3d

  r4mat_det_3d = &
         a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
       + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) ) &
       + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) )

  return
end
function r4mat_det_4d ( a )

!*****************************************************************************80
!
!! R4MAT_DET_4D computes the determinant of a 4 by 4 R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A(4,4), the matrix whose determinant is desired.
!
!    Output, real (kind=4) :: R4MAT_DET_4D, the determinant of the matrix.
!
  implicit none

  real (kind=4) :: a(4,4)
  real (kind=4) :: r4mat_det_4d

  r4mat_det_4d = &
         a(1,1) * ( &
             a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
           - a(2,3) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
           + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) ) &
       - a(1,2) * ( &
             a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
           - a(2,3) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) ) &
           + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) ) &
       + a(1,3) * ( &
             a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
           - a(2,2) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) ) &
           + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) ) &
       - a(1,4) * ( &
             a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
           - a(2,2) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) &
           + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )

  return
end
function r4mat_det_5d ( a )

!*****************************************************************************80
!
!! R4MAT_DET_5D computes the determinant of a 5 by 5 R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A(5,5), the matrix whose determinant is desired.
!
!    Output, real (kind=4) :: R4MAT_DET_5D, the determinant of the matrix.
!
  implicit none

  real (kind=4) :: a(5,5)
  real (kind=4) :: b(4,4)
  integer (kind=4) :: i
  integer (kind=4) :: inc
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: r4mat_det_4d
  real (kind=4) :: r4mat_det_5d
!
!  Expand the determinant into the sum of the determinants of the
!  five 4 by 4 matrices created by dropping row 1, and column k.
!
  r4mat_det_5d = 0.0E+00

  do k = 1, 5

    do i = 1, 4
      do j = 1, 4

        if ( j < k ) then
          inc = 0
        else
          inc = 1
        end if

        b(i,j) = a(i+1,j+inc)

      end do
    end do

    r4mat_det_5d = r4mat_det_5d + (-1)**( k + 1 ) * a(1,k) * r4mat_det_4d ( b )

  end do

  return
end
subroutine r4mat_diag_add_scalar ( n, a, s )

!*****************************************************************************80
!
!! R4MAT_DIAG_ADD_SCALAR adds a scalar to the diagonal of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns.
!
!    Input/output, real (kind=4) :: A(N,N), the N by N matrix to be modified.
!
!    Input, real (kind=4) :: S, the value to be added to the diagonal
!    of the matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  real (kind=4) :: s

  do i = 1, n
    a(i,i) = a(i,i) + s
  end do

  return
end
subroutine r4mat_diag_add_vector ( n, a, v )

!*****************************************************************************80
!
!! R4MAT_DIAG_ADD_VECTOR adds a vector to the diagonal of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns of
!    the matrix.
!
!    Input/output, real (kind=4) :: A(N,N), the N by N matrix.
!
!    Input, real (kind=4) :: V(N), the vector to be added to the diagonal of A.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  real (kind=4) :: v(n)

  do i = 1, n
    a(i,i) = a(i,i) + v(i)
  end do

  return
end
subroutine r4mat_diag_get_vector ( n, a, v )

!*****************************************************************************80
!
!! R4MAT_DIAG_GET_VECTOR gets the value of the diagonal of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns of
!    the matrix.
!
!    Input, real (kind=4) :: A(N,N), the N by N matrix.
!
!    Output, real (kind=4) :: V(N), the diagonal entries
!    of the matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  real (kind=4) :: v(n)

  do i = 1, n
    v(i) = a(i,i)
  end do

  return
end
subroutine r4mat_diag_set_scalar ( n, a, s )

!*****************************************************************************80
!
!! R4MAT_DIAG_SET_SCALAR sets the diagonal of an R4MAT to a scalar value.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns.
!
!    Input/output, real (kind=4) :: A(N,N), the N by N matrix to be modified.
!
!    Input, real (kind=4) :: S, the value to be assigned to the diagonal
!    of the matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  real (kind=4) :: s

  do i = 1, n
    a(i,i) = s
  end do

  return
end
subroutine r4mat_diag_set_vector ( n, a, v )

!*****************************************************************************80
!
!! R4MAT_DIAG_SET_VECTOR sets the diagonal of an R4MAT to a vector.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns.
!
!    Input/output, real (kind=4) :: A(N,N), the N by N matrix.
!
!    Input, real (kind=4) :: V(N), the vector to be assigned to the
!    diagonal of A.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  real (kind=4) :: v(n)

  do i = 1, n
    a(i,i) = v(i)
  end do

  return
end
subroutine r4mat_expand_linear ( m, n, x, mfat, nfat, xfat )

!*****************************************************************************80
!
!! R4MAT_EXPAND_LINEAR linearly interpolates new data into an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    In this routine, the expansion is specified by giving the number
!    of intermediate values to generate between each pair of original
!    data rows and columns.
!
!    The interpolation is not actually linear.  It uses the functions
!
!      1, x, y, and xy.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns of
!    input data.
!
!    Input, real (kind=4) :: X(M,N), the original data.
!
!    Input, integer (kind=4) :: MFAT, NFAT, the number of data values
!    to interpolate between each row, and each column, of original data values.
!
!    Output, real (kind=4) :: XFAT(M2,N2), the fattened data, where
!    M2 = (M-1)*(MFAT+1)+1,
!    N2 = (N-1)*(NFAT+1)+1.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: mfat
  integer (kind=4) :: n
  integer (kind=4) :: nfat

  integer (kind=4) :: i
  integer (kind=4) :: ihi
  integer (kind=4) :: ii
  integer (kind=4) :: iii
  integer (kind=4) :: ip1
  integer (kind=4) :: j
  integer (kind=4) :: jhi
  integer (kind=4) :: jj
  integer (kind=4) :: jjj
  integer (kind=4) :: jp1
  real (kind=4) :: s
  real (kind=4) :: t
  real (kind=4) :: x(m,n)
  real (kind=4) :: x00
  real (kind=4) :: x01
  real (kind=4) :: x10
  real (kind=4) :: x11
  real (kind=4) :: xfat((m-1)*(mfat+1)+1,(n-1)*(nfat+1)+1)

  do i = 1, m

    if ( i < m ) then
      ihi = mfat
    else
      ihi = 0
    end if

    do j = 1, n

      if ( j < n ) then
        jhi = nfat
      else
        jhi = 0
      end if

      if ( i < m ) then
        ip1 = i + 1
      else
        ip1 = i
      end if

      if ( j < n ) then
        jp1 = j + 1
      else
        jp1 = j
      end if

      x00 = x(i,j)
      x10 = x(ip1,j)
      x01 = x(i,jp1)
      x11 = x(ip1,jp1)

      do ii = 0, ihi

        s = real ( ii, kind = 4 ) &
          / real ( ihi + 1, kind = 4 )

        do jj = 0, jhi

          t = real ( jj, kind = 4 ) &
            / real ( jhi + 1, kind = 4 )

          iii = 1 + ( i - 1 ) * ( mfat + 1 ) + ii
          jjj = 1 + ( j - 1 ) * ( nfat + 1 ) + jj

          xfat(iii,jjj) = &
                                            x00   &
              + s     * (       x10       - x00 ) &
              + t     * (             x01 - x00 ) &
              + s * t * ( x11 - x10 - x01 + x00 )

        end do

      end do

    end do

  end do

  return
end
subroutine r4mat_expand_linear2 ( m, n, a, m2, n2, a2 )

!*****************************************************************************80
!
!! R4MAT_EXPAND_LINEAR2 expands an R4MAT by linear interpolation.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    In this version of the routine, the expansion is indicated
!    by specifying the dimensions of the expanded array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns in A.
!
!    Input, real (kind=4) :: A(M,N), a "small" M by N array.
!
!    Input, integer (kind=4) :: M2, N2, the number of rows and columns in A2.
!
!    Output, real (kind=4) :: A2(M2,N2), the expanded array, which
!    contains an interpolated version of the data in A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: m2
  integer (kind=4) :: n
  integer (kind=4) :: n2

  real (kind=4) :: a(m,n)
  real (kind=4) :: a2(m2,n2)
  integer (kind=4) :: i
  integer (kind=4) :: i1
  integer (kind=4) :: i2
  integer (kind=4) :: j
  integer (kind=4) :: j1
  integer (kind=4) :: j2
  real (kind=4) :: r
  real (kind=4) :: r1
  real (kind=4) :: r2
  real (kind=4) :: s
  real (kind=4) :: s1
  real (kind=4) :: s2

  do i = 1, m2

    if ( m2 == 1 ) then
      r = 0.5E+00
    else
      r = real ( i - 1, kind = 4 ) &
        / real ( m2 - 1, kind = 4 )
    end if

    i1 = 1 + int ( r * real ( m - 1, kind = 4 ) )
    i2 = i1 + 1

    if ( m < i2 ) then
      i1 = m - 1
      i2 = m
    end if

    r1 = real ( i1 - 1, kind = 4 ) &
       / real ( m - 1, kind = 4 )

    r2 = real ( i2 - 1, kind = 4 ) &
       / real ( m - 1, kind = 4 )

    do j = 1, n2

      if ( n2 == 1 ) then
        s = 0.5E+00
      else
        s = real ( j - 1, kind = 4 ) &
          / real ( n2 - 1, kind = 4 )
      end if

      j1 = 1 + int ( s * real ( n - 1, kind = 4 ) )
      j2 = j1 + 1

      if ( n < j2 ) then
        j1 = n - 1
        j2 = n
      end if

      s1 = real ( j1 - 1, kind = 4 ) &
         / real ( n - 1, kind = 4 )

      s2 = real ( j2 - 1, kind = 4 ) &
         / real ( n - 1, kind = 4 )

      a2(i,j) = &
        ( ( r2 - r ) * ( s2 - s ) * a(i1,j1) &
        + ( r - r1 ) * ( s2 - s ) * a(i2,j1) &
        + ( r2 - r ) * ( s - s1 ) * a(i1,j2) &
        + ( r - r1 ) * ( s - s1 ) * a(i2,j2) ) &
        / ( ( r2 - r1 ) * ( s2 - s1 ) )

    end do

  end do

  return
end
subroutine r4mat_givens_post ( n, a, row, col, g )

!*****************************************************************************80
!
!! R4MAT_GIVENS_POST computes the Givens postmultiplier rotation matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The Givens post-multiplier matrix G(ROW,COL) has the property that
!    the (ROW,COL)-th entry of A*G is zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrices A and G.
!
!    Input, real (kind=4) :: A(N,N), the matrix to be operated upon.
!
!    Input, integer (kind=4) :: ROW, COL, the row and column of the
!    entry of A*G which is to be zeroed out.
!
!    Output, real (kind=4) :: G(N,N), the Givens rotation matrix.
!    G is an orthogonal matrix, that is, the inverse of
!    G is the transpose of G.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: col
  real (kind=4) :: g(n,n)
  integer (kind=4) :: row
  real (kind=4) :: theta

  call r4mat_identity ( n, g )

  theta = atan2 ( a(row,col), a(row,row) )

  g(row,row) =  cos ( theta )
  g(row,col) = -sin ( theta )
  g(col,row) =  sin ( theta )
  g(col,col) =  cos ( theta )

  return
end
subroutine r4mat_givens_pre ( n, a, row, col, g )

!*****************************************************************************80
!
!! R4MAT_GIVENS_PRE computes the Givens premultiplier rotation matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The Givens premultiplier rotation matrix G(ROW,COL) has the
!    property that the (ROW,COL)-th entry of G*A is zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrices A and G.
!
!    Input, real (kind=4) :: A(N,N), the matrix to be operated upon.
!
!    Input, integer (kind=4) :: ROW, COL, the row and column of the
!    entry of the G*A which is to be zeroed out.
!
!    Output, real (kind=4) :: G(N,N), the Givens rotation matrix.
!    G is an orthogonal matrix, that is, the inverse of
!    G is the transpose of G.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: col
  real (kind=4) :: g(n,n)
  integer (kind=4) :: row
  real (kind=4) :: theta

  call r4mat_identity ( n, g )

  theta = atan2 ( a(row,col), a(col,col) )

  g(row,row) =  cos ( theta )
  g(row,col) = -sin ( theta )
  g(col,row) =  sin ( theta )
  g(col,col) =  cos ( theta )

  return
end
subroutine r4mat_hess ( fx, n, x, h )

!*****************************************************************************80
!
!! R4MAT_HESS approximates a Hessian matrix via finite differences.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    H(I,J) = d2 F / d X(I) d X(J)
!
!    The values returned by this routine will be only approximate.
!    In some cases, they will be so poor that they are useless.
!    However, one of the best applications of this routine is for
!    checking your own Hessian calculations, since as Heraclitus
!    said, you'll never get the same result twice when you differentiate
!    a complicated expression by hand.
!
!    The user function routine, here called "FX", should have the form:
!
!      subroutine fx ( n, x, f )
!      integer n
!      real (kind=4) :: f
!      real (kind=4) :: x(n)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FX, the name of the user function routine.
!
!    Input, integer (kind=4) :: N, the number of variables.
!
!    Input, real (kind=4) :: X(N), the values of the variables.
!
!    Output, real (kind=4) :: H(N,N), the approximated N by N Hessian matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: eps
  real (kind=4) :: f00
  real (kind=4) :: fmm
  real (kind=4) :: fmp
  real (kind=4) :: fpm
  real (kind=4) :: fpp
  external             fx
  real (kind=4) :: h(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: s(n)
  real (kind=4) :: x(n)
  real (kind=4) :: xi
  real (kind=4) :: xj
!
!  Choose the stepsizes.
!
  eps = ( epsilon ( eps ) )**0.33E+00

  do i = 1, n
    s(i) = eps * max ( abs ( x(i) ), 1.0E+00 )
  end do
!
!  Calculate the diagonal elements.
!
  do i = 1, n

    xi = x(i)

    call fx ( n, x, f00 )

    x(i) = xi + s(i)
    call fx ( n, x, fpp )

    x(i) = xi - s(i)
    call fx ( n, x, fmm )

    h(i,i) = ( ( fpp - f00 ) + ( fmm - f00 ) ) / s(i)**2

    x(i) = xi

  end do
!
!  Calculate the off diagonal elements.
!
  do i = 1, n

    xi = x(i)

    do j = i + 1, n

      xj = x(j)

      x(i) = xi + s(i)
      x(j) = xj + s(j)
      call fx ( n, x, fpp )

      x(i) = xi + s(i)
      x(j) = xj - s(j)
      call fx ( n, x, fpm )

      x(i) = xi - s(i)
      x(j) = xj + s(j)
      call fx ( n, x, fmp )

      x(i) = xi - s(i)
      x(j) = xj - s(j)
      call fx ( n, x, fmm )

      h(j,i) = ( ( fpp - fpm ) + ( fmm - fmp ) ) / ( 4.0E+00 * s(i) * s(j) )

      h(i,j) = h(j,i)

      x(j) = xj

    end do

    x(i) = xi

  end do

  return
end
subroutine r4mat_house_axh ( n, a, v, ah )

!*****************************************************************************80
!
!! R4MAT_HOUSE_AXH computes A*H where H is a compact Householder matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The Householder matrix H(V) is defined by
!
!      H(V) = I - 2 * v * v' / ( v' * v )
!
!    This routine is not particularly efficient.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of A.
!
!    Input, real (kind=4) :: A(N,N), the matrix to be postmultiplied.
!
!    Input, real (kind=4) :: V(N), a vector defining a Householder matrix.
!
!    Output, real (kind=4) :: AH(N,N), the product A*H.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: ah(n,n)
  real (kind=4) :: ah_temp(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: v(n)
  real (kind=4) :: v_normsq

  v_normsq = sum ( v(1:n)**2 )
!
!  Compute A*H' = A*H
!
  do i = 1, n
    do j = 1, n
      ah_temp(i,j) = a(i,j)
      do k = 1, n
        ah_temp(i,j) = ah_temp(i,j) - 2.0E+00 * a(i,k) * v(k) * v(j) / v_normsq
      end do
    end do
  end do
!
!  Copy the temporary result into AH.
!  Doing it this way means the user can identify the input arguments A and AH.
!
  ah(1:n,1:n) = ah_temp(1:n,1:n)

  return
end
subroutine r4mat_house_form ( n, v, h )

!*****************************************************************************80
!
!! R4MAT_HOUSE_FORM constructs a Householder matrix from its compact form.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    H(v) = I - 2 * v * v' / ( v' * v )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix.
!
!    Input, real (kind=4) :: V(N), the vector defining the Householder matrix.
!
!    Output, real (kind=4) :: H(N,N), the Householder matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: beta
  real (kind=4) :: h(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: v(n)
!
!  Compute the L2 norm of V.
!
  beta = sum ( v(1:n)**2 )
!
!  Form the matrix H.
!
  call r4mat_identity ( n, h )

  do i = 1, n
    do j = 1, n
      h(i,j) = h(i,j) - 2.0E+00 * v(i) * v(j) / beta
    end do
  end do

  return
end
subroutine r4mat_house_hxa ( n, a, v, ha )

!*****************************************************************************80
!
!! R4MAT_HOUSE_HXA computes H*A where H is a compact Householder matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The Householder matrix H(V) is defined by
!
!      H(V) = I - 2 * v * v' / ( v' * v )
!
!    This routine is not particularly efficient.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of A.
!
!    Input, real (kind=4) :: A(N,N), the matrix to be premultiplied.
!
!    Input, real (kind=4) :: V(N), a vector defining a Householder matrix.
!
!    Output, real (kind=4) :: HA(N,N), the product H*A.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: ha(n,n)
  real (kind=4) :: ha_temp(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: v(n)
  real (kind=4) :: v_normsq

  v_normsq = sum ( v(1:n)**2 )
!
!  Compute A*H' = A*H
!
  do i = 1, n
    do j = 1, n
      ha_temp(i,j) = a(i,j)
      do k = 1, n
        ha_temp(i,j) = ha_temp(i,j) - 2.0E+00 * v(i) * v(k) * a(k,j) / v_normsq
      end do
    end do
  end do
!
!  Copy the temporary result into HA.
!  Doing it this way means the user can identify the input arguments A and HA.
!
  ha(1:n,1:n) = ha_temp(1:n,1:n)

  return
end
subroutine r4mat_house_post ( n, a, row, col, h )

!*****************************************************************************80
!
!! R4MAT_HOUSE_POST computes a Householder post-multiplier matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    H(ROW,COL) has the property that the ROW-th column of
!    A*H(ROW,COL) is zero from entry COL+1 to the end.
!
!    In the most common case, where a QR factorization is being computed,
!    ROW = COL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrices.
!
!    Input, real (kind=4) :: A(N,N), the matrix whose Householder matrix
!    is to be computed.
!
!    Input, integer (kind=4) :: ROW, COL, specify the location of the
!    entry of the matrix A which is to be preserved.  The entries in
!    the same row, but higher column, will be zeroed out if
!    A is postmultiplied by H.
!
!    Output, real (kind=4) :: H(N,N), the Householder matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: col
  real (kind=4) :: h(n,n)
  integer (kind=4) :: row
  real (kind=4) :: v(n)
  real (kind=4) :: w(n)
!
!  Set up the vector V.
!
  w(1:col-1) = 0.0E+00
  w(col:n) = a(row,col:n)

  call r4vec_house_column ( n, w, col, v )
!
!  Form the matrix H(V).
!
  call r4mat_house_form ( n, v, h )

  return
end
subroutine r4mat_house_pre ( n, a, row, col, h )

!*****************************************************************************80
!
!! R4MAT_HOUSE_PRE computes a Householder pre-multiplier matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    H(ROW,COL) has the property that the COL-th column of
!    H(ROW,COL)*A is zero from entry ROW+1 to the end.
!
!    In the most common case, where a QR factorization is being computed,
!    ROW = COL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrices.
!
!    Input, real (kind=4) :: A(N,N), the matrix whose Householder matrix
!    is to be computed.
!
!    Input, integer (kind=4) :: ROW, COL, specify the location of the
!    entry of the matrix A which is to be preserved.  The entries in
!    the same column, but higher rows, will be zeroed out if A is
!    premultiplied by H.
!
!    Output, real (kind=4) :: H(N,N), the Householder matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: col
  real (kind=4) :: h(n,n)
  integer (kind=4) :: row
  real (kind=4) :: v(n)
  real (kind=4) :: w(n)
!
!  Set up the vector V.
!
  w(1:row-1) = 0.0E+00
  w(row:n) = a(row:n,col)

  call r4vec_house_column ( n, w, row, v )
!
!  Form the matrix H(V).
!
  call r4mat_house_form ( n, v, h )

  return
end
subroutine r4mat_identity ( n, a )

!*****************************************************************************80
!
!! R4MAT_IDENTITY stores the identity matrix in an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of A.
!
!    Output, real (kind=4) :: A(N,N), the N by N identity matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i

  a(1:n,1:n) = 0.0E+00

  do i = 1, n
    a(i,i) = 1.0E+00
  end do

  return
end
function r4mat_in_01 ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_IN_01 is TRUE if the entries of an R4MAT are in the range [0,1].
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix.
!
!    Output, logical R4MAT_IN_01, is TRUE if every entry of A is
!    between 0 and 1.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  logical              r4mat_in_01

  if ( any ( a(1:m,1:n) < 0.0E+00 .or. 1.0E+00 < a(1:m,1:n) ) ) then
    r4mat_in_01 = .false.
  else
    r4mat_in_01 = .true.
  end if

  return
end
subroutine r4mat_indicator ( m, n, table )

!*****************************************************************************80
!
!! R4MAT_INDICATOR sets up an "indicator" R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The value of each entry suggests its location, as in:
!
!      11  12  13  14
!      21  22  23  24
!      31  32  33  34
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows of the matrix.
!    M must be positive.
!
!    Input, integer (kind=4) :: N, the number of columns of the matrix.
!    N must be positive.
!
!    Output, real (kind=4) :: TABLE(M,N), the table.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  integer (kind=4) :: fac
  integer (kind=4) :: i
  integer (kind=4) :: i4_log_10
  integer (kind=4) :: j
  real (kind=4) :: table(m,n)

  fac = 10 ** ( i4_log_10 ( n ) + 1 )

  do i = 1, m
    do j = 1, n
      table(i,j) = real ( fac * i + j, kind = 4 )
    end do
  end do

  return
end
subroutine r4mat_inverse_2d ( a, b, det )

!*****************************************************************************80
!
!! R4MAT_INVERSE_2D inverts a 2 by 2 R4MAT using Cramer's rule.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    If the determinant is zero, then A is singular, and does not have an
!    inverse.  In that case, B is simply set to zero, and a
!    message is printed.
!
!    If the determinant is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A(2,2), the matrix to be inverted.
!
!    Output, real (kind=4) :: B(2,2), the inverse of the matrix A.
!
!    Output, real (kind=4) :: DET, the determinant of the matrix A.
!
  implicit none

  real (kind=4) :: a(2,2)
  real (kind=4) :: b(2,2)
  real (kind=4) :: det
  real (kind=4) :: r4mat_det_2d
!
!  Compute the determinant of A.
!
  det = r4mat_det_2d ( a )

  if ( det == 0.0E+00 ) then

    b(1:2,1:2) = 0.0E+00

  else

    b(1,1) =  a(2,2) / det
    b(1,2) = -a(1,2) / det
    b(2,1) = -a(2,1) / det
    b(2,2) =  a(1,1) / det

  end if

  return
end
subroutine r4mat_inverse_3d ( a, b, det )

!*****************************************************************************80
!
!! R4MAT_INVERSE_3D inverts a 3 by 3 R4MAT using Cramer's rule.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    If the determinant is zero, then A is singular, and does not have an
!    inverse.  In that case, B is simply set to zero, and a
!    message is printed.
!
!    If the determinant is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A(3,3), the matrix to be inverted.
!
!    Output, real (kind=4) :: B(3,3), the inverse of the matrix A.
!
!    Output, real (kind=4) :: DET, the determinant of the matrix A.
!
  implicit none

  real (kind=4) :: a(3,3)
  real (kind=4) :: b(3,3)
  real (kind=4) :: det
  real (kind=4) :: r4mat_det_3d
!
!  Compute the determinant of A.
!
  det = r4mat_det_3d ( a )
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0E+00 ) then
    b(1:3,1:3) = 0.0E+00
    return
  end if
!
!  Compute the entries of the inverse matrix using an explicit
!  formula.
!
  b(1,1) =  ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) / det
  b(1,2) = -( a(1,2) * a(3,3) - a(1,3) * a(3,2) ) / det
  b(1,3) =  ( a(1,2) * a(2,3) - a(1,3) * a(2,2) ) / det

  b(2,1) = -( a(2,1) * a(3,3) - a(2,3) * a(3,1) ) / det
  b(2,2) =  ( a(1,1) * a(3,3) - a(1,3) * a(3,1) ) / det
  b(2,3) = -( a(1,1) * a(2,3) - a(1,3) * a(2,1) ) / det

  b(3,1) =  ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) / det
  b(3,2) = -( a(1,1) * a(3,2) - a(1,2) * a(3,1) ) / det
  b(3,3) =  ( a(1,1) * a(2,2) - a(1,2) * a(2,1) ) / det

  return
end
subroutine r4mat_inverse_4d ( a, b, det )

!*****************************************************************************80
!
!! R4MAT_INVERSE_4D inverts a 4 by 4 R4MAT using Cramer's rule.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    If the determinant is zero, then A is singular, and does not have an
!    inverse.  In that case, B is simply set to zero, and a
!    message is printed.
!
!    If the determinant is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A(4,4), the matrix to be inverted.
!
!    Output, real (kind=4) :: B(4,4), the inverse of the matrix A.
!
!    Output, real (kind=4) :: DET, the determinant of the matrix A.
!
  implicit none

  real (kind=4) :: a(4,4)
  real (kind=4) :: b(4,4)
  real (kind=4) :: det
  real (kind=4) :: r4mat_det_4d
!
!  Compute the determinant of A.
!
  det = r4mat_det_4d ( a )
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0E+00 ) then

    b(1:4,1:4) = 0.0E+00

    return
  end if
!
!  Compute the entries of the inverse matrix using an explicit formula.
!
  b(1,1) = +( &
        + a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(2,3) * ( a(3,4) * a(4,2) - a(3,2) * a(4,4) ) &
        + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        ) / det

  b(2,1) = -( &
        + a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(2,3) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) &
        ) / det

  b(3,1) = +( &
        + a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
        + a(2,2) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(4,1) = -( &
        + a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        + a(2,2) * ( a(3,3) * a(4,1) - a(3,1) * a(4,3) ) &
        + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(1,2) = -( &
        + a(1,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(1,3) * ( a(3,4) * a(4,2) - a(3,2) * a(4,4) ) &
        + a(1,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        ) / det

  b(2,2) = +( &
        + a(1,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(1,3) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(1,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) &
        ) / det

  b(3,2) = -( &
        + a(1,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
        + a(1,2) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(1,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(4,2) = +( &
        + a(1,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        + a(1,2) * ( a(3,3) * a(4,1) - a(3,1) * a(4,3) ) &
        + a(1,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(1,3) = +( &
        + a(1,2) * ( a(2,3) * a(4,4) - a(2,4) * a(4,3) ) &
        + a(1,3) * ( a(2,4) * a(4,2) - a(2,2) * a(4,4) ) &
        + a(1,4) * ( a(2,2) * a(4,3) - a(2,3) * a(4,2) ) &
        ) / det

  b(2,3) = -( &
        + a(1,1) * ( a(2,3) * a(4,4) - a(2,4) * a(4,3) ) &
        + a(1,3) * ( a(2,4) * a(4,1) - a(2,1) * a(4,4) ) &
        + a(1,4) * ( a(2,1) * a(4,3) - a(2,3) * a(4,1) ) &
        ) / det

  b(3,3) = +( &
        + a(1,1) * ( a(2,2) * a(4,4) - a(2,4) * a(4,2) ) &
        + a(1,2) * ( a(2,4) * a(4,1) - a(2,1) * a(4,4) ) &
        + a(1,4) * ( a(2,1) * a(4,2) - a(2,2) * a(4,1) ) &
        ) / det

  b(4,3) = -( &
        + a(1,1) * ( a(2,2) * a(4,3) - a(2,3) * a(4,2) ) &
        + a(1,2) * ( a(2,3) * a(4,1) - a(2,1) * a(4,3) ) &
        + a(1,3) * ( a(2,1) * a(4,2) - a(2,2) * a(4,1) ) &
        ) / det

  b(1,4) = -( &
        + a(1,2) * ( a(2,3) * a(3,4) - a(2,4) * a(3,3) ) &
        + a(1,3) * ( a(2,4) * a(3,2) - a(2,2) * a(3,4) ) &
        + a(1,4) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
        ) / det

  b(2,4) = +( &
        + a(1,1) * ( a(2,3) * a(3,4) - a(2,4) * a(3,3) ) &
        + a(1,3) * ( a(2,4) * a(3,1) - a(2,1) * a(3,4) ) &
        + a(1,4) * ( a(2,1) * a(3,3) - a(2,3) * a(3,1) ) &
        ) / det

  b(3,4) = -( &
        + a(1,1) * ( a(2,2) * a(3,4) - a(2,4) * a(3,2) ) &
        + a(1,2) * ( a(2,4) * a(3,1) - a(2,1) * a(3,4) ) &
        + a(1,4) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) &
        ) / det

  b(4,4) = +( &
        + a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
        + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) ) &
        + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) &
        ) / det

  return
end
subroutine r4mat_jac ( m, n, eps, fx, x, fprime )

!*****************************************************************************80
!
!! R4MAT_JAC estimates a dense jacobian matrix of the function FX.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    FPRIME(I,J) = d F(I) / d X(J).
!
!    The jacobian is assumed to be dense, and the LINPACK/LAPACK
!    double precision general matrix storage mode ("DGE") is used.
!
!    Forward differences are used, requiring N+1 function evaluations.
!
!    Values of EPS have typically been chosen between
!    sqrt ( EPSMCH ) and sqrt ( sqrt ( EPSMCH ) ) where EPSMCH is the
!    machine tolerance.
!
!    If EPS is too small, then F(X+EPS) will be the same as
!    F(X), and the jacobian will be full of zero entries.
!
!    If EPS is too large, the finite difference estimate will
!    be inaccurate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of functions.
!
!    Input, integer (kind=4) :: N, the number of variables.
!
!    Input, real (kind=4) :: EPS, a tolerance to be used for shifting the
!    X values during the finite differencing.  No single value
!    of EPS will be reliable for all vectors X and functions FX.
!
!    Input, external FX, the name of the user written
!    routine which evaluates the function at a given point X.
!
!    FX should have the form:
!
!      subroutine fx ( m, n, x, f )
!      integer m
!      integer n
!      real (kind=4) :: f(m)
!      real (kind=4) :: x(n)
!      f(1:m) = ...
!      return
!      end
!
!    Input, real (kind=4) :: X(N), the point where the jacobian
!    is to be estimated.
!
!    Output, real (kind=4) :: FPRIME(M,N), the M by N estimated jacobian
!    matrix.
!

  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: del
  real (kind=4) :: eps
  real (kind=4) :: fprime(m,n)
  external fx
  integer (kind=4) :: j
  real (kind=4) :: x(n)
  real (kind=4) :: xsave
  real (kind=4) :: work1(m)
  real (kind=4) :: work2(m)
!
!  Evaluate the function at the base point, X.
!
  call fx ( m, n, x, work2 )
!
!  Now, one by one, vary each component J of the base point X, and
!  estimate DF(I)/DX(J) = ( F(X+) - F(X) )/ DEL.
!
  do j = 1, n

    xsave = x(j)
    del = eps * ( 1.0E+00 + abs ( x(j) ) )
    x(j) = x(j) + del
    call fx ( m, n, x, work1 )
    x(j) = xsave
    fprime(1:m,j) = ( work1(1:m) - work2(1:m) ) / del

  end do

  return
end
subroutine r4mat_l_inverse ( n, a, b )

!*****************************************************************************80
!
!! R4MAT_L_INVERSE inverts a lower triangular R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    A lower triangular matrix is a matrix whose only nonzero entries
!    occur on or below the diagonal.
!
!    The inverse of a lower triangular matrix is a lower triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, number of rows and columns in the matrix.
!
!    Input, real (kind=4) :: A(N,N), the lower triangular matrix.
!
!    Output, real (kind=4) :: B(N,N), the inverse matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: b(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j

  do j = 1, n

    do i = 1, n

      if ( i < j ) then
        b(i,j) = 0.0E+00
      else if ( j == i ) then
        b(i,j) = 1.0E+00 / a(i,j)
      else
        b(i,j) = - dot_product ( a(i,1:i-1), b(1:i-1,j) ) / a(i,i)
      end if

    end do
  end do

  return
end
subroutine r4mat_l_print ( m, n, a, title )

!*****************************************************************************80
!
!! R4MAT_L_PRINT prints a lower triangular R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Example:
!
!    M = 5, N = 5
!    A = (/ 11, 21, 31, 41, 51, 22, 32, 42, 52, 33, 43, 53, 44, 54, 55 /)
!
!    11
!    21 22
!    31 32 33
!    41 42 43 44
!    51 52 53 54 55
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(*), the M by N matrix.  Only the lower
!    triangular elements are stored, in column major order.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) n

  real      ( kind = 4 ) a(*)
  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) indx(10)
  integer   ( kind = 4 ) j
  integer   ( kind = 4 ) jhi
  integer   ( kind = 4 ) jlo
  integer   ( kind = 4 ) jmax
  integer   ( kind = 4 ) nn
  integer   ( kind = 4 ) size
  character ( len = * )  title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  jmax = min ( n, m )

  if ( m <= n ) then
    size = ( m * ( m + 1 ) ) / 2
  else if ( n < m ) then
    size = ( n * ( n + 1 ) ) / 2 + ( m - n ) * n
  end if

  if ( all ( a(1:size) == aint ( a(1:size) ) ) ) then

    nn = 10

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(a8,10i8)' ) '  Col   ', ( j, j = jlo, jhi )
      write ( *, '(a6)' ) '  Row '
      do i = jlo, m
        jhi = min ( jlo + nn - 1, i, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j - 1 ) ) / 2
        end do
        write ( *, '(i8,10i8)' ) i, int ( a(indx(1:jhi+1-jlo)) )
      end do
    end do

  else if ( maxval ( abs ( a(1:size) ) ) < 1000000.0E+00 ) then

    nn = 5

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(8x,5(i8,6x))' ) ( j, j = jlo, jhi )
      write ( *, '(a)' ) ' '
      do i = jlo, m
        jhi = min ( jlo + nn - 1, i, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j - 1 ) ) / 2
        end do
        write ( *, '(i8,5f14.6)' ) i, a(indx(1:jhi+1-jlo))
      end do
    end do

  else

    nn = 5

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(8x,5(i8,6x))' ) ( j, j = jlo, jhi )
      write ( *, '(a)' ) ' '
      do i = jlo, m
        jhi = min ( jlo + nn - 1, i, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j - 1 ) ) / 2
        end do
        write ( *, '(i8,5g14.6)' ) i, a(indx(1:jhi+1-jlo))
      end do
    end do

  end if

  return
end
subroutine r4mat_l_solve ( n, a, b, x )

!*****************************************************************************80
!
!! R4MAT_L_SOLVE solves a lower triangular linear system.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns of
!    the matrix A.
!
!    Input, real (kind=4) :: A(N,N), the N by N lower triangular matrix.
!
!    Input, real (kind=4) :: B(N), the right hand side of the linear system.
!
!    Output, real (kind=4) :: X(N), the solution of the linear system.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: b(n)
  integer (kind=4) :: i
  real (kind=4) :: x(n)
!
!  Solve L * x = b.
!
  do i = 1, n
    x(i) = ( b(i) - dot_product ( a(i,1:i-1), x(1:i-1) ) ) / a(i,i)
  end do

  return
end
subroutine r4mat_l1_inverse ( n, a, b )

!*****************************************************************************80
!
!! R4MAT_L1_INVERSE inverts a unit lower triangular R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    A unit lower triangular matrix is a matrix with only 1's on the main
!    diagonal, and only 0's above the main diagonal.
!
!    The inverse of a unit lower triangular matrix is also
!    a unit lower triangular matrix.
!
!    This routine can invert a matrix in place, that is, with no extra
!    storage.  If the matrix is stored in A, then the call
!
!      call r4mat_l1_inverse ( n, a, a )
!
!    will result in A being overwritten by its inverse.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, number of rows and columns in the matrix.
!
!    Input, real (kind=4) :: A(N,N), the unit lower triangular matrix.
!
!    Output, real (kind=4) :: B(N,N), the inverse matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: b(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j

  do i = 1, n

    do j = 1, n

      if ( i < j ) then
        b(i,j) = 0.0E+00
      else if ( j == i ) then
        b(i,j) = 1.0E+00
      else
        b(i,j) = -dot_product ( a(i,1:i-1), b(1:i-1,j) )
      end if

    end do
  end do

  return
end
subroutine r4mat_lt_solve ( n, a, b, x )

!*****************************************************************************80
!
!! R4MAT_LT_SOLVE solves a transposed lower triangular linear system.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    Given the lower triangular matrix A, the linear system to be solved is:
!
!      A' * x = b
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns
!    of the matrix.
!
!    Input, real (kind=4) :: A(N,N), the N by N lower triangular matrix.
!
!    Input, real (kind=4) :: B(N), the right hand side of the linear system.
!
!    Output, real (kind=4) :: X(N), the solution of the linear system.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: b(n)
  integer (kind=4) :: i
  real (kind=4) :: x(n)
!
!  Solve L'*x = b.
!
  do i = n, 1, -1
    x(i) = ( b(i) - dot_product ( x(i+1:n), a(i+1:n,i) ) ) / a(i,i)
  end do

  return
end
subroutine r4mat_lu ( m, n, a, l, p, u )

!*****************************************************************************80
!
!! R4MAT_LU computes the LU factorization of a rectangular R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The routine is given an M by N matrix A, and produces
!
!      L, an M by M unit lower triangular matrix,
!      U, an M by N upper triangular matrix, and
!      P, an M by M permutation matrix P,
!
!    so that
!
!      A = P' * L * U.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the M by N matrix to be factored.
!
!    Output, real (kind=4) :: L(M,M), the M by M unit lower triangular factor.
!
!    Output, real (kind=4) :: P(M,M), the M by M permutation matrix.
!
!    Output, real (kind=4) :: U(M,N), the M by N upper triangular factor.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  integer (kind=4) :: ipiv
  integer (kind=4) :: j
  real (kind=4) :: l(m,m)
  real (kind=4) :: p(m,m)
  real (kind=4) :: pivot
  real (kind=4) :: u(m,n)

!  Initialize:
!
!    U:=A
!    L:=Identity
!    P:=Identity
!
  u(1:m,1:n) = a(1:m,1:n)

  call r4mat_identity ( m, l )

  p(1:m,1:m) = l(1:m,1:m)
!
!  On step J, find the pivot row, IPIV, and the pivot value PIVOT.
!
  do j = 1, min ( m - 1, n )

    pivot = 0.0E+00
    ipiv = 0

    do i = j, m

      if ( pivot < abs ( u(i,j) ) ) then
        pivot = abs ( u(i,j) )
        ipiv = i
      end if

    end do
!
!  Unless IPIV is zero, swap rows J and IPIV.
!
    if ( ipiv /= 0 ) then

      call r4row_swap ( m, n, u, j, ipiv )

      call r4row_swap ( m, m, l, j, ipiv )

      call r4row_swap ( m, m, p, j, ipiv )
!
!  Zero out the entries in column J, from row J+1 to M.
!
      do i = j + 1, m

        if ( u(i,j) /= 0.0E+00 ) then

          l(i,j) = u(i,j) / u(j,j)

          u(i,j) = 0.0E+00

          u(i,j+1:n) = u(i,j+1:n) - l(i,j) * u(j,j+1:n)

        end if

      end do

    end if

  end do

  return
end
function r4mat_max ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_MAX returns the maximum entry of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the M by N matrix.
!
!    Output, real (kind=4) :: R4MAT_MAX, the maximum entry of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  real (kind=4) :: r4mat_max

  r4mat_max = maxval ( a(1:m,1:n) )

  return
end
subroutine r4mat_max_index ( m, n, a, i, j )

!*****************************************************************************80
!
!! R4MAT_MAX_INDEX returns the location of the maximum entry of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the M by N matrix.
!
!    Output, integer (kind=4) :: I, J, the indices of the maximum entry of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  integer (kind=4) :: ii
  integer (kind=4) :: j
  integer (kind=4) :: jj

  i = -1
  j = -1

  do jj = 1, n
    do ii = 1, m
      if ( ii == 1 .and. jj == 1 ) then
        i = ii
        j = jj
      else if ( a(i,j) < a(ii,jj) ) then
        i = ii
        j = jj
      end if
    end do
  end do

  return
end
function r4mat_maxcol_minrow ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_MAXCOL_MINROW gets the maximum column minimum row of an M by N R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    R4MAT_MAXCOL_MINROW = max ( 1 <= I <= N ) ( min ( 1 <= J <= M ) A(I,J) )
!
!    For a given matrix, R4MAT_MAXCOL_MINROW <= R4MAT_MINROW_MAXCOL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix.
!
!    Output, real (kind=4) :: R4MAT_MAXCOL_MINROW, the maximum column
!    minimum row entry of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  real (kind=4) :: r4mat_maxcol_minrow
  real (kind=4) :: r4mat_minrow

  r4mat_maxcol_minrow = 0.0E+00

  do i = 1, m

    r4mat_minrow = minval ( a(i,1:n) )

    if ( i == 1 ) then
      r4mat_maxcol_minrow = r4mat_minrow
    else
      r4mat_maxcol_minrow = max ( r4mat_maxcol_minrow, r4mat_minrow )
    end if

  end do

  return
end
function r4mat_maxrow_mincol ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_MAXROW_MINCOL gets the maximum row minimum column of an M by N R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    R4MAT_MAXROW_MINCOL = max ( 1 <= J <= N ) ( min ( 1 <= I <= M ) A(I,J) )
!
!    For a given matrix, R4MAT_MAXROW_MINCOL <= R4MAT_MINCOL_MAXROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix.
!
!    Output, real (kind=4) :: R4MAT_MAXROW_MINCOL, the maximum row
!    minimum column entry of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: j
  real (kind=4) :: r4mat_maxrow_mincol
  real (kind=4) :: r4mat_mincol

  r4mat_maxrow_mincol = 0.0E+00

  do j = 1, n

    r4mat_mincol = minval ( a(1:m,j) )

    if ( j == 1 ) then
      r4mat_maxrow_mincol = r4mat_mincol
    else
      r4mat_maxrow_mincol = max ( r4mat_maxrow_mincol, r4mat_mincol )
    end if

  end do

  return
end
function r4mat_min ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_MIN returns the minimum entry of an M by N R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix.
!
!    Output, real (kind=4) :: R4MAT_MIN, the minimum entry of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  real (kind=4) :: r4mat_min

  r4mat_min = minval ( a(1:m,1:n) )

  return
end
subroutine r4mat_min_index ( m, n, a, i, j )

!*****************************************************************************80
!
!! R4MAT_MIN_INDEX returns the location of the minimum entry of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the M by N matrix.
!
!    Output, integer (kind=4) :: I, J, the indices of the minimum entry of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  integer (kind=4) :: ii
  integer (kind=4) :: j
  integer (kind=4) :: jj

  i = -1
  j = -1

  do jj = 1, n
    do ii = 1, m
      if ( ii == 1 .and. jj == 1 ) then
        i = ii
        j = jj
      else if ( a(ii,jj) < a(i,j) ) then
        i = ii
        j = jj
      end if
    end do
  end do

  return
end
function r4mat_mincol_maxrow ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_MINCOL_MAXROW gets the minimum column maximum row of an M by N R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    R4MAT_MINCOL_MAXROW = min ( 1 <= I <= N ) ( max ( 1 <= J <= M ) A(I,J) )
!
!    For a given matrix, R4MAT_MAXROW_MINCOL <= R4MAT_MINCOL_MAXROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix.
!
!    Output, real (kind=4) :: R4MAT_MINCOL_MAXROW, the minimum column
!    maximum row entry of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  real (kind=4) :: r4mat_mincol_maxrow
  real (kind=4) :: r4mat_maxrow

  r4mat_mincol_maxrow = 0.0E+00

  do i = 1, m

    r4mat_maxrow = maxval ( a(i,1:n) )

    if ( i == 1 ) then
      r4mat_mincol_maxrow = r4mat_maxrow
    else
      r4mat_mincol_maxrow = min ( r4mat_mincol_maxrow, r4mat_maxrow )
    end if

  end do

  return
end
function r4mat_minrow_maxcol ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_MINROW_MAXCOL gets the minimum row maximum column of an M by N R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    R4MAT_MINROW_MAXCOL = min ( 1 <= J <= N ) ( max ( 1 <= I <= M ) A(I,J) )
!
!    For a given matrix, R4MAT_MAXCOL_MINROW <= R4MAT_MINROW_MAXCOL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix.
!
!    Output, real (kind=4) :: R4MAT_MINROW_MAXCOL, the minimum row
!    maximum column entry of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: j
  real (kind=4) :: r4mat_minrow_maxcol
  real (kind=4) :: r4mat_maxcol

  r4mat_minrow_maxcol = 0.0E+00

  do j = 1, n

    r4mat_maxcol = maxval ( a(1:m,j) )

    if ( j == 1 ) then
      r4mat_minrow_maxcol = r4mat_maxcol
    else
      r4mat_minrow_maxcol = min ( r4mat_minrow_maxcol, r4mat_maxcol )
    end if

  end do

  return
end
subroutine r4mat_mm ( n1, n2, n3, a, b, c )

!*****************************************************************************80
!
!! R4MAT_MM multiplies two R4MAT's.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    In FORTRAN90, this operation is more efficiently done by the
!    command:
!
!      C(1:N1,1:N3) = MATMUL ( A(1:N1,1;N2), B(1:N2,1:N3) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N1, N2, N3, the order of the matrices.
!
!    Input, real (kind=4) :: A(N1,N2), B(N2,N3), the matrices to multiply.
!
!    Output, real (kind=4) :: C(N1,N3), the product matrix C = A * B.
!
  implicit none

  integer (kind=4) :: n1
  integer (kind=4) :: n2
  integer (kind=4) :: n3

  real (kind=4) :: a(n1,n2)
  real (kind=4) :: b(n2,n3)
  real (kind=4) :: c(n1,n3)
  integer (kind=4) :: i
  integer (kind=4) :: j
  integer (kind=4) :: k

  do i = 1, n1
    do j = 1, n3
      c(i,j) = 0.0E+00
      do k = 1, n2
        c(i,j) = c(i,j) + a(i,k) * b(k,j)
      end do
    end do
  end do

  return
end
subroutine r4mat_mtv ( m, n, a, x, y )

!*****************************************************************************80
!
!! R4MAT_MTV multiplies a transposed matrix times a vector
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns of
!    the matrix.
!
!    Input, real (kind=4) :: A(M,N), the M by N matrix.
!
!    Input, real (kind=4) :: X(M), the vector to be multiplied by A.
!
!    Output, real (kind=4) :: Y(N), the product A'*X.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  real (kind=4) :: x(m)
  real (kind=4) :: y(n)

  y(1:n) = matmul ( transpose ( a(1:m,1:n) ), x(1:m) )

  return
end
subroutine r4mat_mv ( m, n, a, x, y )

!*****************************************************************************80
!
!! R4MAT_MV multiplies a matrix times a vector.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    In FORTRAN90, this operation can be more efficiently carried
!    out by the command
!
!      Y(1:M) = MATMUL ( A(1:M,1:N), X(1:N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns of
!    the matrix.
!
!    Input, real (kind=4) :: A(M,N), the M by N matrix.
!
!    Input, real (kind=4) :: X(N), the vector to be multiplied by A.
!
!    Output, real (kind=4) :: Y(M), the product A*X.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: x(n)
  real (kind=4) :: y(m)

  do i = 1, m
    y(i) = 0.0E+00
    do j = 1, n
      y(i) = y(i) + a(i,j) * x(j)
    end do
  end do

  return
end
subroutine r4mat_nint ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_NINT rounds the entries of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns of A.
!
!    Input/output, real (kind=4) :: A(M,N), the matrix to be NINT'ed.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)

  a(1:m,1:n) = real ( nint ( a(1:m,1:n) ), kind = 4 )

  return
end
function r4mat_norm_eis ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_NORM_EIS returns the EISPACK norm of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The EISPACK norm is defined as:
!
!      R4MAT_NORM_EIS =
!        sum ( 1 <= I <= M ) sum ( 1 <= J <= N ) abs ( A(I,J) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix whose EISPACK norm is desired.
!
!    Output, real (kind=4) :: R4MAT_NORM_EIS, the EISPACK norm of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  real (kind=4) :: r4mat_norm_eis

  r4mat_norm_eis = sum ( abs ( a(1:m,1:n) ) )

  return
end
function r4mat_norm_fro ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_NORM_FRO returns the Frobenius norm of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The Frobenius norm is defined as
!
!      R4MAT_NORM_FRO = sqrt (
!        sum ( 1 <= I <= M ) sum ( 1 <= j <= N ) A(I,J) * A(I,J) )
!
!    The matrix Frobenius norm is not derived from a vector norm, but
!    is compatible with the vector L2 norm, so that:
!
!      r4vec_norm_l2 ( A * x ) <= r4mat_norm_fro ( A ) * r4vec_norm_l2 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix whose Frobenius
!    norm is desired.
!
!    Output, real (kind=4) :: R4MAT_NORM_FRO, the Frobenius norm of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  real (kind=4) :: r4mat_norm_fro

  r4mat_norm_fro = sqrt ( sum ( a(1:m,1:n)**2 ) )

  return
end
function r4mat_norm_l1 ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_NORM_L1 returns the matrix L1 norm of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The matrix L1 norm is defined as:
!
!      R4MAT_NORM_L1 = max ( 1 <= J <= N )
!        sum ( 1 <= I <= M ) abs ( A(I,J) ).
!
!    The matrix L1 norm is derived from the vector L1 norm, and
!    satisifies:
!
!      r4vec_norm_l1 ( A * x ) <= r4mat_norm_l1 ( A ) * r4vec_norm_l1 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix whose L1 norm is desired.
!
!    Output, real (kind=4) :: R4MAT_NORM_L1, the L1 norm of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: j
  real (kind=4) :: r4mat_norm_l1

  r4mat_norm_l1 = 0.0E+00

  do j = 1, n
    r4mat_norm_l1 = max ( r4mat_norm_l1, sum ( abs ( a(1:m,j) ) ) )
  end do

  return
end
function r4mat_norm_l2 ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_NORM_L2 returns the matrix L2 norm of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The matrix L2 norm is defined as:
!
!      R4MAT_NORM_L2 = sqrt ( max ( 1 <= I <= M ) LAMBDA(I) )
!
!    where LAMBDA contains the eigenvalues of A * A'.
!
!    The matrix L2 norm is derived from the vector L2 norm, and
!    satisifies:
!
!      r4vec_norm_l2 ( A * x ) <= r4mat_norm_l2 ( A ) * r4vec_norm_l2 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix whose L2 norm is desired.
!
!    Output, real (kind=4) :: R4MAT_NORM_L2, the L2 norm of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  real (kind=4) :: b(m,m)
  real (kind=4) :: diag(m)
  real (kind=4) :: r4mat_norm_l2
!
!  Compute B = A * A'.
!
  b(1:m,1:m) = matmul ( a(1:m,1:n), transpose ( a(1:m,1:n) ) )
!
!  Diagonalize B.
!
  call r4mat_symm_jacobi ( m, b )
!
!  Find the maximum eigenvalue, and take its square root.
!
  call r4mat_diag_get_vector ( m, b, diag )

  r4mat_norm_l2 = sqrt ( maxval ( diag(1:m) ) )

  return
end
function r4mat_norm_li ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_NORM_LI returns the matrix L-oo norm of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The matrix L-oo norm is defined as:
!
!      R4MAT_NORM_LI =  max ( 1 <= I <= M ) sum ( 1 <= J <= N ) abs ( A(I,J) ).
!
!    The matrix L-oo norm is derived from the vector L-oo norm,
!    and satisifies:
!
!      r4vec_norm_li ( A * x ) <= r4mat_norm_li ( A ) * r4vec_norm_li ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix whose L-oo
!    norm is desired.
!
!    Output, real (kind=4) :: R4MAT_NORM_LI, the L-oo norm of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  real (kind=4) :: r4mat_norm_li

  r4mat_norm_li = 0.0E+00

  do i = 1, m
    r4mat_norm_li = max ( r4mat_norm_li, sum ( abs ( a(i,1:n) ) ) )
  end do

  return
end
subroutine r4mat_nullspace ( m, n, a, nullspace_size, nullspace )

!*****************************************************************************80
!
!! R4MAT_NULLSPACE computes the nullspace of a matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    Let A be an MxN matrix.
!
!    If X is an N-vector, and A*X = 0, then X is a null vector of A.
!
!    The set of all null vectors of A is called the nullspace of A.
!
!    The 0 vector is always in the null space.
!
!    If the 0 vector is the only vector in the nullspace of A, then A
!    is said to have maximum column rank.  (Because A*X=0 can be regarded
!    as a linear combination of the columns of A).  In particular, if A
!    is square, and has maximum column rank, it is nonsingular.
!
!    The dimension of the nullspace is the number of linearly independent
!    vectors that span the nullspace.  If A has maximum column rank,
!    its nullspace has dimension 0.
!
!    This routine uses the reduced row echelon form of A to determine
!    a set of NULLSPACE_SIZE independent null vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns of
!    the matrix A.
!
!    Input, real (kind=4) :: A(M,N), the matrix to be analyzed.
!
!    Input, integer (kind=4) :: NULLSPACE_SIZE, the size of the nullspace.
!
!    Output, real (kind=4) :: NULLSPACE(N,NULLSPACE_SIZE), vectors that
!    span the nullspace.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n
  integer (kind=4) :: nullspace_size

  real (kind=4) :: a(m,n)
  integer (kind=4) :: col(n)
  integer (kind=4) :: i
  integer (kind=4) :: i2
  integer (kind=4) :: j
  integer (kind=4) :: j2
  real (kind=4) :: nullspace(n,nullspace_size)
  integer (kind=4) :: row(m)
  real (kind=4) :: rref(m,n)
!
!  Make a copy of A.
!
  rref(1:m,1:n) = a(1:m,1:n)
!
!  Get the reduced row echelon form of A.
!
  call r4mat_rref ( m, n, rref )
!
!  Note in ROW the columns of the leading nonzeros.
!  COL(J) = +J if there is a leading 1 in that column, and -J otherwise.
!
  row(1:m) = 0

  do j = 1, n
    col(j) = - j
  end do

  do i = 1, m
    do j = 1, n
      if ( rref(i,j) == 1.0E+00 ) then
        row(i) = j
        col(j) = j
        exit
      end if
    end do
  end do

  nullspace(1:n,1:nullspace_size) = 0.0E+00

  j2 = 0
!
!  If column J does not contain a leading 1, then it contains
!  information about a null vector.
!
  do j = 1, n

    if ( col(j) < 0 ) then

      j2 = j2 + 1

      do i = 1, m
        if ( rref(i,j) /= 0.0E+00 ) then
          i2 = row(i)
          nullspace(i2,j2) = - rref(i,j)
        end if
      end do

      nullspace(j,j2) = 1.0E+00

    end if

  end do

  return
end
subroutine r4mat_nullspace_size ( m, n, a, nullspace_size )

!*****************************************************************************80
!
!! R4MAT_NULLSPACE_SIZE computes the size of the nullspace of a matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    Let A be an MxN matrix.
!
!    If X is an N-vector, and A*X = 0, then X is a null vector of A.
!
!    The set of all null vectors of A is called the nullspace of A.
!
!    The 0 vector is always in the null space.
!
!    If the 0 vector is the only vector in the nullspace of A, then A
!    is said to have maximum column rank.  (Because A*X=0 can be regarded
!    as a linear combination of the columns of A).  In particular, if A
!    is square, and has maximum column rank, it is nonsingular.
!
!    The dimension of the nullspace is the number of linearly independent
!    vectors that span the nullspace.  If A has maximum column rank,
!    its nullspace has dimension 0.
!
!    This routine ESTIMATES the dimension of the nullspace.  Cases of
!    singularity that depend on exact arithmetic will probably be missed.
!
!    The nullspace will be estimated by counting the leading 1's in the
!    reduced row echelon form of A, and subtracting this from N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns of
!    the matrix A.
!
!    Input, real (kind=4) :: A(M,N), the matrix to be analyzed.
!
!    Output, integer (kind=4) :: NULLSPACE_SIZE, the estimated size
!    of the nullspace.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  integer (kind=4) :: leading
  integer (kind=4) :: nullspace_size
  real (kind=4) :: rref(m,n)
!
!  Get the reduced row echelon form of A.
!
  rref(1:m,1:n) = a(1:m,1:n)

  call r4mat_rref ( m, n, rref )
!
!  Count the leading 1's in A.
!
  leading = 0
  do i = 1, m
    do j = 1, n
      if ( rref(i,j) == 1.0E+00 ) then
        leading = leading + 1
        exit
      end if
    end do
  end do

  nullspace_size = n - leading

  return
end
subroutine r4mat_orth_uniform ( n, seed, a )

!*****************************************************************************80
!
!! R4MAT_ORTH_UNIFORM returns a random orthogonal R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    Thanks to Eugene Petrov, B I Stepanov Institute of Physics,
!    National Academy of Sciences of Belarus, for convincingly
!    pointing out the severe deficiencies of an earlier version of
!    this routine.
!
!    Essentially, the computation involves saving the Q factor of the
!    QR factorization of a matrix whose entries are normally distributed.
!    However, it is only necessary to generate this matrix a column at
!    a time, since it can be shown that when it comes time to annihilate
!    the subdiagonal elements of column K, these (transformed) elements of
!    column K are still normally distributed random values.  Hence, there
!    is no need to generate them at the beginning of the process and
!    transform them K-1 times.
!
!    For computational efficiency, the individual Householder transformations
!    could be saved, as recommended in the reference, instead of being
!    accumulated into an explicit matrix format.
!
!  Properties:
!
!    The inverse of A is equal to A'.
!
!    A * A'  = A' * A = I.
!
!    Columns and rows of A have unit Euclidean norm.
!
!    Distinct pairs of columns of A are orthogonal.
!
!    Distinct pairs of rows of A are orthogonal.
!
!    The L2 vector norm of A*x = the L2 vector norm of x for any vector x.
!
!    The L2 matrix norm of A*B = the L2 matrix norm of B for any matrix B.
!
!    The determinant of A is +1 or -1.
!
!    All the eigenvalues of A have modulus 1.
!
!    All singular values of A are 1.
!
!    All entries of A are between -1 and 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 November 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Pete Stewart,
!    Efficient Generation of Random Orthogonal Matrices With an Application
!    to Condition Estimators,
!    SIAM Journal on Numerical Analysis,
!    Volume 17, Number 3, June 1980, pages 403-409.
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of A.
!
!    Input/output, integer (kind=4) :: SEED, a seed for the random
!    number generator.
!
!    Output, real (kind=4) :: A(N,N), the orthogonal matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: r4_normal_01
  integer (kind=4) :: seed
  real (kind=4) :: v(n)
  real (kind=4) :: x(n)
!
!  Start with A = the identity matrix.
!
  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 1.0E+00
      else
        a(i,j) = 0.0E+00
      end if
    end do
  end do
!
!  Now behave as though we were computing the QR factorization of
!  some other random matrix.  Generate the N elements of the first column,
!  compute the Householder matrix H1 that annihilates the subdiagonal elements,
!  and set A := A * H1' = A * H.
!
!  On the second step, generate the lower N-1 elements of the second column,
!  compute the Householder matrix H2 that annihilates them,
!  and set A := A * H2' = A * H2 = H1 * H2.
!
!  On the N-1 step, generate the lower 2 elements of column N-1,
!  compute the Householder matrix HN-1 that annihilates them, and
!  and set A := A * H(N-1)' = A * H(N-1) = H1 * H2 * ... * H(N-1).
!  This is our random orthogonal matrix.
!
  do j = 1, n-1
!
!  Set the vector that represents the J-th column to be annihilated.
!
    x(1:j-1) = 0.0E+00

    do i = j, n
      x(i) = r4_normal_01 ( seed )
    end do
!
!  Compute the vector V that defines a Householder transformation matrix
!  H(V) that annihilates the subdiagonal elements of X.
!
    call r4vec_house_column ( n, x, j, v )
!
!  Postmultiply the matrix A by H'(V) = H(V).
!
    call r4mat_house_axh ( n, a, v, a )

  end do

  return
end
subroutine r4mat_plot ( m, n, a, title )

!*****************************************************************************80
!
!! R4MAT_PLOT "plots" an R4MAT, with an optional title.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) n

  real      ( kind = 4 ) a(m,n)
  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) j
  integer   ( kind = 4 ) jhi
  integer   ( kind = 4 ) jlo
  character              r4mat_plot_symbol
  character ( len = 70 ) string
  character ( len = * )  title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  do jlo = 1, n, 70
    jhi = min ( jlo + 70-1, n )
    write ( *, '(a)' ) ' '
    write ( *, '(8x,2x,70i1)' ) ( mod ( j, 10 ), j = jlo, jhi )
    write ( *, '(a)' ) ' '

    do i = 1, m
      do j = jlo, jhi
        string(j+1-jlo:j+1-jlo) = r4mat_plot_symbol ( a(i,j) )
      end do
      write ( *, '(i8,2x,a)' ) i, string(1:jhi+1-jlo)
    end do
  end do

  return
end
function r4mat_plot_symbol ( r )

!*****************************************************************************80
!
!! R4MAT_PLOT_SYMBOL returns a symbol for an element of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: R, a value whose symbol is desired.
!
!    Output, character R4MAT_PLOT_SYMBOL, is
!    '-' if R is negative,
!    '0' if R is zero,
!    '+' if R is positive.
!
  implicit none

  character              r4mat_plot_symbol
  real      ( kind = 4 ) r

  if ( r < 0.0E+00 ) then
    r4mat_plot_symbol = '-'
  else if ( r == 0.0E+00 ) then
    r4mat_plot_symbol = '0'
  else if ( 0.0E+00 < r ) then
    r4mat_plot_symbol = '+'
  end if

  return
end
subroutine r4mat_poly_char ( n, a, p )

!*****************************************************************************80
!
!! R4MAT_POLY_CHAR computes the characteristic polynomial of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix A.
!
!    Input, real (kind=4) :: A(N,N), the N by N matrix.
!
!    Output, real (kind=4) :: P(0:N), the coefficients of the characteristic
!    polynomial of A.  P(N) contains the coefficient of X**N
!    (which will be 1), P(I) contains the coefficient of X**I,
!    and P(0) contains the constant term.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: order
  real (kind=4) :: p(0:n)
  real (kind=4) :: r4mat_trace
  real (kind=4) :: trace
  real (kind=4) :: work1(n,n)
  real (kind=4) :: work2(n,n)
!
!  Initialize WORK1 to the identity matrix.
!
  call r4mat_identity ( n, work1 )

  p(n) = 1.0E+00

  do order = n-1, 0, -1
!
!  Work2 = A * WORK1.
!
    work2(1:n,1:n) = matmul ( a(1:n,1:n), work1(1:n,1:n) )
!
!  Take the trace.
!
    trace = r4mat_trace ( n, work2 )
!
!  P(ORDER) = -Trace ( WORK2 ) / ( N - ORDER )
!
    p(order) = -trace / real ( n - order, kind = 4 )
!
!  WORK1 := WORK2 + P(ORDER) * Identity.
!
    work1(1:n,1:n) = work2(1:n,1:n)

    do i = 1, n
      work1(i,i) = work1(i,i) + p(order)
    end do

  end do

  return
end
subroutine r4mat_power ( n, a, npow, b )

!*****************************************************************************80
!
!! R4MAT_POWER computes a nonnegative power of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The algorithm is:
!
!      B = I
!      do NPOW times:
!        B = A * B
!      end
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of A.
!
!    Input, real (kind=4) :: A(N,N), the matrix to be raised to a power.
!
!    Input, integer (kind=4) :: NPOW, the power to which A is to be raised.
!    NPOW must be nonnegative.
!
!    Output, real (kind=4) :: B(N,N), the value of A**NPOW.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: b(n,n)
  integer (kind=4) :: ipow
  integer (kind=4) :: npow

  if ( npow < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4MAT_POWER - Fatal error!'
    write ( *, '(a)' ) '  Input value of NPOW < 0.'
    write ( *, '(a,i8)' ) '  NPOW = ', npow
    stop
  end if

  call r4mat_identity ( n, b )

  do ipow = 1, npow
    b(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )
  end do

  return
end
subroutine r4mat_power_method ( n, a, r, v )

!*****************************************************************************80
!
!! R4MAT_POWER_METHOD applies the power method to an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    If the power method has not converged, then calling the routine
!    again immediately with the output from the previous call will
!    continue the iteration.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of A.
!
!    Input, real (kind=4) :: A(N,N), the matrix.
!
!    Output, real (kind=4) :: R, the estimated eigenvalue.
!
!    Input/output, real (kind=4) :: V(N), on input, an estimate
!    for the eigenvector.  On output, an improved estimate for the
!    eigenvector.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: av(n)
  real (kind=4) :: eps
  integer (kind=4) :: it
  real ( kind = 4 ), parameter :: it_eps = 0.0001E+00
  integer ( kind = 4 ), parameter :: it_max = 100
  integer ( kind = 4 ), parameter :: it_min = 10
  integer (kind=4) :: j
  real (kind=4) :: r
  real (kind=4) :: r2
  real (kind=4) :: r_old
  real (kind=4) :: v(n)

  eps = sqrt ( epsilon ( 1.0E+00 ) )

  r = sqrt ( sum ( v(1:n)**2 ) )

  if ( r == 0.0E+00 ) then
    v(1:n) = 1.0E+00
    r = sqrt ( real ( n, kind = 4 ) )
  end if

  v(1:n) = v(1:n) / r

  do it = 1, it_max

    av(1:n) = matmul ( a(1:n,1:n), v(1:n) )

    r_old = r
    r = sqrt ( sum ( av(1:n)**2 ) )

    if ( it_min < it ) then
      if ( abs ( r - r_old ) <= it_eps * ( 1.0E+00 + abs ( r ) ) ) then
        exit
      end if
    end if

    v(1:n) = av(1:n)

    if ( r /= 0.0E+00 ) then
      v(1:n) = v(1:n) / r
    end if
!
!  Perturb V a bit, to avoid cases where the initial guess is exactly
!  the eigenvector of a smaller eigenvalue.
!
    if ( it < it_max / 2 ) then
      j = 1 + mod ( it - 1, n )
      v(j) = v(j) + eps * ( 1.0E+00 + abs ( v(j) ) )
      r2 = sqrt ( sum ( v(1:n)**2 ) )
      v(1:n) = v(1:n) / r2
    end if

  end do

  return
end
subroutine r4mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R4MAT_PRINT prints an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 January 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows in A.
!
!    Input, integer (kind=4) :: N, the number of columns in A.
!
!    Input, real (kind=4) :: A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) n

  real      ( kind = 4 ) a(m,n)
  character ( len = * )  title

  call r4mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R4MAT_PRINT_SOME prints some of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns.
!
!    Input, real (kind=4) :: A(M,N), an M by N matrix to be printed.
!
!    Input, integer (kind=4) :: ILO, JLO, the first row and column to print.
!
!    Input, integer (kind=4) :: IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ), parameter :: incx = 5
  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) n

  real      ( kind = 4 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) i2hi
  integer   ( kind = 4 ) i2lo
  integer   ( kind = 4 ) ihi
  integer   ( kind = 4 ) ilo
  integer   ( kind = 4 ) inc
  integer   ( kind = 4 ) j
  integer   ( kind = 4 ) j2
  integer   ( kind = 4 ) j2hi
  integer   ( kind = 4 ) j2lo
  integer   ( kind = 4 ) jhi
  integer   ( kind = 4 ) jlo
  character ( len = * )  title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( a(i,j) == real ( int ( a(i,j) ), kind = 4 ) ) then
          write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
        else
          write ( ctemp(j2), '(g14.6)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine r4mat_print2 ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_PRINT2 prints an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, the number of rows of A.
!
!    Input, integer (kind=4) :: N, the number of columns of A.
!
!    Input, real (kind=4) :: A(M,N), the M by N matrix to be printed.
!
  implicit none

  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) n

  real      ( kind = 4 ) a(m,n)
  real      ( kind = 4 ) amax
  real      ( kind = 4 ) amin
  integer   ( kind = 4 ) i
  character ( len = 10 ) iform
  integer   ( kind = 4 ) ihi
  integer   ( kind = 4 ) ilo
  logical                integ
  integer   ( kind = 4 ) j
  integer   ( kind = 4 ) jhi
  integer   ( kind = 4 ) jlo
  integer   ( kind = 4 ) lmax
  integer   ( kind = 4 ) npline
  real      ( kind = 4 ) r4_log_10
!
!  Check if all entries are integral.
!
  integ = .true.

  do i = 1, m
    do j = 1, n

      if ( integ ) then
        if ( a(i,j) /= real ( int ( a(i,j) ), kind = 4 ) ) then
          integ = .false.
        end if
      end if

    end do
  end do
!
!  Find the maximum and minimum entries.
!
  amax = maxval ( a(1:m,1:n) )
  amin = minval ( a(1:m,1:n) )
!
!  Use the information about the maximum size of an entry to
!  compute an intelligent format for use with integer entries.
!
!  Later, we might also do this for real matrices.
!
  lmax = int ( r4_log_10 ( amax ) )

  if ( integ ) then
    npline = 79 / ( lmax + 3 )
    write ( iform, '(''('',i2,''I'',i2,'')'')' ) npline, lmax+3
  else
    npline = 5
    iform = ' '
  end if
!
!  Print a scalar quantity.
!
  if ( m == 1 .and. n == 1 ) then

    if ( integ ) then
      write ( *, iform ) int ( a(1,1) )
    else
      write ( *, '(2x,g14.6)' ) a(1,1)
    end if
!
!  Column vector of length M,
!
  else if ( n == 1 ) then

    do ilo = 1, m, npline

      ihi = min ( ilo+npline-1, m )

      if ( integ ) then
        write ( *, iform ) ( int ( a(i,1) ), i = ilo, ihi )
      else
        write ( *, '(2x,5g14.6)' ) a(ilo:ihi,1)
      end if

    end do
!
!  Row vector of length N,
!
  else if ( m == 1 ) then

    do jlo = 1, n, npline

      jhi = min ( jlo+npline-1, n )

      if ( integ ) then
        write ( *, iform ) int ( a(1,jlo:jhi) )
      else
        write ( *, '(2x,5g14.6)' ) a(1,jlo:jhi)
      end if

    end do
!
!  M by N Array
!
  else

    do jlo = 1, n, npline

      jhi = min ( jlo+npline-1, n )

      if ( npline < n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8,a,i8)' ) 'Matrix columns ', jlo, ' to ', jhi
        write ( *, '(a)' ) ' '
      end if

      do i = 1, m

        if ( integ ) then
          write ( *, iform ) int ( a(i,jlo:jhi) )
        else
          write ( *, '(2x,5g14.6)' ) a(i,jlo:jhi)
        end if

      end do
    end do

  end if

  return
end
subroutine r4mat_ref ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_REF computes the row echelon form of a matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    A matrix is in row echelon form if:
!
!    * The first nonzero entry in each row is 1.
!
!    * The leading 1 in a given row occurs in a column to
!      the right of the leading 1 in the previous row.
!
!    * Rows which are entirely zero must occur last.
!
!  Example:
!
!    Input matrix:
!
!     1.0  3.0  0.0  2.0  6.0  3.0  1.0
!    -2.0 -6.0  0.0 -2.0 -8.0  3.0  1.0
!     3.0  9.0  0.0  0.0  6.0  6.0  2.0
!    -1.0 -3.0  0.0  1.0  0.0  9.0  3.0
!
!    Output matrix:
!
!     1.0  3.0  0.0  2.0  6.0  3.0  1.0
!     0.0  0.0  0.0  1.0  2.0  4.5  1.5
!     0.0  0.0  0.0  0.0  0.0  1.0  0.3
!     0.0  0.0  0.0  0.0  0.0  0.0  0.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns of
!    the matrix A.
!
!    Input/output, real (kind=4) :: A(M,N).  On input, the matrix to be
!    analyzed.  On output, the REF form of the matrix.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  integer (kind=4) :: lead
  integer (kind=4) :: r
  real (kind=4) :: temp

  lead = 1

  do r = 1, m

    if ( n < lead ) then
      exit
    end if

    i = r

    do while ( a(i,lead) == 0.0E+00 )

      i = i + 1

      if ( m < i ) then
        i = r
        lead = lead + 1
        if ( n < lead ) then
          lead = -1
          exit
        end if
      end if

    end do

    if ( lead < 0 ) then
      exit
    end if

    do j = 1, n
      temp   = a(i,j)
      a(i,j) = a(r,j)
      a(r,j) = temp
    end do

    a(r,1:n) = a(r,1:n) / a(r,lead)

    do i = r + 1, m
      a(i,1:n) = a(i,1:n) - a(i,lead) * a(r,1:n)
    end do

    lead = lead + 1

  end do

  return
end
subroutine r4mat_rref ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_RREF computes the reduced row echelon form of a matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    A matrix is in row echelon form if:
!
!    * The first nonzero entry in each row is 1.
!
!    * The leading 1 in a given row occurs in a column to
!      the right of the leading 1 in the previous row.
!
!    * Rows which are entirely zero must occur last.
!
!    The matrix is in reduced row echelon form if, in addition to
!    the first three conditions, it also satisfies:
!
!    * Each column containing a leading 1 has no other nonzero entries.
!
!  Example:
!
!    Input matrix:
!
!     1.0  3.0  0.0  2.0  6.0  3.0  1.0
!    -2.0 -6.0  0.0 -2.0 -8.0  3.0  1.0
!     3.0  9.0  0.0  0.0  6.0  6.0  2.0
!    -1.0 -3.0  0.0  1.0  0.0  9.0  3.0
!
!    Output matrix:
!
!     1.0  3.0  0.0  0.0  2.0  0.0  0.0
!     0.0  0.0  0.0  1.0  2.0  0.0  0.0
!     0.0  0.0  0.0  0.0  0.0  1.0  0.3
!     0.0  0.0  0.0  0.0  0.0  0.0  0.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns of
!    the matrix A.
!
!    Input/output, real (kind=4) :: A(M,N).  On input, the matrix to be
!    analyzed.  On output, the RREF form of the matrix.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  integer (kind=4) :: lead
  integer (kind=4) :: r
  real (kind=4) :: temp

  lead = 1

  do r = 1, m

    if ( n < lead ) then
      exit
    end if

    i = r

    do while ( a(i,lead) == 0.0E+00 )

      i = i + 1

      if ( m < i ) then
        i = r
        lead = lead + 1
        if ( n < lead ) then
          lead = -1
          exit
        end if
      end if

    end do

    if ( lead < 0 ) then
      exit
    end if

    do j = 1, n
      temp   = a(i,j)
      a(i,j) = a(r,j)
      a(r,j) = temp
    end do

    a(r,1:n) = a(r,1:n) / a(r,lead)

    do i = 1, m
      if ( i /= r ) then
        a(i,1:n) = a(i,1:n) - a(i,lead) * a(r,1:n)
      end if
    end do

    lead = lead + 1

  end do

  return
end
subroutine r4mat_solve ( n, rhs_num, a, info )

!*****************************************************************************80
!
!! R4MAT_SOLVE uses Gauss-Jordan elimination to solve an N by N linear system.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix.
!
!    Input, integer (kind=4) :: RHS_NUM, the number of right hand sides.
!    RHS_NUM must be at least 0.
!
!    Input/output, real (kind=4) :: A(N,N+RHS_NUM), contains in rows and
!    columns 1 to N the coefficient matrix, and in columns N+1 through
!    N+rhs_num, the right hand sides.  On output, the coefficient matrix
!    area has been destroyed, while the right hand sides have
!    been overwritten with the corresponding solutions.
!
!    Output, integer (kind=4) :: INFO, singularity flag.
!    0, the matrix was not singular, the solutions were computed;
!    J, factorization failed on step J, and the solutions could not
!    be computed.
!
  implicit none

  integer (kind=4) :: n
  integer (kind=4) :: rhs_num

  real (kind=4) :: a(n,n+rhs_num)
  real (kind=4) :: apivot
  real (kind=4) :: factor
  integer (kind=4) :: i
  integer (kind=4) :: info
  integer (kind=4) :: ipivot
  integer (kind=4) :: j
  real (kind=4) :: t(n+rhs_num)

  info = 0

  do j = 1, n
!
!  Choose a pivot row.
!
    ipivot = j
    apivot = a(j,j)

    do i = j + 1, n
      if ( abs ( apivot ) < abs ( a(i,j) ) ) then
        apivot = a(i,j)
        ipivot = i
      end if
    end do

    if ( apivot == 0.0E+00 ) then
      info = j
      return
    end if
!
!  The pivot row moves into the J-th row.
!
    if ( ipivot /= j ) then
      t(       1:n+rhs_num) = a(ipivot,1:n+rhs_num)
      a(ipivot,1:n+rhs_num) = a(j,     1:n+rhs_num)
      a(j,     1:n+rhs_num) = t(       1:n+rhs_num)
    end if
!
!  A(J,J) becomes 1.
!
    a(j,j) = 1.0E+00
    a(j,j+1:n+rhs_num) = a(j,j+1:n+rhs_num) / apivot
!
!  A(I,J) becomes 0.
!
    do i = 1, n

      if ( i /= j ) then
        factor = a(i,j)
        a(i,j) = 0.0E+00
        a(i,j+1:n+rhs_num) = a(i,j+1:n+rhs_num) - factor * a(j,j+1:n+rhs_num)
      end if

    end do

  end do

  return
end
subroutine r4mat_solve_2d ( a, b, det, x )

!*****************************************************************************80
!
!! R4MAT_SOLVE_2D solves a 2 by 2 linear system using Cramer's rule.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    If the determinant DET is returned as zero, then the matrix A is
!    singular, and does not have an inverse.  In that case, X is
!    returned as the zero vector.
!
!    If DET is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A(2,2), the matrix.
!
!    Input, real (kind=4) :: B(2), the right hand side.
!
!    Output, real (kind=4) :: DET, the determinant of the matrix A.
!
!    Output, real (kind=4) :: X(2), the solution of the system,
!    if DET is nonzero.
!
  implicit none

  real (kind=4) :: a(2,2)
  real (kind=4) :: b(2)
  real (kind=4) :: det
  real (kind=4) :: x(2)
!
!  Compute the determinant.
!
  det = a(1,1) * a(2,2) - a(1,2) * a(2,1)
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0E+00 ) then
    x(1:2) = 0.0E+00
    return
  end if
!
!  Compute the solution.
!
  x(1) = (  a(2,2) * b(1) - a(1,2) * b(2) ) / det
  x(2) = ( -a(2,1) * b(1) + a(1,1) * b(2) ) / det

  return
end
subroutine r4mat_solve_3d ( a, b, det, x )

!*****************************************************************************80
!
!! R4MAT_SOLVE_3D solves a 3 by 3 linear system using Cramer's rule.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    If the determinant DET is returned as zero, then the matrix A is
!    singular, and does not have an inverse.  In that case, X is
!    returned as the zero vector.
!
!    If DET is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A(3,3), the matrix.
!
!    Input, real (kind=4) :: B(3), the right hand side.
!
!    Output, real (kind=4) :: DET, the determinant of the matrix A.
!
!    Output, real (kind=4) :: X(3), the solution of the system,
!    if DET is nonzero.
!
  implicit none

  real (kind=4) :: a(3,3)
  real (kind=4) :: b(3)
  real (kind=4) :: det
  real (kind=4) :: x(3)
!
!  Compute the determinant.
!
  det =  a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
       + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) ) &
       + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) )
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0E+00 ) then
    x(1:3) = 0.0E+00
    return
  end if
!
!  Compute the solution.
!
  x(1) = (   ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) * b(1) &
           - ( a(1,2) * a(3,3) - a(1,3) * a(3,2) ) * b(2) &
           + ( a(1,2) * a(2,3) - a(1,3) * a(2,2) ) * b(3) ) / det

  x(2) = ( - ( a(2,1) * a(3,3) - a(2,3) * a(3,1) ) * b(1) &
           + ( a(1,1) * a(3,3) - a(1,3) * a(3,1) ) * b(2) &
           - ( a(1,1) * a(2,3) - a(1,3) * a(2,1) ) * b(3) ) / det

  x(3) = (   ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) * b(1) &
           - ( a(1,1) * a(3,2) - a(1,2) * a(3,1) ) * b(2) &
           + ( a(1,1) * a(2,2) - a(1,2) * a(2,1) ) * b(3) ) / det

  return
end
subroutine r4mat_solve2 ( n, a, b, x, ierror )

!*****************************************************************************80
!
!! R4MAT_SOLVE2 computes the solution of an N by N linear system.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The linear system may be represented as
!
!      A*X = B
!
!    If the linear system is singular, but consistent, then the routine will
!    still produce a solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of equations.
!
!    Input/output, real (kind=4) :: A(N,N).
!    On input, A is the coefficient matrix to be inverted.
!    On output, A has been overwritten.
!
!    Input/output, real (kind=4) :: B(N).
!    On input, B is the right hand side of the system.
!    On output, B has been overwritten.
!
!    Output, real (kind=4) :: X(N), the solution of the linear system.
!
!    Output, integer (kind=4) :: IERROR.
!    0, no error detected.
!    1, consistent singularity.
!    2, inconsistent singularity.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: amax
  real (kind=4) :: b(n)
  integer (kind=4) :: i
  integer (kind=4) :: ierror
  integer (kind=4) :: imax
  integer (kind=4) :: ipiv(n)
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: x(n)

  ierror = 0

  ipiv(1:n) = 0
  x(1:n) = 0.0E+00
!
!  Process the matrix.
!
  do k = 1, n
!
!  In column K:
!    Seek the row IMAX with the properties that:
!      IMAX has not already been used as a pivot;
!      A(IMAX,K) is larger in magnitude than any other candidate.
!
    amax = 0.0E+00
    imax = 0
    do i = 1, n
      if ( ipiv(i) == 0 ) then
        if ( amax < abs ( a(i,k) ) ) then
          imax = i
          amax = abs ( a(i,k) )
        end if
      end if
    end do
!
!  If you found a pivot row IMAX, then,
!    eliminate the K-th entry in all rows that have not been used for pivoting.
!
    if ( imax /= 0 ) then

      ipiv(imax) = k
      a(imax,k+1:n) = a(imax,k+1:n) / a(imax,k)
      b(imax) = b(imax) / a(imax,k)
      a(imax,k) = 1.0E+00

      do i = 1, n

        if ( ipiv(i) == 0 ) then
          a(i,k+1:n) = a(i,k+1:n) - a(i,k) * a(imax,k+1:n)
          b(i) = b(i) - a(i,k) * b(imax)
          a(i,k) = 0.0E+00
        end if

      end do

    end if

  end do
!
!  Now, every row with nonzero IPIV begins with a 1, and
!  all other rows are all zero.  Begin solution.
!
  do j = n, 1, -1

    imax = 0
    do k = 1, n
      if ( ipiv(k) == j ) then
        imax = k
      end if
    end do

    if ( imax == 0 ) then

      x(j) = 0.0E+00

      if ( b(j) == 0.0E+00 ) then
        ierror = 1
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R4MAT_SOLVE2 - Warning:'
        write ( *, '(a,i8)' ) '  Consistent singularity, equation = ', j
      else
        ierror = 2
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R4MAT_SOLVE2 - Error:'
        write ( *, '(a,i8)' ) '  Inconsistent singularity, equation = ', j
      end if

    else

      x(j) = b(imax)

      do i = 1, n
        if ( i /= imax ) then
          b(i) = b(i) - a(i,j) * x(j)
        end if
      end do

    end if

  end do

  return
end
subroutine r4mat_symm_eigen ( n, x, q, a )

!*****************************************************************************80
!
!! R4MAT_SYMM_EIGEN returns a symmetric matrix with given eigensystem.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The user must supply the desired eigenvalue vector, and the desired
!    eigenvector matrix.  The eigenvector matrix must be orthogonal.  A
!    suitable random orthogonal matrix can be generated by R4MAT_ORTH_UNIFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of A.
!
!    Input, real (kind=4) :: X(N), the desired eigenvalues for the matrix.
!
!    Input, real (kind=4) :: Q(N,N), the eigenvector matrix of A.
!
!    Output, real (kind=4) :: A(N,N), a symmetric matrix with
!    eigenvalues X and eigenvectors the columns of Q.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: q(n,n)
  real (kind=4) :: x(n)
!
!  Set A = Q * Lambda * Q'.
!
  a(1:n,1:n) = 0.0E+00

  do i = 1, n
    do j = 1, n
      do k = 1, n
        a(i,j) = a(i,j) + q(i,k) * x(k) * q(j,k)
      end do
    end do
  end do

  return
end
subroutine r4mat_symm_jacobi ( n, a )

!*****************************************************************************80
!
!! R4MAT_SYMM_JACOBI applies Jacobi eigenvalue iteration to a symmetric matrix.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    This code was modified so that it treats as zero the off-diagonal
!    elements that are sufficiently close to, but not exactly, zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of A.
!
!    Input/output, real (kind=4) :: A(N,N), a symmetric N by N matrix.
!    On output, the matrix has been overwritten by an approximately
!    diagonal matrix, with the eigenvalues on the diagonal.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: c
  real (kind=4) :: r4mat_norm_fro
  real ( kind = 4 ), parameter :: eps = 0.00001E+00
  integer (kind=4) :: i
  integer (kind=4) :: it
  integer ( kind = 4 ), parameter :: it_max = 100
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: norm_fro
  real (kind=4) :: s
  real (kind=4) :: sum2
  real (kind=4) :: t
  real (kind=4) :: t1
  real (kind=4) :: t2
  real (kind=4) :: u

  norm_fro = r4mat_norm_fro ( n, n, a )

  it = 0

  do

    it = it + 1

    do i = 1, n
      do j = 1, i - 1

        if ( eps * norm_fro < abs ( a(i,j) ) + abs ( a(j,i) ) ) then

          u = ( a(j,j) - a(i,i) ) / ( a(i,j) + a(j,i) )

          t = sign ( 1.0E+00, u ) / ( abs ( u ) + sqrt ( u * u + 1.0E+00 ) )
          c = 1.0E+00 / sqrt ( t * t + 1.0E+00 )
          s = t * c
!
!  A -> A * Q.
!
          do k = 1, n
            t1 = a(i,k)
            t2 = a(j,k)
            a(i,k) = t1 * c - t2 * s
            a(j,k) = t1 * s + t2 * c
          end do
!
!  A -> QT * A
!
          do k = 1, n
            t1 = a(k,i)
            t2 = a(k,j)
            a(k,i) = c * t1 - s * t2
            a(k,j) = s * t1 + c * t2
          end do

        end if
      end do
    end do
!
!  Test the size of the off-diagonal elements.
!
    sum2 = 0.0E+00
    do i = 1, n
      do j = 1, i - 1
        sum2 = sum2 + abs ( a(i,j) )
      end do
    end do

    if ( sum2 <= eps * ( norm_fro + 1.0E+00 ) ) then
      exit
    end if

    if ( it_max <= it ) then
      exit
    end if

  end do

  return
end
subroutine r4mat_to_r4plu ( n, a, pivot, lu, info )

!*****************************************************************************80
!
!! R4MAT_TO_R4PLU factors a general R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    This routine is a simplified version of the LINPACK routine DGEFA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1.
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix.
!    N must be positive.
!
!    Input, real (kind=4) :: A(N,N), the matrix to be factored.
!
!    Output, integer (kind=4) :: PIVOT(N), a vector of pivot indices.
!
!    Output, real (kind=4) :: LU(N,N), an upper triangular matrix U and
!    the multipliers L which were used to obtain it.  The factorization
!    can be written A = L * U, where L is a product of permutation and
!    unit lower triangular matrices and U is upper triangular.
!
!    Output, integer (kind=4) :: INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: info
  integer (kind=4) :: pivot(n)
  integer (kind=4) :: j
  integer (kind=4) :: k
  integer (kind=4) :: l
  real (kind=4) :: lu(n,n)
  real (kind=4) :: temp

  lu(1:n,1:n) = a(1:n,1:n)

  info = 0

  do k = 1, n - 1
!
!  Find L, the index of the pivot row.
!
    l = k
    do i = k + 1, n
      if ( abs ( lu(l,k) ) < abs ( lu(i,k) ) ) then
        l = i
      end if
    end do

    pivot(k) = l
!
!  If the pivot index is zero, the algorithm has failed.
!
    if ( lu(l,k) == 0.0E+00 ) then
      info = k
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4MAT_TO_R4PLU - Fatal error!'
      write ( *, '(a,i8)' ) '  Zero pivot on step ', info
      return
    end if
!
!  Interchange rows L and K if necessary.
!
    if ( l /= k ) then
      temp    = lu(l,k)
      lu(l,k) = lu(k,k)
      lu(k,k) = temp
    end if
!
!  Normalize the values that lie below the pivot entry A(K,K).
!
    lu(k+1:n,k) = -lu(k+1:n,k) / lu(k,k)
!
!  Row elimination with column indexing.
!
    do j = k + 1, n

      if ( l /= k ) then
        temp    = lu(l,j)
        lu(l,j) = lu(k,j)
        lu(k,j) = temp
      end if

      lu(k+1:n,j) = lu(k+1:n,j) + lu(k+1:n,k) * lu(k,j)

    end do

  end do

  pivot(n) = n

  if ( lu(n,n) == 0.0E+00 ) then
    info = n
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4MAT_TO_R4PLU - Fatal error!'
    write ( *, '(a,i8)' ) '  Zero pivot on step ', info
  end if

  return
end
function r4mat_trace ( n, a )

!*****************************************************************************80
!
!! R4MAT_TRACE computes the trace of an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The trace of a square matrix is the sum of the diagonal elements.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix A.
!
!    Input, real (kind=4) :: A(N,N), the matrix whose trace is desired.
!
!    Output, real (kind=4) :: R4MAT_TRACE, the trace of the matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  real (kind=4) :: r4mat_trace

  r4mat_trace = 0.0E+00
  do i = 1, n
    r4mat_trace = r4mat_trace + a(i,i)
  end do

  return
end
subroutine r4mat_transpose_in_place ( n, a )

!*****************************************************************************80
!
!! R4MAT_TRANSPOSE_IN_PLACE transposes a square matrix in place.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of rows and columns
!    of the matrix A.
!
!    Input/output, real (kind=4) :: A(N,N), the matrix to be transposed.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: t

  do j = 1, n
    do i = 1, j - 1
      t      = a(i,j)
      a(i,j) = a(j,i)
      a(j,i) = t
    end do
  end do

  return
end
subroutine r4mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! R4MAT_TRANSPOSE_PRINT prints an R4MAT, transposed.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns.
!
!    Input, real (kind=4) :: A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) n

  real      ( kind = 4 ) a(m,n)
  character ( len = * )  title

  call r4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r4mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R4MAT_TRANSPOSE_PRINT_SOME prints some of an R4MAT, transposed.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns.
!
!    Input, real (kind=4) :: A(M,N), an M by N matrix to be printed.
!
!    Input, integer (kind=4) :: ILO, JLO, the first row and column to print.
!
!    Input, integer (kind=4) :: IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ), parameter :: incx = 5
  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) n

  real      ( kind = 4 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) i2
  integer   ( kind = 4 ) i2hi
  integer   ( kind = 4 ) i2lo
  integer   ( kind = 4 ) ihi
  integer   ( kind = 4 ) ilo
  integer   ( kind = 4 ) inc
  integer   ( kind = 4 ) j
  integer   ( kind = 4 ) j2hi
  integer   ( kind = 4 ) j2lo
  integer   ( kind = 4 ) jhi
  integer   ( kind = 4 ) jlo
  character ( len = * )  title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = min ( i2hi, m )
    i2hi = min ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    write ( *, '(a)' ) ' '

    do i = i2lo, i2hi
      i2 = i + 1 - i2lo
      write ( ctemp(i2), '(i8,6x)' ) i
    end do

    write ( *, '(''  Row   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Col'
    write ( *, '(a)' ) ' '

    j2lo = max ( jlo, 1 )
    j2hi = min ( jhi, n )

    do j = j2lo, j2hi

      do i2 = 1, inc
        i = i2lo - 1 + i2
        write ( ctemp(i2), '(g14.6)' ) a(i,j)
      end do

      write ( *, '(i5,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

    end do

  end do

  return
end
subroutine r4mat_u_inverse ( n, a, b )

!*****************************************************************************80
!
!! R4MAT_U_INVERSE inverts an upper triangular R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    An upper triangular matrix is a matrix whose only nonzero entries
!    occur on or above the diagonal.
!
!    The inverse of an upper triangular matrix is an upper triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, number of rows and columns in the matrix.
!
!    Input, real (kind=4) :: A(N,N), the upper triangular matrix.
!
!    Output, real (kind=4) :: B(N,N), the inverse matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: b(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j

  do j = n, 1, -1

    do i = n, 1, -1

      if ( j < i ) then
        b(i,j) = 0.0E+00
      else if ( i == j ) then
        b(i,j) = 1.0E+00 / a(i,j)
      else
        b(i,j) = - dot_product ( a(i,i+1:j), b(i+1:j,j) ) / a(i,i)
      end if

    end do
  end do

  return
end
subroutine r4mat_u1_inverse ( n, a, b )

!*****************************************************************************80
!
!! R4MAT_U1_INVERSE inverts a unit upper triangular R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    A unit upper triangular matrix is a matrix with only 1's on the main
!    diagonal, and only 0's below the main diagonal.
!
!    The inverse of a unit upper triangular matrix is also
!    a unit upper triangular matrix.
!
!    This routine can invert a matrix in place, that is, with no extra
!    storage.  If the matrix is stored in A, then the call
!
!      call r4mat_u1_inverse ( n, a, a )
!
!    will result in A being overwritten by its inverse.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt,
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, number of rows and columns in the matrix.
!
!    Input, real (kind=4) :: A(N,N), the unit upper triangular matrix.
!
!    Output, real (kind=4) :: B(N,N), the inverse matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  real (kind=4) :: b(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j

  do j = n, 1, -1

    do i = n, 1, -1

      if ( j < i ) then
        b(i,j) = 0.0E+00
      else if ( i == j ) then
        b(i,j) = 1.0E+00
      else
        b(i,j) = -dot_product ( a(i,i+1:j), b(i+1:j,j) )
      end if

    end do
  end do

  return
end
subroutine r4mat_uniform ( m, n, a, b, seed, r )

!*****************************************************************************80
!
!! R4MAT_UNIFORM fills an R4MAT with scaled pseudorandom numbers.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer  ( kind = 4 )M, N, the number of rows and columns in
!    the array.
!
!    Input, real (kind=4) :: A, B, the lower and upper limits.
!
!    Input/output, integer (kind=4) :: SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real (kind=4) :: R(M,N), the array of pseudorandom values.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a
  real (kind=4) :: b
  integer (kind=4) :: i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer (kind=4) :: j
  integer (kind=4) :: k
  integer (kind=4) :: seed
  real (kind=4) :: r(m,n)

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = a + ( b - a ) * real ( seed, kind = 4 ) * 4.656612875E-10

    end do
  end do

  return
end
subroutine r4mat_uniform_01 ( m, n, seed, r )

!*****************************************************************************80
!
!! R4MAT_UNIFORM_01 fills an R4MAT with unit pseudorandom numbers.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns in
!    the array.
!
!    Input/output, integer (kind=4) :: SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real (kind=4) :: R(M,N), the array of pseudorandom values.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  integer (kind=4) :: i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer (kind=4) :: j
  integer (kind=4) :: k
  integer (kind=4) :: seed
  real (kind=4) :: r(m,n)

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = real ( seed, kind = 4 ) * 4.656612875E-10

    end do
  end do

  return
end
subroutine r4mat_vand2 ( n, x, a )

!*****************************************************************************80
!
!! R4MAT_VAND2 returns the N by N row Vandermonde matrix A.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!    The row Vandermonde matrix returned by this routine reads "across"
!    rather than down.  In particular, each row begins with a 1, followed by
!    some value X, followed by successive powers of X.
!
!  Formula:
!
!    A(I,J) = X(I)**(J-1)
!
!  Properties:
!
!    A is nonsingular if, and only if, the X values are distinct.
!
!    The determinant of A is
!
!      det(A) = product ( 2 <= I <= N ) (
!        product ( 1 <= J <= I-1 ) ( ( X(I) - X(J) ) ) ).
!
!    The matrix A is generally ill-conditioned.
!
!  Example:
!
!    N = 5, X = (2, 3, 4, 5, 6)
!
!    1 2  4   8   16
!    1 3  9  27   81
!    1 4 16  64  256
!    1 5 25 125  625
!    1 6 36 216 1296
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix desired.
!
!    Input, real (kind=4) :: X(N), the values that define A.
!
!    Output, real (kind=4) :: A(N,N), the N by N row Vandermonde matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: x(n)

  do i = 1, n
    do j = 1, n

      if ( j == 1 .and. x(i) == 0.0E+00 ) then
        a(i,j) = 1.0E+00
      else
        a(i,j) = x(i)**(j-1)
      end if

    end do
  end do

  return
end
subroutine r4mat_zero ( m, n, a )

!*****************************************************************************80
!
!! R4MAT_ZERO zeroes an R4MAT.
!
!  Discussion:
!
!    An R4MAT is an array of R4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns.
!
!    Output, real (kind=4) :: A(M,N), the matrix of zeroes.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)

  a(1:m,1:n) = 0.0E+00

  return
end



subroutine r4plu_det ( n, pivot, lu, det )

!*****************************************************************************80
!
!! R4PLU_DET computes the determinant of an R4PLU matrix.
!
!  Discussion:
!
!    The matrix should have been factored by R4MAT_TO_R4PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1.
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix.
!    N must be positive.
!
!    Input, integer (kind=4) :: PIVOT(N), the pivot vector computed
!    by R4MAT_TO_R4PLU.
!
!    Input, real (kind=4) :: LU(N,N), the LU factors computed
!    by R4MAT_TO_R4PLU.
!
!    Output, real (kind=4) :: DET, the determinant of the matrix.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: det
  integer (kind=4) :: i
  real (kind=4) :: lu(n,n)
  integer (kind=4) :: pivot(n)

  det = 1.0E+00

  do i = 1, n
    det = det * lu(i,i)
    if ( pivot(i) /= i ) then
      det = -det
    end if
  end do

  return
end
subroutine r4plu_inverse ( n, pivot, lu, a_inverse )

!*****************************************************************************80
!
!! R4PLU_INVERSE computes the inverse of an R4PLU matrix.
!
!  Discussion:
!
!    The matrix should have been factored by R4MAT_TO_R4PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix A.
!
!    Input, integer (kind=4) :: PIVOT(N), the pivot vector from
!    R4MAT_TO_R4PLU.
!
!    Input, real (kind=4) :: LU(N,N), the LU factors computed by
!    R4MAT_TO_R4PLU.
!
!    Output, real (kind=4) :: A_INVERSE(N,N), the inverse of the original
!    matrix A that was factored by R4MAT_TO_R4PLU.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a_inverse(n,n)
  integer (kind=4) :: i
  real (kind=4) :: lu(n,n)
  integer (kind=4) :: pivot(n)
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: temp
  real (kind=4) :: work(n)

  a_inverse(1:n,1:n) = lu(1:n,1:n)
!
!  Compute Inverse(U).
!
  do k = 1, n

    a_inverse(k,k)     = 1.0E+00 / a_inverse(k,k)
    a_inverse(1:k-1,k) = -a_inverse(1:k-1,k) * a_inverse(k,k)

    do j = k + 1, n

      temp             = a_inverse(k,j)
      a_inverse(k,j)   = 0.0E+00
      a_inverse(1:k,j) = a_inverse(1:k,j) + temp * a_inverse(1:k,k)

    end do

  end do
!
!  Form Inverse(U) * Inverse(L).
!
  do k = n - 1, 1, -1

    work(k+1:n) = a_inverse(k+1:n,k)
    a_inverse(k+1:n,k) = 0.0E+00

    do j = k + 1, n
      a_inverse(1:n,k) = a_inverse(1:n,k) + a_inverse(1:n,j) * work(j)
    end do

    if ( pivot(k) /= k ) then

      do i = 1, n
        temp                  = a_inverse(i,k)
        a_inverse(i,k)        = a_inverse(i,pivot(k))
        a_inverse(i,pivot(k)) = temp
      end do

    end if

  end do

  return
end
subroutine r4plu_mul ( n, pivot, lu, x, b )

!*****************************************************************************80
!
!! R4PLU_MUL computes A * x using the PLU factors of A.
!
!  Discussion:
!
!    It is assumed that R4MAT_TO_R4PLU has computed the PLU factors of
!    the matrix A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix.
!    N must be positive.
!
!    Input, integer (kind=4) :: PIVOT(N), the pivot vector computed
!    by R4MAT_TO_R4PLU.
!
!    Input, real (kind=4) :: LU(N,N), the matrix factors computed by
!    R4MAT_TO_R4PLU.
!
!    Input, real (kind=4) :: X(N), the vector to be multiplied.
!
!    Output, real (kind=4) :: B(N), the result of the multiplication.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: b(n)
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: lu(n,n)
  integer (kind=4) :: pivot(n)
  real (kind=4) :: temp
  real (kind=4) :: x(n)

  b(1:n) = x(1:n)
!
!  Y = U * X.
!
  do j = 1, n
    b(1:j-1) = b(1:j-1) + lu(1:j-1,j) * b(j)
    b(j) = lu(j,j) * b(j)
  end do
!
!  B = PL * Y = PL * U * X = A * x.
!
  do j = n - 1, 1, -1

    b(j+1:n) = b(j+1:n) - lu(j+1:n,j) * b(j)

    k = pivot(j)

    if ( k /= j ) then
      temp = b(k)
      b(k) = b(j)
      b(j) = temp
    end if

  end do

  return
end
subroutine r4plu_sol ( n, pivot, lu, b, x )

!*****************************************************************************80
!
!! R4PLU_SOL solves a linear system A*x=b from the PLU factors.
!
!  Discussion:
!
!    The PLU factors should have been computed by R4MAT_TO_R4PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix.
!
!    Input, integer (kind=4) :: PIVOT(N), the pivot vector from R4MAT_TO_R4PLU.
!
!    Input, real (kind=4) :: LU(N,N), the LU factors from R4MAT_TO_R4PLU.
!
!    Input, real (kind=4) :: B(N), the right hand side vector.
!
!    Output, real (kind=4) :: X(N), the solution vector.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: b(n)
  integer (kind=4) :: pivot(n)
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: lu(n,n)
  real (kind=4) :: temp
  real (kind=4) :: x(n)
!
!  Solve PL * Y = B.
!
  x(1:n) = b(1:n)

  do k = 1, n - 1

    j = pivot(k)

    if ( j /= k ) then
      temp = x(j)
      x(j) = x(k)
      x(k) = temp
    end if

    x(k+1:n) = x(k+1:n) + lu(k+1:n,k) * x(k)

  end do
!
!  Solve U * X = Y.
!
  do k = n, 1, -1
    x(k) = x(k) / lu(k,k)
    x(1:k-1) = x(1:k-1) - lu(1:k-1,k) * x(k)
  end do

  return
end
subroutine r4plu_to_r4mat ( n, pivot, lu, a )

!*****************************************************************************80
!
!! R4PLU_TO_R4MAT recovers the matrix A that was factored by R4MAT_TO_R4PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the matrix.
!    N must be positive.
!
!    Input, integer (kind=4) :: PIVOT(N), the pivot vector computed
!    by R4MAT_TO_R4PLU.
!
!    Input, real (kind=4) :: LU(N,N), the matrix factors computed by
!    R4MAT_TO_R4PLU.
!
!    Output, real (kind=4) :: A(N,N), the matrix whose factors are
!    represented by LU and PIVOT.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n,n)
  integer (kind=4) :: i
  real (kind=4) :: lu(n,n)
  integer (kind=4) :: pivot(n)
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: temp

  a(1:n,1:n) = 0.0E+00
  do i = 1, n
    a(i,i) = 1.0E+00
  end do

  do j = 1, n

    do i = 1, n
      a(1:i-1,j) = a(1:i-1,j) + lu(1:i-1,i) * a(i,j)
      a(i,j) = lu(i,i) * a(i,j)
    end do
!
!  B = PL * Y = PL * U * X = A * x.
!
    do i = n - 1, 1, -1

      a(i+1:n,j) = a(i+1:n,j) - lu(i+1:n,i) * a(i,j)

      k = pivot(i)

      if ( k /= i ) then
        temp   = a(k,j)
        a(k,j) = a(i,j)
        a(i,j) = temp
      end if

    end do

  end do

  return
end


end module jburk_r4mat_
