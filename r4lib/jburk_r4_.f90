module     jburk_r4_
use, intrinsic :: iso_fortran_env
implicit none

contains
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer (kind=4) :: IUNIT, the free unit number.
!
  implicit none

  integer (kind=4) :: i
  integer (kind=4) :: ios
  integer (kind=4) :: iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
function i4_log_10 ( i )

!*****************************************************************************80
!
!! I4_LOG_10 returns the integer part of the logarithm base 10 of an I4.
!
!  Discussion:
!
!    I4_LOG_10 ( I ) + 1 is the number of decimal digits in I.
!
!    An I4 is an integer (kind=4) :: value.
!
!  Example:
!
!        I  I4_LOG_10
!    -----  --------
!        0    0
!        1    0
!        2    0
!        9    0
!       10    1
!       11    1
!       99    1
!      100    2
!      101    2
!      999    2
!     1000    3
!     1001    3
!     9999    3
!    10000    4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 June 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: I, the number whose logarithm base 10
!    is desired.
!
!    Output, integer (kind=4) :: I4_LOG_10, the integer part of the
!    logarithm base 10 of the absolute value of X.
!
  implicit none

  integer (kind=4) :: i
  integer (kind=4) :: i_abs
  integer (kind=4) :: i4_log_10
  integer (kind=4) :: ten_pow

  if ( i == 0 ) then

    i4_log_10 = 0

  else

    i4_log_10 = 0
    ten_pow = 10

    i_abs = abs ( i )

    do while ( ten_pow <= i_abs )
      i4_log_10 = i4_log_10 + 1
      ten_pow = ten_pow * 10
    end do

  end if

  return
end


subroutine i4int_to_r4int ( imin, imax, i, rmin, rmax, r )

!*****************************************************************************80
!
!! I4INT_TO_R4INT maps an I4INT to an R4INT.
!
!  Discussion:
!
!    The formula used is:
!
!      R := RMIN + ( RMAX - RMIN ) * ( I - IMIN ) / ( IMAX - IMIN )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: IMIN, IMAX, the range.
!
!    Input, integer (kind=4) :: I, the integer to be converted.
!
!    Input, real (kind=4) :: RMIN, RMAX, the range.
!
!    Output, real (kind=4) :: R, the corresponding value in [RMIN,RMAX].
!
  implicit none

  integer (kind=4) :: i
  integer (kind=4) :: imax
  integer (kind=4) :: imin
  real (kind=4) :: r
  real (kind=4) :: rmax
  real (kind=4) :: rmin

  if ( imax == imin ) then

    r = 0.5E+00 * ( rmin + rmax )

  else

    r = ( real ( imax - i,        kind = 4 ) * rmin   &
        + real (        i - imin, kind = 4 ) * rmax ) &
        / real ( imax     - imin, kind = 4 )

  end if

  return
end

subroutine i4vec_indicator ( n, a )

!*****************************************************************************80
!
!! I4VEC_INDICATOR sets an I4VEC to the indicator vector.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of elements of A.
!
!    Output, integer (kind=4) :: A(N), the array to be initialized.
!
  implicit none

  integer (kind=4) :: n

  integer (kind=4) :: a(n)
  integer (kind=4) :: i

  do i = 1, n
    a(i) = i
  end do

  return
end
subroutine i4vec_permute ( n, p, a )

!*****************************************************************************80
!
!! I4VEC_PERMUTE permutes an I4VEC in place.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    This routine permutes an array of integer "objects", but the same
!    logic can be used to permute an array of objects of any arithmetic
!    type, or an array of objects of any complexity.  The only temporary
!    storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,   4,   5,   1,   3 )
!      A = (   1,   2,   3,   4,   5 )
!
!    Output:
!
!      A    = (   2,   4,   5,   1,   3 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of objects.
!
!    Input, integer (kind=4) :: P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.
!
!    Input/output, integer (kind=4) :: A(N), the array to be permuted.
!
  implicit none

  integer (kind=4) :: n

  integer (kind=4) :: a(n)
  integer (kind=4) :: a_temp
  integer ( kind = 4 ), parameter :: base = 1
  integer (kind=4) :: ierror
  integer (kind=4) :: iget
  integer (kind=4) :: iput
  integer (kind=4) :: istart
  integer (kind=4) :: p(n)

  call perm_check ( n, p, base, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_PERMUTE - Fatal error!'
    write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
    stop
  end if
!
!  Search for the next element of the permutation that has not been used.
!
  do istart = 1, n

    if ( p(istart) < 0 ) then

      cycle

    else if ( p(istart) == istart ) then

      p(istart) = - p(istart)
      cycle

    else

      a_temp = a(istart)
      iget = istart
!
!  Copy the new value into the vacated entry.
!
      do

        iput = iget
        iget = p(iget)

        p(iput) = - p(iput)

        if ( iget < 1 .or. n < iget ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'I4VEC_PERMUTE - Fatal error!'
          write ( *, '(a)' ) '  A permutation index is out of range.'
          write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
          stop
        end if

        if ( iget == istart ) then
          a(iput) = a_temp
          exit
        end if

        a(iput) = a(iget)

      end do

    end if

  end do
!
!  Restore the signs of the entries.
!
  p(1:n) = - p(1:n)

  return
end
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of components of the vector.
!
!    Input, integer (kind=4) :: A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ) n

  integer   ( kind = 4 ) a(n)
  integer   ( kind = 4 ) i
  character ( len = * )  title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,a,2x,i12)' ) i, ':', a(i)
  end do

  return
end
subroutine perm_check ( n, p, base, ierror )

!*****************************************************************************80
!
!! PERM_CHECK checks that a vector represents a permutation.
!
!  Discussion:
!
!    The routine verifies that each of the integers from BASE to
!    to BASE+N-1 occurs among the N entries of the permutation.
!
!    Set the input quantity BASE to 0, if P is a 0-based permutation,
!    or to 1 if P is a 1-based permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of entries.
!
!    Input, integer (kind=4) :: P(N), the array to check.
!
!    Input, integer (kind=4) :: BASE, the index base.
!
!    Output, integer (kind=4) :: IERROR, error flag.
!    0, the array represents a permutation.
!    nonzero, the array does not represent a permutation.  The smallest
!    missing value is equal to IERROR.
!
  implicit none

  integer (kind=4) :: n

  integer (kind=4) :: base
  integer (kind=4) :: find
  integer (kind=4) :: ierror
  integer (kind=4) :: p(n)
  integer (kind=4) :: seek

  ierror = 0

  do seek = base, base + n - 1

    ierror = 1

    do find = 1, n
      if ( p(find) == seek ) then
        ierror = 0
        exit
      end if
    end do

    if ( ierror /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PERM_CHECK - Fatal error!'
      write ( *, '(a)' ) '  The input array does not represent'
      write ( *, '(a)' ) '  a proper permutation.'
      stop
    end if

  end do

  return
end
subroutine perm_uniform ( n, base, seed, p )

!*****************************************************************************80
!
!! PERM_UNIFORM selects a random permutation of N objects.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of objects to be permuted.
!
!    Input, integer (kind=4) :: BASE, is 0 for a 0-based permutation and 1 for
!    a 1-based permutation.
!
!    Input/output, integer (kind=4) :: SEED, a seed for the random
!    number generator.
!
!    Output, integer (kind=4) :: P(N), the permutation.  P(I) is the "new"
!    location of the object originally at I.
!
  implicit none

  integer (kind=4) :: n

  integer (kind=4) :: base
  integer (kind=4) :: i
  integer (kind=4) :: i4_uniform
  integer (kind=4) :: j
  integer (kind=4) :: k
  integer (kind=4) :: p(n)
  integer (kind=4) :: seed

  do i = 1, n
    p(i) = ( i - 1 ) + base
  end do

  do i = 1, n
    j = i4_uniform ( i, n, seed )
    k    = p(i)
    p(i) = p(j)
    p(j) = k
  end do

  return
end
function r4_abs ( x )

!*****************************************************************************80
!
!! R4_ABS returns the absolute value of an R4.
!
!  Discussion:
!
!    An R4 is a real (kind=4) :: value.
!
!    FORTRAN90 supplies the ABS function, which should be used instead
!    of this function!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number whose absolute value is desired.
!
!    Output, real (kind=4) :: R4_ABS, the absolute value of X.
!
  implicit none

  real (kind=4) :: r4_abs
  real (kind=4) :: x

  if ( 0.0E+00 <= x ) then
    r4_abs = + x
  else
    r4_abs = - x
  end if

  return
end
function r4_add ( x, y )

!*****************************************************************************80
!
!! R4_ADD returns the sum of two R4's.
!
!  Discussion:
!
!    An R4 is a real (kind=4) :: value.
!
!    FORTRAN90 supplies the + operator, which should generally be used instead
!    of this function!
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
!    Input, real (kind=4) :: X, Y, the numbers to be added.
!
!    Output, real (kind=4) :: R4_ADD, the sum.
!
  implicit none

  real (kind=4) :: r4_add
  real (kind=4) :: x
  real (kind=4) :: y

  r4_add = x + y

  return
end
function r4_atan ( y, x )

!*****************************************************************************80
!
!! R4_ATAN computes the inverse tangent of the ratio Y / X.
!
!  Discussion:
!
!    R4_ATAN returns an angle whose tangent is ( Y / X ), a job which
!    the built in functions ATAN and ATAN2 already do.
!
!    However:
!
!    * R4_ATAN always returns a positive angle, between 0 and 2 PI,
!      while ATAN and ATAN2 return angles in the interval [-PI/2,+PI/2]
!      and [-PI,+PI] respectively;
!
!    * R4_ATAN accounts for the signs of X and Y, (as does ATAN2).  The ATAN
!     function by contrast always returns an angle in the first or fourth
!     quadrants.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: Y, X, two quantities which represent the
!    tangent of an angle.  If Y is not zero, then the tangent is (Y/X).
!
!    Output, real (kind=4) :: R4_ATAN, an angle between 0 and 2 * PI, whose
!    tangent is (Y/X), and which lies in the appropriate quadrant so that
!    the signs of its cosine and sine match those of X and Y.
!
  implicit none

  real (kind=4) :: abs_x
  real (kind=4) :: abs_y
  real ( kind = 4 ), parameter :: pi = 3.141592653589793E+00
  real (kind=4) :: r4_atan
  real (kind=4) :: theta
  real (kind=4) :: theta_0
  real (kind=4) :: x
  real (kind=4) :: y
!
!  Special cases:
!
  if ( x == 0.0E+00 ) then

    if ( 0.0E+00 < y ) then
      theta = pi / 2.0E+00
    else if ( y < 0.0E+00 ) then
      theta = 3.0E+00 * pi / 2.0E+00
    else if ( y == 0.0E+00 ) then
      theta = 0.0E+00
    end if

  else if ( y == 0.0E+00 ) then

    if ( 0.0E+00 < x ) then
      theta = 0.0E+00
    else if ( x < 0.0E+00 ) then
      theta = pi
    end if
!
!  We assume that ATAN2 is correct when both arguments are positive.
!
  else

    abs_y = abs ( y )
    abs_x = abs ( x )

    theta_0 = atan2 ( abs_y, abs_x )

    if ( 0.0E+00 < x .and. 0.0E+00 < y ) then
      theta = theta_0
    else if ( x < 0.0E+00 .and. 0.0E+00 < y ) then
      theta = pi - theta_0
    else if ( x < 0.0E+00 .and. y < 0.0E+00 ) then
      theta = pi + theta_0
    else if ( 0.0E+00 < x .and. y < 0.0E+00 ) then
      theta = 2.0E+00 * pi - theta_0
    end if

  end if

  r4_atan = theta

  return
end
function r4_cas ( x )

!*****************************************************************************80
!
!! R4_CAS returns the "casine" of an R4.
!
!  Discussion:
!
!    The "casine", used in the discrete Hartley transform, is abbreviated
!    CAS(X), and defined by:
!
!      CAS(X) = cos ( X ) + sin( X )
!             = sqrt ( 2 ) * sin ( X + pi/4 )
!             = sqrt ( 2 ) * cos ( X - pi/4 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ralph Hartley,
!    A More Symmetrical Fourier Analysis Applied to Transmission Problems,
!    Proceedings of the Institute of Radio Engineers,
!    Volume 30, pages 144-150, 1942.
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number whose casine is desired.
!
!    Output, real (kind=4) :: R4_CAS, the casine of X, which will be between
!    plus or minus the square root of 2.
!
  implicit none

  real (kind=4) :: r4_cas
  real (kind=4) :: x

  r4_cas = cos ( x ) + sin ( x )

  return
end
function r4_ceiling ( r )

!*****************************************************************************80
!
!! R4_CEILING rounds an R4 "up" (towards +oo) to the next I4.
!
!  Example:
!
!    R     Value
!
!   -1.1  -1
!   -1.0  -1
!   -0.9   0
!    0.0   0
!    5.0   5
!    5.1   6
!    5.9   6
!    6.0   6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: R, the value to be rounded up.
!
!    Output, integer (kind=4) :: R4_CEILING, the rounded value.
!
  implicit none

  real (kind=4) :: r
  integer (kind=4) :: r4_ceiling
  integer (kind=4) :: value

  value = int ( r )
  if ( real ( value, kind = 4 ) < r ) then
    value = value + 1
  end if

  r4_ceiling = value

  return
end
function r4_choose ( n, k )

!*****************************************************************************80
!
!! R4_CHOOSE computes the binomial coefficient C(N,K) as an R4.
!
!  Discussion:
!
!    The value is calculated in such a way as to avoid overflow and
!    roundoff.  The calculation is done in R4 arithmetic.
!
!    The formula used is:
!
!      C(N,K) = N! / ( K! * (N-K)! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    ML Wolfson, HV Wright,
!    Algorithm 160:
!    Combinatorial of M Things Taken N at a Time,
!    Communications of the ACM,
!    Volume 6, Number 4, April 1963, page 161.
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, K, are the values of N and K.
!
!    Output, real (kind=4) :: R4_CHOOSE, the number of combinations of N
!    things taken K at a time.
!
  implicit none

  integer (kind=4) :: i
  integer (kind=4) :: k
  integer (kind=4) :: mn
  integer (kind=4) :: mx
  integer (kind=4) :: n
  real (kind=4) :: r4_choose
  real (kind=4) :: value

  mn = min ( k, n - k )

  if ( mn < 0 ) then

    value = 0.0E+00

  else if ( mn == 0 ) then

    value = 1.0E+00

  else

    mx = max ( k, n - k )
    value = real ( mx + 1, kind = 4 )

    do i = 2, mn
      value = ( value * real ( mx + i, kind = 4 ) ) / real ( i, kind = 4 )
    end do

  end if

  r4_choose = value

  return
end
function r4_chop ( place, x )

!*****************************************************************************80
!
!! R4_CHOP chops an R4 to a given number of binary places.
!
!  Example:
!
!    3.875 = 2 + 1 + 1/2 + 1/4 + 1/8.
!
!    The following values would be returned for the 'chopped' value of
!    3.875:
!
!    PLACE  Value
!
!       1      2
!       2      3     = 2 + 1
!       3      3.5   = 2 + 1 + 1/2
!       4      3.75  = 2 + 1 + 1/2 + 1/4
!       5+     3.875 = 2 + 1 + 1/2 + 1/4 + 1/8
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: PLACE, the number of binary places to preserve.
!    PLACE = 0 means return the integer part of X.
!    PLACE = 1 means return the value of X, correct to 1/2.
!    PLACE = 2 means return the value of X, correct to 1/4.
!    PLACE = -1 means return the value of X, correct to 2.
!
!    Input, real (kind=4) :: X, the number to be chopped.
!
!    Output, real (kind=4) :: R4_CHOP, the chopped number.
!
  implicit none

  real (kind=4) :: fac
  integer (kind=4) :: place
  real (kind=4) :: r4_chop
  real (kind=4) :: r4_log_2
  real (kind=4) :: r4_sign
  real (kind=4) :: s
  integer (kind=4) :: temp
  real (kind=4) :: x

  s = r4_sign ( x )
  temp = int ( r4_log_2 ( abs ( x ) ) )
  fac = 2.0E+00**( temp - place + 1 )
  r4_chop = s * real ( int ( abs ( x ) / fac ), kind = 4 ) * fac

  return
end
function r4_csqrt ( x )

!*****************************************************************************80
!
!! R4_CSQRT returns the complex square root of an R4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number whose square root is desired.
!
!    Output, complex ( kind = 4 ) R4_CSQRT, the square root of X:
!
  implicit none

  real (kind=4) :: argument
  real (kind=4) :: magnitude
  real ( kind = 4 ), parameter :: pi = 3.141592653589793E+00
  complex ( kind = 4 ) r4_csqrt
  real (kind=4) :: x

  if ( 0.0E+00 < x ) then
    magnitude = x
    argument = 0.0E+00
  else if ( 0.0E+00 == x ) then
    magnitude = 0.0E+00
    argument = 0.0E+00
  else if ( x < 0.0E+00 ) then
    magnitude = -x
    argument = pi
  end if

  magnitude = sqrt ( magnitude )
  argument = argument / 2.0E+00

  r4_csqrt = magnitude * cmplx ( cos ( argument ), sin ( argument ), kind = 4 )

  return
end
function r4_cube_root ( x )

!*****************************************************************************80
!
!! R4_CUBE_ROOT returns the cube root of an R4.
!
!  Discussion:
!
!    This routine is designed to avoid the possible problems that can occur
!    when formulas like 0.0**(1/3) or (-1.0)**(1/3) are to be evaluated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number whose cube root is desired.
!
!    Output, real (kind=4) :: R4_CUBE_ROOT, the cube root of X.
!
  implicit none

  real (kind=4) :: r4_cube_root
  real (kind=4) :: value
  real (kind=4) :: x

  if ( 0.0E+00 < x ) then
    value = x**(1.0E+00/3.0E+00)
  else if ( x == 0.0E+00 ) then
    value = 0.0E+00
  else
    value = -( abs ( x ) )**(1.0E+00/3.0E+00)
  end if

  r4_cube_root = value

  return
end
function r4_diff ( x, y, n )

!*****************************************************************************80
!
!! R4_DIFF computes the difference of two R4's to a specified accuracy.
!
!  Discussion:
!
!    The user controls how many binary digits of accuracy
!    are to be used.
!
!    N determines the accuracy of the value of the result.  If N = 10,
!    for example, only 11 binary places will be used in the arithmetic.
!    In general, only N+1 binary places will be used.
!
!    N may be zero.  However, a negative value of N should
!    not be used, since this will cause both X and Y to look like 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, Y, the two values whose difference is desired.
!
!    Input, integer (kind=4) :: N, the number of binary digits to use.
!
!    Output, real (kind=4) :: R4_DIFF, the value of X-Y.
!
  implicit none

  real (kind=4) :: cx
  real (kind=4) :: cy
  integer (kind=4) :: n
  real (kind=4) :: pow2
  real (kind=4) :: r4_diff
  real (kind=4) :: size
  real (kind=4) :: x
  real (kind=4) :: y

  if ( x == y ) then
    r4_diff = 0.0E+00
    return
  end if

  pow2 = 2.0E+00**n
!
!  Compute the magnitude of X and Y, and take the larger of the
!  two.  At least one of the two values is not zero!
!
  size = max ( abs ( x ), abs ( y ) )
!
!  Make normalized copies of X and Y.  One of the two values will
!  actually be equal to 1.
!
  cx = x / size
  cy = y / size
!
!  Here's where rounding comes in.  We know that the larger of the
!  the two values equals 1.  We multiply both values by 2**N,
!  where N+1 is the number of binary digits of accuracy we want
!  to use, truncate the values, and divide back by 2**N.
!
  cx = real ( int ( cx * pow2 + sign ( 0.5E+00, cx ) ), kind = 4 ) / pow2
  cy = real ( int ( cy * pow2 + sign ( 0.5E+00, cy ) ), kind = 4 ) / pow2
!
!  Take the difference now.
!
  r4_diff = cx - cy
!
!  Undo the scaling.
!
  r4_diff = r4_diff * size

  return
end
subroutine r4_digit ( x, idigit, digit )

!*****************************************************************************80
!
!! R4_DIGIT returns a particular decimal digit of an R4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number whose NDIG-th decimal digit
!    is desired.  If X is zero, all digits will be returned as 0.
!
!    Input, integer (kind=4) :: IDIGIT, the position of the desired decimal
!    digit.  A value of 1 means the leading digit, a value of 2 the second digit
!    and so on.
!
!    Output, integer (kind=4) :: DIGIT, the value of the IDIGIT-th decimal
!    digit of X.
!
  implicit none

  integer (kind=4) :: digit
  integer (kind=4) :: i
  integer (kind=4) :: idigit
  integer (kind=4) :: ival
  real (kind=4) :: x
  real (kind=4) :: xcopy

  if ( x == 0.0E+00 ) then
    digit = 0
    return
  end if

  if ( idigit <= 0 ) then
    digit = 0
    return
  end if
!
!  Set XCOPY = X, and then force XCOPY to lie between 1 and 10.
!
  xcopy = abs ( x )

  do while ( xcopy < 1.0E+00 )
    xcopy = xcopy * 10.0E+00
  end do

  do while ( 10.0E+00 <= xcopy )
    xcopy = xcopy / 10.0E+00
  end do

  do i = 1, idigit
    ival = int ( xcopy )
    xcopy = ( xcopy - ival ) * 10.0E+00
  end do

  digit = ival

  return
end
function r4_epsilon ( )

!*****************************************************************************80
!
!! R4_EPSILON returns the R4 roundoff unit.
!
!  Discussion:
!
!    The roundoff unit is a number R which is a power of 2 with the
!    property that, to the precision of the computer's arithmetic,
!      1 < 1 + R
!    but
!      1 = ( 1 + R / 2 )
!
!    FORTRAN90 provides the superior library routine
!
!      EPSILON ( X )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real (kind=4) :: R4_EPSILON, the round-off unit.
!
  implicit none

  real (kind=4) :: r4_epsilon

  r4_epsilon = 1.19209290E-07

  return
end
function r4_epsilon_compute ( )

!*****************************************************************************80
!
!! R4_EPSILON_COMPUTE computes the R4 roundoff unit.
!
!  Discussion:
!
!    The roundoff unit is a number R which is a power of 2 with the
!    property that, to the precision of the computer's arithmetic,
!      1 < 1 + R
!    but
!      1 = ( 1 + R / 2 )
!
!    FORTRAN90 provides the superior library routine
!
!      EPSILON ( X )
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
!    Output, real (kind=4) :: R4_EPSILON_COMPUTE, the R4 round-off unit.
!
  implicit none

  real (kind=4) :: one
  real (kind=4) :: r4_add
  real (kind=4) :: r4_epsilon_compute
  real (kind=4) :: temp
  real (kind=4) :: test
  real (kind=4) :: value

  one = real ( 1, kind = 4 )

  value = one
  temp = value / 2.0E+00
  test = r4_add ( one, temp )

  do while ( one < test )
    value = temp
    temp = value / 2.0E+00
    test = r4_add ( one, temp )
  end do

  r4_epsilon_compute = value

  return
end
function r4_exp ( x )

!*****************************************************************************80
!
!! R4_EXP computes the exponential function, avoiding overflow and underflow.
!
!  Discussion:
!
!    My experience with the G95 compiler has included many unpleasant
!    floating point exceptions when very small arguments are given to
!    the exponential function.
!
!    This routine is designed to avoid such problems.
!
!    Ideally, the rule would be:
!
!                    X <= log ( TINY ) => R4_EXP ( X ) = 0
!    log ( HUGE ) <= X                 => R4_EXP ( X ) = HUGE
!
!    However, the G95 math library seems to produce infinity for
!    EXP ( LOG ( HUGE ( X ) ), rather than HUGE ( X ), so we've
!    included a fudge factor.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the argument of the exponential function.
!
!    Output, real (kind=4) :: R4_EXP, the value of exp ( X ).
!
  implicit none

  real ( kind = 4 ), parameter :: log_max = 88.71397E+00
  real ( kind = 4 ), parameter :: log_min = -87.34528E+00
  real (kind=4) :: r4_exp
  real (kind=4) :: x

  if ( x <= log_min ) then
    r4_exp = 0.0E+00
  else if ( x < log_max ) then
    r4_exp = exp ( x )
  else
    r4_exp = huge ( x )
  end if

  return
end
function r4_factorial ( n )

!*****************************************************************************80
!
!! R4_FACTORIAL computes the factorial of N.
!
!  Discussion:
!
!    factorial ( N ) = product ( 1 <= I <= N ) I
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the argument of the factorial function.
!    If N is less than 1, the function value is returned as 1.
!
!    Output, real (kind=4) :: R4_FACTORIAL, the factorial of N.
!
  implicit none

  real (kind=4) :: r4_factorial
  integer (kind=4) :: i
  integer (kind=4) :: n

  r4_factorial = 1.0E+00

  do i = 1, n
    r4_factorial = r4_factorial * real ( i, kind = 4 )
  end do

  return
end
function r4_factorial2 ( n )

!*****************************************************************************80
!
!! R4_FACTORIAL2 computes the double factorial function.
!
!  Discussion:
!
!    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
!                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
!
!  Example:
!
!     N    N!!
!
!     0     1
!     1     1
!     2     2
!     3     3
!     4     8
!     5    15
!     6    48
!     7   105
!     8   384
!     9   945
!    10  3840
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the argument of the double factorial
!    function.  If N is less than 1, the value is returned as 1.0.
!
!    Output, real (kind=4) :: R4_FACTORIAL2, the value.
!
  implicit none

  integer (kind=4) :: n
  real (kind=4) :: r4_factorial2
  real (kind=4) :: r4_n

  if ( n < 1 ) then
    r4_factorial2 = 1.0E+00
    return
  end if

  r4_n = real ( n, kind = 4 )
  r4_factorial2 = 1.0E+00

  do while ( 1.0E+00 < r4_n )
    r4_factorial2 = r4_factorial2 * r4_n
    r4_n = r4_n - 2.0E+00
  end do

  return
end
function r4_floor ( r )

!*****************************************************************************80
!
!! R4_FLOOR rounds an R4 "down" (towards -oo) to the next integer.
!
!  Example:
!
!    R     Value
!
!   -1.1  -2
!   -1.0  -1
!   -0.9  -1
!    0.0   0
!    5.0   5
!    5.1   5
!    5.9   5
!    6.0   6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: R, the value to be rounded down.
!
!    Output, integer (kind=4) :: R4_FLOOR, the rounded value.
!
  implicit none

  real (kind=4) :: r
  integer (kind=4) :: r4_floor
  integer (kind=4) :: value

  value = int ( r )
  if ( r < real ( value, kind = 4 ) ) then
    value = value - 1
  end if

  r4_floor = value

  return
end
function r4_fraction ( i, j )

!*****************************************************************************80
!
!! R4_FRACTION uses real arithmetic on an integer ratio.
!
!  Discussion:
!
!    Given integer variables I and J, both FORTRAN and C will evaluate
!    an expression such as "I/J" using what is called "integer division",
!    with the result being an integer.  It is often convenient to express
!    the parts of a fraction as integers but expect the result to be computed
!    using real arithmetic.  This function carries out that operation.
!
!  Example:
!
!       I     J   I/J  R4_FRACTION
!
!       1     2     0  0.5
!       7     4     1  1.75
!       8     4     2  2.00
!       9     4     2  2.25
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: I, J, the arguments.
!
!    Output, real (kind=4) :: R4_FRACTION, the value of the ratio.
!
  implicit none

  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: r4_fraction

  r4_fraction = real ( i, kind = 4 ) / real ( j, kind = 4 )

  return
end
function r4_fractional ( x )

!*****************************************************************************80
!
!! R4_FRACTIONAL returns the fraction part of an R4.
!
!  Discussion:
!
!    If we regard a real number as
!
!      R = SIGN * ( WHOLE + FRACTION )
!
!    where
!
!      SIGN is +1 or -1,
!      WHOLE is a nonnegative integer
!      FRACTION is a nonnegative real number strictly less than 1,
!
!    then this routine returns the value of FRACTION.
!
!  Example:
!
!     R      FRACTION
!
!    0.00      0.00
!    1.01      0.01
!    2.02      0.02
!   19.73      0.73
!   -4.34      0.34
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the argument.
!
!    Output, real (kind=4) :: R4_FRACTIONAL, the fraction part of X.
!
  implicit none

  real (kind=4) :: r4_fractional
  real (kind=4) :: x

  r4_fractional = abs ( x ) - real ( int ( abs ( x ) ), kind = 4 )

  return
end
function r4_huge ( )

!*****************************************************************************80
!
!! R4_HUGE returns the largest legal R4.
!
!  Discussion:
!
!    FORTRAN90 provides a built-in routine HUGE ( X ) that
!    is more suitable for this purpose.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real (kind=4) :: R4_HUGE, a "huge" value.
!
  implicit none

  real (kind=4) :: r4_huge

  r4_huge = 1.0E+30

  return
end
function r4_in_01 ( a )

!*****************************************************************************80
!
!! R4_IN_01 is TRUE if an R4 is in the range [0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A, the value.
!
!    Output, logical R4_IN_01, is TRUE if 0 <= A <= 1.
!
  implicit none

  real (kind=4) :: a
  logical              r4_in_01
  logical              value

  if ( a < 0.0E+00 .or. 1.0E+00 < a ) then
    value = .false.
  else
    value = .true.
  end if

  r4_in_01 = value

  return
end
function r4_is_int ( r )

!*****************************************************************************80
!
!! R4_IS_INT determines if an R4 represents an integer value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: R, the number to be checked.
!
!    Output, logical R4_IS_INT, is TRUE if R is an integer value.
!
  implicit none

  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  real (kind=4) :: r
  logical              r4_is_int
  logical              value

  if ( real ( i4_huge, kind = 4 ) < r ) then
    value = .false.
  else if ( r < - real ( i4_huge, kind = 4 ) ) then
    value = .false.
  else if ( r == real ( int ( r ), kind = 4 ) ) then
    value = .true.
  else
    value = .false.
  end if

  r4_is_int = value

  return
end
function r4_log_2 ( x )

!*****************************************************************************80
!
!! R4_LOG_2 returns the logarithm base 2 of an R4.
!
!  Discussion:
!
!    value = Log ( |X| ) / Log ( 2.0 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number whose base 2 logarithm is desired.
!    X should not be 0.
!
!    Output, real (kind=4) :: R4_LOG_2, the logarithm base 2 of the absolute
!    value of X.  It should be true that |X| = 2**R4_LOG_2.
!
  implicit none

  real (kind=4) :: r4_log_2
  real (kind=4) :: x

  if ( x == 0.0E+00 ) then
    r4_log_2 = - huge ( x )
  else
    r4_log_2 = log ( abs ( x ) ) / log ( 2.0E+00 )
  end if

  return
end
function r4_log_10 ( x )

!*****************************************************************************80
!
!! R4_LOG_10 returns the logarithm base 10 of an R4.
!
!  Discussion:
!
!    value = Log10 ( |X| )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number whose base 2 logarithm is desired.
!    X should not be 0.
!
!    Output, real (kind=4) :: R4_LOG_10, the logarithm base 10 of the absolute
!    value of X.  It should be true that |X| = 10**R4_LOG_10.
!
  implicit none

  real (kind=4) :: r4_log_10
  real (kind=4) :: x

  if ( x == 0.0E+00 ) then
    r4_log_10 = - huge ( x )
  else
    r4_log_10 = log10 ( abs ( x ) )
  end if

  return
end
function r4_log_b ( x, b )

!*****************************************************************************80
!
!! R4_LOG_B returns the logarithm base B of an R4.
!
!  Discussion:
!
!    value = log ( |X| ) / log ( |B| )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number whose base B logarithm is desired.
!    X should not be 0.
!
!    Input, real (kind=4) :: B, the base, which should not be 0, 1 or -1.
!
!    Output, real (kind=4) :: R4_LOG_B, the logarithm base B of the absolute
!    value of X.  It should be true that |X| = |B|**R4_LOG_B.
!
  implicit none

  real (kind=4) :: b
  real (kind=4) :: r4_log_b
  real (kind=4) :: x

  if ( b == 0.0E+00 .or. b == 1.0E+00 .or. b == - 1.0E+00 ) then
    r4_log_b = - huge ( x )
  else if ( abs ( x ) == 0.0E+00 ) then
    r4_log_b = - huge ( x )
  else
    r4_log_b = log ( abs ( x ) ) / log ( abs ( b ) )
  end if

  return
end
subroutine r4_mant ( x, s, r, l )

!*****************************************************************************80
!
!! R4_MANT computes the "mantissa" or "fraction part" of an R4.
!
!  Discussion:
!
!    X = S * R * 2**L
!
!    S is +1 or -1,
!    R is an real value between 1.0 and 2.0,
!    L is an integer.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number to be decomposed.
!
!    Output, integer (kind=4) :: S, the "sign" of the number.
!    S will be -1 if X is less than 0, and +1 if X is greater
!    than or equal to zero.
!
!    Output, real (kind=4) :: R, the mantissa of X.  R will be greater
!    than or equal to 1, and strictly less than 2.  The one
!    exception occurs if X is zero, in which case R will also
!    be zero.
!
!    Output, integer (kind=4) :: L, the integer part of the logarithm
!    (base 2) of X.
!
  implicit none

  integer (kind=4) :: l
  real (kind=4) :: r
  integer (kind=4) :: s
  real (kind=4) :: x
!
!  Determine the sign.
!
  if ( x < 0.0E+00 ) then
    s = -1
  else
    s = + 1
  end if
!
!  Set R to the absolute value of X, and L to zero.
!  Then force R to lie between 1 and 2.
!
  if ( x < 0.0E+00 ) then
    r = - x
  else
    r = + x
  end if

  l = 0
!
!  Time to bail out if X is zero.
!
  if ( x == 0.0E+00 ) then
    return
  end if

  do while ( 2.0E+00 <= r )
    r = r / 2.0E+00
    l = l + 1
  end do

  do while ( r < 1.0E+00 )
    r = r * 2.0E+00
    l = l - 1
  end do

  return
end
function r4_mod ( x, y )

!*****************************************************************************80
!
!! R4_MOD returns the remainder of R4 division.
!
!  Discussion:
!
!    If
!      REM = R4_MOD ( X, Y )
!      RMULT = ( X - REM ) / Y
!    then
!      X = Y * RMULT + REM
!    where REM has the same sign as X, and abs ( REM ) < Y.
!
!  Example:
!
!        X         Y     R4_MOD  R4_MOD Factorization
!
!      107        50       7      107 =  2 *  50 + 7
!      107       -50       7      107 = -2 * -50 + 7
!     -107        50      -7     -107 = -2 *  50 - 7
!     -107       -50      -7     -107 =  2 * -50 - 7
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number to be divided.
!
!    Input, real (kind=4) :: Y, the number that divides X.
!
!    Output, real (kind=4) :: R4_MOD, the remainder when X is divided by Y.
!
  implicit none

  real (kind=4) :: r4_mod
  real (kind=4) :: x
  real (kind=4) :: y

  if ( y == 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4_MOD - Fatal error!'
    write ( *, '(a,g14.6)' ) '  R4_MOD ( X, Y ) called with Y = ', y
    stop
  end if

  r4_mod = x - real ( int ( x / y ), kind = 4 ) * y

  if ( x < 0.0E+00 .and. 0.0E+00 < r4_mod ) then
    r4_mod = r4_mod - abs ( y )
  else if ( 0.0E+00 < x .and. r4_mod < 0.0E+00 ) then
    r4_mod = r4_mod + abs ( y )
  end if

  return
end
function r4_modp ( x, y )

!*****************************************************************************80
!
!! R4_MODP returns the nonnegative remainder of R4 division.
!
!  Discussion:
!
!    If
!      REM = R4_MODP ( X, Y )
!      RMULT = ( X - REM ) / Y
!    then
!      X = Y * RMULT + REM
!    where REM is always nonnegative.
!
!    The MOD function computes a result with the same sign as the
!    quantity being divided.  Thus, suppose you had an angle A,
!    and you wanted to ensure that it was between 0 and 360.
!    Then mod(A,360.0) would do, if A was positive, but if A
!    was negative, your result would be between -360 and 0.
!
!    On the other hand, R4_MODP(A,360.0) is between 0 and 360, always.
!
!  Example:
!
!        X         Y     MOD R4_MODP  R4_MODP Factorization
!
!      107        50       7       7    107 =  2 *  50 + 7
!      107       -50       7       7    107 = -2 * -50 + 7
!     -107        50      -7      43   -107 = -3 *  50 + 43
!     -107       -50      -7      43   -107 =  3 * -50 + 43
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number to be divided.
!
!    Input, real (kind=4) :: Y, the number that divides X.
!
!    Output, real (kind=4) :: R4_MODP, the nonnegative remainder
!    when X is divided by Y.
!
  implicit none

  real (kind=4) :: r4_modp
  real (kind=4) :: x
  real (kind=4) :: y

  if ( y == 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4_MODP - Fatal error!'
    write ( *, '(a,g14.6)' ) '  R4_MODP ( X, Y ) called with Y = ', y
    stop
  end if

  r4_modp = mod ( x, y )

  if ( r4_modp < 0.0E+00 ) then
    r4_modp = r4_modp + abs ( y )
  end if

  return
end
function r4_mop ( i )

!*****************************************************************************80
!
!! R4_MOP returns the I-th power of -1 as an R4.
!
!  Discussion:
!
!    An R4 is a real (kind=4) :: value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: I, the power of -1.
!
!    Output, real (kind=4) :: R4_MOP, the I-th power of -1.
!
  implicit none

  integer (kind=4) :: i
  real (kind=4) :: r4_mop

  if ( mod ( i, 2 ) == 0 ) then
    r4_mop = + 1.0E+00
  else
    r4_mop = - 1.0E+00
  end if

  return
end
function r4_nint ( x )

!*****************************************************************************80
!
!! R4_NINT returns the nearest integer to an R4.
!
!  Example:
!
!        X        R4_NINT
!
!      1.3         1
!      1.4         1
!      1.5         1 or 2
!      1.6         2
!      0.0         0
!     -0.7        -1
!     -1.1        -1
!     -1.6        -2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the value.
!
!    Output, integer (kind=4) :: R4_NINT, the nearest integer to X.
!
  implicit none

  integer (kind=4) :: r4_nint
  integer (kind=4) :: s
  real (kind=4) :: x

  if ( x < 0.0E+00 ) then
    s = - 1
  else
    s = + 1
  end if

  r4_nint = s * int ( abs ( x ) + 0.5E+00 )

  return
end
function r4_normal ( a, b, seed )

!*****************************************************************************80
!
!! R4_NORMAL returns a scaled pseudonormal R4.
!
!  Discussion:
!
!    The normal probability distribution function (PDF) is sampled,
!    with mean A and standard deviation B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A, the mean of the PDF.
!
!    Input, real (kind=4) :: B, the standard deviation of the PDF.
!
!    Input/output, integer (kind=4) :: SEED, a seed for the random
!    number generator.
!
!    Output, real (kind=4) :: R4_NORMAL, a sample of the normal PDF.
!
  implicit none

  real (kind=4) :: a
  real (kind=4) :: b
  real ( kind = 4 ), parameter :: pi = 3.141592653589793E+00
  real (kind=4) :: r1
  real (kind=4) :: r2
  real (kind=4) :: r4_normal
  real (kind=4) :: r4_uniform_01
  integer (kind=4) :: seed
  integer ( kind = 4 ), save :: seed2 = 0
  integer ( kind = 4 ), save :: used = 0
  real (kind=4) :: x
  real ( kind = 4 ), save :: y = 0.0E+00
!
!  On odd numbered calls, generate two uniforms, create two normals,
!  return the first normal and its corresponding seed.
!
  if ( mod ( used, 2 ) == 0 ) then

    r1 = r4_uniform_01 ( seed )

    if ( r1 == 0.0E+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4_NORMAL - Fatal error!'
      write ( *, '(a)' ) '  R4_UNIFORM_01 returned a value of 0.'
      stop
    end if

    seed2 = seed
    r2 = r4_uniform_01 ( seed2 )

    x = sqrt ( - 2.0E+00 * log ( r1 ) ) * cos ( 2.0E+00 * pi * r2 )
    y = sqrt ( - 2.0E+00 * log ( r1 ) ) * sin ( 2.0E+00 * pi * r2 )
!
!  On odd calls, return the second normal and its corresponding seed.
!
  else

    seed = seed2
    x = y

  end if

  used = used + 1

  r4_normal = a + b * x

  return
end
function r4_normal_01 ( seed )

!*****************************************************************************80
!
!! R4_NORMAL_01 returns a unit pseudonormal R4.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    Because this routine uses the Box Muller method, it requires pairs
!    of uniform random values to generate a pair of normal random values.
!    This means that on every other call, essentially, the input value of
!    SEED is ignored, since the code saves the second normal random value.
!
!    If you didn't know this, you might be confused since, usually, the
!    output of a random number generator can be completely controlled by
!    the input value of the SEED.  If I were more careful, I could rewrite
!    this routine so that it would distinguish between cases where the input
!    value of SEED is the output value from the previous call (all is well)
!    and those cases where it is not (the user has decided to do something
!    new.  Restart the uniform random number sequence.)  But I'll leave
!    that for later.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer (kind=4) :: SEED, a seed for the random
!    number generator.
!
!    Output, real (kind=4) :: R4_NORMAL_01, a sample of the standard
!    normal PDF.
!
  implicit none

  real ( kind = 4 ), parameter :: pi = 3.141592653589793E+00
  real (kind=4) :: r1
  real (kind=4) :: r2
  real (kind=4) :: r4_normal_01
  real (kind=4) :: r4_uniform_01
  integer (kind=4) :: seed
  integer ( kind = 4 ), save :: seed2 = 0
  integer ( kind = 4 ), save :: used = 0
  real (kind=4) :: x
  real ( kind = 4 ), save :: y = 0.0E+00
!
!  On odd numbered calls, generate two uniforms, create two normals,
!  return the first normal and its corresponding seed.
!
  if ( mod ( used, 2 ) == 0 ) then

    r1 = r4_uniform_01 ( seed )

    if ( r1 == 0.0E+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4_NORMAL_01 - Fatal error!'
      write ( *, '(a)' ) '  R4_UNIFORM_01 returned a value of 0.'
      stop
    end if

    seed2 = seed
    r2 = r4_uniform_01 ( seed2 )

    x = sqrt ( - 2.0E+00 * log ( r1 ) ) * cos ( 2.0E+00 * pi * r2 )
    y = sqrt ( - 2.0E+00 * log ( r1 ) ) * sin ( 2.0E+00 * pi * r2 )
!
!  On odd calls, return the second normal and its corresponding seed.
!
  else

    seed = seed2
    x = y

  end if

  used = used + 1

  r4_normal_01 = x

  return
end
function r4_pi ( )

!*****************************************************************************80
!
!! R4_PI returns the value of pi as an R4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real (kind=4) :: R4_PI, the value of pi.
!
  implicit none

  real (kind=4) :: r4_pi

  r4_pi = 3.1415926E+00

  return
end
function r4_power ( r, p )

!*****************************************************************************80
!
!! R4_POWER computes the P-th power of an R4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: R, the base.
!
!    Input, integer (kind=4) :: P, the power, which may be negative.
!
!    Output, real (kind=4) :: R4_POWER, the value of the P-th power of R.
!
  implicit none

  integer (kind=4) :: p
  real (kind=4) :: r
  real (kind=4) :: r4_power
  real (kind=4) :: value
!
!  Special case.  R^0 = 1.
!
  if ( p == 0 ) then

    value = 1.0E+00
!
!  Special case.  Positive powers of 0 are 0.
!  For negative powers of 0, we go ahead and compute R^P,
!  relying on the software to complain.
!
  else if ( r == 0.0E+00 ) then

    if ( 0 < p ) then
      value = 0.0E+00
    else
      value = r**p
    end if

  else if ( 1 <= p ) then
    value = r**p
  else
    value = 1.0E+00 / r**(-p)
  end if

  r4_power = value

  return
end
subroutine r4_power_fast ( r, p, rp, mults )

!*****************************************************************************80
!
!! R4_POWER_FAST computes an integer power of an R4.
!
!  Discussion:
!
!    Obviously, R**P can be computed using P-1 multiplications.
!
!    However, R**P can also be computed using at most 2*LOG2(P) multiplications.
!    To do the calculation this way, let N = LOG2(P).
!    Compute A, A**2, A**4, ..., A**N by N-1 successive squarings.
!    Start the value of R**P at A, and each time that there is a 1 in
!    the binary expansion of P, multiply by the current result of the squarings.
!
!    This algorithm is not optimal.  For small exponents, and for special
!    cases, the result can be computed even more quickly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: R, the base.
!
!    Input, integer (kind=4) :: P, the power, which may be negative.
!
!    Output, real (kind=4) :: RP, the value of R**P.
!
!    Output, integer (kind=4) :: MULTS, the number of multiplications
!    and divisions.
!
  implicit none

  integer (kind=4) :: mults
  integer (kind=4) :: p
  integer (kind=4) :: p_mag
  integer (kind=4) :: p_sign
  real (kind=4) :: r
  real (kind=4) :: r2
  real (kind=4) :: rp

  mults = 0
!
!  Special bases.
!
  if ( r == 1.0E+00 ) then
    rp = 1.0E+00
    return
  end if

  if ( r == -1.0E+00 ) then

    if ( mod ( p, 2 ) == 1 ) then
      rp = -1.0E+00
    else
      rp = 1.0E+00
    end if

    return

  end if

  if ( r == 0.0E+00 ) then

    if ( p <= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4_POWER_FAST - Fatal error!'
      write ( *, '(a)' ) '  Base R is zero, and exponent is negative.'
      write ( *, '(a,i8)' ) '  Exponent P = ', p
      stop
    end if

    rp = 0.0E+00
    return

  end if
!
!  Special powers.
!
  if ( p == -1 ) then
    rp = 1.0E+00 / r
    mults = mults + 1
    return
  else if ( p == 0 ) then
    rp = 1.0E+00
    return
  else if ( p == 1 ) then
    rp = r
    return
  end if
!
!  Some work to do.
!
  p_mag = abs ( p )
  p_sign = sign ( 1, p )

  rp = 1.0E+00
  r2 = r

  do while ( 0 < p_mag )

    if ( mod ( p_mag, 2 ) == 1 ) then
      rp = rp * r2
      mults = mults + 1
    end if

    p_mag = p_mag / 2
    r2 = r2 * r2
    mults = mults + 1

  end do

  if ( p_sign == -1 ) then
    rp = 1.0E+00 / rp
    mults = mults + 1
  end if

  return
end
function r4_pythag ( a, b )

!*****************************************************************************80
!
!! R4_PYTHAG computes sqrt ( A * A + B * B ), avoiding overflow and underflow.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A, B, the values for which sqrt ( A * A + B * B )
!    is desired.
!
!    Output, real (kind=4) :: R4_PYTHAG, the value of sqrt ( A * A + B * B ).
!
  implicit none

  real (kind=4) :: a
  real (kind=4) :: a_abs
  real (kind=4) :: b
  real (kind=4) :: b_abs
  real (kind=4) :: r4_pythag

  a_abs = abs ( a )
  b_abs = abs ( b )

  if ( b_abs < a_abs ) then
    r4_pythag = a_abs * sqrt ( 1.0E+00 + ( b_abs / a_abs ) * ( b_abs / a_abs ) )
  else if ( b_abs == 0.0E+00 ) then
    r4_pythag = 0.0E+00
  else if ( a_abs <= b_abs ) then
    r4_pythag = b_abs * sqrt ( 1.0E+00 + ( a_abs / b_abs ) * ( a_abs / b_abs ) )
  end if

  return
end
subroutine r4_round2 ( nplace, x, xround )

!*****************************************************************************80
!
!! R4_ROUND2 rounds an R4 to a specified number of binary digits.
!
!  Discussion:
!
!    Assume that the input quantity X has the form
!
!      X = S * J * 2^L
!
!    where S is plus or minus 1, L is an integer, and J is a binary
!    mantissa which is either exactly zero, or greater than or equal
!    to 0.5 and strictly less than 1.0.
!
!    Then on return, XROUND will satisfy
!
!      XROUND = S * K * 2^L
!
!    where S and L are unchanged, and K is a binary mantissa which
!    agrees with J in the first NPLACE binary digits and is zero
!    thereafter.
!
!    If NPLACE is 0, XROUND will always be zero.
!
!    If NPLACE is 1, the mantissa of XROUND will be 0 or 0.5.
!
!    If NPLACE is 2, the mantissa of XROUND will be 0, 0.25, 0.50,
!    or 0.75.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: NPLACE, the number of binary digits to
!    preserve.  NPLACE should be 0 or positive.
!
!    Input, real (kind=4) :: X, the number to be decomposed.
!
!    Output, real (kind=4) :: XROUND, the rounded value of X.
!
  implicit none

  integer (kind=4) :: iplace
  integer (kind=4) :: l
  integer (kind=4) :: nplace
  integer (kind=4) :: s
  real (kind=4) :: x
  real (kind=4) :: xmant
  real (kind=4) :: xround
  real (kind=4) :: xtemp

  xround = 0.0E+00
!
!  1: Handle the special case of 0.
!
  if ( x == 0.0E+00 ) then
    return
  end if

  if ( nplace <= 0 ) then
    return
  end if
!
!  2: Determine the sign S.
!
  if ( 0.0E+00 < x ) then
    s = 1
    xtemp = x
  else
    s = -1
    xtemp = -x
  end if
!
!  3: Force XTEMP to lie between 1 and 2, and compute the
!  logarithm L.
!
  l = 0

  do while ( 2.0E+00 <= xtemp )
    xtemp = xtemp / 2.0E+00
    l = l + 1
  end do

  do while ( xtemp < 1.0E+00 )
    xtemp = xtemp * 2.0E+00
    l = l - 1
  end do
!
!  4: Strip out the digits of the mantissa as XMANT, and decrease L.
!
  xmant = 0.0E+00
  iplace = 0

  do

    xmant = 2.0E+00 * xmant

    if ( 1.0E+00 <= xtemp ) then
      xmant = xmant + 1.0E+00
      xtemp = xtemp - 1.0E+00
    end if

    iplace = iplace + 1

    if ( xtemp == 0.0E+00 .or. nplace <= iplace ) then
      xround = s * xmant * 2.0E+00**l
      exit
    end if

    l = l - 1
    xtemp = xtemp * 2.0E+00

  end do

  return
end
subroutine r4_roundb ( base, nplace, x, xround )

!*****************************************************************************80
!
!! R4_ROUNDB rounds an R4 to a given number of digits in a given base.
!
!  Discussion:
!
!    The code does not seem to do a good job of rounding when
!    the base is negative!
!
!    Assume that the input quantity X has the form
!
!      X = S * J * BASE^L
!
!    where S is plus or minus 1, L is an integer, and J is a
!    mantissa base BASE which is either exactly zero, or greater
!    than or equal to (1/BASE) and less than 1.0.
!
!    Then on return, XROUND will satisfy
!
!      XROUND = S * K * BASE^L
!
!    where S and L are unchanged, and K is a mantissa base BASE
!    which agrees with J in the first NPLACE digits and is zero
!    thereafter.
!
!    Note that because of rounding, for most bases, most numbers
!    with a fractional quantities cannot be stored exactly in the
!    computer, and hence will have trailing "bogus" digits.
!
!    If NPLACE is 0, XROUND will always be zero.
!
!    If NPLACE is 1, the mantissa of XROUND will be 0,
!    1/BASE, 2/BASE, ..., (BASE-1)/BASE.
!
!    If NPLACE is 2, the mantissa of XROUND will be 0,
!    BASE/BASE^2, (BASE+1)/BASE^2, ...,
!    BASE^2-2/BASE^2, BASE^2-1/BASE^2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: BASE, the base of the arithmetic.
!    BASE must not be zero.  Theoretically, BASE may be negative.
!
!    Input, integer (kind=4) :: NPLACE, the number of digits base BASE to
!    preserve.  NPLACE should be 0 or positive.
!
!    Input, real (kind=4) :: X, the number to be decomposed.
!
!    Output, real (kind=4) :: XROUND, the rounded value of X.
!
  implicit none

  integer (kind=4) :: base
  integer (kind=4) :: iplace
  integer (kind=4) :: is
  integer (kind=4) :: js
  integer (kind=4) :: l
  integer (kind=4) :: nplace
  real (kind=4) :: x
  real (kind=4) :: xmant
  real (kind=4) :: xround
  real (kind=4) :: xtemp

  xround = 0.0E+00
!
!  0: Error checks.
!
  if ( base == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4_ROUNDB - Fatal error!'
    write ( *, '(a)' ) '  The base BASE cannot be zero.'
    stop
  end if
!
!  1: Handle the special case of 0.
!
  if ( x == 0.0E+00 ) then
    return
  end if

  if ( nplace <= 0 ) then
    return
  end if
!
!  2: Determine the sign IS.
!
  if ( 0.0E+00 < x ) then
    is = 1
    xtemp = x
  else
    is = -1
    xtemp = -x
  end if
!
!  3: Force XTEMP to lie between 1 and ABS(BASE), and compute the
!  logarithm L.
!
  l = 0

  do while ( abs ( base ) <= abs ( xtemp ) )

    xtemp = xtemp / real ( base, kind = 4 )

    if ( xtemp < 0.0E+00 ) then
      is = -is
      xtemp = -xtemp
    end if

    l = l + 1

  end do

  do while ( abs ( xtemp ) < 1.0E+00 )

    xtemp = xtemp * base

    if ( xtemp < 0.0E+00 ) then
      is = -is
      xtemp = -xtemp
    end if

    l = l - 1

  end do
!
!  4: Now strip out the digits of the mantissa as XMANT, and
!  decrease L.
!
  xmant = 0.0E+00
  iplace = 0
  js = is

  do

    xmant = base * xmant

    if ( xmant < 0.0E+00 ) then
      js = -js
      xmant = -xmant
    end if

    if ( 1.0E+00 <= xtemp ) then
      xmant = xmant + int ( xtemp )
      xtemp = xtemp - int ( xtemp )
    end if

    iplace = iplace + 1

    if ( xtemp == 0.0E+00 .or. nplace <= iplace ) then
      xround = js * xmant * real ( base, kind = 4 )**l
      exit
    end if

    l = l - 1
    xtemp = xtemp * base

    if ( xtemp < 0.0E+00 ) then
      is = -is
      xtemp = -xtemp
    end if

  end do

  return
end
subroutine r4_roundx ( nplace, x, xround )

!*****************************************************************************80
!
!! R4_ROUNDX rounds an R4.
!
!  Discussion:
!
!    Assume that the input quantity X has the form
!
!      X = S * J * 10^L
!
!    where S is plus or minus 1, L is an integer, and J is a decimal
!    mantissa which is either exactly zero, or greater than or equal
!    to 0.1 and less than 1.0.
!
!    Then on return, XROUND will satisfy
!
!      XROUND = S * K * 10^L
!
!    where S and L are unchanged, and K is a decimal mantissa which
!    agrees with J in the first NPLACE decimal digits and is zero
!    thereafter.
!
!    Note that because of rounding, most decimal fraction quantities
!    cannot be stored exactly in the computer, and hence will have
!    trailing "bogus" digits.
!
!    If NPLACE is 0, XROUND will always be zero.
!
!    If NPLACE is 1, the mantissa of XROUND will be 0, 0.1,
!    0.2, ..., or 0.9.
!
!    If NPLACE is 2, the mantissa of XROUND will be 0, 0.01, 0.02,
!    0.03, ..., 0.98, 0.99.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: NPLACE, the number of decimal digits to
!    preserve.  NPLACE should be 0 or positive.
!
!    Input, real (kind=4) :: X, the number to be decomposed.
!
!    Output, real (kind=4) :: XROUND, the rounded value of X.
!
  implicit none

  integer (kind=4) :: iplace
  integer (kind=4) :: is
  integer (kind=4) :: l
  integer (kind=4) :: nplace
  real (kind=4) :: x
  real (kind=4) :: xmant
  real (kind=4) :: xround
  real (kind=4) :: xtemp

  xround = 0.0E+00
!
!  1: Handle the special case of 0.
!
  if ( x == 0.0E+00 ) then
    return
  end if

  if ( nplace <= 0 ) then
    return
  end if
!
!  2: Determine the sign IS.
!
  if ( 0.0E+00 < x ) then
    is = 1
    xtemp = x
  else
    is = -1
    xtemp = -x
  end if
!
!  3: Force XTEMP to lie between 1 and 10, and compute the
!  logarithm L.
!
  l = 0

  do while ( 10.0E+00 <= x )
    xtemp = xtemp / 10.0E+00
    l = l + 1
  end do

  do while ( xtemp < 1.0E+00 )
    xtemp = xtemp * 10.0E+00
    l = l - 1
  end do
!
!  4: Now strip out the digits of the mantissa as XMANT, and
!  decrease L.
!
  xmant = 0.0E+00
  iplace = 0

  do

    xmant = 10.0E+00 * xmant

    if ( 1.0E+00 <= xtemp ) then
      xmant = xmant + int ( xtemp )
      xtemp = xtemp - int ( xtemp )
    end if

    iplace = iplace + 1

    if ( xtemp == 0.0E+00 .or. nplace <= iplace ) then
      xround = is * xmant * ( 10.0E+00**l )
      exit
    end if

    l = l - 1
    xtemp = xtemp * 10.0E+00

  end do

  return
end
function r4_sign ( x )

!*****************************************************************************80
!
!! R4_SIGN returns the sign of an R4.
!
!  Discussion:
!
!    value = -1 if X < 0;
!    value =  0 if X => 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the number whose sign is desired.
!
!    Output, real (kind=4) :: R4_SIGN, the sign of X:
!
  implicit none

  real (kind=4) :: r4_sign
  real (kind=4) :: x

  if ( x < 0.0E+00 ) then
    r4_sign = -1.0E+00
  else
    r4_sign = +1.0E+00
  end if

  return
end
function r4_sign_opposite ( r1, r2 )

!*****************************************************************************80
!
!! R4_SIGN_OPPOSITE is TRUE if two R4's are not of the same sign.
!
!  Discussion:
!
!    This test could be coded numerically as
!
!      if ( r1 * r2 <= 0.0 ) then ...
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: R1, R2, the values to check.
!
!    Output, logical R4_SIGN_OPPOSITE, is TRUE if ( R1 <= 0 and 0 <= R2 )
!    or ( R2 <= 0 and 0 <= R1 ).
!
  implicit none

  real (kind=4) :: r1
  real (kind=4) :: r2
  logical r4_sign_opposite

  r4_sign_opposite = ( r1 <= 0.0E+00 .and. 0.0E+00 <= r2 ) .or. &
                     ( r2 <= 0.0E+00 .and. 0.0E+00 <= r1 )

  return
end
function r4_sign_opposite_strict ( r1, r2 )

!*****************************************************************************80
!
!! R4_SIGN_OPPOSITE_STRICT is TRUE if two R4's are strictly of opposite sign.
!
!  Discussion:
!
!    This test could be coded numerically as
!
!      if ( r1 * r2 < 0.0 ) then ...
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: R1, R2, the values to check.
!
!    Output, logical R4_SIGN_OPPOSITE_STRICT, is TRUE if ( R1 < 0 and 0 < R2 )
!    or ( R2 < 0 and 0 < R1 ).
!
  implicit none

  real (kind=4) :: r1
  real (kind=4) :: r2
  logical r4_sign_opposite_strict

  r4_sign_opposite_strict = ( r1 < 0.0E+00 .and. 0.0E+00 < r2 ) .or. &
                            ( r2 < 0.0E+00 .and. 0.0E+00 < r1 )

  return
end
subroutine r4_swap ( x, y )

!*****************************************************************************80
!
!! R4_SWAP swaps two R4's.
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
!    Input/output, real (kind=4) :: X, Y.  On output, the values of X and
!    Y have been interchanged.
!
  implicit none

  real (kind=4) :: x
  real (kind=4) :: y
  real (kind=4) :: z

  z = x
  x = y
  y = z

  return
end
subroutine r4_swap3 ( x, y, z )

!*****************************************************************************80
!
!! R4_SWAP3 swaps three R4's.
!
!  Example:
!
!    Input:
!
!      X = 1, Y = 2, Z = 3
!
!    Output:
!
!      X = 2, Y = 3, Z = 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real (kind=4) :: X, Y, Z, three values to be swapped.
!
  implicit none

  real (kind=4) :: w
  real (kind=4) :: x
  real (kind=4) :: y
  real (kind=4) :: z

  w = x
  x = y
  y = z
  z = w

  return
end
function r4_tiny ( )

!*****************************************************************************80
!
!! R4_TINY returns the smallest positive R4.
!
!  Discussion:
!
!    FORTRAN90 provides a built-in routine TINY ( X ) that
!    is more suitable for this purpose.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real (kind=4) :: R4_TINY, a "tiny" value.
!
  implicit none

  real (kind=4) :: r4_tiny

  r4_tiny = 0.1175494350822E-37

  return
end
subroutine r4_to_r4_discrete ( r, rmin, rmax, nr, rd )

!*****************************************************************************80
!
!! R4_TO_R4_DISCRETE maps R to RD in [RMIN, RMAX] with NR possible values.
!
!  Formula:
!
!    if ( R < RMIN ) then
!      RD = RMIN
!    else if ( RMAX < R ) then
!      RD = RMAX
!    else
!      T = nint ( ( NR - 1 ) * ( R - RMIN ) / ( RMAX - RMIN ) )
!      RD = RMIN + T * ( RMAX - RMIN ) / real ( NR - 1 )
!
!    In the special case where NR = 1, when
!
!      XD = 0.5 * ( RMAX + RMIN )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: R, the number to be converted.
!
!    Input, real (kind=4) :: RMAX, RMIN, the maximum and minimum
!    values for RD.
!
!    Input, integer (kind=4) :: NR, the number of allowed values for XD.
!    NR should be at least 1.
!
!    Output, real (kind=4) :: RD, the corresponding discrete value.
!
  implicit none

  integer (kind=4) :: f
  integer (kind=4) :: nr
  real (kind=4) :: r
  real (kind=4) :: rd
  real (kind=4) :: rmax
  real (kind=4) :: rmin
!
!  Check for errors.
!
  if ( nr < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4_TO_R4_DISCRETE - Fatal error!'
    write ( *, '(a,i8)' ) '  NR = ', nr
    write ( *, '(a)' ) '  but NR must be at least 1.'
    stop
  end if

  if ( nr == 1 ) then
    rd = 0.5E+00 * ( rmin + rmax )
    return
  end if

  if ( rmax == rmin ) then
    rd = rmax
    return
  end if

  f = nint ( real ( nr, kind = 4 ) * ( rmax - r ) / ( rmax - rmin ) )
  f = max ( f, 0 )
  f = min ( f, nr )

  rd = ( real (      f, kind = 4 ) * rmin   &
       + real ( nr - f, kind = 4 ) * rmax ) &
       / real ( nr,     kind = 4 )

  return
end
subroutine r4_to_dhms ( r, d, h, m, s )

!*****************************************************************************80
!
!! R4_TO_DHMS converts decimal days into days, hours, minutes, seconds.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: R, a decimal number representing a time
!    period measured in days.
!
!    Output, integer (kind=4) :: D, H, M, S, the equivalent number of days,
!    hours, minutes and seconds.
!
  implicit none

  integer (kind=4) :: d
  integer (kind=4) :: h
  integer (kind=4) :: m
  real (kind=4) :: r
  real (kind=4) :: r_copy
  integer (kind=4) :: s

  r_copy = abs ( r )

  d = int ( r_copy )

  r_copy = r_copy - d
  r_copy = 24.0E+00 * r_copy
  h = int ( r_copy )

  r_copy = r_copy - h
  r_copy = 60.0E+00 * r_copy
  m = int ( r_copy )

  r_copy = r_copy - m
  r_copy = 60.0E+00 * r_copy
  s = int ( r_copy )

  if ( r < 0.0E+00 ) then
    d = -d
    h = -h
    m = -m
    s = -s
  end if

  return
end
subroutine r4_to_i4 ( x, xmin, xmax, ixmin, ixmax, ix )

!*****************************************************************************80
!
!! R4_TO_I4 maps X in [XMIN, XMAX] to integer IX in [IXMIN, IXMAX].
!
!  Formula:
!
!    IX := IXMIN + ( IXMAX - IXMIN ) * ( X - XMIN ) / ( XMAX - XMIN )
!    IX := min ( IX, max ( IXMIN, IXMAX ) )
!    IX := max ( IX, min ( IXMIN, IXMAX ) )
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
!    Input, real (kind=4) :: X, the number to be converted.
!
!    Input, real (kind=4) :: XMIN, XMAX, the range.  XMAX and
!    XMIN must not be equal.  It is not necessary that XMIN be less than XMAX.
!
!    Input, integer (kind=4) :: IXMIN, IXMAX, the allowed range of the output
!    variable.  IXMAX corresponds to XMAX, and IXMIN to XMIN.
!    It is not necessary that IXMIN be less than IXMAX.
!
!    Output, integer (kind=4) :: IX, the value in the range [IXMIN,IXMAX] that
!    corresponds to X.
!
  implicit none

  integer (kind=4) :: ix
  integer (kind=4) :: ixmax
  integer (kind=4) :: ixmin
  real (kind=4) :: temp
  real (kind=4) :: x
  real (kind=4) :: xmax
  real (kind=4) :: xmin

  if ( xmax == xmin ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4_TO_I4 - Fatal error!'
    write ( *, '(a)' ) '  XMAX = XMIN, making a zero divisor.'
    write ( *, '(a,g14.6)' ) '  XMAX = ', xmax
    write ( *, '(a,g14.6)' ) '  XMIN = ', xmin
    stop
  end if

  temp = &
      ( ( xmax - x        ) * real ( ixmin, kind = 4 )  &
      + (        x - xmin ) * real ( ixmax, kind = 4 ) ) &
      / ( xmax     - xmin )

  if ( 0.0E+00 <= temp ) then
    temp = temp + 0.5E+00
  else
    temp = temp - 0.5E+00
  end if

  ix = int ( temp )

  return
end
function r4_uniform ( a, b, seed )

!*****************************************************************************80
!
!! R4_UNIFORM returns a scaled pseudorandom R4.
!
!  Discussion:
!
!    An R4 is a real (kind=4) :: value.
!
!    The pseudorandom number should be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, real (kind=4) :: A, B, the limits of the interval.
!
!    Input/output, integer (kind=4) :: SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real (kind=4) :: R4_UNIFORM, a number strictly between A and B.
!
  implicit none

  real (kind=4) :: a
  real (kind=4) :: b
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer (kind=4) :: k
  real (kind=4) :: r4_uniform
  integer (kind=4) :: seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4_UNIFORM - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r4_uniform = a + ( b - a ) * real ( seed, kind = 4 ) * 4.656612875E-10

  return
end
function r4_uniform_01 ( seed )

!*****************************************************************************80
!
!! R4_UNIFORM_01 returns a unit pseudorandom R4.
!
!  Discussion:
!
!    An R4 is a real (kind=4) :: value.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2**31 - 1 )
!      r4_uniform_01 = seed / ( 2**31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R4_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2004
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
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input/output, integer (kind=4) :: SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real (kind=4) :: R4_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
  implicit none

  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer (kind=4) :: k
  integer (kind=4) :: seed
  real (kind=4) :: r4_uniform_01

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r4_uniform_01 = real ( seed, kind = 4 ) * 4.656612875E-10

  return
end
subroutine r4_unswap3 ( x, y, z )

!*****************************************************************************80
!
!! R4_UNSWAP3 unswaps three R4's.
!
!  Example:
!
!    Input:
!
!      X = 2, Y = 3, Z = 1
!
!    Output:
!
!      X = 1, Y = 2, Z = 3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real (kind=4) :: X, Y, Z, three values to be swapped.
!
  implicit none

  real (kind=4) :: w
  real (kind=4) :: x
  real (kind=4) :: y
  real (kind=4) :: z

  w = z
  z = y
  y = x
  x = w

  return
end
function r4_walsh_1d ( x, digit )

!*****************************************************************************80
!
!! R4_WALSH_1D evaluates the Walsh function.
!
!  Discussion:
!
!    Consider the binary representation of X, and number the digits
!    in descending order, from leading to lowest, with the units digit
!    being numbered 0.
!
!    The Walsh function W(J)(X) is equal to the J-th binary digit of X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 May 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X, the argument of the Walsh function.
!
!    Input, integer (kind=4) :: DIGIT, the index of the Walsh function.
!
!    Output, real (kind=4) :: R4_WALSH_1D, the value of the Walsh function.
!
  implicit none

  integer (kind=4) :: digit
  integer (kind=4) :: n
  real (kind=4) :: r4_walsh_1d
  real (kind=4) :: x
  real (kind=4) :: x_copy
!
!  Hide the effect of the sign of X.
!
  x_copy = abs ( x )
!
!  If DIGIT is positive, divide by 2 DIGIT times.
!  If DIGIT is negative, multiply by 2 (-DIGIT) times.
!
  x_copy = x_copy / 2.0E+00**digit
!
!  Make it an integer.
!  Because it's positive, and we're using INT, we don't change the
!  units digit.
!
  n = int ( x_copy )
!
!  Is the units digit odd or even?
!
  if ( mod ( n, 2 ) == 0 ) then
    r4_walsh_1d = 0.0E+00
  else
    r4_walsh_1d = 1.0E+00
  end if

  return
end
subroutine r42_cheby ( n, alo, ahi, a )

!*****************************************************************************80
!
!! R42_CHEBY sets up the Chebyshev abscissas in an R4 interval.
!
!  Discussion:
!
!    The routine sets up a vector of X values spaced between the values
!    XLO and XHI in a similar way to the spacing of the Chebyshev
!    points of the same order in the interval [-1,1].
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
!    Input, integer (kind=4) :: N, the number of points to compute.
!
!    Input, real (kind=4) :: ALO, AHI, the range.
!
!    Output, real (kind=4) :: A(N), the computed X values.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n)
  real (kind=4) :: ahi
  real (kind=4) :: alo
  real (kind=4) :: arg
  integer (kind=4) :: i
  real ( kind = 4 ), parameter :: pi = 3.141592653589793E+00

  if ( n == 1 ) then

    a(1) = 0.5E+00 * ( alo + ahi )

  else if ( 1 < n ) then

    do i = 1, n

      arg = real ( 2 * i - 1, kind = 4 ) * pi &
          / real ( 2 * n, kind = 4 )

      a(i) = 0.5E+00 * ( ( 1.0E+00 + cos ( arg ) ) * alo &
                       + ( 1.0E+00 - cos ( arg ) ) * ahi )

    end do

  end if

  return
end
function r42_dist_l2 ( a1, a2 )

!*****************************************************************************80
!
!! R42_DIST_L2 returns the L2 distance between a pair of R42's.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
!
!    The vector L2 norm is defined as:
!
!      sqrt ( sum ( 1 <= I <= N ) A(I) * A(I) ).
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
!    Input, real (kind=4) :: A1(2), A2(2), the vectors.
!
!    Output, real (kind=4) :: R42_DIST_L2, the L2 norm of the distance
!    between A1 and A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a1(dim_num)
  real (kind=4) :: a2(dim_num)
  real (kind=4) :: r42_dist_l2

  r42_dist_l2 = sqrt ( sum ( ( a1(1:dim_num) - a2(1:dim_num) )**2 ) )

  return
end
function r42_eq ( a1, a2 )

!*****************************************************************************80
!
!! R42_EQ == ( A1 == A2 ) for two R42's.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 == A2  <=>  A1(1) == A2(1) and A1(2) == A2(2).
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
!    Input, real (kind=4) :: A1(2), A2(2), two R42 vectors to be compared.
!
!    Output, logical R42_EQ, is TRUE if and only if A1 == A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a1(dim_num)
  real (kind=4) :: a2(dim_num)
  logical              r42_eq

  if ( all ( a1(1:dim_num) == a2(1:dim_num) ) ) then
    r42_eq = .true.
  else
    r42_eq = .false.
  end if

  return
end
function r42_ge ( a1, a2 )

!*****************************************************************************80
!
!! R42_GE == ( A1 >= A2 ) for two R42's.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 >= A2  <=>  A1(1) > A2(1) or ( A1(1) == A2(1) and A1(2) >= A2(2) ).
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
!    Input, real (kind=4) :: A1(2), A2(2), R42 vectors to be compared.
!
!    Output, logical R92_GE, is TRUE if and only if A1 >= A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a1(dim_num)
  real (kind=4) :: a2(dim_num)
  integer (kind=4) :: i
  logical              r42_ge

  r42_ge = .true.

  do i = 1, dim_num

    if ( a2(i) < a1(i) ) then
      r42_ge = .true.
      exit
    else if ( a1(i) < a2(i) ) then
      r42_ge = .false.
      exit
    end if

  end do

  return
end
function r42_gt ( a1, a2 )

!*****************************************************************************80
!
!! R42_GT == ( A1 > A2 ) for two R42's.
!
!  Discussion:
!
!    An R42 is a vector of type R2, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 > A2  <=>  A1(1) > A2(1) or ( A1(1) == A2(1) and A1(2) > A2(2) ).
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
!    Input, real (kind=4) :: A1(2), A2(2), R42 vectors to be compared.
!
!    Output, logical R42_GT, is TRUE if and only if A1 > A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a1(dim_num)
  real (kind=4) :: a2(dim_num)
  integer (kind=4) :: i
  logical              r42_gt

  r42_gt = .false.

  do i = 1, dim_num

    if ( a2(i) < a1(i) ) then
      r42_gt = .true.
      exit
    else if ( a1(i) < a2(i) ) then
      r42_gt = .false.
      exit
    end if

  end do

  return
end
function r42_le ( a1, a2 )

!*****************************************************************************80
!
!! R42_LE == ( A1 <= A2 ) for two R42's.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 <= A2  <=>  A1(1) < A2(1) or ( A1(1) == A2(1) and A1(2) <= A2(2) ).
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
!    Input, real (kind=4) :: A1(2), A2(2), R42 vectors to be compared.
!
!    Output, logical R42_LE, is TRUE if and only if A1 <= A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a1(dim_num)
  real (kind=4) :: a2(dim_num)
  integer (kind=4) :: i
  logical              r42_le

  r42_le = .true.

  do i = 1, dim_num

    if ( a1(i) < a2(i) ) then
      r42_le = .true.
      exit
    else if ( a2(i) < a1(i) ) then
      r42_le = .false.
      exit
    end if

  end do

  return
end
function r42_lt ( a1, a2 )

!*****************************************************************************80
!
!! R42_LT == ( A1 < A2 ) for two R42's.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 < A2  <=>  A1(1) < A2(1) or ( A1(1) == A2(1) and A1(2) < A2(2) ).
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
!    Input, real (kind=4) :: A1(2), A2(2), R42 vectors to be compared.
!
!    Output, logical R42_LT, is TRUE if and only if A1 < A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a1(dim_num)
  real (kind=4) :: a2(dim_num)
  integer (kind=4) :: i
  logical              r42_lt

  r42_lt = .false.

  do i = 1, dim_num

    if ( a1(i) < a2(i) ) then
      r42_lt = .true.
      exit
    else if ( a2(i) < a1(i) ) then
      r42_lt = .false.
      exit
    end if

  end do

  return
end
function r42_ne ( a1, a2 )

!*****************************************************************************80
!
!! R42_NE == ( A1 /= A2 ) for two R42's.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 /= A2  <=>  A1(1) /= A2(1) or A1(2) /= A2(2).
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
!    Input, real (kind=4) :: A1(2), A2(2), R42 vectors to be compared.
!
!    Output, logical R42_NE, is TRUE if and only if A1 /= A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a1(dim_num)
  real (kind=4) :: a2(dim_num)
  logical              r42_ne

  if ( any ( a1(1:dim_num) /= a2(1:dim_num) ) ) then
    r42_ne = .true.
  else
    r42_ne = .false.
  end if

  return
end
function r42_norm ( a )

!*****************************************************************************80
!
!! R42_NORM returns the Euclidean norm of an R42.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
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
!    Input, real (kind=4) :: A(2), the vector.
!
!    Output, real (kind=4) :: R42_NORM, the norm.
!
  implicit none

  real (kind=4) :: a(2)
  real (kind=4) :: r42_norm

  r42_norm = sqrt ( a(1) * a(1) + a(2) * a(2) )

  return
end
subroutine r42_normalize ( a )

!*****************************************************************************80
!
!! R42_NORMALIZE Euclidean normalizes an R42.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
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
!    Input/output, real (kind=4) :: A(2), the components of the vector.
!
  implicit none

  real (kind=4) :: a(2)
  real (kind=4) :: norm

  norm = sqrt ( a(1) * a(1) + a(2) * a(2) )

  if ( norm /= 0.0E+00 ) then
    a(1:2) = a(1:2) / norm
  end if

  return
end
subroutine r42_print ( a, title )

!*****************************************************************************80
!
!! R42_PRINT prints an R42.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
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
!    Input, real (kind=4) :: A(2), the coordinates of the vector.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  real      ( kind = 4 ) a(2)
  character ( len = * )  title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '( 2x, a, a4, g14.6, a1, g14.6, a1 )' ) &
      trim ( title ), ' : (', a(1), ',', a(2), ')'
  else
    write ( *, '( 2x, a1, g14.6, a1, g14.6, a1 )' ) '(', a(1), ',', a(2), ')'

  end if

  return
end
subroutine r42_swap ( x, y )

!*****************************************************************************80
!
!! R42_SWAP swaps two R42 values.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
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
!    Input/output, real (kind=4) :: X(2), Y(2).  On output, the values of X and
!    Y have been interchanged.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: x(dim_num)
  real (kind=4) :: y(dim_num)
  real (kind=4) :: z(dim_num)

  z(1:dim_num) = x(1:dim_num)
  x(1:dim_num) = y(1:dim_num)
  y(1:dim_num) = z(1:dim_num)

  return
end
subroutine r42_uniform ( b, c, seed, a )

!*****************************************************************************80
!
!! R42_UNIFORM returns a random R42 value in a given range.
!
!  Discussion:
!
!    An R42 is a vector of type R4, with two entries.
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
!    Input, real (kind=4) :: B, C, the minimum and maximum values.
!
!    Input/output, integer (kind=4) :: SEED, a seed for the random number
!    generator.
!
!    Output, real (kind=4) :: A(2), the randomly chosen value.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a(dim_num)
  real (kind=4) :: b
  real (kind=4) :: c
  real (kind=4) :: r4_uniform
  integer (kind=4) :: i
  integer (kind=4) :: seed

  do i = 1, dim_num
    a(i) = r4_uniform ( b, c, seed )
  end do

  return
end
subroutine r42poly2_print ( a, b, c, d, e, f )

!*****************************************************************************80
!
!! R42POLY2_PRINT prints a second order polynomial in two variables.
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
!    Input, real (kind=4) :: A, B, C, D, E, F, the coefficients.
!
  implicit none

  real (kind=4) :: a
  real (kind=4) :: b
  real (kind=4) :: c
  real (kind=4) :: d
  real (kind=4) :: e
  real (kind=4) :: f

  write ( *, &
    '( 2x, f8.4, '' * x^2 + '', f8.4, '' * y^2 + '', f8.4, '' * xy  + '' )' ) &
    a, b, c

  write ( *, &
    '( 2x, f8.4, '' * x + '', f8.4, '' * y + '', f8.4, '' = 0 '' )' ) d, e, f

  return
end
subroutine r42poly2_type ( a, b, c, d, e, f, type )

!*****************************************************************************80
!
!! R42POLY2_TYPE analyzes a second order polynomial in two variables.
!
!  Discussion:
!
!    The polynomial has the form
!
!      A x^2 + B y^2 + C xy + Dx + Ey + F = 0
!
!    The possible types of the solution set are:
!
!     1: a hyperbola;
!        9x^2 -  4y^2       -36x - 24y -  36 = 0
!     2: a parabola;
!        4x^2 +  1y^2 - 4xy + 3x -  4y +   1 = 0;
!     3: an ellipse;
!        9x^2 + 16y^2       +36x - 32y -  92 = 0;
!     4: an imaginary ellipse (no real solutions);
!         x^2 +   y^2       - 6x - 10y + 115 = 0;
!     5: a pair of intersecting lines;
!                        xy + 3x -   y -   3 = 0
!     6: one point;
!         x^2 +  2y^2       - 2x + 16y +  33 = 0;
!     7: a pair of distinct parallel lines;
!                 y^2            -  6y +   8 = 0
!     8: a pair of imaginary parallel lines (no real solutions);
!                 y^2            -  6y +  10 = 0
!     9: a pair of coincident lines.
!                 y^2            -  2y +   1 = 0
!    10: a single line;
!                             2x -   y +   1 = 0;
!    11; all space;
!                                          0 = 0;
!    12; no solutions;
!                                          1 = 0;
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
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    CRC Press, 30th Edition, 1996, pages 282-284.
!
!  Parameters:
!
!    Input, real (kind=4) :: A, B, C, D, E, F, the coefficients.
!
!    Output, integer (kind=4) :: TYPE, indicates the type of the solution set.
!
  implicit none

  real (kind=4) :: a
  real (kind=4) :: b
  real (kind=4) :: c
  real (kind=4) :: d
  real (kind=4) :: delta
  real (kind=4) :: e
  real (kind=4) :: f
  real (kind=4) :: j
  real (kind=4) :: k
  integer (kind=4) :: type
!
!  Handle the degenerate case.
!
  if ( a == 0.0E+00 .and. &
       b == 0.0E+00 .and. &
       c == 0.0E+00 ) then
    if ( d == 0.0E+00 .and. e == 0.0E+00 ) then
      if ( f == 0.0E+00 ) then
        type = 11
      else
        type = 12
      end if
    else
      type = 10
    end if
    return
  end if

  delta = &
      8.0E+00 * a * b * f &
    + 2.0E+00 * c * e * d &
    - 2.0E+00 * a * e * e &
    - 2.0E+00 * b * d * d &
    - 2.0E+00 * f * c * c

  j = 4.0E+00 * a * b - c * c

  if ( delta /= 0.0E+00 ) then
    if ( j < 0.0E+00 ) then
      type = 1
    else if ( j == 0.0E+00 ) then
      type = 2
    else if ( 0.0E+00 < j ) then
      if ( sign ( 1.0E+00, delta ) /= sign ( 1.0E+00, ( a + b ) ) ) then
        type = 3
      else if ( sign ( 1.0E+00, delta ) == sign ( 1.0E+00, ( a + b ) ) ) then
        type = 4
      end if
    end if
  else if ( delta == 0.0E+00 ) then
    if ( j < 0.0E+00 ) then
      type = 5
    else if ( 0.0E+00 < j ) then
      type = 6
    else if ( j == 0.0E+00 ) then

      k = 4.0E+00 * ( a + b ) * f - d * d - e * e

      if ( k < 0.0E+00 ) then
        type = 7
      else if ( 0.0E+00 < k ) then
        type = 8
      else if ( k == 0.0E+00 ) then
        type = 9
      end if

    end if
  end if

  return
end
subroutine r42poly2_type_print ( type )

!*****************************************************************************80
!
!! R42POLY2_TYPE_PRINT prints the meaning of the output from R42POLY2_TYPE.
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
!    Input, integer (kind=4) :: TYPE, the type index returned by R42POLY2_TYPE.
!
  implicit none

  integer (kind=4) :: type

  if ( type == 1 ) then
    write ( *, '(a)' ) '  The set of solutions forms a hyperbola.'
  else if ( type == 2 ) then
    write ( *, '(a)' ) '  The set of solutions forms a parabola.'
  else if ( type == 3 ) then
    write ( *, '(a)' ) '  The set of solutions forms an ellipse.'
  else if ( type == 4 ) then
    write ( *, '(a)' ) '  The set of solutions forms an imaginary ellipse.'
    write ( *, '(a)' ) '  (There are no real solutions).'
  else if ( type == 5 ) then
    write ( *, '(a)' ) &
      '  The set of solutions forms a pair of intersecting lines.'
  else if ( type == 6 ) then
    write ( *, '(a)' ) '  The set of solutions is a single point.'
  else if ( type == 7 ) then
    write ( *, '(a)' ) &
      '  The set of solutions form a pair of distinct parallel lines.'
  else if ( type == 8 ) then
    write ( *, '(a)' ) &
      '  The set of solutions forms a pair of imaginary parallel lines.'
    write ( *, '(a)' ) '  (There are no real solutions).'
  else if ( type == 9 ) then
    write ( *, '(a)' ) &
      '  The set of solutions forms a pair of coincident lines.'
  else if ( type == 10 ) then
    write ( *, '(a)' ) '  The set of solutions forms a single line.'
  else if ( type == 11 ) then
    write ( *, '(a)' ) '  The set of solutions is all space.'
  else if ( type == 12 ) then
    write ( *, '(a)' ) '  The set of solutions is empty.'
  else
    write ( *, '(a)' ) '  This type index is unknown.'
  end if

  return
end
subroutine r42vec_max ( n, a, amax )

!*****************************************************************************80
!
!! R42VEC_MAX returns the maximum value in an R42VEC.
!
!  Discussion:
!
!    An R42VEC is an array of pairs of R4's.
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
!    Input, integer (kind=4) :: N, the number of entries in the array.
!
!    Input, real (kind=4) :: A(2,N), the array.
!
!    Output, real (kind=4) :: AMAX(2); the largest entries in each row.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(2,n)
  real (kind=4) :: amax(2)

  amax(1) = maxval ( a(1,1:n) )
  amax(2) = maxval ( a(2,1:n) )

  return
end
subroutine r42vec_min ( n, a, amin )

!*****************************************************************************80
!
!! R42VEC_MIN returns the minimum value in an R42VEC.
!
!  Discussion:
!
!    An R42VEC is an array of pairs of R42's.
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
!    Input, integer (kind=4) :: N, the number of entries in the array.
!
!    Input, real (kind=4) :: A(2,N), the array.
!
!    Output, real (kind=4) :: AMIN(2); the smallest entries in each row.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(2,n)
  real (kind=4) :: amin(2)

  amin(1) = minval ( a(1,1:n) )
  amin(2) = minval ( a(2,1:n) )

  return
end
subroutine r42vec_order_type ( n, a, order )

!*****************************************************************************80
!
!! R42VEC_ORDER_TYPE finds the order type of an R42VEC.
!
!  Discussion:
!
!    An R42VEC is an array of pairs of R4 values.
!
!    The dictionary or lexicographic ordering is used.
!
!    (X1,Y1) < (X2,Y2)  <=>  X1 < X2 or ( X1 = X2 and Y1 < Y2).
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
!    Input, integer (kind=4) :: N, the number of entries of the array.
!
!    Input, real (kind=4) :: A(2,N), the array to be checked.
!
!    Output, integer (kind=4) :: ORDER, order indicator:
!    -1, no discernable order;
!    0, all entries are equal;
!    1, ascending order;
!    2, strictly ascending order;
!    3, descending order;
!    4, strictly descending order.
!
  implicit none

  integer (kind=4) :: n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a(dim_num,n)
  integer (kind=4) :: i
  integer (kind=4) :: order
!
!  Search for the first value not equal to A(1,1).
!
  i = 1

  do

    i = i + 1

    if ( n < i ) then
      order = 0
      return
    end if

    if ( &
         a(1,1) <  a(1,i) .or. &
       ( a(1,1) == a(1,i) .and. a(2,1) < a(2,i) ) &
    ) then

      if ( i == 2 ) then
        order = 2
      else
        order = 1
      end if

      exit

    else if ( &
        a(1,i) <  a(1,1)  .or. &
      ( a(1,i) == a(1,1) .and. a(2,i) < a(2,1) ) &
    ) then

      if ( i == 2 ) then
        order = 4
      else
        order = 3
      end if

      exit

    end if

  end do
!
!  Now we have a "direction".  Examine subsequent entries.
!
  do

    i = i + 1
    if ( n < i ) then
      exit
    end if

    if ( order == 1 ) then

      if ( &
          a(1,i) <  a(1,i-1) .or. &
        ( a(1,i) == a(1,i-1) .and. a(2,i) < a(2,i-1) ) &
      ) then
        order = -1
        exit
      end if

    else if ( order == 2 ) then

      if ( &
          a(1,i) <  a(1,i-1) .or. &
        ( a(1,i) == a(1,i-1) .and. a(2,i) < a(2,i-1) ) &
      ) then
        order = -1
        exit
      else if ( &
         a(1,i) == a(1,i-1) .and. a(2,i) == a(2,i-1) ) then
        order = 1
      end if

    else if ( order == 3 ) then

      if ( &
          a(1,i-1) <  a(1,i) .or. &
        ( a(1,i-1) == a(1,i) .and. a(2,i-1) < a(2,i) ) &
      ) then
        order = -1
        exit
      end if

    else if ( order == 4 ) then

      if ( &
          a(1,i-1) <  a(1,i) .or. &
        ( a(1,i-1) == a(1,i) .and. a(2,i-1) < a(2,i) ) &
      ) then
        order = -1
        exit
      else if ( a(1,i) == a(1,i-1) .and. a(2,i) == a(2,i-1) ) then
        order = 3
      end if

    end if

  end do

  return
end
subroutine r42vec_part_quick_a ( n, a, l, r )

!*****************************************************************************80
!
!! R42VEC_PART_QUICK_A reorders an R42VEC as part of a quick sort.
!
!  Discussion:
!
!    An R42VEC is an array of pairs of R42 values.
!
!    The routine reorders the entries of A.  Using A(1:2,1) as a
!    key, all entries of A that are less than or equal to the key will
!    precede the key, which precedes all entries that are greater than the key.
!
!  Example:
!
!    Input:
!
!      N = 8
!
!      A = ( (2,4), (8,8), (6,2), (0,2), (10,6), (10,0), (0,6), (4,8) )
!
!    Output:
!
!      L = 2, R = 4
!
!      A = ( (0,2), (0,6), (2,4), (8,8), (6,2), (10,6), (10,0), (4,8) )
!             -----------          ----------------------------------
!             LEFT          KEY    RIGHT
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
!    Input, integer (kind=4) :: N, the number of entries of A.
!
!    Input/output, real (kind=4) :: A(2,N).  On input, the array to be checked.
!    On output, A has been reordered as described above.
!
!    Output, integer (kind=4) :: L, R, the indices of A that define the three
!    segments.  Let KEY = the input value of A(1:2,1).  Then
!    I <= L                 A(1:2,I) < KEY;
!         L < I < R         A(1:2,I) = KEY;
!                 R <= I    KEY < A(1:2,I).
!
  implicit none

  integer (kind=4) :: n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a(dim_num,n)
  integer (kind=4) :: i
  real (kind=4) :: key(dim_num)
  integer (kind=4) :: l
  integer (kind=4) :: m
  integer (kind=4) :: r
  logical              r4vec_eq
  logical              r4vec_gt
  logical              r4vec_lt

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R42VEC_PART_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    write ( *, '(a,i8)' ) '  N = ', n
    stop
  else if ( n == 1 ) then
    l = 0
    r = 2
    return
  end if

  key(1:dim_num) = a(1:dim_num,1)
  m = 1
!
!  The elements of unknown size have indices between L+1 and R-1.
!
  l = 1
  r = n + 1

  do i = 2, n

    if ( r4vec_gt ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
      r = r - 1
      call r4vec_swap ( dim_num, a(1:dim_num,r), a(1:dim_num,l+1) )
    else if ( r4vec_eq ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
      m = m + 1
      call r4vec_swap ( dim_num, a(1:dim_num,m), a(1:dim_num,l+1) )
      l = l + 1
    else if ( r4vec_lt ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
      l = l + 1
    end if

  end do
!
!  Now shift small elements to the left, and KEY elements to center.
!
  do i = 1, l - m
    a(1:dim_num,i) = a(1:dim_num,i+m)
  end do

  l = l - m

  do i = 1, dim_num
    a(i,l+1:l+m) = key(i)
  end do

  return
end
subroutine r42vec_permute ( n, p, a )

!*****************************************************************************80
!
!! R42VEC_PERMUTE permutes an R42VEC in place.
!
!  Discussion:
!
!    An R42VEC is an array of pairs of R4 values.
!
!    The same logic can be used to permute an array of objects of any
!    arithmetic type, or an array of objects of any complexity.  The only
!    temporary storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,    4,    5,    1,    3 )
!      A = ( 1.0,  2.0,  3.0,  4.0,  5.0 )
!          (11.0, 22.0, 33.0, 44.0, 55.0 )
!
!    Output:
!
!      A    = (  2.0,  4.0,  5.0,  1.0,  3.0 )
!             ( 22.0, 44.0, 55.0, 11.0, 33.0 ).
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
!    Input, integer (kind=4) :: N, the number of objects.
!
!    Input, integer (kind=4) :: P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.
!
!    Input/output, real (kind=4) :: A(2,N), the array to be permuted.
!
  implicit none

  integer (kind=4) :: n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a(dim_num,n)
  real (kind=4) :: a_temp(dim_num)
  integer ( kind = 4 ), parameter :: base = 1
  integer (kind=4) :: ierror
  integer (kind=4) :: iget
  integer (kind=4) :: iput
  integer (kind=4) :: istart
  integer (kind=4) :: p(n)

  call perm_check ( n, p, base, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R42VEC_PERMUTE - Fatal error!'
    write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
    stop
  end if
!
!  Search for the next element of the permutation that has not been used.
!
  do istart = 1, n

    if ( p(istart) < 0 ) then

    else if ( p(istart) == istart ) then

      p(istart) = - p(istart)

    else

      a_temp(1:dim_num) = a(1:dim_num,istart)
      iget = istart
!
!  Copy the new value into the vacated entry.
!
      do

        iput = iget
        iget = p(iget)

        p(iput) = - p(iput)

        if ( iget < 1 .or. n < iget ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R42VEC_PERMUTE - Fatal error!'
          write ( *, '(a)' ) '  A permutation index is out of range.'
          write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
          stop
        end if

        if ( iget == istart ) then
          a(1:dim_num,iput) = a_temp(1:dim_num)
          exit
        end if

        a(1:dim_num,iput) = a(1:dim_num,iget)

      end do

    end if

  end do
!
!  Restore the signs of the entries.
!
  p(1:n) = - p(1:n)

  return
end
subroutine r42vec_print ( n, a, title )

!*****************************************************************************80
!
!! R42VEC_PRINT prints an R42VEC.
!
!  Discussion:
!
!    An R42VEC is an array of pairs of R42's.
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
!    Input, integer (kind=4) :: N, the number of components of the vector.
!
!    Input, real (kind=4) :: A(2,N), the R42 vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ) n
  integer   ( kind = 4 ), parameter :: dim_num = 2

  real      ( kind = 4 ) a(dim_num,n)
  integer   ( kind = 4 ) i
  character ( len = * )  title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,(5g14.6))' ) i, a(1:dim_num,i)
  end do

  return
end
subroutine r42vec_sort_heap_index_a ( n, a, indx )

!*****************************************************************************80
!
!! R42VEC_SORT_HEAP_INDEX_A ascending index heaps an R42VEC.
!
!  Discussion:
!
!    An R42VEC is an array of R42's.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      A(1:2,INDX(1:N)) is sorted,
!
!    or explicitly, by the call
!
!      call r42vec_permute ( n, indx, a )
!
!    after which A(1:2,I), I = 1 to N is sorted.
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
!    Input, integer (kind=4) :: N, the number of entries in the array.
!
!    Input, real (kind=4) :: A(2,N), an array to be index-sorted.
!
!    Output, integer (kind=4) :: INDX(N), the sort index.  The
!    I-th element of the sorted array is A(1:2,INDX(I)).
!
  implicit none

  integer (kind=4) :: n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a(dim_num,n)
  real (kind=4) :: aval(dim_num)
  integer (kind=4) :: i
  integer (kind=4) :: indx(n)
  integer (kind=4) :: indxt
  integer (kind=4) :: ir
  integer (kind=4) :: j
  integer (kind=4) :: l

  if ( n < 1 ) then
    return
  end if

  do i = 1, n
    indx(i) = i
  end do

  if ( n == 1 ) then
    return
  end if

  l = n / 2 + 1
  ir = n

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      aval(1:dim_num) = a(1:dim_num,indxt)

    else

      indxt = indx(ir)
      aval(1:dim_num) = a(1:dim_num,indxt)
      indx(ir) = indx(1)
      ir = ir - 1

      if ( ir == 1 ) then
        indx(1) = indxt
        exit
      end if

    end if

    i = l
    j = l + l

    do while ( j <= ir )

      if ( j < ir ) then
        if (   a(1,indx(j)) <  a(1,indx(j+1)) .or. &
             ( a(1,indx(j)) == a(1,indx(j+1)) .and. &
               a(2,indx(j)) <  a(2,indx(j+1)) ) ) then
          j = j + 1
        end if
      end if

      if (   aval(1) <  a(1,indx(j)) .or. &
           ( aval(1) == a(1,indx(j)) .and. &
             aval(2) <  a(2,indx(j)) ) ) then
        indx(i) = indx(j)
        i = j
        j = j + j
      else
        j = ir + 1
      end if

    end do

    indx(i) = indxt

  end do

  return
end
subroutine r42vec_sort_quick_a ( n, a )

!*****************************************************************************80
!
!! R42VEC_SORT_QUICK_A ascending sorts an R42VEC using quick sort.
!
!  Discussion:
!
!    An R42VEC is an array of R42's.
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
!    Input, integer (kind=4) :: N, the number of entries in the array.
!
!    Input/output, real (kind=4) :: A(2,N).
!    On input, the array to be sorted.
!    On output, the array has been sorted.
!
  implicit none

  integer ( kind = 4 ), parameter :: level_max = 30
  integer (kind=4) :: n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real (kind=4) :: a(dim_num,n)
  integer (kind=4) :: base
  integer (kind=4) :: l_segment
  integer (kind=4) :: level
  integer (kind=4) :: n_segment
  integer (kind=4) :: rsave(level_max)
  integer (kind=4) :: r_segment

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R42VEC_SORT_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    write ( *, '(a,i8)' ) '  N = ', n
    stop
  else if ( n == 1 ) then
    return
  end if

  level = 1
  rsave(level) = n + 1
  base = 1
  n_segment = n

  do
!
!  Partition the segment.
!
    call r42vec_part_quick_a ( n_segment, a(1,base), l_segment, r_segment )
!
!  If the left segment has more than one element, we need to partition it.
!
    if ( 1 < l_segment ) then

      if ( level_max < level ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R42VEC_SORT_QUICK_A - Fatal error!'
        write ( *, '(a,i8)' ) '  Exceeding recursion maximum of ', level_max
        stop
      end if

      level = level + 1
      n_segment = l_segment
      rsave(level) = r_segment + base - 1
!
!  The left segment and the middle segment are sorted.
!  Must the right segment be partitioned?
!
    else if ( r_segment < n_segment ) then

      n_segment = n_segment + 1 - r_segment
      base = base + r_segment - 1
!
!  Otherwise, we back up a level if there is an earlier one.
!
    else

      do

        if ( level <= 1 ) then
          return
        end if

        base = rsave(level)
        n_segment = rsave(level-1) - rsave(level)
        level = level - 1

        if ( 0 < n_segment ) then
          exit
        end if

      end do

    end if

  end do

  return
end
function r43_norm ( x, y, z )

!*****************************************************************************80
!
!! R43_NORM returns the Euclidean norm of an R43.
!
!  Discussion:
!
!    An R43 is a vector of 3 R4's.
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
!    Input, real (kind=4) :: X, Y, Z, the vector.
!
!    Output, real (kind=4) :: R43_NORM, the norm of the vector.
!
  implicit none

  real (kind=4) :: r43_norm
  real (kind=4) :: x
  real (kind=4) :: y
  real (kind=4) :: z

  r43_norm = sqrt ( x * x + y * y + z * z )

  return
end
subroutine r43_normalize ( x, y, z )

!*****************************************************************************80
!
!! R43_NORMALIZE normalizes an R43.
!
!  Discussion:
!
!    An R43 is a vector of 3 R4's.
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
!    Input/output, real (kind=4) :: X, Y, Z, the components of the vector.
!
  implicit none

  real (kind=4) :: norm
  real (kind=4) :: x
  real (kind=4) :: y
  real (kind=4) :: z

  norm = sqrt ( x * x + y * y + z * z )

  if ( norm /= 0.0E+00 ) then
    x = x / norm
    y = y / norm
    z = z / norm
  end if

  return
end
subroutine r43_print ( x, y, z, title )

!*****************************************************************************80
!
!! R43_PRINT prints an R43.
!
!  Discussion:
!
!    An R43 is a vector of 3 R4's.
!
!    A format is used which suggests a coordinate triple:
!
!  Example:
!
!    Center : ( 1.23, 7.45, -1.45 )
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
!    Input, real (kind=4) :: X, Y, Z, the coordinates of the vector.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  character ( len = * )  title
  real      ( kind = 4 ) x
  real      ( kind = 4 ) y
  real      ( kind = 4 ) z

  if ( 0 < len_trim ( title ) ) then
    write ( *, '( 2x, a, a4, g14.6, a1, g14.6, a1, g14.6, a1 )' ) &
      trim ( title ), ' : (', x, ',', y, ',', z, ')'
  else
    write ( *, '( 2x, a1, g14.6, a1, g14.6, a1, g14.6, a1 )' ) &
      '(', x, ',', y, ',', z, ')'
  end if

  return
end
subroutine r43_swap ( x, y )

!*****************************************************************************80
!
!! R43_SWAP swaps two R43's.
!
!  Discussion:
!
!    An R43 is a vector of 3 R4's.
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
!    Input/output, real (kind=4) :: X(3), Y(3).  On output, the values
!    of X and Y have been interchanged.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real (kind=4) :: x(dim_num)
  real (kind=4) :: y(dim_num)
  real (kind=4) :: z(dim_num)

  z(1:dim_num) = x(1:dim_num)
  x(1:dim_num) = y(1:dim_num)
  y(1:dim_num) = z(1:dim_num)

  return
end
subroutine r43vec_normalize ( n, x )

!*****************************************************************************80
!
!! R43VEC_NORMALIZE normalizes each R43 in an R43VEC.
!
!  Discussion:
!
!    An R43VEC is a vector of R43's.
!
!    An R43 is a vector of 3 R4's.
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
!    Input, integer (kind=4) :: N, the number of R43 vectors.
!
!    Input/output, real (kind=4) :: X(3,N), the coordinates of N R43 vectors.
!    On output, the nonzero vectors have been scaled to have unit L2 norm.
!
  implicit none

  integer (kind=4) :: n
  integer ( kind = 4 ), parameter :: dim_num = 3

  integer (kind=4) :: i
  real (kind=4) :: norm
  real (kind=4) :: x(dim_num,n)

  do i = 1, n

    norm = sqrt ( sum ( x(1:dim_num,i)**2 ) )

    if ( norm /= 0.0E+00 ) then
      x(1:dim_num,i) = x(1:dim_num,i) / norm
    end if

  end do

  return
end
subroutine r44_normalize ( v )

!*****************************************************************************80
!
!! R44_NORMALIZE normalizes an R44.
!
!  Discussion:
!
!    An R44 is a vector of four R4's.
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
!    Input/output, real (kind=4) :: V(4), the components of the vector.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 4

  real (kind=4) :: norm
  real (kind=4) :: v(dim_num)

  norm = sqrt ( sum ( v(1:dim_num)**2 ) )

  if ( norm /= 0.0E+00 ) then
    v(1:dim_num) = v(1:dim_num) / norm
  end if

  return
end
subroutine r4block_expand_linear ( l, m, n, x, lfat, mfat, nfat, xfat )

!*****************************************************************************80
!
!! R4BLOCK_EXPAND_LINEAR linearly interpolates new data into an R4BLOCK.
!
!  Discussion:
!
!    An R4BLOCK is a 3D array of R4 values.
!
!    In this routine, the expansion is specified by giving the number
!    of intermediate values to generate between each pair of original
!    data rows and columns.
!
!    The interpolation is not actually linear.  It uses the functions
!
!      1, x, y, z, xy, xz, yz, xyz.
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
!    Input, integer (kind=4) :: L, M, N, the dimensions of the input data.
!
!    Input, real (kind=4) :: X(L,M,N), the original data.
!
!    Input, integer (kind=4) :: LFAT, MFAT, NFAT, the number of data values
!    to interpolate original data values in the first, second and third
!    dimensions.
!
!    Output, real (kind=4) :: XFAT(L2,M2,N2), the fattened data, where
!    L2 = (L-1)*(LFAT+1)+1,
!    M2 = (M-1)*(MFAT+1)+1,
!    N2 = (N-1)*(NFAT+1)+1.
!
  implicit none

  integer (kind=4) :: l
  integer (kind=4) :: lfat
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
  integer (kind=4) :: k
  integer (kind=4) :: khi
  integer (kind=4) :: kk
  integer (kind=4) :: kkk
  integer (kind=4) :: kp1
  real (kind=4) :: r
  real (kind=4) :: s
  real (kind=4) :: t
  real (kind=4) :: x(l,m,n)
  real (kind=4) :: x000
  real (kind=4) :: x001
  real (kind=4) :: x010
  real (kind=4) :: x011
  real (kind=4) :: x100
  real (kind=4) :: x101
  real (kind=4) :: x110
  real (kind=4) :: x111
  real (kind=4) :: xfat((l-1)*(lfat+1)+1,(m-1)*(mfat+1)+1,(n-1)*(nfat+1)+1)

  do i = 1, l

    if ( i < l ) then
      ihi = lfat
    else
      ihi = 0
    end if

    do j = 1, m

      if ( j < m ) then
        jhi = mfat
      else
        jhi = 0
      end if

      do k = 1, n

        if ( k < n ) then
          khi = nfat
        else
          khi = 0
        end if

        if ( i < l ) then
          ip1 = i + 1
        else
          ip1 = i
        end if

        if ( j < m ) then
          jp1 = j + 1
        else
          jp1 = j
        end if

        if ( k < n ) then
          kp1 = k + 1
        else
          kp1 = k
        end if

        x000 = x(i,j,k)
        x001 = x(i,j,kp1)
        x100 = x(ip1,j,k)
        x101 = x(ip1,j,kp1)
        x010 = x(i,jp1,k)
        x011 = x(i,jp1,kp1)
        x110 = x(ip1,jp1,k)
        x111 = x(ip1,jp1,kp1)

        do ii = 0, ihi

          r = real ( ii,      kind = 4 ) &
            / real ( ihi + 1, kind = 4 )

          do jj = 0, jhi

            s = real ( jj,      kind = 4 ) &
              / real ( jhi + 1, kind = 4 )

            do kk = 0, khi

              t = real ( kk,      kind = 4 ) &
                / real ( khi + 1, kind = 4 )

              iii = 1 + ( i - 1 ) * ( lfat + 1 ) + ii
              jjj = 1 + ( j - 1 ) * ( mfat + 1 ) + jj
              kkk = 1 + ( k - 1 ) * ( nfat + 1 ) + kk

              xfat(iii,jjj,kkk) = &
                  x000 * ( 1.0E+00 - r ) * ( 1.0E+00 - s ) * ( 1.0E+00 - t ) &
                + x001 * ( 1.0E+00 - r ) * ( 1.0E+00 - s ) * (           t ) &
                + x010 * ( 1.0E+00 - r ) * (           s ) * ( 1.0E+00 - t ) &
                + x011 * ( 1.0E+00 - r ) * (           s ) * (           t ) &
                + x100 * (           r ) * ( 1.0E+00 - s ) * ( 1.0E+00 - t ) &
                + x101 * (           r ) * ( 1.0E+00 - s ) * (           t ) &
                + x110 * (           r ) * (           s ) * ( 1.0E+00 - t ) &
                + x111 * (           r ) * (           s ) * (           t )

            end do

          end do

        end do

      end do

    end do

  end do

  return
end
subroutine r4block_print ( l, m, n, a, title )

!*****************************************************************************80
!
!! R4BLOCK_PRINT prints an R4BLOCK.
!
!  Discussion:
!
!    An R4BLOCK is a 3D array of R4 values.
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
!    Input, integer (kind=4) :: L, M, N, the dimensions of the block.
!
!    Input, real (kind=4) :: A(L,M,N), the matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ) l
  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) n

  real      ( kind = 4 ) a(l,m,n)
  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) j
  integer   ( kind = 4 ) jhi
  integer   ( kind = 4 ) jlo
  integer   ( kind = 4 ) k
  character ( len = * )  title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  do k = 1, n

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  K = ', k
    write ( *, '(a)' ) ' '

    do jlo = 1, m, 5
      jhi = min ( jlo + 4, m )
      write ( *, '(a)' ) ' '
      write ( *, '(10x,5(i8,6x))' ) (j, j = jlo, jhi )
      write ( *, '(a)' ) ' '
      do i = 1, l
        write ( *, '(2x,i8,5g14.6)' ) i, a(i,jlo:jhi,k)
      end do
    end do

  end do

  return
end









end module jburk_r4_
