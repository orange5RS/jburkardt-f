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
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
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
!    An I4 is an integer ( kind = 4 ) value.
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
!    Input, integer ( kind = 4 ) I, the number whose logarithm base 10
!    is desired.
!
!    Output, integer ( kind = 4 ) I4_LOG_10, the integer part of the
!    logarithm base 10 of the absolute value of X.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_abs
  integer ( kind = 4 ) i4_log_10
  integer ( kind = 4 ) ten_pow

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
function i4_modp ( i, j )

!*****************************************************************************80
!
!! I4_MODP returns the nonnegative remainder of I4 division.
!
!  Discussion:
!
!    If
!      NREM = I4_MODP ( I, J )
!      NMULT = ( I - NREM ) / J
!    then
!      I = J * NMULT + NREM
!    where NREM is always nonnegative.
!
!    The MOD function computes a result with the same sign as the
!    quantity being divided.  Thus, suppose you had an angle A,
!    and you wanted to ensure that it was between 0 and 360.
!    Then mod(A,360) would do, if A was positive, but if A
!    was negative, your result would be between -360 and 0.
!
!    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Example:
!
!        I     J     MOD I4_MODP    Factorization
!
!      107    50       7       7    107 =  2 *  50 + 7
!      107   -50       7       7    107 = -2 * -50 + 7
!     -107    50      -7      43   -107 = -3 *  50 + 43
!     -107   -50      -7      43   -107 =  3 * -50 + 43
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the number to be divided.
!
!    Input, integer ( kind = 4 ) J, the number that divides I.
!
!    Output, integer ( kind = 4 ) I4_MODP, the nonnegative remainder when I is
!    divided by J.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) j
  integer ( kind = 4 ) value

  if ( j == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_MODP - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal divisor J = ', j
    stop
  end if

  value = mod ( i, j )

  if ( value < 0 ) then
    value = value + abs ( j )
  end if

  i4_modp = value

  return
end
function i4_uniform_ab ( a, b, seed )

!*****************************************************************************80
!
!! I4_UNIFORM_AB returns a scaled pseudorandom I4.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ) value.
!
!    The pseudorandom number will be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2006
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
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) I4_UNIFORM_AB, a number between A and B.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  real ( kind = 4 ) r
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) value

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r = real ( seed, kind = 4 ) * 4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
  r = ( 1.0E+00 - r ) * ( real ( min ( a, b ), kind = 4 ) - 0.5E+00 ) &
    +             r   * ( real ( max ( a, b ), kind = 4 ) + 0.5E+00 )
!
!  Use rounding to convert R to an integer between A and B.
!
  value = nint ( r, kind = 4 )

  value = max ( value, min ( a, b ) )
  value = min ( value, max ( a, b ) )

  i4_uniform_ab = value

  return
end
function i4_wrap ( ival, ilo, ihi )

!*****************************************************************************80
!
!! I4_WRAP forces an I4 to lie between given limits by wrapping.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ) value.
!
!    There appears to be a bug in the GFORTRAN compiler which can lead to
!    erroneous results when the first argument of I4_WRAP is an expression.
!    In particular:
!
!    do i = 1, 3
!      if ( test ) then
!        i4 = i4_wrap ( i + 1, 1, 3 )
!      end if
!    end do
!
!    was, when I = 3, returning I4 = 3.  So I had to replace this with
!
!    do i = 1, 3
!      if ( test ) then
!        i4 = i + 1
!        i4 = i4_wrap ( i4, 1, 3 )
!      end if
!    end do
!
!  Example:
!
!    ILO = 4, IHI = 8
!
!    I  Value
!
!    -2     8
!    -1     4
!     0     5
!     1     6
!     2     7
!     3     8
!     4     4
!     5     5
!     6     6
!     7     7
!     8     8
!     9     4
!    10     5
!    11     6
!    12     7
!    13     8
!    14     4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IVAL, a value.
!
!    Input, integer ( kind = 4 ) ILO, IHI, the desired bounds.
!
!    Output, integer ( kind = 4 ) I4_WRAP, a "wrapped" version of the value.
!
  implicit none

  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) i4_wrap
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) value
  integer ( kind = 4 ) wide

  jlo = min ( ilo, ihi )
  jhi = max ( ilo, ihi )

  wide = jhi - jlo + 1

  if ( wide == 1 ) then
    value = jlo
  else
    value = jlo + i4_modp ( ival - jlo, wide )
  end if

  i4_wrap = value

  return
end
subroutine i4int_to_r8int ( imin, imax, i, rmin, rmax, r )

!*****************************************************************************80
!
!! I4INT_TO_R8INT maps an I4INT to an R8INT.
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
!    Input, integer ( kind = 4 ) IMIN, IMAX, the range.
!
!    Input, integer ( kind = 4 ) I, the integer to be converted.
!
!    Input, real ( kind = 8 ) RMIN, RMAX, the range.
!
!    Output, real ( kind = 8 ) R, the corresponding value in [RMIN,RMAX].
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) imax
  integer ( kind = 4 ) imin
  real ( kind = 8 ) r
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmin

  if ( imax == imin ) then

    r = 0.5D+00 * ( rmin + rmax )

  else

    r = ( real ( imax - i,        kind = 8 ) * rmin   &
        + real (        i - imin, kind = 8 ) * rmax ) &
        / real ( imax     - imin, kind = 8 )

  end if

  return
end


subroutine legendre_zeros ( n, x )

!*****************************************************************************80
!
!! LEGENDRE_ZEROS computes the zeros of the Legendre polynomial of degree N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 June 2011
!
!  Author:
!
!    Original FORTRAN77 version by Philip Davis, Philip Rabinowitz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order.
!    0 < N.
!
!    Output, real ( kind = 8 ) X(N), the locations of the zeros.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) d1
  real ( kind = 8 ) d2pn
  real ( kind = 8 ) d3pn
  real ( kind = 8 ) d4pn
  real ( kind = 8 ) dp
  real ( kind = 8 ) dpn
  real ( kind = 8 ) e1
  real ( kind = 8 ) fx
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iback
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mp1mi
  integer ( kind = 4 ) ncopy
  integer ( kind = 4 ) nmove
  real ( kind = 8 ) p
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) pk
  real ( kind = 8 ) pkm1
  real ( kind = 8 ) pkp1
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x0
  real ( kind = 8 ) xtemp

  e1 = real ( n * ( n + 1 ), kind = 8 )

  m = ( n + 1 ) / 2

  do i = 1, m

    mp1mi = m + 1 - i

    t = real ( 4 * i - 1, kind = 8 ) * pi &
      / real ( 4 * n + 2, kind = 8 )

    x0 = cos ( t ) * ( 1.0D+00 - ( 1.0D+00 - 1.0D+00 &
      / real ( n, kind = 8 ) ) &
      / real ( 8 * n * n, kind = 8 ) )

    pkm1 = 1.0D+00
    pk = x0

    do k = 2, n
      pkp1 = 2.0D+00 * x0 * pk - pkm1 - ( x0 * pk - pkm1 ) &
        / real ( k, kind = 8 )
      pkm1 = pk
      pk = pkp1
    end do

    d1 = real ( n, kind = 8 ) * ( pkm1 - x0 * pk )

    dpn = d1 / ( 1.0D+00 - x0 ) / ( 1.0D+00 + x0 )

    d2pn = ( 2.0D+00 * x0 * dpn - e1 * pk ) / ( 1.0D+00 - x0 ) &
      / ( 1.0D+00 + x0 )

    d3pn = ( 4.0D+00 * x0 * d2pn + ( 2.0D+00 - e1 ) * dpn ) &
      / ( 1.0D+00 - x0 ) / ( 1.0D+00 + x0 )

    d4pn = ( 6.0D+00 * x0 * d3pn + ( 6.0D+00 - e1 ) * d2pn ) &
      / ( 1.0D+00 - x0 ) / ( 1.0D+00 + x0 )

    u = pk / dpn
    v = d2pn / dpn
!
!  Initial approximation H:
!
    h = - u * ( 1.0D+00 + 0.5D+00 * u * ( v + u * ( v * v - d3pn / &
      ( 3.0D+00 * dpn ) ) ) )
!
!  Refine H using one step of Newton's method:
!
    p = pk + h * ( dpn + 0.5D+00 * h * ( d2pn + h / 3.0D+00 &
      * ( d3pn + 0.25D+00 * h * d4pn ) ) )

    dp = dpn + h * ( d2pn + 0.5D+00 * h * ( d3pn + h * d4pn / 3.0D+00 ) )

    h = h - p / dp

    xtemp = x0 + h

    x(mp1mi) = xtemp

    fx = d1 - h * e1 * ( pk + 0.5D+00 * h * ( dpn + h / 3.0D+00 &
      * ( d2pn + 0.25D+00 * h * ( d3pn + 0.2D+00 * h * d4pn ) ) ) )

  end do

  if ( mod ( n, 2 ) == 1 ) then
    x(1) = 0.0D+00
  end if
!
!  Shift the data up.
!
  nmove = ( n + 1 ) / 2
  ncopy = n - nmove

  do i = 1, nmove
    iback = n + 1 - i
    x(iback) = x(iback-ncopy)
  end do
!
!  Reflect values for the negative abscissas.
!
  do i = 1, n - nmove
    x(i) = - x(n+1-i)
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
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Input, integer ( kind = 4 ) P(N), the array to check.
!
!    Input, integer ( kind = 4 ) BASE, the index base.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, the array represents a permutation.
!    nonzero, the array does not represent a permutation.  The smallest
!    missing value is equal to IERROR.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) base
  integer ( kind = 4 ) find
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seek

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
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects to be permuted.
!
!    Input, integer ( kind = 4 ) BASE, is 0 for a 0-based permutation and 1 for
!    a 1-based permutation.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) P(N), the permutation.  P(I) is the "new"
!    location of the object originally at I.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) base
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seed

  do i = 1, n
    p(i) = ( i - 1 ) + base
  end do

  do i = 1, n
    j = i4_uniform_ab ( i, n, seed )
    k    = p(i)
    p(i) = p(j)
    p(j) = k
  end do

  return
end


subroutine r8_mant ( x, s, r, l )

!*****************************************************************************80
!
!! R8_MANT computes the "mantissa" or "fraction part" of an R8.
!
!  Discussion:
!
!    X = S * R * 2^L
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
!    Input, real ( kind = 8 ) X, the number to be decomposed.
!
!    Output, integer ( kind = 4 ) S, the "sign" of the number.
!    S will be -1 if X is less than 0, and +1 if X is greater
!    than or equal to zero.
!
!    Output, real ( kind = 8 ) R, the mantissa of X.  R will be greater
!    than or equal to 1, and strictly less than 2.  The one
!    exception occurs if X is zero, in which case R will also
!    be zero.
!
!    Output, integer ( kind = 4 ) L, the integer part of the logarithm
!    (base 2) of X.
!
  implicit none

  integer ( kind = 4 ) l
  real ( kind = 8 ) r
  integer ( kind = 4 ) s
  real ( kind = 8 ) x
!
!  Determine the sign.
!
  if ( x < 0.0D+00 ) then
    s = -1
  else
    s = + 1
  end if
!
!  Set R to the absolute value of X, and L to zero.
!  Then force R to lie between 1 and 2.
!
  if ( x < 0.0D+00 ) then
    r = - x
  else
    r = + x
  end if

  l = 0
!
!  Time to bail out if X is zero.
!
  if ( x == 0.0D+00 ) then
    return
  end if

  do while ( 2.0D+00 <= r )
    r = r / 2.0D+00
    l = l + 1
  end do

  do while ( r < 1.0D+00 )
    r = r * 2.0D+00
    l = l - 1
  end do

  return
end
function r8_mod ( x, y )

!*****************************************************************************80
!
!! R8_MOD returns the remainder of R8 division.
!
!  Discussion:
!
!    If
!      REM = R8_MOD ( X, Y )
!      RMULT = ( X - REM ) / Y
!    then
!      X = Y * RMULT + REM
!    where REM has the same sign as X, and abs ( REM ) < Y.
!
!  Example:
!
!        X         Y     R8_MOD  R8_MOD Factorization
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
!    Input, real ( kind = 8 ) X, the number to be divided.
!
!    Input, real ( kind = 8 ) Y, the number that divides X.
!
!    Output, real ( kind = 8 ) R8_MOD, the remainder when X is divided by Y.
!
  implicit none

  real ( kind = 8 ) r8_mod
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( y == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_MOD - Fatal error!'
    write ( *, '(a,g14.6)' ) '  R8_MOD ( X, Y ) called with Y = ', y
    stop
  end if

  r8_mod = x - real ( int ( x / y ), kind = 8 ) * y

  if ( x < 0.0D+00 .and. 0.0D+00 < r8_mod ) then
    r8_mod = r8_mod - abs ( y )
  else if ( 0.0D+00 < x .and. r8_mod < 0.0D+00 ) then
    r8_mod = r8_mod + abs ( y )
  end if

  return
end
function r8_modp ( x, y )

!*****************************************************************************80
!
!! R8_MODP returns the nonnegative remainder of R8 division.
!
!  Discussion:
!
!    If
!      REM = R8_MODP ( X, Y )
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
!    On the other hand, R8_MODP(A,360.0) is between 0 and 360, always.
!
!  Example:
!
!        X         Y     MOD R8_MODP  R8_MODP Factorization
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
!    19 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the number to be divided.
!
!    Input, real ( kind = 8 ) Y, the number that divides X.
!
!    Output, real ( kind = 8 ) R8_MODP, the nonnegative remainder
!    when X is divided by Y.
!
  implicit none

  real ( kind = 8 ) r8_modp
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( y == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_MODP - Fatal error!'
    write ( *, '(a,g14.6)' ) '  R8_MODP ( X, Y ) called with Y = ', y
    stop
  end if

  r8_modp = mod ( x, y )

  if ( r8_modp < 0.0D+00 ) then
    r8_modp = r8_modp + abs ( y )
  end if

  return
end
function r8_mop ( i )

!*****************************************************************************80
!
!! R8_MOP returns the I-th power of -1 as an R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
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
!    Input, integer ( kind = 4 ) I, the power of -1.
!
!    Output, real ( kind = 8 ) R8_MOP, the I-th power of -1.
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_mop

  if ( mod ( i, 2 ) == 0 ) then
    r8_mop = + 1.0D+00
  else
    r8_mop = - 1.0D+00
  end if

  return
end
function r8_nint ( x )

!*****************************************************************************80
!
!! R8_NINT returns the nearest integer to an R8.
!
!  Example:
!
!        X        R8_NINT
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
!    08 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value.
!
!    Output, integer ( kind = 4 ) R8_NINT, the nearest integer to X.
!
  implicit none

  integer ( kind = 4 ) r8_nint
  integer ( kind = 4 ) s
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    s = - 1
  else
    s = + 1
  end if

  r8_nint = s * int ( abs ( x ) + 0.5D+00 )

  return
end
function r8_normal ( a, b, seed )

!*****************************************************************************80
!
!! R8_NORMAL returns a scaled pseudonormal R8.
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
!    17 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the mean of the PDF.
!
!    Input, real ( kind = 8 ) B, the standard deviation of the PDF.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) R8_NORMAL, a sample of the normal PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_normal
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ), save :: seed2 = 0
  integer ( kind = 4 ), save :: used = 0
  real ( kind = 8 ) x
  real ( kind = 8 ), save :: y = 0.0D+00
!
!  On odd numbered calls, generate two uniforms, create two normals,
!  return the first normal and its corresponding seed.
!
  if ( mod ( used, 2 ) == 0 ) then

    r1 = r8_uniform_01 ( seed )

    if ( r1 == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_NORMAL - Fatal error!'
      write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
      stop
    end if

    seed2 = seed
    r2 = r8_uniform_01 ( seed2 )

    x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * pi * r2 )
    y = sqrt ( - 2.0D+00 * log ( r1 ) ) * sin ( 2.0D+00 * pi * r2 )
!
!  On odd calls, return the second normal and its corresponding seed.
!
  else

    seed = seed2
    x = y

  end if

  used = used + 1

  r8_normal = a + b * x

  return
end
function r8_normal_01 ( seed )

!*****************************************************************************80
!
!! R8_NORMAL_01 returns a unit pseudonormal R8.
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
!    17 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) R8_NORMAL_01, a sample of the standard
!    normal PDF.
!
  implicit none

  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_normal_01
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ), save :: seed2 = 0
  integer ( kind = 4 ), save :: used = 0
  real ( kind = 8 ) x
  real ( kind = 8 ), save :: y = 0.0D+00
!
!  On odd numbered calls, generate two uniforms, create two normals,
!  return the first normal and its corresponding seed.
!
  if ( mod ( used, 2 ) == 0 ) then

    r1 = r8_uniform_01 ( seed )

    if ( r1 == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_NORMAL_01 - Fatal error!'
      write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
      stop
    end if

    seed2 = seed
    r2 = r8_uniform_01 ( seed2 )

    x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * pi * r2 )
    y = sqrt ( - 2.0D+00 * log ( r1 ) ) * sin ( 2.0D+00 * pi * r2 )
!
!  On odd calls, return the second normal and its corresponding seed.
!
  else

    seed = seed2
    x = y

  end if

  used = used + 1

  r8_normal_01 = x

  return
end
function r8_pi ( )

!*****************************************************************************80
!
!! R8_PI returns the value of pi as an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_PI, the value of pi.
!
  implicit none

  real ( kind = 8 ) r8_pi

  r8_pi = 3.141592653589793D+00

  return
end
function r8_pi_sqrt ( )

!*****************************************************************************80
!
!! R8_PI_SQRT returns the square root of pi as an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_PI_SQRT, the square root of pi.
!
  implicit none

  real ( kind = 8 ) r8_pi_sqrt

  r8_pi_sqrt = 1.7724538509055160273D+00

  return
end
function r8_power ( r, p )

!*****************************************************************************80
!
!! R8_POWER computes the P-th power of an R8.
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
!    Input, real ( kind = 8 ) R, the base.
!
!    Input, integer ( kind = 4 ) P, the power, which may be negative.
!
!    Output, real ( kind = 8 ) R8_POWER, the value of the P-th power of R.
!
  implicit none

  integer ( kind = 4 ) p
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_power
  real ( kind = 8 ) value
!
!  Special case.  R^0 = 1.
!
  if ( p == 0 ) then

    value = 1.0D+00
!
!  Special case.  Positive powers of 0 are 0.
!  For negative powers of 0, we go ahead and compute R^P,
!  relying on the software to complain.
!
  else if ( r == 0.0D+00 ) then

    if ( 0 < p ) then
      value = 0.0D+00
    else
      value = r**p
    end if

  else if ( 1 <= p ) then
    value = r**p
  else
    value = 1.0D+00 / r**(-p)
  end if

  r8_power = value

  return
end
subroutine r8_power_fast ( r, p, rp, mults )

!*****************************************************************************80
!
!! R8_POWER_FAST computes an integer power of an R8.
!
!  Discussion:
!
!    Obviously, R^P can be computed using P-1 multiplications.
!
!    However, R^P can also be computed using at most 2*LOG2(P) multiplications.
!    To do the calculation this way, let N = LOG2(P).
!    Compute A, A^2, A^4, ..., A^N by N-1 successive squarings.
!    Start the value of R^P at A, and each time that there is a 1 in
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
!    Input, real ( kind = 8 ) R, the base.
!
!    Input, integer ( kind = 4 ) P, the power, which may be negative.
!
!    Output, real ( kind = 8 ) RP, the value of R^P.
!
!    Output, integer ( kind = 4 ) MULTS, the number of multiplications
!    and divisions.
!
  implicit none

  integer ( kind = 4 ) mults
  integer ( kind = 4 ) p
  integer ( kind = 4 ) p_mag
  integer ( kind = 4 ) p_sign
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  real ( kind = 8 ) rp

  mults = 0
!
!  Special bases.
!
  if ( r == 1.0D+00 ) then
    rp = 1.0D+00
    return
  end if

  if ( r == -1.0D+00 ) then

    if ( mod ( p, 2 ) == 1 ) then
      rp = -1.0D+00
    else
      rp = 1.0D+00
    end if

    return

  end if

  if ( r == 0.0D+00 ) then

    if ( p <= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_POWER_FAST - Fatal error!'
      write ( *, '(a)' ) '  Base R is zero, and exponent is negative.'
      write ( *, '(a,i8)' ) '  Exponent P = ', p
      stop
    end if

    rp = 0.0D+00
    return

  end if
!
!  Special powers.
!
  if ( p == -1 ) then
    rp = 1.0D+00 / r
    mults = mults + 1
    return
  else if ( p == 0 ) then
    rp = 1.0D+00
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

  rp = 1.0D+00
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
    rp = 1.0D+00 / rp
    mults = mults + 1
  end if

  return
end
function r8_pythag ( a, b )

!*****************************************************************************80
!
!! R8_PYTHAG computes sqrt ( A * A + B * B ), avoiding overflow and underflow.
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
!    Input, real ( kind = 8 ) A, B, the values for which sqrt ( A * A + B * B )
!    is desired.
!
!    Output, real ( kind = 8 ) R8_PYTHAG, the value of sqrt ( A * A + B * B ).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a_abs
  real ( kind = 8 ) b
  real ( kind = 8 ) b_abs
  real ( kind = 8 ) r8_pythag

  a_abs = abs ( a )
  b_abs = abs ( b )

  if ( b_abs < a_abs ) then
    r8_pythag = a_abs * sqrt ( 1.0D+00 + ( b_abs / a_abs ) * ( b_abs / a_abs ) )
  else if ( b_abs == 0.0D+00 ) then
    r8_pythag = 0.0D+00
  else if ( a_abs <= b_abs ) then
    r8_pythag = b_abs * sqrt ( 1.0D+00 + ( a_abs / b_abs ) * ( a_abs / b_abs ) )
  end if

  return
end
function r8_round ( x )

!*****************************************************************************80
!
!! R8_ROUND sets an R8 to the nearest integral value.
!
!  Example:
!
!        X        R8_ROUND
!
!      1.3         1.0
!      1.4         1.0
!      1.5         1.0 or 2.0
!      1.6         2.0
!      0.0         0.0
!     -0.7        -1.0
!     -1.1        -1.0
!     -1.6        -2.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value.
!
!    Output, real ( kind = 8 ) R8_ROUND, the rounded value.
!
  implicit none

  real ( kind = 8 ) r8_round
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    value = - real ( int ( - x + 0.5D+00 ), kind = 8 )
  else
    value =   real ( int ( + x + 0.5D+00 ), kind = 8 )
  end if

  r8_round = value

  return
end
subroutine r8_round2 ( nplace, x, xround )

!*****************************************************************************80
!
!! R8_ROUND2 rounds an R8 in base 2.
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
!    Input, integer ( kind = 4 ) NPLACE, the number of binary digits to
!    preserve.  NPLACE should be 0 or positive.
!
!    Input, real ( kind = 8 ) X, the number to be decomposed.
!
!    Output, real ( kind = 8 ) XROUND, the rounded value of X.
!
  implicit none

  integer ( kind = 4 ) iplace
  integer ( kind = 4 ) l
  integer ( kind = 4 ) nplace
  integer ( kind = 4 ) s
  real ( kind = 8 ) x
  real ( kind = 8 ) xmant
  real ( kind = 8 ) xround
  real ( kind = 8 ) xtemp

  xround = 0.0D+00
!
!  1: Handle the special case of 0.
!
  if ( x == 0.0D+00 ) then
    return
  end if

  if ( nplace <= 0 ) then
    return
  end if
!
!  2: Determine the sign S.
!
  if ( 0.0D+00 < x ) then
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

  do while ( 2.0D+00 <= xtemp )
    xtemp = xtemp / 2.0D+00
    l = l + 1
  end do

  do while ( xtemp < 1.0D+00 )
    xtemp = xtemp * 2.0D+00
    l = l - 1
  end do
!
!  4: Strip out the digits of the mantissa as XMANT, and decrease L.
!
  xmant = 0.0D+00
  iplace = 0

  do

    xmant = 2.0D+00 * xmant

    if ( 1.0D+00 <= xtemp ) then
      xmant = xmant + 1.0D+00
      xtemp = xtemp - 1.0D+00
    end if

    iplace = iplace + 1

    if ( xtemp == 0.0D+00 .or. nplace <= iplace ) then
      xround = s * xmant * 2.0D+00**l
      exit
    end if

    l = l - 1
    xtemp = xtemp * 2.0D+00

  end do

  return
end
subroutine r8_roundb ( base, nplace, x, xround )

!*****************************************************************************80
!
!! R8_ROUNDB rounds an R8 in a given base.
!
!  Discussion:
!
!    The code does not seem to do a good job of rounding when
!    the base is negative.
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
!    Input, integer ( kind = 4 ) BASE, the base of the arithmetic.
!    BASE must not be zero.  Theoretically, BASE may be negative.
!
!    Input, integer ( kind = 4 ) NPLACE, the number of digits base BASE to
!    preserve.  NPLACE should be 0 or positive.
!
!    Input, real ( kind = 8 ) X, the number to be decomposed.
!
!    Output, real ( kind = 8 ) XROUND, the rounded value of X.
!
  implicit none

  integer ( kind = 4 ) base
  integer ( kind = 4 ) iplace
  integer ( kind = 4 ) is
  integer ( kind = 4 ) js
  integer ( kind = 4 ) l
  integer ( kind = 4 ) nplace
  real ( kind = 8 ) x
  real ( kind = 8 ) xmant
  real ( kind = 8 ) xround
  real ( kind = 8 ) xtemp

  xround = 0.0D+00
!
!  0: Error checks.
!
  if ( base == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_ROUNDB - Fatal error!'
    write ( *, '(a)' ) '  The base BASE cannot be zero.'
    stop
  end if
!
!  1: Handle the special case of 0.
!
  if ( x == 0.0D+00 ) then
    return
  end if

  if ( nplace <= 0 ) then
    return
  end if
!
!  2: Determine the sign IS.
!
  if ( 0.0D+00 < x ) then
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

    xtemp = xtemp / real ( base, kind = 8 )

    if ( xtemp < 0.0D+00 ) then
      is = -is
      xtemp = -xtemp
    end if

    l = l + 1

  end do

  do while ( abs ( xtemp ) < 1.0D+00 )

    xtemp = xtemp * base

    if ( xtemp < 0.0D+00 ) then
      is = -is
      xtemp = -xtemp
    end if

    l = l - 1

  end do
!
!  4: Now strip out the digits of the mantissa as XMANT, and
!  decrease L.
!
  xmant = 0.0D+00
  iplace = 0
  js = is

  do

    xmant = base * xmant

    if ( xmant < 0.0D+00 ) then
      js = -js
      xmant = -xmant
    end if

    if ( 1.0D+00 <= xtemp ) then
      xmant = xmant + int ( xtemp )
      xtemp = xtemp - int ( xtemp )
    end if

    iplace = iplace + 1

    if ( xtemp == 0.0D+00 .or. nplace <= iplace ) then
      xround = js * xmant * ( real ( base, kind = 8 ) )**l
      exit
    end if

    l = l - 1
    xtemp = xtemp * base

    if ( xtemp < 0.0D+00 ) then
      is = -is
      xtemp = -xtemp
    end if

  end do

  return
end
subroutine r8_roundx ( nplace, x, xround )

!*****************************************************************************80
!
!! R8_ROUNDX rounds an R8 in base 10.
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
!    Input, integer ( kind = 4 ) NPLACE, the number of decimal digits to
!    preserve.  NPLACE should be 0 or positive.
!
!    Input, real ( kind = 8 ) X, the number to be decomposed.
!
!    Output, real ( kind = 8 ) XROUND, the rounded value of X.
!
  implicit none

  integer ( kind = 4 ) iplace
  integer ( kind = 4 ) is
  integer ( kind = 4 ) l
  integer ( kind = 4 ) nplace
  real ( kind = 8 ) x
  real ( kind = 8 ) xmant
  real ( kind = 8 ) xround
  real ( kind = 8 ) xtemp

  xround = 0.0D+00
!
!  1: Handle the special case of 0.
!
  if ( x == 0.0D+00 ) then
    return
  end if

  if ( nplace <= 0 ) then
    return
  end if
!
!  2: Determine the sign IS.
!
  if ( 0.0D+00 < x ) then
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

  do while ( 10.0D+00 <= x )
    xtemp = xtemp / 10.0D+00
    l = l + 1
  end do

  do while ( xtemp < 1.0D+00 )
    xtemp = xtemp * 10.0D+00
    l = l - 1
  end do
!
!  4: Now strip out the digits of the mantissa as XMANT, and
!  decrease L.
!
  xmant = 0.0D+00
  iplace = 0

  do

    xmant = 10.0D+00 * xmant

    if ( 1.0D+00 <= xtemp ) then
      xmant = xmant + int ( xtemp )
      xtemp = xtemp - int ( xtemp )
    end if

    iplace = iplace + 1

    if ( xtemp == 0.0D+00 .or. nplace <= iplace ) then
      xround = is * xmant * ( 10.0D+00**l )
      exit
    end if

    l = l - 1
    xtemp = xtemp * 10.0D+00

  end do

  return
end
function r8_sech ( x )

!*****************************************************************************80
!
!! R8_SECH evaluates the hyperbolic secant, while avoiding COSH overflow.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) R8_SECH, the value of the function.
!
  implicit none

  real ( kind = 8 ) :: log_huge = 80.0D+00
  real ( kind = 8 ) r8_sech
  real ( kind = 8 ) x

  if ( log_huge < abs ( x ) ) then
    r8_sech = 0.0D+00
  else
    r8_sech = 1.0D+00 / cosh ( x )
  end if

  return
end
function r8_sign ( x )

!*****************************************************************************80
!
!! R8_SIGN returns the sign of an R8.
!
!  Discussion:
!
!    value = -1 if X < 0;
!    value = +1 if X => 0.
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
!    Input, real ( kind = 8 ) X, the number whose sign is desired.
!
!    Output, real ( kind = 8 ) R8_SIGN, the sign of X:
!
  implicit none

  real ( kind = 8 ) r8_sign
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    r8_sign = -1.0D+00
  else
    r8_sign = +1.0D+00
  end if

  return
end
function r8_sign_char ( x )

!*****************************************************************************80
!
!! R8_SIGN_CHAR returns a character indicating the sign of an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the number whose sign is desired.
!
!    Output, character R8_SIGN_CHAR, the sign of X, '-', '0' or '+'.
!
  implicit none

  character r8_sign_char
  character value
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    value = '-'
  else if ( x == 0.0D+00 ) then
    value = '0'
  else
    value = '+'
  end if

  r8_sign_char = value

  return
end
function r8_sign_match ( r1, r2 )

!*****************************************************************************80
!
!! R8_SIGN_MATCH is TRUE if two R8's are of the same sign.
!
!  Discussion:
!
!    This test could be coded numerically as
!
!      if ( 0 <= r1 * r2 ) then ...
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R1, R2, the values to check.
!
!    Output, logical R8_SIGN_MATCH, is TRUE if ( R1 <= 0 and R2 <= 0 )
!    or ( 0 <= R1 and 0 <= R2 ).
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  logical r8_sign_match

  r8_sign_match = ( r1 <= 0.0D+00 .and. r2 <= 0.0D+00 ) .or. &
                  ( 0.0D+00 <= r1 .and. 0.0D+00 <= r2 )

  return
end
function r8_sign_match_strict ( r1, r2 )

!*****************************************************************************80
!
!! R8_SIGN_MATCH_STRICT is TRUE if two R8's are of the same strict sign.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R1, R2, the values to check.
!
!    Output, logical R8_SIGN_MATCH_STRICT, is TRUE if the signs match.
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  logical r8_sign_match_strict

  r8_sign_match_strict = &
    (           r1 <  0.0D+00 .and. r2 <  0.0D+00 ) .or. &
    (           r1 == 0.0D+00 .and. r2 == 0.0D+00 ) .or. &
    ( 0.0D+00 < r1            .and.       0.0D+00 < r2 )

  return
end
function r8_sign_opposite ( r1, r2 )

!*****************************************************************************80
!
!! R8_SIGN_OPPOSITE is TRUE if two R8's are not of the same sign.
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
!    Input, real ( kind = 8 ) R1, R2, the values to check.
!
!    Output, logical R8_SIGN_OPPOSITE, is TRUE if ( R1 <= 0 and 0 <= R2 )
!    or ( R2 <= 0 and 0 <= R1 ).
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  logical r8_sign_opposite

  r8_sign_opposite = ( r1 <= 0.0D+00 .and. 0.0D+00 <= r2 ) .or. &
                     ( r2 <= 0.0D+00 .and. 0.0D+00 <= r1 )

  return
end
function r8_sign_opposite_strict ( r1, r2 )

!*****************************************************************************80
!
!! R8_SIGN_OPPOSITE_STRICT is TRUE if two R8's are strictly of opposite sign.
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
!    Input, real ( kind = 8 ) R1, R2, the values to check.
!
!    Output, logical R8_SIGN_OPPOSITE_STRICT, is TRUE if ( R1 < 0 and 0 < R2 )
!    or ( R2 < 0 and 0 < R1 ).
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  logical r8_sign_opposite_strict

  r8_sign_opposite_strict = ( r1 < 0.0D+00 .and. 0.0D+00 < r2 ) .or. &
                            ( r2 < 0.0D+00 .and. 0.0D+00 < r1 )

  return
end
function r8_sqrt_i4 ( i )

!*****************************************************************************80
!
!! R8_SQRT_I4 returns the square root of an I4 as an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 June 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the number whose square root is desired.
!
!    Output, real ( kind = 8 ) R8_SQRT_I4, the value of sqrt(I).
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_sqrt_i4

  r8_sqrt_i4 = sqrt ( real ( i, kind = 8 ) )

  return
end
subroutine r8_swap ( x, y )

!*****************************************************************************80
!
!! R8_SWAP swaps two R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) X, Y.  On output, the values of X and
!    Y have been interchanged.
!
  implicit none

  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  z = x
  x = y
  y = z

  return
end
subroutine r8_swap3 ( x, y, z )

!*****************************************************************************80
!
!! R8_SWAP3 swaps three R8's.
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
!    Input/output, real ( kind = 8 ) X, Y, Z, three values to be swapped.
!
  implicit none

  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  w = x
  x = y
  y = z
  z = w

  return
end
function r8_tiny ( )

!*****************************************************************************80
!
!! R8_TINY returns a very small but positive R8.
!
!  Discussion:
!
!    FORTRAN90 provides a built-in routine TINY ( X ) that
!    is more suitable for this purpose, returning the smallest positive
!    but normalized real number.
!
!    This routine does NOT try to provide an accurate value for TINY.
!    Instead, it simply returns a "reasonable" value, that is, a rather
!    small, but representable, real number.
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
!    Output, real ( kind = 8 ) R8_TINY, a "tiny" value.
!
  implicit none

  real ( kind = 8 ) r8_tiny

  r8_tiny = 1.0D-30

  return
end
subroutine r8_to_r8_discrete ( r, rmin, rmax, nr, rd )

!*****************************************************************************80
!
!! R8_TO_R8_DISCRETE maps R to RD in [RMIN, RMAX] with NR possible values.
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
!    Input, real ( kind = 8 ) R, the number to be converted.
!
!    Input, real ( kind = 8 ) RMAX, RMIN, the maximum and minimum
!    values for RD.
!
!    Input, integer ( kind = 4 ) NR, the number of allowed values for XD.
!    NR should be at least 1.
!
!    Output, real ( kind = 8 ) RD, the corresponding discrete value.
!
  implicit none

  integer ( kind = 4 ) f
  integer ( kind = 4 ) nr
  real ( kind = 8 ) r
  real ( kind = 8 ) rd
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmin
!
!  Check for errors.
!
  if ( nr < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_TO_R8_DISCRETE - Fatal error!'
    write ( *, '(a,i8)' ) '  NR = ', nr
    write ( *, '(a)' ) '  but NR must be at least 1.'
    stop
  end if

  if ( nr == 1 ) then
    rd = 0.5D+00 * ( rmin + rmax )
    return
  end if

  if ( rmax == rmin ) then
    rd = rmax
    return
  end if

  f = nint ( real ( nr, kind = 8 ) * ( rmax - r ) / ( rmax - rmin ) )
  f = max ( f, 0 )
  f = min ( f, nr )

  rd = ( real (      f, kind = 8 ) * rmin   &
       + real ( nr - f, kind = 8 ) * rmax ) &
       / real ( nr,     kind = 8 )

  return
end
subroutine r8_to_dhms ( r, d, h, m, s )

!*****************************************************************************80
!
!! R8_TO_DHMS converts decimal days into days, hours, minutes, seconds.
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
!    Input, real ( kind = 8 ) R, a decimal number representing a time
!    period measured in days.
!
!    Output, integer ( kind = 4 ) D, H, M, S, the equivalent number of days,
!    hours, minutes and seconds.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  real ( kind = 8 ) r
  real ( kind = 8 ) r_copy
  integer ( kind = 4 ) s

  r_copy = abs ( r )

  d = int ( r_copy )

  r_copy = r_copy - d
  r_copy = 24.0D+00 * r_copy
  h = int ( r_copy )

  r_copy = r_copy - h
  r_copy = 60.0D+00 * r_copy
  m = int ( r_copy )

  r_copy = r_copy - m
  r_copy = 60.0D+00 * r_copy
  s = int ( r_copy )

  if ( r < 0.0D+00 ) then
    d = -d
    h = -h
    m = -m
    s = -s
  end if

  return
end
subroutine r8_to_i4 ( x, xmin, xmax, ixmin, ixmax, ix )

!*****************************************************************************80
!
!! R8_TO_I4 maps X in [XMIN, XMAX] to integer IX in [IXMIN, IXMAX].
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
!    Input, real ( kind = 8 ) X, the number to be converted.
!
!    Input, real ( kind = 8 ) XMIN, XMAX, the range.  XMAX and
!    XMIN must not be equal.  It is not necessary that XMIN be less than XMAX.
!
!    Input, integer ( kind = 4 ) IXMIN, IXMAX, the allowed range of the output
!    variable.  IXMAX corresponds to XMAX, and IXMIN to XMIN.
!    It is not necessary that IXMIN be less than IXMAX.
!
!    Output, integer ( kind = 4 ) IX, the value in the range [IXMIN,IXMAX] that
!    corresponds to X.
!
  implicit none

  integer ( kind = 4 ) ix
  integer ( kind = 4 ) ixmax
  integer ( kind = 4 ) ixmin
  real ( kind = 8 ) temp
  real ( kind = 8 ) x
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  if ( xmax == xmin ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_TO_I4 - Fatal error!'
    write ( *, '(a)' ) '  XMAX = XMIN, making a zero divisor.'
    write ( *, '(a,g14.6)' ) '  XMAX = ', xmax
    write ( *, '(a,g14.6)' ) '  XMIN = ', xmin
    stop
  end if

  temp = &
      ( ( xmax - x        ) * real ( ixmin, kind = 8 )  &
      + (        x - xmin ) * real ( ixmax, kind = 8 ) ) &
      / ( xmax     - xmin )

  if ( 0.0D+00 <= temp ) then
    temp = temp + 0.5D+00
  else
    temp = temp - 0.5D+00
  end if

  ix = int ( temp )

  return
end
function r8_uniform_ab ( a, b, seed )

!*****************************************************************************80
!
!! R8_UNIFORM_AB returns a scaled pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer variable.
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
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_AB, a number strictly between A and B.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_ab = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
function r8_uniform_01 ( seed )

!*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer variable.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      r8_uniform_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R8_UNIFORM_01
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
!    05 July 2006
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
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0. On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
  implicit none

  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
subroutine r8_unswap3 ( x, y, z )

!*****************************************************************************80
!
!! R8_UNSWAP3 unswaps three R8's.
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
!    Input/output, real ( kind = 8 ) X, Y, Z, three values to be swapped.
!
  implicit none

  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  w = z
  z = y
  y = x
  x = w

  return
end
function r8_walsh_1d ( x, digit )

!*****************************************************************************80
!
!! R8_WALSH_1D evaluates the Walsh function.
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
!    Input, real ( kind = 8 ) X, the argument of the Walsh function.
!
!    Input, integer ( kind = 4 ) DIGIT, the index of the Walsh function.
!
!    Output, real ( kind = 8 ) R8_WALSH_1D, the value of the Walsh function.
!
  implicit none

  integer ( kind = 4 ) digit
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_walsh_1d
  real ( kind = 8 ) x
  real ( kind = 8 ) x_copy
!
!  Hide the effect of the sign of X.
!
  x_copy = abs ( x )
!
!  If DIGIT is positive, divide by 2 DIGIT times.
!  If DIGIT is negative, multiply by 2 (-DIGIT) times.
!
  x_copy = x_copy / 2.0D+00**digit
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
    r8_walsh_1d = 0.0D+00
  else
    r8_walsh_1d = 1.0D+00
  end if

  return
end
function r8_wrap ( r, rlo, rhi )

!*****************************************************************************80
!
!! R8_WRAP forces an R8 to lie between given limits by wrapping.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!  Example:
!
!    RLO = 4.0, RHI = 8.0
!
!     R  Value
!
!    -2     8
!    -1     4
!     0     5
!     1     6
!     2     7
!     3     8
!     4     4
!     5     5
!     6     6
!     7     7
!     8     8
!     9     4
!    10     5
!    11     6
!    12     7
!    13     8
!    14     4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 July 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, a value.
!
!    Input, real ( kind = 8 ) RLO, RHI, the desired bounds.
!
!    Output, real ( kind = 8 ) R8_WRAP, a "wrapped" version of the value.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_wrap
  real ( kind = 8 ) rhi
  real ( kind = 8 ) rhi2
  real ( kind = 8 ) rlo
  real ( kind = 8 ) rlo2
  real ( kind = 8 ) rwide
  real ( kind = 8 ) value
!
!  Guarantee RLO2 < RHI2.
!
  rlo2 = min ( rlo, rhi )
  rhi2 = max ( rlo, rhi )
!
!  Find the width.
!
  rwide = rhi2 - rlo2
!
!  Add enough copies of (RHI2-RLO2) to R so that the
!  result ends up in the interval RLO2 - RHI2.
!
  if ( rwide == 0.0D+00 ) then
    value = rlo
  else if ( r < rlo2 ) then
    n = int ( ( rlo2 - r ) / rwide ) + 1
    value = r + n * rwide
    if ( value == rhi ) then
      value = rlo
    end if
  else
    n = int ( ( r - rlo2 ) / rwide )
    value = r - n * rwide
    if ( value == rlo ) then
      value = rhi
    end if
  end if

  r8_wrap = value

  return
end
subroutine r82_cheby ( n, alo, ahi, a )

!*****************************************************************************80
!
!! R82_CHEBY sets up the Chebyshev abscissas in an R8 interval.
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
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points to compute.
!
!    Input, real ( kind = 8 ) ALO, AHI, the range.
!
!    Output, real ( kind = 8 ) A(N), the computed X values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) ahi
  real ( kind = 8 ) alo
  real ( kind = 8 ) arg
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00

  if ( n == 1 ) then

    a(1) = 0.5D+00 * ( alo + ahi )

  else if ( 1 < n ) then

    do i = 1, n

      arg = real ( 2 * i - 1, kind = 8 ) * pi &
          / real ( 2 * n, kind = 8 )

      a(i) = 0.5D+00 * ( ( 1.0D+00 + cos ( arg ) ) * alo &
                       + ( 1.0D+00 - cos ( arg ) ) * ahi )

    end do

  end if

  return
end
function r82_dist_l2 ( a1, a2 )

!*****************************************************************************80
!
!! R82_DIST_L2 returns the L2 distance between a pair of R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
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
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A1(2), A2(2), the vectors.
!
!    Output, real ( kind = 8 ) R82_DIST_L2, the L2 norm of the distance
!    between A1 and A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a1(dim_num)
  real ( kind = 8 ) a2(dim_num)
  real ( kind = 8 ) r82_dist_l2

  r82_dist_l2 = sqrt ( sum ( ( a1(1:dim_num) - a2(1:dim_num) )**2 ) )

  return
end
function r82_eq ( a1, a2 )

!*****************************************************************************80
!
!! R82_EQ == ( A1 == A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
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
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A1(2), A2(2), two R82 vectors to be compared.
!
!    Output, logical R82_EQ, is TRUE if and only if A1 == A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a1(dim_num)
  real ( kind = 8 ) a2(dim_num)
  logical r82_eq

  if ( all ( a1(1:dim_num) == a2(1:dim_num) ) ) then
    r82_eq = .true.
  else
    r82_eq = .false.
  end if

  return
end
function r82_ge ( a1, a2 )

!*****************************************************************************80
!
!! R82_GE == ( A1 >= A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
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
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A1(2), A2(2), R82 vectors to be compared.
!
!    Output, logical R92_GE, is TRUE if and only if A1 >= A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a1(dim_num)
  real ( kind = 8 ) a2(dim_num)
  integer ( kind = 4 ) i
  logical r82_ge

  r82_ge = .true.

  do i = 1, dim_num

    if ( a2(i) < a1(i) ) then
      r82_ge = .true.
      exit
    else if ( a1(i) < a2(i) ) then
      r82_ge = .false.
      exit
    end if

  end do

  return
end
function r82_gt ( a1, a2 )

!*****************************************************************************80
!
!! R82_GT == ( A1 > A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R2, with two entries.
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
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A1(2), A2(2), R82 vectors to be compared.
!
!    Output, logical R82_GT, is TRUE if and only if A1 > A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a1(dim_num)
  real ( kind = 8 ) a2(dim_num)
  integer ( kind = 4 ) i
  logical r82_gt

  r82_gt = .false.

  do i = 1, dim_num

    if ( a2(i) < a1(i) ) then
      r82_gt = .true.
      exit
    else if ( a1(i) < a2(i) ) then
      r82_gt = .false.
      exit
    end if

  end do

  return
end
function r82_le ( a1, a2 )

!*****************************************************************************80
!
!! R82_LE == ( A1 <= A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
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
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A1(2), A2(2), R82 vectors to be compared.
!
!    Output, logical R82_LE, is TRUE if and only if A1 <= A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a1(dim_num)
  real ( kind = 8 ) a2(dim_num)
  integer ( kind = 4 ) i
  logical r82_le

  r82_le = .true.

  do i = 1, dim_num

    if ( a1(i) < a2(i) ) then
      r82_le = .true.
      exit
    else if ( a2(i) < a1(i) ) then
      r82_le = .false.
      exit
    end if

  end do

  return
end
function r82_lt ( a1, a2 )

!*****************************************************************************80
!
!! R82_LT == ( A1 < A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
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
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A1(2), A2(2), R82 vectors to be compared.
!
!    Output, logical R82_LT, is TRUE if and only if A1 < A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a1(dim_num)
  real ( kind = 8 ) a2(dim_num)
  integer ( kind = 4 ) i
  logical r82_lt

  r82_lt = .false.

  do i = 1, dim_num

    if ( a1(i) < a2(i) ) then
      r82_lt = .true.
      exit
    else if ( a2(i) < a1(i) ) then
      r82_lt = .false.
      exit
    end if

  end do

  return
end
function r82_ne ( a1, a2 )

!*****************************************************************************80
!
!! R82_NE == ( A1 /= A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
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
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A1(2), A2(2), R82 vectors to be compared.
!
!    Output, logical R82_NE, is TRUE if and only if A1 /= A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a1(dim_num)
  real ( kind = 8 ) a2(dim_num)
  logical r82_ne

  if ( any ( a1(1:dim_num) /= a2(1:dim_num) ) ) then
    r82_ne = .true.
  else
    r82_ne = .false.
  end if

  return
end
function r82_norm ( a )

!*****************************************************************************80
!
!! R82_NORM returns the Euclidean norm of an R82.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2), the vector.
!
!    Output, real ( kind = 8 ) R82_NORM, the norm.
!
  implicit none

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) r82_norm

  r82_norm = sqrt ( a(1) * a(1) + a(2) * a(2) )

  return
end
subroutine r82_normalize ( a )

!*****************************************************************************80
!
!! R82_NORMALIZE Euclidean normalizes an R82.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) A(2), the components of the vector.
!
  implicit none

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) norm

  norm = sqrt ( a(1) * a(1) + a(2) * a(2) )

  if ( norm /= 0.0D+00 ) then
    a(1:2) = a(1:2) / norm
  end if

  return
end
subroutine r82_print ( a, title )

!*****************************************************************************80
!
!! R82_PRINT prints an R82.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
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
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2), the coordinates of the vector.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  real ( kind = 8 ) a(2)
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '( 2x, a, a4, g14.6, a1, g14.6, a1 )' ) &
      trim ( title ), ' : (', a(1), ',', a(2), ')'
  else
    write ( *, '( 2x, a1, g14.6, a1, g14.6, a1 )' ) '(', a(1), ',', a(2), ')'

  end if

  return
end
subroutine r82_swap ( x, y )

!*****************************************************************************80
!
!! R82_SWAP swaps two R82 values.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) X(2), Y(2).  On output, the values of X and
!    Y have been interchanged.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) x(dim_num)
  real ( kind = 8 ) y(dim_num)
  real ( kind = 8 ) z(dim_num)

  z(1:dim_num) = x(1:dim_num)
  x(1:dim_num) = y(1:dim_num)
  y(1:dim_num) = z(1:dim_num)

  return
end
subroutine r82_uniform_ab ( b, c, seed, a )

!*****************************************************************************80
!
!! R82_UNIFORM_AB returns a random R82 value in a given range.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) B, C, the minimum and maximum values.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) A(2), the randomly chosen value.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  do i = 1, dim_num
    a(i) = r8_uniform_ab ( b, c, seed )
  end do

  return
end
subroutine r82poly2_print ( a, b, c, d, e, f )

!*****************************************************************************80
!
!! R82POLY2_PRINT prints a second order polynomial in two variables.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, D, E, F, the coefficients.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) f

  write ( *, &
    '( 2x, f8.4, '' * x^2 + '', f8.4, '' * y^2 + '', f8.4, '' * xy  + '' )' ) &
    a, b, c

  write ( *, &
    '( 2x, f8.4, '' * x + '', f8.4, '' * y + '', f8.4, '' = 0 '' )' ) d, e, f

  return
end
subroutine r82poly2_type ( a, b, c, d, e, f, type )

!*****************************************************************************80
!
!! R82POLY2_TYPE analyzes a second order polynomial in two variables.
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
!    09 December 2004
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
!    Input, real ( kind = 8 ) A, B, C, D, E, F, the coefficients.
!
!    Output, integer ( kind = 4 ) TYPE, indicates the type of the solution set.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) delta
  real ( kind = 8 ) e
  real ( kind = 8 ) f
  real ( kind = 8 ) j
  real ( kind = 8 ) k
  integer ( kind = 4 ) type
!
!  Handle the degenerate case.
!
  if ( a == 0.0D+00 .and. &
       b == 0.0D+00 .and. &
       c == 0.0D+00 ) then
    if ( d == 0.0D+00 .and. e == 0.0D+00 ) then
      if ( f == 0.0D+00 ) then
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
      8.0D+00 * a * b * f &
    + 2.0D+00 * c * e * d &
    - 2.0D+00 * a * e * e &
    - 2.0D+00 * b * d * d &
    - 2.0D+00 * f * c * c

  j = 4.0D+00 * a * b - c * c

  if ( delta /= 0.0D+00 ) then
    if ( j < 0.0D+00 ) then
      type = 1
    else if ( j == 0.0D+00 ) then
      type = 2
    else if ( 0.0D+00 < j ) then
      if ( sign ( 1.0D+00, delta ) /= sign ( 1.0D+00, ( a + b ) ) ) then
        type = 3
      else if ( sign ( 1.0D+00, delta ) == sign ( 1.0D+00, ( a + b ) ) ) then
        type = 4
      end if
    end if
  else if ( delta == 0.0D+00 ) then
    if ( j < 0.0D+00 ) then
      type = 5
    else if ( 0.0D+00 < j ) then
      type = 6
    else if ( j == 0.0D+00 ) then

      k = 4.0D+00 * ( a + b ) * f - d * d - e * e

      if ( k < 0.0D+00 ) then
        type = 7
      else if ( 0.0D+00 < k ) then
        type = 8
      else if ( k == 0.0D+00 ) then
        type = 9
      end if

    end if
  end if

  return
end
subroutine r82poly2_type_print ( type )

!*****************************************************************************80
!
!! R82POLY2_TYPE_PRINT prints the meaning of the output from R82POLY2_TYPE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) TYPE, the type index returned by R82POLY2_TYPE.
!
  implicit none

  integer ( kind = 4 ) type

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
subroutine r82vec_max ( n, a, amax )

!*****************************************************************************80
!
!! R82VEC_MAX returns the maximum value in an R82VEC.
!
!  Discussion:
!
!    An R82VEC is an array of pairs of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(2,N), the array.
!
!    Output, real ( kind = 8 ) AMAX(2); the largest entries in each row.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2,n)
  real ( kind = 8 ) amax(2)

  amax(1) = maxval ( a(1,1:n) )
  amax(2) = maxval ( a(2,1:n) )

  return
end
subroutine r82vec_min ( n, a, amin )

!*****************************************************************************80
!
!! R82VEC_MIN returns the minimum value in an R82VEC.
!
!  Discussion:
!
!    An R82VEC is an array of pairs of R82's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(2,N), the array.
!
!    Output, real ( kind = 8 ) AMIN(2); the smallest entries in each row.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2,n)
  real ( kind = 8 ) amin(2)

  amin(1) = minval ( a(1,1:n) )
  amin(2) = minval ( a(2,1:n) )

  return
end

subroutine r82vec_order_type ( n, a, order )

!*****************************************************************************80
!
!! R82VEC_ORDER_TYPE finds the order type of an R82VEC.
!
!  Discussion:
!
!    An R82VEC is an array of pairs of R8 values.
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
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the array.
!
!    Input, real ( kind = 8 ) A(2,N), the array to be checked.
!
!    Output, integer ( kind = 4 ) ORDER, order indicator:
!    -1, no discernable order;
!    0, all entries are equal;
!    1, ascending order;
!    2, strictly ascending order;
!    3, descending order;
!    4, strictly descending order.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
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
subroutine r82vec_part_quick_a ( n, a, l, r )

!*****************************************************************************80
!
!! R82VEC_PART_QUICK_A reorders an R82VEC as part of a quick sort.
!
!  Discussion:
!
!    An R82VEC is an array of pairs of R82 values.
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
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of A.
!
!    Input/output, real ( kind = 8 ) A(2,N).  On input, the array to be checked.
!    On output, A has been reordered as described above.
!
!    Output, integer ( kind = 4 ) L, R, the indices of A that define the three
!    segments.  Let KEY = the input value of A(1:2,1).  Then
!    I <= L                 A(1:2,I) < KEY;
!         L < I < R         A(1:2,I) = KEY;
!                 R <= I    KEY < A(1:2,I).
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) key(dim_num)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) r
  logical r8vec_eq
  logical r8vec_gt
  logical r8vec_lt

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R82VEC_PART_QUICK_A - Fatal error!'
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

    if ( r8vec_gt ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
      r = r - 1
      call r8vec_swap ( dim_num, a(1:dim_num,r), a(1:dim_num,l+1) )
    else if ( r8vec_eq ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
      m = m + 1
      call r8vec_swap ( dim_num, a(1:dim_num,m), a(1:dim_num,l+1) )
      l = l + 1
    else if ( r8vec_lt ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
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
subroutine r82vec_permute ( n, p, a )

!*****************************************************************************80
!
!! R82VEC_PERMUTE permutes an R82VEC in place.
!
!  Discussion:
!
!    An R82VEC is an array of pairs of R8 values.
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
!    13 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects.
!
!    Input, integer ( kind = 4 ) P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.
!
!    Input/output, real ( kind = 8 ) A(2,N), the array to be permuted.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  real ( kind = 8 ) a_temp(dim_num)
  integer ( kind = 4 ), parameter :: base = 1
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) p(n)

  call perm_check ( n, p, base, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R82VEC_PERMUTE - Fatal error!'
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
          write ( *, '(a)' ) 'R82VEC_PERMUTE - Fatal error!'
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
subroutine r82vec_print ( n, a, title )

!*****************************************************************************80
!
!! R82VEC_PRINT prints an R82VEC.
!
!  Discussion:
!
!    An R82VEC is an array of pairs of R82's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(2,N), the R82 vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,(5g14.6))' ) i, a(1:dim_num,i)
  end do

  return
end
subroutine r82vec_print_part ( n, a, max_print, title )

!*****************************************************************************80
!
!! R82VEC_PRINT_PART prints "part" of an R82VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, real ( kind = 8 ) A(2,N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)
    end do
    write ( *, '(a)' ) '  ........  ..............  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,a)' ) i, ':', a(1:2,i), &
      '...more entries...'

  end if

  return
end
subroutine r82vec_sort_heap_index_a ( n, a, indx )

!*****************************************************************************80
!
!! R82VEC_SORT_HEAP_INDEX_A ascending index heaps an R82VEC.
!
!  Discussion:
!
!    An R82VEC is an array of R82's.
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
!      call r82vec_permute ( n, indx, a )
!
!    after which A(1:2,I), I = 1 to N is sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(2,N), an array to be index-sorted.
!
!    Output, integer ( kind = 4 ) INDX(N), the sort index.  The
!    I-th element of the sorted array is A(1:2,INDX(I)).
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  real ( kind = 8 ) aval(dim_num)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indxt
  integer ( kind = 4 ) ir
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l

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
subroutine r82vec_sort_quick_a ( n, a )

!*****************************************************************************80
!
!! R82VEC_SORT_QUICK_A ascending sorts an R82VEC using quick sort.
!
!  Discussion:
!
!    An R82VEC is an array of R82's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, real ( kind = 8 ) A(2,N).
!    On input, the array to be sorted.
!    On output, the array has been sorted.
!
  implicit none

  integer ( kind = 4 ), parameter :: level_max = 30
  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  integer ( kind = 4 ) base
  integer ( kind = 4 ) l_segment
  integer ( kind = 4 ) level
  integer ( kind = 4 ) n_segment
  integer ( kind = 4 ) rsave(level_max)
  integer ( kind = 4 ) r_segment

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R82VEC_SORT_QUICK_A - Fatal error!'
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
    call r82vec_part_quick_a ( n_segment, a(1,base), l_segment, r_segment )
!
!  If the left segment has more than one element, we need to partition it.
!
    if ( 1 < l_segment ) then

      if ( level_max < level ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R82VEC_SORT_QUICK_A - Fatal error!'
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
function r83_norm ( x, y, z )

!*****************************************************************************80
!
!! R83_NORM returns the Euclidean norm of an R83.
!
!  Discussion:
!
!    An R83 is a vector of 3 R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, Y, Z, the vector.
!
!    Output, real ( kind = 8 ) R83_NORM, the norm of the vector.
!
  implicit none

  real ( kind = 8 ) r83_norm
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  r83_norm = sqrt ( x * x + y * y + z * z )

  return
end
subroutine r83_normalize ( x, y, z )

!*****************************************************************************80
!
!! R83_NORMALIZE normalizes an R83.
!
!  Discussion:
!
!    An R83 is a vector of 3 R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) X, Y, Z, the components of the vector.
!
  implicit none

  real ( kind = 8 ) norm
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  norm = sqrt ( x * x + y * y + z * z )

  if ( norm /= 0.0D+00 ) then
    x = x / norm
    y = y / norm
    z = z / norm
  end if

  return
end
subroutine r83_print ( x, y, z, title )

!*****************************************************************************80
!
!! R83_PRINT prints an R83.
!
!  Discussion:
!
!    An R83 is a vector of 3 R8's.
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
!    03 July 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, Y, Z, the coordinates of the vector.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  character ( len = * ) title
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  if ( 0 < len_trim ( title ) ) then
    write ( *, '( 2x, a, a4, g14.6, a1, g14.6, a1, g14.6, a1 )' ) &
      trim ( title ), ' : (', x, ',', y, ',', z, ')'
  else
    write ( *, '( 2x, a1, g14.6, a1, g14.6, a1, g14.6, a1 )' ) &
      '(', x, ',', y, ',', z, ')'
  end if

  return
end
subroutine r83_swap ( x, y )

!*****************************************************************************80
!
!! R83_SWAP swaps two R83's.
!
!  Discussion:
!
!    An R83 is a vector of 3 R8's.
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
!    Input/output, real ( kind = 8 ) X(3), Y(3).  On output, the values
!    of X and Y have been interchanged.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) x(dim_num)
  real ( kind = 8 ) y(dim_num)
  real ( kind = 8 ) z(dim_num)

  z(1:dim_num) = x(1:dim_num)
  x(1:dim_num) = y(1:dim_num)
  y(1:dim_num) = z(1:dim_num)

  return
end
subroutine r83vec_max ( n, a, amax )

!*****************************************************************************80
!
!! R83VEC_MAX returns the maximum value in an R83VEC.
!
!  Discussion:
!
!    An R83VEC is an array of R83's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(3,N), the array.
!
!    Output, real ( kind = 8 ) AMAX(3); the largest entries in each row.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(3,n)
  real ( kind = 8 ) amax(3)
  integer ( kind = 4 ) i

  do i = 1, 3
    amax(i) = maxval ( a(i,1:n) )
  end do

  return
end
subroutine r83vec_min ( n, a, amin )

!*****************************************************************************80
!
!! R83VEC_MIN returns the minimum value in an R83VEC.
!
!  Discussion:
!
!    An R83VEC is an array of R83's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(3,N), the array.
!
!    Output, real ( kind = 8 ) AMIN(3); the smallest entries in each row.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(3,n)
  real ( kind = 8 ) amin(3)
  integer ( kind = 4 ) i

  do i = 1, 3
    amin(i) = minval ( a(i,1:n) )
  end do

  return
end
subroutine r83vec_normalize ( n, x )

!*****************************************************************************80
!
!! R83VEC_NORMALIZE normalizes each R83 in an R83VEC.
!
!  Discussion:
!
!    An R83VEC is a vector of R83's.
!
!    An R83 is a vector of 3 R8's.
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
!    Input, integer ( kind = 4 ) N, the number of R83 vectors.
!
!    Input/output, real ( kind = 8 ) X(3,N), the coordinates of N R83 vectors.
!    On output, the nonzero vectors have been scaled to have unit L2 norm.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 3

  integer ( kind = 4 ) i
  real ( kind = 8 ) norm
  real ( kind = 8 ) x(dim_num,n)

  do i = 1, n

    norm = sqrt ( sum ( x(1:dim_num,i)**2 ) )

    if ( norm /= 0.0D+00 ) then
      x(1:dim_num,i) = x(1:dim_num,i) / norm
    end if

  end do

  return
end
subroutine r83vec_print_part ( n, a, max_print, title )

!*****************************************************************************80
!
!! R83VEC_PRINT_PART prints "part" of an R83VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, real ( kind = 8 ) A(3,N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(3,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do
    write ( *, '(a)' ) &
      '  ........  ..............  ..............  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6,2x,a)' ) i, ':', a(1:3,i), &
      '...more entries...'

  end if

  return
end
subroutine r84_normalize ( v )

!*****************************************************************************80
!
!! R84_NORMALIZE normalizes an R84.
!
!  Discussion:
!
!    An R84 is a vector of four R8's.
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
!    Input/output, real ( kind = 8 ) V(4), the components of the vector.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 4

  real ( kind = 8 ) norm
  real ( kind = 8 ) v(dim_num)

  norm = sqrt ( sum ( v(1:dim_num)**2 ) )

  if ( norm /= 0.0D+00 ) then
    v(1:dim_num) = v(1:dim_num) / norm
  end if

  return
end
subroutine r8block_expand_linear ( l, m, n, x, lfat, mfat, nfat, xfat )

!*****************************************************************************80
!
!! R8BLOCK_EXPAND_LINEAR linearly interpolates new data into an R8BLOCK.
!
!  Discussion:
!
!    An R8BLOCK is a 3D array of R8 values.
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
!    17 October 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) L, M, N, the dimensions of the input data.
!
!    Input, real ( kind = 8 ) X(L,M,N), the original data.
!
!    Input, integer ( kind = 4 ) LFAT, MFAT, NFAT, the number of data values
!    to interpolate original data values in the first, second and third
!    dimensions.
!
!    Output, real ( kind = 8 ) XFAT(L2,M2,N2), the fattened data, where
!    L2 = (L-1)*(LFAT+1)+1,
!    M2 = (M-1)*(MFAT+1)+1,
!    N2 = (N-1)*(NFAT+1)+1.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lfat
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mfat
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nfat

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) iii
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jj
  integer ( kind = 4 ) jjj
  integer ( kind = 4 ) jp1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) khi
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) kkk
  integer ( kind = 4 ) kp1
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) x(l,m,n)
  real ( kind = 8 ) x000
  real ( kind = 8 ) x001
  real ( kind = 8 ) x010
  real ( kind = 8 ) x011
  real ( kind = 8 ) x100
  real ( kind = 8 ) x101
  real ( kind = 8 ) x110
  real ( kind = 8 ) x111
  real ( kind = 8 ) xfat((l-1)*(lfat+1)+1,(m-1)*(mfat+1)+1,(n-1)*(nfat+1)+1)

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

          r = real ( ii,      kind = 8 ) &
            / real ( ihi + 1, kind = 8 )

          do jj = 0, jhi

            s = real ( jj,      kind = 8 ) &
              / real ( jhi + 1, kind = 8 )

            do kk = 0, khi

              t = real ( kk,      kind = 8 ) &
                / real ( khi + 1, kind = 8 )

              iii = 1 + ( i - 1 ) * ( lfat + 1 ) + ii
              jjj = 1 + ( j - 1 ) * ( mfat + 1 ) + jj
              kkk = 1 + ( k - 1 ) * ( nfat + 1 ) + kk

              xfat(iii,jjj,kkk) = &
                  x000 * ( 1.0D+00 - r ) * ( 1.0D+00 - s ) * ( 1.0D+00 - t ) &
                + x001 * ( 1.0D+00 - r ) * ( 1.0D+00 - s ) * (           t ) &
                + x010 * ( 1.0D+00 - r ) * (           s ) * ( 1.0D+00 - t ) &
                + x011 * ( 1.0D+00 - r ) * (           s ) * (           t ) &
                + x100 * (           r ) * ( 1.0D+00 - s ) * ( 1.0D+00 - t ) &
                + x101 * (           r ) * ( 1.0D+00 - s ) * (           t ) &
                + x110 * (           r ) * (           s ) * ( 1.0D+00 - t ) &
                + x111 * (           r ) * (           s ) * (           t )

            end do

          end do

        end do

      end do

    end do

  end do

  return
end
subroutine r8block_print ( l, m, n, a, title )

!*****************************************************************************80
!
!! R8BLOCK_PRINT prints an R8BLOCK.
!
!  Discussion:
!
!    An R8BLOCK is a 3D array of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) L, M, N, the dimensions of the block.
!
!    Input, real ( kind = 8 ) A(L,M,N), the matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(l,m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) k
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  do k = 1, n

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  K = ', k

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


function r8r8_compare ( x1, y1, x2, y2 )

!*****************************************************************************80
!
!! R8R8_COMPARE compares two R8R8's.
!
!  Discussion:
!
!    An R8R8 is simply a pair of R8 values, stored separately.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X1, Y1, the first vector.
!
!    Input, real ( kind = 8 ) X2, Y2, the second vector.
!
!    Output, integer ( kind = 4 ) R8R8_COMPARE:
!    -1, (X1,Y1) < (X2,Y2);
!     0, (X1,Y1) = (X2,Y2);
!    +1, (X1,Y1) > (X2,Y2).
!
  implicit none

  integer ( kind = 4 ) compare
  integer ( kind = 4 ) r8r8_compare
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2

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

  r8r8_compare = compare

  return
end
subroutine r8r8_print ( a1, a2, title )

!*****************************************************************************80
!
!! R8R8_PRINT prints an R8R8.
!
!  Discussion:
!
!    An R8R8 is simply a pair of R8R8's, stored separately.
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
!    29 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A1, A2, the coordinates of the vector.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '( 2x, a, a4, g14.6, a1, g14.6, a1 )' ) &
      trim ( title ), ' : (', a1, ',', a2, ')'
  else
    write ( *, '( 2x, a1, g14.6, a1, g14.6, a1 )' ) '(', a1, ',', a2, ')'
  end if

  return
end
function r8r8r8_compare ( x1, y1, z1, x2, y2, z2 )

!*****************************************************************************80
!
!! R8R8R8_COMPARE compares two R8R8R8's.
!
!  Discussion:
!
!    An R8R8R8 is simply 3 R8 values, stored as scalars.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X1, Y1, Z1, the first vector.
!
!    Input, real ( kind = 8 ) X2, Y2, Z2, the second vector.
!
!    Output, integer ( kind = 4 ) R8R8R8_COMPARE:
!    -1, (X1,Y1,Z1) < (X2,Y2,Z2);
!     0, (X1,Y1,Z1) = (X2,Y2,Z2);
!    +1, (X1,Y1,Z1) > (X2,Y2,Z2).
!
  implicit none

  integer ( kind = 4 ) compare
  integer ( kind = 4 ) r8r8r8_compare
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) z1
  real ( kind = 8 ) z2

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

  r8r8r8_compare = compare

  return
end
subroutine r8r8r8vec_index_insert_unique ( n_max, n, x, y, z, indx, &
  xval, yval, zval, ival, ierror )

!*****************************************************************************80
!
!! R8R8R8VEC_INDEX_INSERT_UNIQUE inserts unique R8R8R in an indexed sorted list.
!
!  Discussion:
!
!    An R8R8R8VEC is set of N R8R8R8 items.
!
!    An R8R8R8 is simply 3 R8 values, stored as scalars.
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
!    06 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N_MAX, the maximum size of the list.
!
!    Input/output, integer ( kind = 4 ) N, the size of the list.
!
!    Input/output, real ( kind = 8 ) X(N), Y(N), Z(N), the R8R8R8 vector.
!
!    Input/output, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Input, real ( kind = 8 ) XVAL, YVAL, ZVAL, the value to be inserted
!    if it is not already in the list.
!
!    Output, integer ( kind = 4 ) IVAL, the index in X, Y, Z corresponding
!    to the value XVAL, YVAL, ZVAL.
!
!    Output, integer ( kind = 4 ) IERROR, 0 for no error, 1 if an error
!    occurred.
!
  implicit none

  integer ( kind = 4 ) n_max

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  integer ( kind = 4 ) n
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n_max)
  real ( kind = 8 ) yval
  real ( kind = 8 ) z(n_max)
  real ( kind = 8 ) zval

  ierror = 0

  if ( n <= 0 ) then

    if ( n_max <= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8R8R8VEC_INDEX_INSERT_UNIQUE - Fatal error!'
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
  call r8r8r8vec_index_search ( n, x, y, z, indx, xval, yval, zval, &
    less, equal, more )

  if ( equal == 0 ) then

    if ( n_max <= n ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8R8R8VEC_INDEX_INSERT_UNIQUE - Fatal error!'
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
subroutine r8r8r8vec_index_search ( n, x, y, z, indx, xval, yval, &
  zval, less, equal, more )

!*****************************************************************************80
!
!! R8R8R8VEC_INDEX_SEARCH searches for R8R8R8 value in an indexed sorted list.
!
!  Discussion:
!
!    An R8R8R8VEC is set of N R8R8R8 items.
!
!    An R8R8R8 is simply 3 R8 values, stored as scalars.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the list.
!
!    Input, real ( kind = 8 ) X(N), Y(N), Z(N), the list.
!
!    Input, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Input, real ( kind = 8 ) XVAL, YVAL, ZVAL, the value to be sought.
!
!    Output, integer ( kind = 4 ) LESS, EQUAL, MORE, the indexes in INDX of the
!    entries of X that are just less than, equal to, and just greater
!    than XVAL.  If XVAL does not occur in X, then EQUAL is zero.
!    If XVAL is the minimum entry of X, then LESS is 0.  If XVAL
!    is the greatest entry of X, then MORE is N+1.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) compare
  integer ( kind = 4 ) r8r8r8_compare
  integer ( kind = 4 ) equal
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) mid
  integer ( kind = 4 ) more
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xmid
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) yhi
  real ( kind = 8 ) ylo
  real ( kind = 8 ) ymid
  real ( kind = 8 ) yval
  real ( kind = 8 ) z(n)
  real ( kind = 8 ) zhi
  real ( kind = 8 ) zlo
  real ( kind = 8 ) zmid
  real ( kind = 8 ) zval

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

  compare = r8r8r8_compare ( xval, yval, zval, xlo, ylo, zlo )

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

  compare = r8r8r8_compare ( xval, yval, zval, xhi, yhi, zhi )

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

    compare = r8r8r8_compare ( xval, yval, zval, xmid, ymid, zmid )

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
subroutine r8r8vec_index_insert_unique ( n_max, n, x, y, indx, xval, yval, &
  ival, ierror )

!*****************************************************************************80
!
!! R8R8VEC_INDEX_INSERT_UNIQUE inserts a unique R8R8 in an indexed sorted list.
!
!  Discussion:
!
!    An R8R8VEC is set of N R8R8 items.
!
!    An R8R8 is simply 2 R8 values, stored as scalars.
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
!    06 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N_MAX, the maximum size of the list.
!
!    Input/output, integer ( kind = 4 ) N, the size of the list.
!
!    Input/output, real ( kind = 8 ) X(N), Y(N), the list of R8R8 vectors.
!
!    Input/output, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Input, real ( kind = 8 ) XVAL, YVAL, the value to be inserted if it is
!    not already in the list.
!
!    Output, integer ( kind = 4 ) IVAL, the index in X, Y corresponding to the
!    value XVAL, YVAL.
!
!    Output, integer ( kind = 4 ) IERROR, 0 for no error, 1 if an
!    error occurred.
!
  implicit none

  integer ( kind = 4 ) n_max

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  integer ( kind = 4 ) n
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n_max)
  real ( kind = 8 ) yval

  ierror = 0

  if ( n <= 0 ) then

    if ( n_max <= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8R8VEC_INDEX_INSERT_UNIQUE - Fatal error!'
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
  call r8r8vec_index_search ( n, x, y, indx, xval, yval, less, equal, more )

  if ( equal == 0 ) then

    if ( n_max <= n ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8R8VEC_INDEX_INSERT_UNIQUE - Fatal error!'
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
subroutine r8r8vec_index_search ( n, x, y, indx, xval, yval, less, equal, &
  more )

!*****************************************************************************80
!
!! R8R8VEC_INDEX_SEARCH searches for an R8R8 in an indexed sorted list.
!
!  Discussion:
!
!    An R8R8VEC is set of N R8R8 items.
!
!    An R8R8 is simply 2 R8 values, stored as scalars.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the current list.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the list.
!
!    Input, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Input, real ( kind = 8 ) XVAL, YVAL, the value to be sought.
!
!    Output, integer ( kind = 4 ) LESS, EQUAL, MORE, the indexes in INDX of the
!    entries of X that are just less than, equal to, and just greater
!    than XVAL.  If XVAL does not occur in X, then EQUAL is zero.
!    If XVAL is the minimum entry of X, then LESS is 0.  If XVAL
!    is the greatest entry of X, then MORE is N+1.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) compare
  integer ( kind = 4 ) r8r8_compare
  integer ( kind = 4 ) equal
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) mid
  integer ( kind = 4 ) more
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xmid
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) yhi
  real ( kind = 8 ) ylo
  real ( kind = 8 ) ymid
  real ( kind = 8 ) yval

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

  compare = r8r8_compare ( xval, yval, xlo, ylo )

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

  compare = r8r8_compare ( xval, yval, xhi, yhi )

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

    compare = r8r8_compare ( xval, yval, xmid, ymid )

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
subroutine r8int_to_r8int ( rmin, rmax, r, r2min, r2max, r2 )

!*****************************************************************************80
!
!! R8INT_TO_R8INT maps one R8INT to another.
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
!    01 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) RMIN, RMAX, the first range.
!
!    Input, real ( kind = 8 ) R, the number to be converted.
!
!    Input, real ( kind = 8 ) R2MAX, R2MIN, the second range.
!
!    Output, real ( kind = 8 ) R2, the corresponding value in
!    the range [R2MIN,R2MAX].
!
  implicit none

  real ( kind = 8 ) r
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmin
  real ( kind = 8 ) r2
  real ( kind = 8 ) r2max
  real ( kind = 8 ) r2min

  if ( rmax == rmin ) then

    r2 = ( r2max + r2min ) / 2.0D+00

  else

    r2 = ( ( ( rmax - r        ) * r2min   &
           + (        r - rmin ) * r2max ) &
           / ( rmax     - rmin ) )

  end if

  return
end
subroutine r8int_to_i4int ( rmin, rmax, r, imin, imax, i )

!*****************************************************************************80
!
!! R8INT_TO_I4INT maps an R8INT to an integer interval.
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
!    01 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) RMIN, RMAX, the range.
!
!    Input, real ( kind = 8 ) R, the number to be converted.
!
!    Input, integer ( kind = 4 ) IMAX, IMIN, the integer range.
!
!    Output, integer ( kind = 4 ) I, the corresponding value in the
!    range [IMIN,IMAX].
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) imax
  integer ( kind = 4 ) imin
  real ( kind = 8 ) r
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmin

  if ( rmax == rmin ) then

    i = ( imax + imin ) / 2

  else

    i = nint ( &
      ( ( rmax - r        ) * real ( imin, kind = 8 )   &
      + (        r - rmin ) * real ( imax, kind = 8 ) ) &
      / ( rmax     - rmin ) )

  end if

  return
end
subroutine r8mat_add ( m, n, alpha, a, beta, b, c )

!*****************************************************************************80
!
!! R8MAT_ADD computes C = alpha * A + beta * B for R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) ALPHA, the multiplier for A.
!
!    Input, real ( kind = 8 ) A(M,N), the first matrix.
!
!    Input, real ( kind = 8 ) BETA, the multiplier for A.
!
!    Input, real ( kind = 8 ) B(M,N), the second matrix.
!
!    Output, real ( kind = 8 ) C(M,N), the sum of alpha*A+beta*B.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(m,n)
  real ( kind = 8 ) beta
  real ( kind = 8 ) c(m,n)

  c(1:m,1:n) = alpha * a(1:m,1:n) + beta * b(1:m,1:n)

  return
end

subroutine r8plu_det ( n, pivot, lu, det )

!*****************************************************************************80
!
!! R8PLU_DET computes the determinant of an R8PLU matrix.
!
!  Discussion:
!
!    The matrix should have been factored by R8MAT_TO_R8PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
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
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, integer ( kind = 4 ) PIVOT(N), the pivot vector computed
!    by R8MAT_TO_R8PLU.
!
!    Input, real ( kind = 8 ) LU(N,N), the LU factors computed
!    by R8MAT_TO_R8PLU.
!
!    Output, real ( kind = 8 ) DET, the determinant of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)

  det = 1.0D+00

  do i = 1, n
    det = det * lu(i,i)
    if ( pivot(i) /= i ) then
      det = -det
    end if
  end do

  return
end
subroutine r8plu_inverse ( n, pivot, lu, a_inverse )

!*****************************************************************************80
!
!! R8PLU_INVERSE computes the inverse of an R8PLU matrix.
!
!  Discussion:
!
!    The matrix should have been factored by R8MAT_TO_R8PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, integer ( kind = 4 ) PIVOT(N), the pivot vector from
!    R8MAT_TO_R8PLU.
!
!    Input, real ( kind = 8 ) LU(N,N), the LU factors computed by
!    R8MAT_TO_R8PLU.
!
!    Output, real ( kind = 8 ) A_INVERSE(N,N), the inverse of the original
!    matrix A that was factored by R8MAT_TO_R8PLU.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a_inverse(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) temp
  real ( kind = 8 ) work(n)

  a_inverse(1:n,1:n) = lu(1:n,1:n)
!
!  Compute Inverse(U).
!
  do k = 1, n

    a_inverse(k,k)     = 1.0D+00 / a_inverse(k,k)
    a_inverse(1:k-1,k) = -a_inverse(1:k-1,k) * a_inverse(k,k)

    do j = k + 1, n

      temp             = a_inverse(k,j)
      a_inverse(k,j)   = 0.0D+00
      a_inverse(1:k,j) = a_inverse(1:k,j) + temp * a_inverse(1:k,k)

    end do

  end do
!
!  Form Inverse(U) * Inverse(L).
!
  do k = n - 1, 1, -1

    work(k+1:n) = a_inverse(k+1:n,k)
    a_inverse(k+1:n,k) = 0.0D+00

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
subroutine r8plu_mul ( n, pivot, lu, x, b )

!*****************************************************************************80
!
!! R8PLU_MUL computes A * x using the PLU factors of A.
!
!  Discussion:
!
!    It is assumed that R8MAT_TO_R8PLU has computed the PLU factors of
!    the matrix A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, integer ( kind = 4 ) PIVOT(N), the pivot vector computed
!    by R8MAT_TO_R8PLU.
!
!    Input, real ( kind = 8 ) LU(N,N), the matrix factors computed by
!    R8MAT_TO_R8PLU.
!
!    Input, real ( kind = 8 ) X(N), the vector to be multiplied.
!
!    Output, real ( kind = 8 ) B(N), the result of the multiplication.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  real ( kind = 8 ) temp
  real ( kind = 8 ) x(n)

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
subroutine r8plu_sol ( n, pivot, lu, b, x )

!*****************************************************************************80
!
!! R8PLU_SOL solves a linear system A*x=b from the PLU factors.
!
!  Discussion:
!
!    The PLU factors should have been computed by R8MAT_TO_R8PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, integer ( kind = 4 ) PIVOT(N), the pivot vector from R8MAT_TO_R8PLU.
!
!    Input, real ( kind = 8 ) LU(N,N), the LU factors from R8MAT_TO_R8PLU.
!
!    Input, real ( kind = 8 ) B(N), the right hand side vector.
!
!    Output, real ( kind = 8 ) X(N), the solution vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) lu(n,n)
  real ( kind = 8 ) temp
  real ( kind = 8 ) x(n)
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
subroutine r8plu_to_r8mat ( n, pivot, lu, a )

!*****************************************************************************80
!
!! R8PLU_TO_R8MAT recovers the matrix A that was factored by R8MAT_TO_R8PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, integer ( kind = 4 ) PIVOT(N), the pivot vector computed
!    by R8MAT_TO_R8PLU.
!
!    Input, real ( kind = 8 ) LU(N,N), the matrix factors computed by
!    R8MAT_TO_R8PLU.
!
!    Output, real ( kind = 8 ) A(N,N), the matrix whose factors are
!    represented by LU and PIVOT.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) temp

  a(1:n,1:n) = 0.0D+00
  do i = 1, n
    a(i,i) = 1.0D+00
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
subroutine r8poly_degree ( na, a, degree )

!*****************************************************************************80
!
!! R8POLY_DEGREE returns the degree of a polynomial.
!
!  Discussion:
!
!    The degree of a polynomial is the index of the highest power
!    of X with a nonzero coefficient.
!
!    The degree of a constant polynomial is 0.  The degree of the
!    zero polynomial is debatable, but this routine returns the
!    degree as 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NA, the dimension of A.
!
!    Input, real ( kind = 8 ) A(0:NA), the coefficients of the polynomials.
!
!    Output, integer ( kind = 4 ) DEGREE, the degree of A.
!
  implicit none

  integer ( kind = 4 ) na

  real ( kind = 8 ) a(0:na)
  integer ( kind = 4 ) degree

  degree = na

  do while ( 0 < degree )

    if ( a(degree) /= 0.0D+00 ) then
      return
    end if

    degree = degree - 1

  end do

  return
end
subroutine r8poly_deriv ( n, c, p, cp )

!*****************************************************************************80
!
!! R8POLY_DERIV returns the derivative of a polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the degree of the polynomial.
!
!    Input, real ( kind = 8 ) C(0:N), the polynomial coefficients.
!    C(I) is the coefficient of X^I.
!
!    Input, integer ( kind = 4 ) P, the order of the derivative.
!    0 means no derivative is taken.
!    1 means first derivative,
!    2 means second derivative and so on.
!    Values of P less than 0 are meaningless.  Values of P greater
!    than N are meaningful, but the code will behave as though the
!    value of P was N+1.
!
!    Output, real ( kind = 8 ) CP(0:N-P), the polynomial coefficients of
!    the derivative.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) c(0:n)
  real ( kind = 8 ) cp(0:*)
  real ( kind = 8 ) cp_temp(0:n)
  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) p

  if ( n < p ) then
    return
  end if

  cp_temp(0:n) = c(0:n)

  do d = 1, p
    do i = 0, n - d
      cp_temp(i) = real ( i + 1, kind = 8 ) * cp_temp(i+1)
    end do
    cp_temp(n-d+1) = 0.0D+00
  end do

  cp(0:n-p) = cp_temp(0:n-p)

  return
end
subroutine r8poly_lagrange_0 ( npol, xpol, xval, wval )

!*****************************************************************************80
!
!! R8POLY_LAGRANGE_0 evaluates the Lagrange factor at a point.
!
!  Formula:
!
!    W(X) = Product ( 1 <= I <= NPOL ) ( X - XPOL(I) )
!
!  Discussion:
!
!    For a set of points XPOL(I), 1 <= I <= NPOL, the IPOL-th Lagrange basis
!    polynomial L(IPOL)(X), has the property:
!
!      L(IPOL)( XPOL(J) ) = delta ( IPOL, J )
!
!    and may be expressed as:
!
!      L(IPOL)(X) = W(X) / ( ( X - XPOL(IPOL) ) * W'(XPOL(IPOL)) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NPOL, the number of abscissas.
!    NPOL must be at least 1.
!
!    Input, real ( kind = 8 ) XPOL(NPOL), the abscissas, which
!    should be distinct.
!
!    Input, real ( kind = 8 ) XVAL, the point at which the Lagrange
!    factor is to be evaluated.
!
!    Output, real ( kind = 8 ) WVAL, the value of the Lagrange factor at XVAL.
!
  implicit none

  integer ( kind = 4 ) npol

  real ( kind = 8 ) wval
  real ( kind = 8 ) xpol(npol)
  real ( kind = 8 ) xval

  wval = product ( xval - xpol(1:npol) )

  return
end


subroutine r8row_compare ( m, n, a, i, j, value )

!*****************************************************************************80
!
!! R8ROW_COMPARE compares rows in an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8's, regarded as an array of M rows,
!    each of length N.
!
!  Example:
!
!    Input:
!
!      M = 4, N = 3, I = 2, J = 4
!
!      A = (
!        1  5  9
!        2  6 10
!        3  7 11
!        4  8 12 )
!
!    Output:
!
!      VALUE = -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), the M by N array.
!
!    Input, integer ( kind = 4 ) I, J, the rows to be compared.
!    I and J must be between 1 and M.
!
!    Output, integer ( kind = 4 ) VALUE, the results of the comparison:
!    -1, row I < row J,
!     0, row I = row J,
!    +1, row J < row I.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) value
!
!  Check.
!
  if ( i < 1 .or. m < i ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8ROW_COMPARE - Fatal error!'
    write ( *, '(a)' ) '  Row index I is out of bounds.'
    write ( *, '(a,i8)' ) '  I = ', i
    stop
  end if

  if ( j < 1 .or. m < j ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8ROW_COMPARE - Fatal error!'
    write ( *, '(a)' ) '  Row index J is out of bounds.'
    write ( *, '(a,i8)' ) '  J = ', j
    stop
  end if

  value = 0

  if ( i == j ) then
    return
  end if

  k = 1

  do while ( k <= n )

    if ( a(i,k) < a(j,k) ) then
      value = -1
      return
    else if ( a(j,k) < a(i,k) ) then
      value = +1
      return
    end if

    k = k + 1

  end do

  return
end
subroutine r8row_max ( m, n, a, amax )

!*****************************************************************************80
!
!! R8ROW_MAX returns the maximums of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
!
!  Example:
!
!    A =
!      1  2  3
!      2  6  7
!
!    MAX =
!      3
!      7
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input, real ( kind = 8 ) A(M,N), the array to be examined.
!
!    Output, real ( kind = 8 ) AMAX(M), the maximums of the rows.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) amax(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do i = 1, m

    amax(i) = a(i,1)
    do j = 2, n
      if ( amax(i) < a(i,j) ) then
        amax(i) = a(i,j)
      end if
    end do

  end do

  return
end
subroutine r8row_mean ( m, n, a, mean )

!*****************************************************************************80
!
!! R8ROW_MEAN returns the means of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
!
!  Example:
!
!    A =
!      1  2  3
!      2  6  7
!
!    MEAN =
!      2
!      5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), the array to be examined.
!
!    Output, real ( kind = 8 ) MEAN(M), the means, or averages, of the rows.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean(m)

  do i = 1, m
    mean(i) = sum ( a(i,1:n) ) / real ( n, kind = 8 )
  end do

  return
end
subroutine r8row_min ( m, n, a, amin )

!*****************************************************************************80
!
!! R8ROW_MIN returns the minimums of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
!
!  Example:
!
!    A =
!      1  2  3
!      2  6  7
!
!    MIN =
!      1
!      2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input, real ( kind = 8 ) A(M,N), the array to be examined.
!
!    Output, real ( kind = 8 ) AMIN(M), the minimums of the rows.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) amin(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do i = 1, m

    amin(i) = a(i,1)
    do j = 2, n
      if ( a(i,j) < amin(i) ) then
        amin(i) = a(i,j)
      end if
    end do

  end do

  return
end
subroutine r8row_part_quick_a ( m, n, a, l, r )

!*****************************************************************************80
!
!! R8ROW_PART_QUICK_A reorders the rows of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8's, regarded as an array of M rows,
!    each of length N.
!
!    The routine reorders the rows of A.  Using A(1,1:N) as a
!    key, all entries of A that are less than or equal to the key will
!    precede the key, which precedes all entries that are greater than the key.
!
!  Example:
!
!    Input:
!
!      M = 8, N = 2
!      A = ( 2 4
!            8 8
!            6 2
!            0 2
!           10 6
!           10 0
!            0 6
!            5 8 )
!
!    Output:
!
!      L = 2, R = 4
!
!      A = ( 0 2    LEFT
!            0 6
!            ----
!            2 4    KEY
!            ----
!            8 8    RIGHT
!            6 2
!           10 6
!           10 0
!            5 8 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the row dimension of A.
!
!    Input, integer ( kind = 4 ) N, the column dimension of A, and the
!    length of a row.
!
!    Input/output, real ( kind = 8 ) A(M,N).  On input, the array to be checked.
!    On output, A has been reordered as described above.
!
!    Output, integer ( kind = 4 ) L, R, the indices of A that define the three
!    segments.  Let KEY = the input value of A(1,1:N).  Then
!    I <= L                 A(I,1:N) < KEY;
!         L < I < R         A(I,1:N) = KEY;
!                 R <= I    KEY < A(I,1:N).
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) key(n)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) r
  logical r8vec_eq
  logical r8vec_gt
  logical r8vec_lt

  if ( m < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8ROW_PART_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  M < 1.'
    return
  end if

  if ( m == 1 ) then
    l = 0
    r = 2
    return
  end if

  key(1:n) = a(1,1:n)
  k = 1
!
!  The elements of unknown size have indices between L+1 and R-1.
!
  l = 1
  r = m + 1

  do j = 2, m

    if ( r8vec_gt ( n, a(l+1,1:n), key(1:n) ) ) then
      r = r - 1
      call r8vec_swap ( n, a(r,1:n), a(l+1,1:n) )
    else if ( r8vec_eq ( n, a(l+1,1:n), key(1:n) ) ) then
      k = k + 1
      call r8vec_swap ( n, a(k,1:n), a(l+1,1:n) )
      l = l + 1
    else if ( r8vec_lt ( n, a(l+1,1:n), key(1:n) ) ) then
      l = l + 1
    end if

  end do
!
!  Shift small elements to the left.
!
  do j = 1, l - k
    a(j,1:n) = a(j+k,1:n)
  end do
!
!  Shift KEY elements to center.
!
  do j = l - k + 1, l
    a(j,1:n) = key(1:n)
  end do
!
!  Update L.
!
  l = l - k

  return
end
subroutine r8row_sort_heap_a ( m, n, a )

!*****************************************************************************80
!
!! R8ROW_SORT_HEAP_A ascending heapsorts an R8ROWL.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8's, regarded as an array of M rows,
!    each of length N.
!
!    In lexicographic order, the statement "X < Y", applied to two real
!    vectors X and Y of length M, means that there is some index I, with
!    1 <= I <= M, with the property that
!
!      X(J) = Y(J) for J < I,
!    and
!      X(I) < Y(I).
!
!    In other words, the first time they differ, X is smaller.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input/output, real ( kind = 8 ) A(M,N).
!    On input, the array of M rows of N-vectors.
!    On output, the rows of A have been sorted in lexicographic order.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j

  if ( m <= 0 ) then
    return
  end if

  if ( m <= 1 ) then
    return
  end if
!
!  Initialize.
!
  i = 0
  indx = 0
  isgn = 0
  j = 0
!
!  Call the external heap sorter.
!
  do

    call sort_heap_external ( m, indx, i, j, isgn )
!
!  Interchange the I and J objects.
!
    if ( 0 < indx ) then

      call r8row_swap ( m, n, a, i, j )
!
!  Compare the I and J objects.
!
    else if ( indx < 0 ) then

      call r8row_compare ( m, n, a, i, j, isgn )

    else if ( indx == 0 ) then

      exit

    end if

  end do

  return
end
subroutine r8row_sort_heap_index_a ( m, n, a, indx )

!*****************************************************************************80
!
!! R8ROW_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8's, regarded as an array of M rows,
!    each of length N.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    A(I1,*) < A(I1,*) if the first nonzero entry of A(I1,*)-A(I2,*)
!    is negative.
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      A(INDX(1:M),1:N) is sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in each column of A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), the array.
!
!    Output, integer ( kind = 4 ) INDX(M), the sort index.  The I-th element
!    of the sorted array is row INDX(I).
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(m)
  integer ( kind = 4 ) indxt
  integer ( kind = 4 ) ir
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  real ( kind = 8 ) row(n)

  if ( n < 1 ) then
    return
  end if

  do i = 1, m
    indx(i) = i
  end do

  if ( m == 1 ) then
    return
  end if

  l = ( m / 2 ) + 1
  ir = m

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      row(1:n) = a(indxt,1:n)

    else

      indxt = indx(ir)
      row(1:n) = a(indxt,1:n)
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

        call r8row_compare ( m, n, a, indx(j), indx(j+1), isgn )

        if ( isgn < 0 ) then
          j = j + 1
        end if

      end if

      call r8vec_compare ( n, row, a(indx(j),1:n), isgn )

      if ( isgn < 0 ) then
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
subroutine r8row_sort_quick_a ( m, n, a )

!*****************************************************************************80
!
!! R8ROW_SORT_QUICK_A ascending quick sorts an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8's, regarded as an array of M rows,
!    each of length N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows of A.
!
!    Input, integer ( kind = 4 ) N, the number of columns of A,
!    and the length of a row.
!
!    Input/output, real ( kind = 8 ) A(M,N).
!    On input, the array to be sorted.
!    On output, the array has been sorted.
!
  implicit none

  integer ( kind = 4 ), parameter :: level_max = 30
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) base
  integer ( kind = 4 ) l_segment
  integer ( kind = 4 ) level
  integer ( kind = 4 ) m_segment
  integer ( kind = 4 ) rsave(level_max)
  integer ( kind = 4 ) r_segment

  if ( n <= 0 ) then
    return
  end if

  if ( m < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8ROW_SORT_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  M < 1.'
    write ( *, '(a,i8)' ) '  M = ', m
    stop
  end if

  if ( m == 1 ) then
    return
  end if

  level = 1
  rsave(level) = m + 1
  base = 1
  m_segment = m

  do
!
!  Partition the segment.
!
    call r8row_part_quick_a ( m_segment, n, a(base:base+m_segment-1,1:n), &
      l_segment, r_segment )
!
!  If the left segment has more than one element, we need to partition it.
!
    if ( 1 < l_segment ) then

      if ( level_max < level ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8ROW_SORT_QUICK_A - Fatal error!'
        write ( *, '(a,i8)' ) '  Exceeding recursion maximum of ', level_max
        stop
      end if

      level = level + 1
      m_segment = l_segment
      rsave(level) = r_segment + base - 1
!
!  The left segment and the middle segment are sorted.
!  Must the right segment be partitioned?
!
    else if ( r_segment < m_segment ) then

      m_segment = m_segment + 1 - r_segment
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
        m_segment = rsave(level-1) - rsave(level)
        level = level - 1

        if ( 0 < m_segment ) then
          exit
        end if

      end do

    end if

  end do

  return
end
subroutine r8row_sorted_unique_count ( m, n, a, unique_num )

!*****************************************************************************80
!
!! R8ROW_SORTED_UNIQUE_COUNT counts unique elements in an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
!
!    The rows of the array may be ascending or descending sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), a sorted array, containing
!    M rows of data.
!
!    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique rows.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) unique_num

  if ( n <= 0 ) then
    unique_num = 0
    return
  end if

  unique_num = 1
  i1 = 1

  do i2 = 2, m

    if ( any ( a(i1,1:n) /= a(i2,1:n) ) ) then
      unique_num = unique_num + 1
      i1 = i2
    end if

  end do

  return
end
subroutine r8row_sum ( m, n, a, rowsum )

!*****************************************************************************80
!
!! R8ROW_SUM returns the sums of the rows of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), the M by N array.
!
!    Output, real ( kind = 8 ) ROWSUM(M), the sum of the entries of
!    each row.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) rowsum(m)

  do i = 1, m
    rowsum(i) = sum ( a(i,1:n) )
  end do

  return
end
subroutine r8row_swap ( m, n, a, i1, i2 )

!*****************************************************************************80
!
!! R8ROW_SWAP swaps two rows of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
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
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input/output, real ( kind = 8 ) A(M,N), the M by N array.
!
!    Input, integer ( kind = 4 ) I1, I2, the two rows to swap.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  real ( kind = 8 ) row(n)

  if ( i1 < 1 .or. m < i1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8ROW_SWAP - Fatal error!'
    write ( *, '(a)' ) '  I1 is out of range.'
    write ( *, '(a,i8)' ) '  I1 = ', i1
    stop
  end if

  if ( i2 < 1 .or. m < i2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8ROW_SWAP - Fatal error!'
    write ( *, '(a)' ) '  I2 is out of range.'
    write ( *, '(a,i8)' ) '  I2 = ', i2
    stop
  end if

  if ( i1 == i2 ) then
    return
  end if

  row(1:n) = a(i1,1:n)
  a(i1,1:n) = a(i2,1:n)
  a(i2,1:n) = row(1:n)

  return
end
subroutine r8row_to_r8vec ( m, n, a, x )

!*****************************************************************************80
!
!! R8ROW_TO_R8VEC converts an R8ROW into an R8VEC.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
!
!  Example:
!
!    M = 3, N = 4
!
!    A =
!      11 12 13 14
!      21 22 23 24
!      31 32 33 34
!
!    X = ( 11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 34 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), the M by N array.
!
!    Output, real ( kind = 8 ) X(M*N), a vector containing the M rows of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(m*n)

  j = 1
  do i = 1, m
    x(j:j+n-1) = a(i,1:n)
    j = j + n
  end do

  return
end
subroutine r8row_variance ( m, n, a, variance )

!*****************************************************************************80
!
!! R8ROW_VARIANCE returns the variances of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input, real ( kind = 8 ) A(M,N), the array whose variances are desired.
!
!    Output, real ( kind = 8 ) VARIANCE(M), the variances of the rows.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance(m)

  do i = 1, m

    mean = sum ( a(i,1:n) ) / real ( n, kind = 8 )

    variance(i) = 0.0D+00
    do j = 1, n
      variance(i) = variance(i) + ( a(i,j) - mean )**2
    end do

    if ( 1 < n ) then
      variance(i) = variance(i) / real ( n - 1, kind = 8 )
    else
      variance(i) = 0.0D+00
    end if

  end do

  return
end
subroutine r8slmat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8SLMAT_PRINT prints a strict lower triangular R8MAT.
!
!  Example:
!
!    M = 5, N = 5
!    A = (/ 21, 31, 41, 51, 32, 42, 52, 43, 53, 54 /)
!
!    21
!    31 32
!    41 42 43
!    51 52 53 54
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(*), the M by N matrix.  Only the strict
!    lower triangular elements are stored, in column major order.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(10)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) jmax
  integer ( kind = 4 ) nn
  integer ( kind = 4 ) size
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  jmax = min ( n, m - 1 )

  if ( m-1 <= n ) then
    size = ( m * ( m - 1 ) ) / 2
  else if ( n < m-1 ) then
    size = ( n * ( n - 1 ) ) / 2 + ( m - n - 1 ) * n
  end if

  if ( all ( a(1:size) == aint ( a(1:size) ) ) ) then

    nn = 10

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(a8,10i8)' ) '     Col', ( j, j = jlo, jhi )
      write ( *, '(a8)' )      '     Row'
      do i = jlo + 1, m
        jhi = min ( jlo + nn - 1, i - 1, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j + 1 ) ) / 2
        end do
        write ( *, '(2x,i8,10i8)' ) i, int ( a(indx(1:jhi+1-jlo)) )
      end do
    end do

  else if ( maxval ( abs ( a(1:size) ) ) < 1000000.0D+00 ) then

    nn = 5

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(a10,5(i8,6x))' ) '       Col', ( j, j = jlo, jhi )
      write ( *, '(a10)' )          '       Row'
      do i = jlo + 1, m
        jhi = min ( jlo + nn - 1, i - 1, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j + 1 ) ) / 2
        end do
        write ( *, '(2x,i8,5f14.6)' ) i, a(indx(1:jhi+1-jlo))
      end do
    end do

  else

    nn = 5

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(a10,5(i8,6x))' ) '       Col', ( j, j = jlo, jhi )
      write ( *, '(a10)' ) '       Row'
      do i = jlo + 1, m
        jhi = min ( jlo + nn - 1, i - 1, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j + 1 ) ) / 2
        end do
        write ( *, '(2x,i8,5g14.6)' ) i, a(indx(1:jhi+1-jlo))
      end do
    end do

  end if

  return
end


subroutine roots_to_r8poly ( n, x, c )

!*****************************************************************************80
!
!! ROOTS_TO_R8POLY converts polynomial roots to polynomial coefficients.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of roots specified.
!
!    Input, real ( kind = 8 ) X(N), the roots.
!
!    Output, real ( kind = 8 ) C(0:N), the coefficients of the polynomial.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) c(0:n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)
!
!  Initialize C to (0, 0, ..., 0, 1).
!  Essentially, we are setting up a divided difference table.
!
  c(0:n-1) = 0.0D+00
  c(n) = 1.0D+00
!
!  Convert to standard polynomial form by shifting the abscissas
!  of the divided difference table to 0.
!
  do j = 1, n
    do i = 1, n + 1 - j
      c(n-i) = c(n-i) - x(n+1-i-j+1) * c(n-i+1)
    end do
  end do

  return
end
subroutine sort_heap_external ( n, indx, i, j, isgn )

!*****************************************************************************80
!
!! SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
!
!  Discussion:
!
!    The actual list of data is not passed to the routine.  Hence this
!    routine may be used to sort integers, reals, numbers, names,
!    dates, shoe sizes, and so on.  After each call, the routine asks
!    the user to compare or interchange two items, until a special
!    return value signals that the sorting is completed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2004
!
!  Author:
!
!    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt.
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
!    Input, integer ( kind = 4 ) N, the number of items to be sorted.
!
!    Input/output, integer ( kind = 4 ) INDX, the main communication signal.
!
!    The user must set INDX to 0 before the first call.
!    Thereafter, the user should not change the value of INDX until
!    the sorting is done.
!
!    On return, if INDX is
!
!      greater than 0,
!      * interchange items I and J;
!      * call again.
!
!      less than 0,
!      * compare items I and J;
!      * set ISGN = -1 if I < J, ISGN = +1 if J < I;
!      * call again.
!
!      equal to 0, the sorting is done.
!
!    Output, integer ( kind = 4 ) I, J, the indices of two items.
!    On return with INDX positive, elements I and J should be interchanged.
!    On return with INDX negative, elements I and J should be compared, and
!    the result reported in ISGN on the next call.
!
!    Input, integer ( kind = 4 ) ISGN, results of comparison of elements
!    I and J. (Used only when the previous call returned INDX less than 0).
!    ISGN <= 0 means I is less than or equal to J;
!    0 <= ISGN means I is greater than or equal to J.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ), save :: i_save = 0
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j
  integer ( kind = 4 ), save :: j_save = 0
  integer ( kind = 4 ), save :: k = 0
  integer ( kind = 4 ), save :: k1 = 0
  integer ( kind = 4 ) n
  integer ( kind = 4 ), save :: n1 = 0
!
!  INDX = 0: This is the first call.
!
  if ( indx == 0 ) then

    i_save = 0
    j_save = 0
    k = n / 2
    k1 = k
    n1 = n
!
!  INDX < 0: The user is returning the results of a comparison.
!
  else if ( indx < 0 ) then

    if ( indx == -2 ) then

      if ( isgn < 0 ) then
        i_save = i_save + 1
      end if

      j_save = k1
      k1 = i_save
      indx = -1
      i = i_save
      j = j_save
      return

    end if

    if ( 0 < isgn ) then
      indx = 2
      i = i_save
      j = j_save
      return
    end if

    if ( k <= 1 ) then

      if ( n1 == 1 ) then
        i_save = 0
        j_save = 0
        indx = 0
      else
        i_save = n1
        n1 = n1 - 1
        j_save = 1
        indx = 1
      end if

      i = i_save
      j = j_save
      return

    end if

    k = k - 1
    k1 = k
!
!  0 < INDX, the user was asked to make an interchange.
!
  else if ( indx == 1 ) then

    k1 = k

  end if

  do

    i_save = 2 * k1

    if ( i_save == n1 ) then
      j_save = k1
      k1 = i_save
      indx = -1
      i = i_save
      j = j_save
      return
    else if ( i_save <= n1 ) then
      j_save = i_save + 1
      indx = -2
      i = i_save
      j = j_save
      return
    end if

    if ( k <= 1 ) then
      exit
    end if

    k = k - 1
    k1 = k

  end do

  if ( n1 == 1 ) then
    i_save = 0
    j_save = 0
    indx = 0
    i = i_save
    j = j_save
  else
    i_save = n1
    n1 = n1 - 1
    j_save = 1
    indx = 1
    i = i_save
    j = j_save
  end if

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
