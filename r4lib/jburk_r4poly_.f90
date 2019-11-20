module     jburk_r4poly_
use, intrinsic :: iso_fortran_env
implicit none

   interface           r4poly_deriv
      module procedure r4poly_deriv
   end interface       r4poly_deriv
   public              r4poly_deriv


contains


subroutine r4poly_degree ( na, a, degree )

!*****************************************************************************80
!
!! R4POLY_DEGREE returns the degree of a polynomial.
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
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: NA, the dimension of A.
!
!    Input, real (kind=4) :: A(0:NA), the coefficients of the polynomials.
!
!    Output, integer (kind=4) :: DEGREE, the degree of A.
!
  implicit none

  integer (kind=4) :: na

  real (kind=4) :: a(0:na)
  integer (kind=4) :: degree

  degree = na

  do while ( 0 < degree )

    if ( a(degree) /= 0.0E+00 ) then
      return
    end if

    degree = degree - 1

  end do

  return
end


   subroutine r4poly_deriv (n, c, p, cp)
   !*****************************************************************************80
   !! R4POLY_DERIV returns the derivative of a polynomial.
   !
   !  Licensing:
   !    This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !    02 May 2011
   !
   !  Author:
   !    John Burkardt
   !
   !  Parameters:
   !    Input, integer (kind=4) :: N, the degree of the polynomial.
   !
   !    Input, real (kind=4) :: C(0:N), the polynomial coefficients.
   !    C(I) is the coefficient of X**I.
   !
   !    Input, integer (kind=4) :: P, the order of the derivative.
   !    0 means no derivative is taken.
   !    1 means first derivative,
   !    2 means second derivative and so on.
   !    Values of P less than 0 are meaningless. Values of P greater
   !    than N are meaningful, but the code will behave as though the
   !    value of P was N+1.
   !
   !    Output, real (kind=4) :: CP(0:N-P), the polynomial coefficients of
   !    the derivative.
   implicit none
      integer (kind=4), intent(in) :: n
      real (kind=4), intent(in) :: c(0:n)
      real (kind=4), intent(out) :: cp(0:*)
      real (kind=4) :: cp_temp(0:n)
      integer (kind=4) :: d
      integer (kind=4) :: i
      integer (kind=4), intent(in) :: p

      if ( n < p ) then
        return
      end if

      cp_temp(0:n) = c(0:n)

      do d = 1, p
         do i = 0, n - d
            cp_temp(i) = real(i+1, kind=4) * cp_temp(i+1)
         end do
         cp_temp(n-d+1) = 0.0E+0
      end do

      cp(0:n-p) = cp_temp(0:n-p)

      return
   end subroutine r4poly_deriv



subroutine r4poly_lagrange_0 ( npol, xpol, xval, wval )

!*****************************************************************************80
!
!! R4POLY_LAGRANGE_0 evaluates the Lagrange factor at a point.
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
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: NPOL, the number of abscissas.
!    NPOL must be at least 1.
!
!    Input, real (kind=4) :: XPOL(NPOL), the abscissas, which
!    should be distinct.
!
!    Input, real (kind=4) :: XVAL, the point at which the Lagrange
!    factor is to be evaluated.
!
!    Output, real (kind=4) :: WVAL, the value of the Lagrange factor at XVAL.
!
  implicit none

  integer (kind=4) :: npol

  real (kind=4) :: wval
  real (kind=4) :: xpol(npol)
  real (kind=4) :: xval

  wval = product ( xval - xpol(1:npol) )

  return
end


subroutine r4poly_lagrange_1 ( npol, xpol, xval, dwdx )

!*****************************************************************************80
!
!! R4POLY_LAGRANGE_1 evaluates the first derivative of the Lagrange factor.
!
!  Formula:
!
!    W(XPOL(1:NPOL))(X) = Product ( 1 <= I <= NPOL ) ( X - XPOL(I) )
!
!    W'(XPOL(1:NPOL))(X)
!      = Sum ( 1 <= J <= NPOL ) Product ( I /= J ) ( X - XPOL(I) )
!
!    We also have the recursion:
!
!      W'(XPOL(1:NPOL))(X) = d/dX ( ( X - XPOL(NPOL) ) * W(XPOL(1:NPOL-1))(X) )
!                    = W(XPOL(1:NPOL-1))(X)
!                    + ( X - XPOL(NPOL) ) * W'(XPOL(1:NPOL-1))(X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: NPOL, the number of abscissas.
!
!    Input, real (kind=4) :: XPOL(NPOL), the abscissas, which should
!    be distinct.
!
!    Input, real (kind=4) :: XVAL, the point at which the Lagrange
!    factor is to be evaluated.
!
!    Output, real (kind=4) :: DWDX, the derivative of W with respect to X.
!
  implicit none

  integer (kind=4) :: npol

  real (kind=4) :: dwdx
  integer (kind=4) :: i
  real (kind=4) :: w
  real (kind=4) :: xpol(npol)
  real (kind=4) :: xval

  dwdx = 0.0E+00
  w = 1.0E+00

  do i = 1, npol

    dwdx = w + ( xval - xpol(i) ) * dwdx
    w = w * ( xval - xpol(i) )

  end do

  return
end
subroutine r4poly_lagrange_2 ( npol, xpol, xval, dw2dx2 )

!*****************************************************************************80
!
!! R4POLY_LAGRANGE_2 evaluates the second derivative of the Lagrange factor.
!
!  Formula:
!
!    W(X)  = Product ( 1 <= I <= NPOL ) ( X - XPOL(I) )
!
!    W'(X) = Sum ( 1 <= J <= NPOL )
!            Product ( I /= J ) ( X - XPOL(I) )
!
!    W"(X) = Sum ( 1 <= K <= NPOL )
!            Sum ( J =/ K )
!            Product ( I /= K, J ) ( X - XPOL(I) )
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
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: NPOL, the number of abscissas.
!    NPOL must be at least 1.
!
!    Input, real (kind=4) :: XPOL(NPOL), the abscissas, which should
!    be distinct.
!
!    Input, real (kind=4) :: XVAL, the point at which the Lagrange
!    factor is to be evaluated.
!
!    Output, real (kind=4) :: DW2DX2, the second derivative of W
!    with respect to XVAL.
!
  implicit none

  integer (kind=4) :: npol

  real (kind=4) :: dw2dx2
  integer (kind=4) :: i
  integer (kind=4) :: j
  integer (kind=4) :: k
  real (kind=4) :: term
  real (kind=4) :: xpol(npol)
  real (kind=4) :: xval

  dw2dx2 = 0.0E+00

  do k = 1, npol

    do j = 1, npol

      if ( j /= k ) then
        term = 1.0E+00

        do i = 1, npol
          if ( i /= j .and. i /= k ) then
            term = term * ( xval - xpol(i) )
          end if
        end do

        dw2dx2 = dw2dx2 + term

      end if

    end do

  end do

  return
end
subroutine r4poly_lagrange_coef ( npol, ipol, xpol, pcof )

!*****************************************************************************80
!
!! R4POLY_LAGRANGE_COEF returns the coefficients of a Lagrange polynomial.
!
!  Discussion:
!
!    Given distinct abscissas XPOL(1:NPOL), the IPOL-th Lagrange
!    polynomial L(IPOL)(X) is defined as the polynomial of degree
!    NPOL - 1 which is 1 at XPOL(IPOL) and 0 at the NPOL - 1 other
!    abscissas.
!
!    A formal representation is:
!
!      L(IPOL)(X) = Product ( 1 <= I <= NPOL, I /= IPOL )
!       ( X - X(I) ) / ( X(IPOL) - X(I) )
!
!    However sometimes it is desirable to be able to write down
!    the standard polynomial coefficients of L(IPOL)(X).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: NPOL, the number of abscissas.
!    NPOL must be at least 1.
!
!    Input, integer (kind=4) :: IPOL, the index of the polynomial to evaluate.
!    IPOL must be between 1 and NPOL.
!
!    Input, real (kind=4) :: XPOL(NPOL), the abscissas of the
!    Lagrange polynomials.  The entries in XPOL must be distinct.
!
!    Output, real (kind=4) :: PCOF(0:NPOL-1), the standard polynomial
!    coefficients of the IPOL-th Lagrange polynomial:
!      L(IPOL)(X) = SUM ( 0 <= I <= NPOL-1 ) PCOF(I) * X**I
!
  implicit none

  integer (kind=4) :: npol

  integer (kind=4) :: i
  integer (kind=4) :: indx
  integer (kind=4) :: ipol
  integer (kind=4) :: j
  real (kind=4) :: pcof(0:npol-1)
  logical              r4vec_distinct
  real (kind=4) :: xpol(npol)
!
!  Make sure IPOL is legal.
!
  if ( ipol < 1 .or. npol < ipol ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY_LAGRANGE_COEF - Fatal error!'
    write ( *, '(a)' ) '  1 <= IPOL <= NPOL is required.'
    write ( *, '(a,i8)' ) '  IPOL = ', ipol
    write ( *, '(a,i8)' ) '  NPOL = ', npol
    stop
  end if
!
!  Check that the abscissas are distinct.
!
  if ( .not. r4vec_distinct ( npol, xpol ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY_LAGRANGE_COEF - Fatal error!'
    write ( *, '(a)' ) '  Two or more entries of XPOL are equal:'
    stop
  end if

  pcof(0) = 1.0E+00
  pcof(1:npol-1) = 0.0E+00

  indx = 0

  do i = 1, npol

    if ( i /= ipol ) then

      indx = indx + 1

      do j = indx, 0, -1

        pcof(j) = -xpol(i) * pcof(j) / ( xpol(ipol) - xpol(i) )

        if ( 0 < j ) then
          pcof(j) = pcof(j) + pcof(j-1) / ( xpol(ipol) - xpol(i) )
        end if

      end do

    end if

  end do

  return
end
subroutine r4poly_lagrange_factor ( npol, xpol, xval, wval, dwdx )

!*****************************************************************************80
!
!! R4POLY_LAGRANGE_FACTOR evaluates the polynomial Lagrange factor at a point.
!
!  Formula:
!
!    W(X) = Product ( 1 <= I <= NPOL ) ( X - XPOL(I) )
!
!  Discussion:
!
!    Suppose F(X) is at least N times continuously differentiable in the
!    interval [A,B].  Pick NPOL distinct points XPOL(I) in [A,B] and compute
!    the interpolating polynomial P(X) of order NPOL ( and degree NPOL-1)
!    which passes through all the points ( XPOL(I), F(XPOL(I)) ).
!    Then in the interval [A,B], the maximum error
!
!      abs ( F(X) - P(X) )
!
!    is bounded by:
!
!      C * FNMAX * W(X)
!
!    where
!
!      C is a constant,
!      FNMAX is the maximum value of the NPOL-th derivative of F in [A,B],
!      W(X) is the Lagrange factor.
!
!    Thus, the value of W(X) is useful as part of an estimated bound
!    for the interpolation error.
!
!    Note that the Chebyshev abscissas have the property that they minimize
!    the value of W(X) over the interval [A,B].  Hence, if the abscissas may
!    be chosen arbitrarily, the Chebyshev abscissas have this advantage over
!    other choices.
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
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: NPOL, the number of abscissas.
!    NPOL must be at least 1.
!
!    Input, real (kind=4) :: XPOL(NPOL), the abscissas, which should
!    be distinct.
!
!    Input, real (kind=4) :: XVAL, the point at which the Lagrange
!    factor is to be evaluated.
!
!    Output, real (kind=4) :: WVAL, the value of the Lagrange factor at XVAL.
!
!    Output, real (kind=4) :: DWDX, the derivative of W with respect to XVAL.
!
  implicit none

  integer (kind=4) :: npol

  real (kind=4) :: dwdx
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: term
  real (kind=4) :: wval
  real (kind=4) :: xpol(npol)
  real (kind=4) :: xval

  wval = product ( xval - xpol(1:npol) )

  dwdx = 0.0E+00

  do i = 1, npol

    term = 1.0E+00

    do j = 1, npol
      if ( i /= j ) then
        term = term * ( xval - xpol(j) )
      end if
    end do

    dwdx = dwdx + term

  end do

  return
end
subroutine r4poly_lagrange_val ( npol, ipol, xpol, xval, pval, dpdx )

!*****************************************************************************80
!
!! R4POLY_LAGRANGE_VAL evaluates the IPOL-th Lagrange polynomial.
!
!  Discussion:
!
!    Given NPOL distinct abscissas, XPOL(1:NPOL), the IPOL-th Lagrange
!    polynomial L(IPOL)(X) is defined as the polynomial of degree
!    NPOL - 1 which is 1 at XPOL(IPOL) and 0 at the NPOL - 1 other
!    abscissas.
!
!    A formal representation is:
!
!      L(IPOL)(X) = Product ( 1 <= I <= NPOL, I /= IPOL )
!       ( X - X(I) ) / ( X(IPOL) - X(I) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: NPOL, the number of abscissas.
!    NPOL must be at least 1.
!
!    Input, integer (kind=4) :: IPOL, the index of the polynomial to evaluate.
!    IPOL must be between 1 and NPOL.
!
!    Input, real (kind=4) :: XPOL(NPOL), the abscissas of the Lagrange
!    polynomials.  The entries in XPOL must be distinct.
!
!    Input, real (kind=4) :: XVAL, the point at which the IPOL-th
!    Lagrange polynomial is to be evaluated.
!
!    Output, real (kind=4) :: PVAL, the value of the IPOL-th Lagrange
!    polynomial at XVAL.
!
!    Output, real (kind=4) :: DPDX, the derivative of the IPOL-th
!    Lagrange polynomial at XVAL.
!
  implicit none

  integer (kind=4) :: npol

  real (kind=4) :: dpdx
  integer (kind=4) :: i
  integer (kind=4) :: ipol
  integer (kind=4) :: j
  real (kind=4) :: p2
  real (kind=4) :: pval
  logical              r4vec_distinct
  real (kind=4) :: xpol(npol)
  real (kind=4) :: xval
!
!  Make sure IPOL is legal.
!
  if ( ipol < 1 .or. npol < ipol ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY_LAGRANGE_VAL - Fatal error!'
    write ( *, '(a)' ) '  1 <= IPOL <= NPOL is required.'
    write ( *, '(a,i8)' ) '  IPOL = ', ipol
    stop
  end if
!
!  Check that the abscissas are distinct.
!
  if ( .not. r4vec_distinct ( npol, xpol ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY_LAGRANGE_VAL - Fatal error!'
    write ( *, '(a)' ) '  Two or more entries of XPOL are equal:'
    stop
  end if
!
!  Evaluate the polynomial.
!
  pval = 1.0E+00

  do i = 1, npol

    if ( i /= ipol ) then

      pval = pval * ( xval - xpol(i) ) / ( xpol(ipol) - xpol(i) )

    end if

  end do
!
!  Evaluate the derivative, which can be found by summing up the result
!  of differentiating one factor at a time, successively.
!
  dpdx = 0.0E+00

  do i = 1, npol

    if ( i /= ipol ) then

      p2 = 1.0E+00
      do j = 1, npol

        if ( j == i ) then
          p2 = p2                      / ( xpol(ipol) - xpol(j) )
        else if ( j /= ipol ) then
          p2 = p2 * ( xval - xpol(j) ) / ( xpol(ipol) - xpol(j) )
        end if

      end do

      dpdx = dpdx + p2

    end if

  end do

  return
end
subroutine r4poly_order ( na, a, order )

!*****************************************************************************80
!
!! R4POLY_ORDER returns the order of a polynomial.
!
!  Discussion:
!
!    The order of a polynomial is one more than the degree.
!
!    The order of a constant polynomial is 1.  The order of the
!    zero polynomial is debatable, but this routine returns the
!    order as 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: NA, the dimension of A.
!
!    Input, real (kind=4) :: A(0:NA), the coefficients of the polynomials.
!
!    Output, integer (kind=4) :: ORDER, the order of A.
!
  implicit none

  integer (kind=4) :: na

  real (kind=4) :: a(0:na)
  integer (kind=4) :: order

  order = na + 1

  do while ( 1 < order )

    if ( a(order-1) /= 0.0E+00 ) then
      return
    end if

    order = order - 1

  end do

  return
end
subroutine r4poly_print ( n, a, title )

!*****************************************************************************80
!
!! R4POLY_PRINT prints out a polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the dimension of A.
!
!    Input, real (kind=4) :: A(0:N), the polynomial coefficients.
!    A(0) is the constant term and
!    A(N) is the coefficient of X**N.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer   ( kind = 4 ) n

  real      ( kind = 4 ) a(0:n)
  integer   ( kind = 4 ) i
  real      ( kind = 4 ) mag
  integer   ( kind = 4 ) n2
  character              plus_minus
  character ( len = * )  title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  call r4poly_degree ( n, a, n2 )

  if ( n2 <= 0 ) then
    write ( *, '( ''  p(x) = 0'' )' )
    return
  end if

  if ( a(n2) < 0.0E+00 ) then
    plus_minus = '-'
  else
    plus_minus = ' '
  end if

  mag = abs ( a(n2) )

  if ( 2 <= n2 ) then
    write ( *, '( ''  p(x) = '', a1, g14.6, '' * x ^ '', i3 )' ) &
      plus_minus, mag, n2
  else if ( n2 == 1 ) then
    write ( *, '( ''  p(x) = '', a1, g14.6, '' * x'' )' ) &
      plus_minus, mag
  else if ( n2 == 0 ) then
    write ( *, '( ''  p(x) = '', a1, g14.6 )' ) plus_minus, mag
  end if

  do i = n2-1, 0, -1

    if ( a(i) < 0.0E+00 ) then
      plus_minus = '-'
    else
      plus_minus = '+'
    end if

    mag = abs ( a(i) )

    if ( mag /= 0.0E+00 ) then

      if ( 2 <= i ) then
        write ( *, ' ( ''         '', a1, g14.6, '' * x ^ '', i3 )' ) &
          plus_minus, mag, i
      else if ( i == 1 ) then
        write ( *, ' ( ''         '', a1, g14.6, '' * x'' )' ) plus_minus, mag
      else if ( i == 0 ) then
        write ( *, ' ( ''         '', a1, g14.6 )' ) plus_minus, mag
      end if
    end if

  end do

  return
end
subroutine r4poly_shift ( scale, shift, n, poly_cof )

!*****************************************************************************80
!
!! R4POLY_SHIFT adjusts the coefficients of a polynomial for a new argument.
!
!  Discussion:
!
!    Assuming P(X) is a polynomial in the argument X, of the form:
!
!      P(X) =
!          C(N) * X**N
!        + ...
!        + C(1) * X
!        + C(0),
!
!    and that Z is related to X by the formula:
!
!      Z = SCALE * X + SHIFT
!
!    then this routine computes coefficients C for the polynomial Q(Z):
!
!      Q(Z) =
!          C(N) * Z^N
!        + ...
!        + C(1) * Z
!        + C(0)
!
!    so that:
!
!      Q(Z(X)) = P(X)
!
!  Example:
!
!    P(X) = 2 * X^2 - X + 6
!
!    Z = 2.0 * X + 3.0
!
!    Q(Z) = 0.5 *         Z^2 -  3.5 * Z + 12
!
!    Q(Z(X)) = 0.5 * ( 4.0 * X^2 + 12.0 * X +  9 )
!            - 3.5 * (              2.0 * X +  3 )
!                                           + 12
!
!            = 2.0         * X^2 -  1.0 * X +  6
!
!            = P(X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Reference:
!
!    William Press, Brian Flannery, Saul Teukolsky, William Vetterling,
!    Numerical Recipes: The Art of Scientific Computing,
!    Cambridge University Press.
!
!  Parameters:
!
!    Input, real (kind=4) :: SHIFT, SCALE, the shift and scale applied to X,
!    so that Z = SCALE * X + SHIFT.
!
!    Input, integer (kind=4) :: N, the number of coefficients.
!
!    Input/output, real (kind=4) :: POLY_COF(0:N).
!    On input, the coefficient array in terms of the X variable.
!    On output, the coefficient array in terms of the Z variable.
!
  implicit none

  integer (kind=4) :: n

  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: poly_cof(0:n)
  real (kind=4) :: scale
  real (kind=4) :: shift

  do i = 1, n
    poly_cof(i:n) = poly_cof(i:n) / scale
  end do

  do i = 0, n - 1
    do j = n - 1, i, -1
      poly_cof(j) = poly_cof(j) - shift * poly_cof(j+1)
    end do
  end do

  return
end
subroutine r4poly_val_horner ( n, c, x, cx )

!*****************************************************************************80
!
!! R4POLY_VAL_HORNER evaluates a polynomial using Horner's method.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the dimension of C.
!
!    Input, real (kind=4) :: C(0:N), the polynomial coefficients.
!    C(I) is the coefficient of X^I.
!
!    Input, real (kind=4) :: X, the point at which the polynomial is
!    to be evaluated.
!
!    Output, real (kind=4) :: CX, the value of the polynomial at X.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: c(0:n)
  real (kind=4) :: cx
  integer (kind=4) :: i
  real (kind=4) :: x

  cx = c(n)
  do i = n - 1, 0, -1
    cx = cx * x + c(i)
  end do

  return
end
function r4poly_value ( n, a, x )

!*****************************************************************************80
!
!! R4POLY_VALUE evaluates an R4POLY
!
!  Discussion:
!
!    For sanity's sake, the value of N indicates the NUMBER of
!    coefficients, or more precisely, the ORDER of the polynomial,
!    rather than the DEGREE of the polynomial.  The two quantities
!    differ by 1, but cause a great deal of confusion.
!
!    Given N and A, the form of the polynomial is:
!
!      p(x) = a(1) + a(2) * x + ... + a(n-1) * x^(n-2) + a(n) * x^(n-1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the order of the polynomial.
!
!    Input, real (kind=4) :: A(N), the coefficients of the polynomial.
!    A(1) is the constant term.
!
!    Input, real (kind=4) :: X, the point at which the polynomial is
!    to be evaluated.
!
!    Output, real (kind=4) :: R4POLY_VALUE, the value of the polynomial at X.
!
  implicit none

  integer (kind=4) :: n

  real (kind=4) :: a(n)
  integer (kind=4) :: i
  real (kind=4) :: r4poly_value
  real (kind=4) :: x

  r4poly_value = a(n)
  do i = n - 1, 1, -1
    r4poly_value = r4poly_value * x + a(i)
  end do

  return
end
subroutine r4poly2_ex ( x1, y1, x2, y2, x3, y3, x, y, ierror )

!*****************************************************************************80
!
!! R4POLY2_EX finds the extremal point of a parabola determined by three points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X1, Y1, X2, Y2, X3, Y3, the coordinates of
!    three points on the parabola.  X1, X2 and X3 must be distinct.
!
!    Output, real (kind=4) :: X, Y, the X coordinate of the extremal point
!    of the parabola, and the value of the parabola at that point.
!
!    Output, integer (kind=4) :: IERROR, error flag.
!    0, no error.
!    1, two of the X values are equal.
!    2, the data lies on a straight line; there is no finite extremal
!    point.
!
  implicit none

  real (kind=4) :: bot
  integer (kind=4) :: ierror
  real (kind=4) :: x
  real (kind=4) :: x1
  real (kind=4) :: x2
  real (kind=4) :: x3
  real (kind=4) :: y
  real (kind=4) :: y1
  real (kind=4) :: y2
  real (kind=4) :: y3

  ierror = 0

  if ( x1 == x2 .or. x2 == x3 .or. x3 == x1 ) then
    ierror = 1
    return
  end if

  if ( y1 == y2 .and. y2 == y3 .and. y3 == y1 ) then
    x = x1
    y = y1
    return
  end if

  bot = ( x2 - x3 ) * y1 - ( x1 - x3 ) * y2 + ( x1 - x2 ) * y3

  if ( bot == 0.0E+00 ) then
    ierror = 2
    return
  end if

  x = 0.5E+00 * ( &
          x1**2 * ( y3 - y2 ) &
        + x2**2 * ( y1 - y3 ) &
        + x3**2 * ( y2 - y1 ) ) / bot

  y = ( &
         ( x - x2 ) * ( x - x3 ) * ( x2 - x3 ) * y1 &
       - ( x - x1 ) * ( x - x3 ) * ( x1 - x3 ) * y2 &
       + ( x - x1 ) * ( x - x2 ) * ( x1 - x2 ) * y3 ) / &
       ( ( x1 - x2 ) * ( x2 - x3 ) * ( x1 - x3 ) )

  return
end
subroutine r4poly2_ex2 ( x1, y1, x2, y2, x3, y3, x, y, a, b, c, ierror )

!*****************************************************************************80
!
!! R4POLY2_EX2 finds extremal point of a parabola determined by three points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X1, Y1, X2, Y2, X3, Y3, the coordinates of
!    three points on the parabola.  X1, X2 and X3 must be distinct.
!
!    Output, real (kind=4) :: X, Y, the X coordinate of the extremal
!    point of the parabola, and the value of the parabola at that point.
!
!    Output, real (kind=4) :: A, B, C, the coefficients that define the
!    parabola: P(X) = A * X * X + B * X + C.
!
!    Output, integer (kind=4) :: IERROR, error flag.
!    0, no error.
!    1, two of the X values are equal.
!    2, the data lies on a straight line; there is no finite extremal
!    point.
!
  implicit none

  real (kind=4) :: a
  real (kind=4) :: b
  real (kind=4) :: c
  real (kind=4) :: det
  integer (kind=4) :: ierror
  real (kind=4) :: v(3,3)
  real (kind=4) :: w(3,3)
  real (kind=4) :: x
  real (kind=4) :: x1
  real (kind=4) :: x2
  real (kind=4) :: x3
  real (kind=4) :: y
  real (kind=4) :: y1
  real (kind=4) :: y2
  real (kind=4) :: y3

  ierror = 0

  if ( x1 == x2 .or. x2 == x3 .or. x3 == x1 ) then
    ierror = 1
    return
  end if

  if ( y1 == y2 .and. y2 == y3 .and. y3 == y1 ) then
    x = x1
    y = y1
    return
  end if
!
!  Set up the Vandermonde matrix.
!
  v(1,1) = 1.0E+00
  v(1,2) = x1
  v(1,3) = x1 * x1

  v(2,1) = 1.0E+00
  v(2,2) = x2
  v(2,3) = x2 * x2

  v(3,1) = 1.0E+00
  v(3,2) = x3
  v(3,3) = x3 * x3
!
!  Get the inverse.
!
  call r4mat_inverse_3d ( v, w, det )
!
!  Compute the parabolic coefficients.
!
  c = w(1,1) * y1 + w(1,2) * y2 + w(1,3) * y3
  b = w(2,1) * y1 + w(2,2) * y2 + w(2,3) * y3
  a = w(3,1) * y1 + w(3,2) * y2 + w(3,3) * y3
!
!  Determine the extremal point.
!
  if ( a == 0.0E+00 ) then
    ierror = 2
    return
  end if

  x = -b / ( 2.0E+00 * a )
  y = a * x * x + b * x + c

  return
end
subroutine r4poly2_root ( a, b, c, r1, r2 )

!*****************************************************************************80
!
!! R4POLY2_ROOT returns the two roots of a quadratic polynomial.
!
!  Discussion:
!
!    The polynomial has the form:
!
!      A * X * X + B * X + C = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A, B, C, the coefficients of the polynomial.
!    A must not be zero.
!
!    Output, complex ( kind = 4 ) R1, R2, the roots of the polynomial, which
!    might be real and distinct, real and equal, or complex conjugates.
!
  implicit none

  real (kind=4) :: a
  real (kind=4) :: b
  real (kind=4) :: c
  complex ( kind = 4 ) disc
  complex ( kind = 4 ) q
  complex ( kind = 4 ) r1
  complex ( kind = 4 ) r2

  if ( a == 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY2_ROOT - Fatal error!'
    write ( *, '(a)' ) '  The coefficient A is zero.'
    stop
  end if

  disc = b * b - 4.0E+00 * a * c
  q = -0.5E+00 * ( b + sign ( 1.0E+00, b ) * sqrt ( disc ) )
  r1 = q / a
  r2 = c / q

  return
end
subroutine r4poly2_rroot ( a, b, c, r1, r2 )

!*****************************************************************************80
!
!! R4POLY2_RROOT returns the real parts of the roots of a quadratic polynomial.
!
!  Example:
!
!     A    B    C       roots              R1   R2
!    --   --   --     ------------------   --   --
!     1   -4    3     1          3          1    3
!     1    0    4     2*i      - 2*i        0    0
!     2   -6    5     3 +   i    3 -   i    3    3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: A, B, C, the coefficients of the quadratic
!    polynomial A * X * X + B * X + C = 0 whose roots are desired.
!    A must not be zero.
!
!    Output, real (kind=4) :: R1, R2, the real parts of the roots
!    of the polynomial.
!
  implicit none

  real (kind=4) :: a
  real (kind=4) :: b
  real (kind=4) :: c
  real (kind=4) :: disc
  real (kind=4) :: q
  real (kind=4) :: r1
  real (kind=4) :: r2

  if ( a == 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY2_RROOT - Fatal error!'
    write ( *, '(a)' ) '  The coefficient A is zero.'
    stop
  end if

  disc = b * b - 4.0E+00 * a * c
  disc = max ( disc, 0.0E+00 )

  q = ( b + sign ( 1.0E+00, b ) * sqrt ( disc ) )
  r1 = -0.5E+00 * q / a
  r2 = -2.0E+00 * c / q

  return
end
subroutine r4poly2_val ( x1, y1, x2, y2, x3, y3, x, y, yp, ypp )

!*****************************************************************************80
!
!! R4POLY2_VAL evaluates a parabola defined by three data values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real (kind=4) :: X1, Y1, X2, Y2, X3, Y3, three pairs of data.
!    If the X values are distinct, then all the Y values represent
!    actual values of the parabola.
!
!    Three special cases are allowed:
!
!      X1 == X2 /= X3: Y2 is the derivative at X1;
!      X1 /= X2 == X3: Y3 is the derivative at X3;
!      X1 == X2 == X3: Y2 is the derivative at X1, and
!                      Y3 is the second derivative at X1.
!
!    Input, real (kind=4) :: X, an abscissa at which the parabola is to be
!    evaluated.
!
!    Output, real (kind=4) :: Y, YP, YPP, the values of the parabola and
!    its first and second derivatives at X.
!
  implicit none

  integer (kind=4) :: distinct
  real (kind=4) :: dif1
  real (kind=4) :: dif2
  real (kind=4) :: x
  real (kind=4) :: x1
  real (kind=4) :: x2
  real (kind=4) :: x3
  real (kind=4) :: y
  real (kind=4) :: y1
  real (kind=4) :: y2
  real (kind=4) :: y3
  real (kind=4) :: yp
  real (kind=4) :: ypp
!
!  If any X's are equal, put them and the Y data first.
!
  if ( x1 == x2 .and. x2 == x3 ) then
    distinct = 1
  else if ( x1 == x2 ) then
    distinct = 2
  else if ( x1 == x3 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY2_VAL - Fatal error!'
    write ( *, '(a)' ) '  X1 = X3 =/= X2.'
    write ( *, '(a,g14.6)' ) '  X1 = ', x1
    write ( *, '(a,g14.6)' ) '  X2 = ', x2
    write ( *, '(a,g14.6)' ) '  X3 = ', x3
    stop
  else if ( x2 == x3 ) then
    distinct = 2
    call r4_swap ( x1, x2 )
    call r4_swap ( x2, x3 )
    call r4_swap ( y1, y2 )
    call r4_swap ( y2, y3 )
  else
    distinct = 3
  end if
!
!  Set up the coefficients.
!
  if ( distinct == 1 ) then

    dif1 = y2
    dif2 = 0.5E+00 * y3

  else if ( distinct == 2 ) then

    dif1 = y2
    dif2 = ( ( y3 - y1 ) / ( x3 - x1 ) - y2 ) / ( x3 - x2 )

  else if ( distinct == 3 ) then

    dif1 = ( y2 - y1 ) / ( x2 - x1 )
    dif2 =  ( ( y3 - y1 ) / ( x3 - x1 ) &
            - ( y2 - y1 ) / ( x2 - x1 ) ) / ( x3 - x2 )

  end if
!
!  Evaluate.
!
  y = y1 + ( x - x1 ) * dif1 + ( x - x1 ) * ( x - x2 ) * dif2
  yp = dif1 + ( 2.0E+00 * x - x1 - x2 ) * dif2
  ypp = 2.0E+00 * dif2

  return
end
subroutine r4poly2_val2 ( dim_num, ndata, tdata, ydata, left, tval, yval )

!*****************************************************************************80
!
!! R4POLY2_VAL2 evaluates a parabolic interpolant through tabular data.
!
!  Discussion:
!
!    This routine is a utility routine used by OVERHAUSER_SPLINE_VAL.
!    It constructs the parabolic interpolant through the data in
!    3 consecutive entries of a table and evaluates this interpolant
!    at a given abscissa value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: DIM_NUM, the dimension of a single data point.
!    DIM_NUM must be at least 1.
!
!    Input, integer (kind=4) :: NDATA, the number of data points.
!    NDATA must be at least 3.
!
!    Input, real (kind=4) :: TDATA(NDATA), the abscissas of the data points.
!    The values in TDATA must be in strictly ascending order.
!
!    Input, real (kind=4) :: YDATA(DIM_NUM,NDATA), the data points
!    corresponding to the abscissas.
!
!    Input, integer (kind=4) :: LEFT, the location of the first of the three
!    consecutive data points through which the parabolic interpolant
!    must pass.  1 <= LEFT <= NDATA - 2.
!
!    Input, real (kind=4) :: TVAL, the value of T at which the parabolic
!    interpolant is to be evaluated.  Normally, TDATA(1) <= TVAL <= T(NDATA),
!    and the data will be interpolated.  For TVAL outside this range,
!    extrapolation will be used.
!
!    Output, real (kind=4) :: YVAL(DIM_NUM), the value of the parabolic
!    interpolant at TVAL.
!
  implicit none

  integer (kind=4) :: ndata
  integer (kind=4) :: dim_num

  real (kind=4) :: dif1
  real (kind=4) :: dif2
  integer (kind=4) :: i
  integer (kind=4) :: left
  real (kind=4) :: t1
  real (kind=4) :: t2
  real (kind=4) :: t3
  real (kind=4) :: tval
  real (kind=4) :: tdata(ndata)
  real (kind=4) :: ydata(dim_num,ndata)
  real (kind=4) :: y1
  real (kind=4) :: y2
  real (kind=4) :: y3
  real (kind=4) :: yval(dim_num)
!
!  Check.
!
  if ( left < 1 .or. ndata-2 < left ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY2_VAL2 - Fatal error!'
    write ( *, '(a)' ) '  LEFT < 1 or NDATA-2 < LEFT.'
    write ( *, '(a,i8)' ) '  LEFT = ', left
    stop
  end if

  if ( dim_num < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY2_VAL2 - Fatal error!'
    write ( *, '(a)' ) '  DIM_NUM < 1.'
    write ( *, '(a,i8)' ) '  DIM_NUM = ', dim_num
    stop
  end if
!
!  Copy out the three abscissas.
!
  t1 = tdata(left)
  t2 = tdata(left+1)
  t3 = tdata(left+2)

  if ( t2 <= t1 .or. t3 <= t2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY2_VAL2 - Fatal error!'
    write ( *, '(a)' ) '  T2 <= T1 or T3 <= T2.'
    write ( *, '(a,g14.6)' ) '  T1 = ', t1
    write ( *, '(a,g14.6)' ) '  T2 = ', t2
    write ( *, '(a,g14.6)' ) '  T3 = ', t3
    stop
  end if
!
!  Construct and evaluate a parabolic interpolant for the data
!  in each dimension.
!
  do i = 1, dim_num

    y1 = ydata(i,left)
    y2 = ydata(i,left+1)
    y3 = ydata(i,left+2)

    dif1 = ( y2 - y1 ) / ( t2 - t1 )
    dif2 = ( ( y3 - y1 ) / ( t3 - t1 ) &
           - ( y2 - y1 ) / ( t2 - t1 ) ) / ( t3 - t2 )

    yval(i) = y1 + ( tval - t1 ) * ( dif1 + ( tval - t2 ) * dif2 )

  end do

  return
end
subroutine r4poly3_root ( a, b, c, d, r1, r2, r3 )

!*****************************************************************************80
!
!! R4POLY3_ROOT returns the three roots of a cubic polynomial.
!
!  Discussion:
!
!    The polynomial has the form
!
!      A * X^3 + B * X^2 + C * X + D = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Parameters:
!
!    Input, real (kind=4) :: A, B, C, D, the coefficients of the polynomial.
!    A must not be zero.
!
!    Output, complex ( kind = 4 ) R1, R2, R3, the roots of the polynomial, which
!    will include at least one real root.
!
  implicit none

  real (kind=4) :: a
  real (kind=4) :: b
  real (kind=4) :: c
  real (kind=4) :: d
  complex ( kind = 4 ) i
  complex ( kind = 4 ) one
  real ( kind = 4 ), parameter :: pi = 3.141592653589793E+00
  real (kind=4) :: q
  real (kind=4) :: r
  complex ( kind = 4 ) r1
  complex ( kind = 4 ) r2
  complex ( kind = 4 ) r3
  real (kind=4) :: s1
  real (kind=4) :: s2
  real (kind=4) :: temp
  real (kind=4) :: theta

  if ( a == 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY3_ROOT - Fatal error!'
    write ( *, '(a)' ) '  A must not be zero!'
    stop
  end if

  one = cmplx ( 1.0d+00, 0.0E+00, kind = 4 )
  i = sqrt ( -one )

  q = ( ( b / a )**2 - 3.0E+00 * ( c / a ) ) / 9.0E+00

  r = ( 2.0E+00 * ( b / a )**3 - 9.0E+00 * ( b / a ) * ( c / a ) &
      + 27.0E+00 * ( d / a ) ) / 54.0E+00

  if ( r * r < q * q * q ) then

    theta = acos ( r / sqrt ( q**3 ) )
    r1 = -2.0E+00 * sqrt ( q ) * cos (   theta                  / 3.0E+00 )
    r2 = -2.0E+00 * sqrt ( q ) * cos ( ( theta + 2.0E+00 * pi ) / 3.0E+00 )
    r3 = -2.0E+00 * sqrt ( q ) * cos ( ( theta + 4.0E+00 * pi ) / 3.0E+00 )

  else if ( q * q * q <= r * r ) then

    temp = -r + sqrt ( r**2 - q**3 )
    s1 = sign ( 1.0E+00, temp ) * ( abs ( temp ) )**(1.0E+00/3.0E+00)

    temp = -r - sqrt ( r**2 - q**3 )
    s2 = sign ( 1.0E+00, temp ) * ( abs ( temp ) )**(1.0E+00/3.0E+00)

    r1 = s1 + s2
    r2 = -0.5E+00 * ( s1 + s2 ) + i * 0.5E+00 * sqrt ( 3.0E+00 ) * ( s1 - s2 )
    r3 = -0.5E+00 * ( s1 + s2 ) - i * 0.5E+00 * sqrt ( 3.0E+00 ) * ( s1 - s2 )

  end if

  r1 = r1 - b / ( 3.0E+00 * a )
  r2 = r2 - b / ( 3.0E+00 * a )
  r3 = r3 - b / ( 3.0E+00 * a )

  return
end


subroutine r4poly4_root ( a, b, c, d, e, r1, r2, r3, r4 )

!*****************************************************************************80
!
!! R4POLY4_ROOT returns the four roots of a quartic polynomial.
!
!  Discussion:
!
!    The polynomial has the form:
!
!      A * X^4 + B * X^3 + C * X^2 + D * X + E = 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2011
!
!  Parameters:
!
!    Input, real (kind=4) :: A, B, C, D, the coefficients of the polynomial.
!    A must not be zero.
!
!    Output, complex ( kind = 4 ) R1, R2, R3, R4, the roots of the polynomial.
!
  implicit none

  real (kind=4) :: a
  real (kind=4) :: a3
  real (kind=4) :: a4
  real (kind=4) :: b
  real (kind=4) :: b3
  real (kind=4) :: b4
  real (kind=4) :: c
  real (kind=4) :: c3
  real (kind=4) :: c4
  real (kind=4) :: d
  real (kind=4) :: d3
  real (kind=4) :: d4
  real (kind=4) :: e
  complex ( kind = 4 ) p
  complex ( kind = 4 ) q
  complex ( kind = 4 ) r
  complex ( kind = 4 ) r1
  complex ( kind = 4 ) r2
  complex ( kind = 4 ) r3
  complex ( kind = 4 ) r4
  complex ( kind = 4 ) zero

  zero = cmplx ( 0.0E+00, 0.0E+00, kind = 4 )

  if ( a == 0.0E+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4POLY4_ROOT - Fatal error!'
    write ( *, '(a)') '  A must not be zero!'
    stop
  end if

  a4 = b / a
  b4 = c / a
  c4 = d / a
  d4 = e / a
!
!  Set the coefficients of the resolvent cubic equation.
!
  a3 = 1.0E+00
  b3 = -b4
  c3 = a4 * c4 - 4.0E+00 * d4
  d3 = -a4 * a4 * d4 + 4.0E+00 * b4 * d4 - c4 * c4
!
!  Find the roots of the resolvent cubic.
!
  call r4poly3_root ( a3, b3, c3, d3, r1, r2, r3 )
!
!  Choose one root of the cubic, here R1.
!
!  Set R = sqrt ( 0.25E+00 * A4**2 - B4 + R1 )
!
  r = sqrt ( 0.25E+00 * a4**2 - b4 + r1 )

  if ( r /= zero ) then

    p = sqrt ( 0.75E+00 * a4**2 - r**2 - 2.0E+00 * b4 &
        + 0.25E+00 * ( 4.0E+00 * a4 * b4 - 8.0E+00 * c4 - a4**3 ) / r )

    q = sqrt ( 0.75E+00 * a4**2 - r**2 - 2.0E+00 * b4 &
        - 0.25E+00 * ( 4.0E+00 * a4 * b4 - 8.0E+00 * c4 - a4**3 ) / r )

  else

    p = sqrt ( 0.75E+00 * a4**2 - 2.0E+00 * b4 &
      + 2.0E+00 * sqrt ( r1**2 - 4.0E+00 * d4 ) )

    q = sqrt ( 0.75E+00 * a4**2 - 2.0E+00 * b4 &
      - 2.0E+00 * sqrt ( r1**2 - 4.0E+00 * d4 ) )

  end if
!
!  Set the roots.
!
  r1 = -0.25E+00 * a4 + 0.5E+00 * r + 0.5E+00 * p
  r2 = -0.25E+00 * a4 + 0.5E+00 * r - 0.5E+00 * p
  r3 = -0.25E+00 * a4 - 0.5E+00 * r + 0.5E+00 * q
  r4 = -0.25E+00 * a4 - 0.5E+00 * r - 0.5E+00 * q

  return
end


end module jburk_r4poly_
