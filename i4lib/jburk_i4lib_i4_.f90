module     jburk_i4lib_i4_
use, intrinsic :: iso_fortran_env
implicit none

contains

function i4_bit_hi1 ( n )

    !*****************************************************************************80
    !
    !! I4_BIT_HI1 returns the position of the high 1 bit base 2 in an I4.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Example:
    !
    !       N    Binary    Hi 1
    !    ----    --------  ----
    !       0           0     0
    !       1           1     1
    !       2          10     2
    !       3          11     2
    !       4         100     3
    !       5         101     3
    !       6         110     3
    !       7         111     3
    !       8        1000     4
    !       9        1001     4
    !      10        1010     4
    !      11        1011     4
    !      12        1100     4
    !      13        1101     4
    !      14        1110     4
    !      15        1111     4
    !      16       10000     5
    !      17       10001     5
    !    1023  1111111111    10
    !    1024 10000000000    11
    !    1025 10000000001    11
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    01 June 2007
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the integer to be measured.
    !    N should be nonnegative.  If N is nonpositive, the function
    !    will always be 0.
    !
    !    Output, integer ( kind = 4 ) I4_BIT_HI1, the position of the highest bit.
    !
      implicit none
    
      integer ( kind = 4 ) bit
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_bit_hi1
      integer ( kind = 4 ) n
    
      i = n
      bit = 0
    
      do
    
        if ( i <= 0 ) then
          exit
        end if
    
        bit = bit + 1
        i = i / 2
    
      end do
    
      i4_bit_hi1 = bit
    
      return
    end
    function i4_bit_lo0 ( n )
    
    !*****************************************************************************80
    !
    !! I4_BIT_LO0 returns the position of the low 0 bit base 2 in an I4.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Example:
    !
    !       N    Binary    Lo 0
    !    ----    --------  ----
    !       0           0     1
    !       1           1     2
    !       2          10     1
    !       3          11     3
    !       4         100     1
    !       5         101     2
    !       6         110     1
    !       7         111     4
    !       8        1000     1
    !       9        1001     2
    !      10        1010     1
    !      11        1011     3
    !      12        1100     1
    !      13        1101     2
    !      14        1110     1
    !      15        1111     5
    !      16       10000     1
    !      17       10001     2
    !    1023  1111111111     1
    !    1024 10000000000     1
    !    1025 10000000001     1
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    16 February 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the integer to be measured.
    !    N should be nonnegative.
    !
    !    Output, integer ( kind = 4 ) I4_BIT_LO0, the position of the low 1 bit.
    !
      implicit none
    
      integer ( kind = 4 ) bit
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) i4_bit_lo0
      integer ( kind = 4 ) n
    
      bit = 0
      i = n
    
      do
    
        bit = bit + 1
        i2 = i / 2
    
        if ( i == 2 * i2 ) then
          exit
        end if
    
        i = i2
    
      end do
    
      i4_bit_lo0 = bit
    
      return
    end
    function i4_bit_lo1 ( n )
    
    !*****************************************************************************80
    !
    !! I4_BIT_LO1 returns the position of the low 1 bit base 2 in an I4.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Example:
    !
    !       N    Binary    Lo 1
    !    ----    --------  ----
    !       0           0     0
    !       1           1     1
    !       2          10     2
    !       3          11     1
    !       4         100     3
    !       5         101     1
    !       6         110     2
    !       7         111     1
    !       8        1000     4
    !       9        1001     1
    !      10        1010     2
    !      11        1011     1
    !      12        1100     3
    !      13        1101     1
    !      14        1110     2
    !      15        1111     1
    !      16       10000     5
    !      17       10001     1
    !    1023  1111111111     1
    !    1024 10000000000    11
    !    1025 10000000001     1
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    16 February 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the integer to be measured.
    !    N should be nonnegative.
    !
    !    Output, integer ( kind = 4 ) I4_BIT_LO1, the position of the low 1 bit.
    !
      implicit none
    
      integer ( kind = 4 ) bit
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) i4_bit_lo1
      integer ( kind = 4 ) n
    
      bit = 0
      i = n
    
      do
    
        bit = bit + 1
        i2 = i / 2
    
        if ( i /= 2 * i2 ) then
          exit
        end if
    
        i = i2
    
      end do
    
      i4_bit_lo1 = bit
    
      return
    end
    function i4_bit_reverse ( i, n )
    
    !*****************************************************************************80
    !
    !! I4_BIT_REVERSE reverses the bits in an I4.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Example:
    !
    !       I      N  2^N     I4_BIT_REVERSE ( I, N )
    !    ----    --------  -----------------------
    !       0      0    1     0
    !       1      0    1     1
    !
    !       0      3    8     0
    !       1      3    8     4
    !       2      3    8     2
    !       3      3    8     6
    !       4      3    8     1
    !       5      3    8     5
    !       6      3    8     3
    !       7      3    8     7
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    18 March 2008
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, the integer to be bit reversed.
    !    I should be nonnegative.  Normally I < 2^N.
    !
    !    Input, integer ( kind = 4 ) N, indicates the number of bits to
    !    be reverse (N+1) or the base with respect to which the integer is to
    !    be reversed (2^N).  N should be nonnegative.
    !
    !    Output, integer ( kind = 4 ) I4_BIT_REVERSE, the bit reversed value.
    !
      implicit none
    
      integer ( kind = 4 ) b
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_bit_reverse
      integer ( kind = 4 ) j
      integer ( kind = 4 ) n
      integer ( kind = 4 ) value
    
      if ( i < 0 ) then
    
        value = -1
    
      else if ( n < 0 ) then
    
        value = -1
    
      else
    
        b = 2**n
        j = mod ( i, b )
    
        value = 0
    
        do
    
          if ( b == 1 ) then
    
            value = value + j
            j = 0
            exit
    
          else
    
            if ( mod ( j, 2 ) == 1 ) then
              value = value + b / 2
              j = j - 1
            end if
    
            j = j / 2
            b = b / 2
    
          end if
    
        end do
    
      end if
    
      i4_bit_reverse = value
    
      return
    end
    function i4_ceiling ( r )
    
    !*****************************************************************************80
    !
    !! I4_CEILING rounds an R8 "up" (towards +oo) to the next I4.
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
    !    10 November 2011
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( kind = 8 ) R, the value to be rounded up.
    !
    !    Output, integer ( kind = 4 ) I4_CEILING, the rounded value.
    !
      implicit none
    
      integer ( kind = 4 ) i4_ceiling
      real ( kind = 8 ) r
      integer ( kind = 4 ) value
    
      value = int ( r )
      if ( real ( value, kind = 8 ) < r ) then
        value = value + 1
      end if
    
      i4_ceiling = value
    
      return
    end
    function i4_characteristic ( q )
    
    !*****************************************************************************80
    !
    !! I4_CHARACTERISTIC gives the characteristic for an I4.
    !
    !  Discussion:
    !
    !    For any positive integer Q, the characteristic is:
    !
    !    Q, if Q is a prime;
    !    P, if Q = P^N for some prime P and some integer N;
    !    0, otherwise, that is, if Q is negative, 0, 1, or the product
    !       of more than one distinct prime.
    !
    !    An I4 is an integer ( kind = 4 ) value.
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
    !    FORTRAN90 version by John Burkardt
    !
    !  Reference:
    !
    !    Paul Bratley, Bennett Fox, Harald Niederreiter,
    !    Algorithm 738:
    !    Programs to Generate Niederreiter's Low-Discrepancy Sequences,
    !    ACM Transactions on Mathematical Software,
    !    Volume 20, Number 4, 1994, pages 494-495.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) Q, the value to be tested.
    !
    !    Output, integer ( kind = 4 ) I4_CHARACTERISTIC, the characteristic of Q.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_characteristic
      integer ( kind = 4 ) i_max
      integer ( kind = 4 ) q
      integer ( kind = 4 ) q_copy
    
      if ( q <= 1 ) then
        i4_characteristic = 0
        return
      end if
    !
    !  If Q is not prime, then there is at least one prime factor
    !  of Q no greater than SQRT(Q)+1.
    !
    !  A faster code would only consider prime values of I,
    !  but that entails storing a table of primes and limiting the
    !  size of Q.  Simplicity and flexibility for now!
    !
      i_max = int ( sqrt ( real ( q ) ) ) + 1
      q_copy = q
    
      do i = 2, i_max
    
        if ( mod ( q_copy, i ) == 0 ) then
    
          do while ( mod ( q_copy, i ) == 0 )
            q_copy = q_copy / i
          end do
    
          if ( q_copy == 1 ) then
            i4_characteristic = i
          else
            i4_characteristic = 0
          end if
    
          return
    
        end if
    
      end do
    !
    !  If no factor was found, then Q is prime.
    !
      i4_characteristic = q
    
      return
    end
    function i4_choose ( n, k )
    
    !*****************************************************************************80
    !
    !! I4_CHOOSE computes the binomial coefficient C(N,K) as an I4.
    !
    !  Discussion:
    !
    !    The value is calculated in such a way as to avoid overflow and
    !    roundoff.  The calculation is done in integer arithmetic.
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
    !    02 June 2007
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
    !    Input, integer ( kind = 4 ) N, K, are the values of N and K.
    !
    !    Output, integer ( kind = 4 ) I4_CHOOSE, the number of combinations of N
    !    things taken K at a time.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_choose
      integer ( kind = 4 ) k
      integer ( kind = 4 ) mn
      integer ( kind = 4 ) mx
      integer ( kind = 4 ) n
      integer ( kind = 4 ) value
    
      mn = min ( k, n - k )
    
      if ( mn < 0 ) then
    
        value = 0
    
      else if ( mn == 0 ) then
    
        value = 1
    
      else
    
        mx = max ( k, n - k )
        value = mx + 1
    
        do i = 2, mn
          value = ( value * ( mx + i ) ) / i
        end do
    
      end if
    
      i4_choose = value
    
      return
    end
    function i4_div_rounded ( a, b )
    
    !*****************************************************************************80
    !
    !! I4_DIV_ROUNDED computes the rounded result of I4 division.
    !
    !  Discussion:
    !
    !    This routine computes C = A / B, where A, B and C are integers
    !    and C is the closest integer value to the exact real result.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    23 October 2011
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) A, the number to be divided.
    !
    !    Input, integer ( kind = 4 ) B, the divisor, or the number of parts.
    !
    !    Output, integer ( kind = 4 ) I4_DIV_ROUNDED, the rounded result
    !    of the division.
    !
      implicit none
    
      integer ( kind = 4 ) a
      integer ( kind = 4 ) a_abs
      integer ( kind = 4 ) b
      integer ( kind = 4 ) b_abs
      integer ( kind = 4 ) i4_div_rounded
      integer ( kind = 4 ), parameter :: i4_huge = 2147483647
      integer ( kind = 4 ) value
    
      if ( a == 0 .and. b == 0 ) then
    
        value = i4_huge
     
      else if ( a == 0 ) then
    
        value = 0
    
      else if ( b == 0 ) then
    
        if ( a < 0 ) then
          value = - i4_huge
        else
          value = + i4_huge
        end if
    
      else
    
        a_abs = abs ( a )
        b_abs = abs ( b )
    
        value = a_abs / b_abs
    !
    !  Round the value.
    !
        if ( ( 2 * value + 1 ) * b_abs < 2 * a_abs ) then
          value = value + 1
        end if
    !
    !  Set the sign.
    !
        if ( ( a < 0 .and. 0 < b ) .or. ( 0 < a .and. b < 0 ) ) then
          value = - value
        end if
    
      end if
    
      i4_div_rounded = value
    
      return
    end
    function i4_divp ( i, j )
    
    !*****************************************************************************80
    !
    !! I4_DIVP returns the smallest multiple of J greater than or equal to I.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Example:
    !
    !    I  J  I4_DIVP(I,J)
    !
    !    0  4    0
    !    1  4    1
    !    2  4    1
    !    3  4    1
    !    4  4    1
    !    5  4    2
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    03 March 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, the number to be analyzed.
    !
    !    Input, integer ( kind = 4 ) J, the number, multiples of which will
    !    be compared against I.  J may not be zero.
    !
    !    Output, integer ( kind = 4 ) I4_DIVP, the smallest multiple of J that
    !    is greater than or equal to I.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_divp
      integer ( kind = 4 ) j
    
      if ( j /= 0 ) then
        i4_divp = 1 + ( i - 1 ) / j
      else
        i4_divp = 0
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_DIVP - Fatal error!'
        write ( *, '(a)' ) '  The input value of J was zero!'
        stop
      end if
    
      return
    end
    function i4_even ( i )
    
    !*****************************************************************************80
    !
    !! I4_EVEN returns TRUE if an I4 is even.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 May 2002
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, the integer to be tested.
    !
    !    Output, logical I4_EVEN, is TRUE if I is even.
    !
      implicit none
    
      integer ( kind = 4 ) i
      logical i4_even
    
      i4_even = ( mod ( i, 2 ) == 0 )
    
      return
    end
    function i4_factorial ( n )
    
    !*****************************************************************************80
    !
    !! I4_FACTORIAL computes the factorial of N.
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
    !    26 June 2008
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the argument of the factorial function.
    !    If N is less than 1, the function value is returned as 1.
    !    0 <= N <= 13 is required.
    !
    !    Output, integer ( kind = 4 ) I4_FACTORIAL, the factorial of N.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_factorial
      integer ( kind = 4 ) n
    
      i4_factorial = 1
    
      if ( 13 < n ) then
        i4_factorial = - 1
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_FACTORIAL - Fatal error!'
        write ( *, '(a)' ) '  I4_FACTORIAL(N) cannot be computed as an integer'
        write ( *, '(a)' ) '  for 13 < N.'
        write ( *, '(a,i8)' ) '  Input value N = ', n
        stop
      end if
    
      do i = 1, n
        i4_factorial = i4_factorial * i
      end do
    
      return
    end
    function i4_factorial2 ( n )
    
    !*****************************************************************************80
    !
    !! I4_FACTORIAL2 computes the double factorial function.
    !
    !  Discussion:
    !
    !    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
    !                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license. 
    !
    !  Modified:
    !
    !    12 December 2001
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the argument of the double factorial 
    !    function.  If N is less than 1, I4_FACTORIAL2 is returned as 1.
    !
    !    Output, integer ( kind = 4 ) I4_FACTORIAL2, the value of N!!.
    !
      implicit none
    
      integer ( kind = 4 ) i4_factorial2
      integer ( kind = 4 ) n
      integer ( kind = 4 ) n_copy
    
      if ( n < 1 ) then
        i4_factorial2 = 1
        return
      end if
    
      n_copy = n
      i4_factorial2 = 1
    
      do while ( 1 < n_copy )
        i4_factorial2 = i4_factorial2 * n_copy
        n_copy = n_copy - 2
      end do
    
      return
    end
    function i4_floor ( r )
    
    !*****************************************************************************80
    !
    !! I4_FLOOR rounds an R8 "down" (towards -oo) to the nearest I4.
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
    !    10 November 2011
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, real ( kind = 8 ) R, the value to be rounded down.
    !
    !    Output, integer ( kind = 4 ) I4_FLOOR, the rounded value.
    !
      implicit none
    
      integer ( kind = 4 ) i4_floor
      real ( kind = 8 ) r
      integer ( kind = 4 ) value
    
      value = int ( r )
      if ( r < real ( value, kind = 8 ) ) then
        value = value - 1
      end if
    
      i4_floor = value
    
      return
    end
    subroutine i4_fraction ( i, j, k )
    
    !*****************************************************************************80
    !
    !! I4_FRACTION computes a ratio and returns an integer result.
    !
    !  Discussion:
    !
    !    Given integer variables I and J, FORTRAN will evaluate the expression
    !    "I/J" using integer arithmetic.  This routine, which carries out the
    !    same operation, is thus not needed in FORTRAN.  It is provided simply
    !    to match the corresponding function in MATLAB, where the default
    !    result of "I/J" is a real number.
    !
    !  Example:
    !
    !       I     J     Real     K = I4_FRACTION ( I, J)
    !
    !       1     2     0.5      0
    !       8     4     2.00     2
    !       9     4     2.25     2
    !       7     4     1.75     1
    !      -7     4    -1.75    -1
    !       7    -4    -1.75    -1
    !      -7    -4     1.75     1
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
    !    Input, integer ( kind = 4 ) I, J, the arguments.
    !
    !    Output, integer ( kind = 4 ) K, the value of the ratio.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
    
      k = i / j
    
      return
    end
    function i4_gcd ( i, j )
    
    !*****************************************************************************80
    !
    !! I4_GCD finds the greatest common divisor of two I4's.
    !
    !  Discussion:
    !
    !    Note that only the absolute values of I and J are
    !    considered, so that the result is always nonnegative.
    !
    !    If I or J is 0, I4_GCD is returned as max ( 1, abs ( I ), abs ( J ) ).
    !
    !    If I and J have no common factor, I4_GCD is returned as 1.
    !
    !    Otherwise, using the Euclidean algorithm, I4_GCD is the
    !    greatest common divisor of I and J.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    25 March 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, J, two numbers whose GCD is desired.
    !
    !    Output, integer ( kind = 4 ) I4_GCD, the greatest common divisor
    !    of I and J.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_gcd
      integer ( kind = 4 ) j
      integer ( kind = 4 ) p
      integer ( kind = 4 ) q
      integer ( kind = 4 ) r
    
      i4_gcd = 1
    !
    !  Return immediately if either I or J is zero.
    !
      if ( i == 0 ) then
        i4_gcd = max ( 1, abs ( j ) )
        return
      else if ( j == 0 ) then
        i4_gcd = max ( 1, abs ( i ) )
        return
      end if
    !
    !  Set P to the larger of I and J, Q to the smaller.
    !  This way, we can alter P and Q as we go.
    !
      p = max ( abs ( i ), abs ( j ) )
      q = min ( abs ( i ), abs ( j ) )
    !
    !  Carry out the Euclidean algorithm.
    !
      do
    
        r = mod ( p, q )
    
        if ( r == 0 ) then
          exit
        end if
    
        p = q
        q = r
    
      end do
    
      i4_gcd = q
    
      return
    end
    function i4_gcdb ( i, j, k )
    
    !*****************************************************************************80
    !
    !! I4_GCDB finds the greatest common divisor of the form K**N of two I4's.
    !
    !  Discussion:
    !
    !    Note that if J is negative, I4_GCDB will also be negative.
    !    This is because it is likely that the caller is forming
    !    the fraction I/J, and so any minus sign should be
    !    factored out of J.
    !
    !    If I and J are both zero, I4_GCDB is returned as 1.
    !
    !    If I is zero and J is not, I4_GCDB is returned as J,
    !    and vice versa.
    !
    !    If I and J are nonzero, and have no common divisor of the
    !    form K**N, I4_GCDB is returned as 1.
    !
    !    Otherwise, I4_GCDB is returned as the largest common divisor
    !    of the form K**N shared by I and J.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    03 March 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, J, two numbers whose greatest common
    !    divisor K**N is desired.
    !
    !    Input, integer ( kind = 4 ) K, the possible divisor of I and J.
    !
    !    Output, integer ( kind = 4 ) I4_GCDB, the greatest common divisor of
    !    the form K**N shared by I and J.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) icopy
      integer ( kind = 4 ) i4_gcdb
      integer ( kind = 4 ) j
      integer ( kind = 4 ) jcopy
      integer ( kind = 4 ) k
    
      i4_gcdb = 1
    !
    !  If both I and J are zero, I4_GCDB is 1.
    !
      if ( i == 0 .and. j == 0 ) then
        i4_gcdb = 1
        return
      end if
    !
    !  If just one of I and J is zero, I4_GCDB is the other one.
    !
      if ( i == 0 ) then
        i4_gcdb = j
        return
      else if ( j == 0 ) then
        i4_gcdb = i
        return
      end if
    !
    !  Divide out K as long as you can.
    !
      if ( 0 < j ) then
        i4_gcdb = 1
      else
        i4_gcdb = -1
      end if
    
      icopy = i
      jcopy = j
    
      do
    
        if ( mod ( icopy, k ) /= 0 .or. mod ( jcopy, k ) /= 0 ) then
          exit
        end if
    
        i4_gcdb = i4_gcdb * k
        icopy = icopy / k
        jcopy = jcopy / k
    
      end do
    
      return
    end
    function i4_huge ( )
    
    !*****************************************************************************80
    !
    !! I4_HUGE returns a "huge" I4.
    !
    !  Discussion:
    !
    !    On an IEEE 32 bit machine, I4_HUGE should be 2^31 - 1, and its
    !    bit pattern should be
    !
    !     01111111111111111111111111111111
    !
    !    In this case, its numerical value is 2147483647.
    !
    !    Using the Dec/Compaq/HP Alpha FORTRAN compiler FORT, I could
    !    use I4_HUGE() and HUGE interchangeably.
    !
    !    However, when using the G95, the values returned by HUGE were
    !    not equal to 2147483647, apparently, and were causing severe
    !    and obscure errors in my random number generator, which needs to
    !    add I4_HUGE to the seed whenever the seed is negative.  So I
    !    am backing away from invoking HUGE, whereas I4_HUGE is under
    !    my control.
    !
    !    Explanation: because under G95 the default integer type is 64 bits!
    !    So HUGE ( 1 ) = a very very huge integer indeed, whereas
    !    I4_HUGE ( ) = the same old 32 bit big value.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    26 January 2007
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Output, integer ( kind = 4 ) I4_HUGE, a "huge" I4.
    !
      implicit none
    
      integer ( kind = 4 ) i4_huge
    
      i4_huge = 2147483647
    
      return
    end
    function i4_huge_normalizer ( )
    
    !*****************************************************************************80
    !
    !! I4_HUGE_NORMALIZER returns the "normalizer" for I4_HUGE.
    !
    !  Discussion:
    !
    !    The value returned is 1 / ( I4_HUGE + 1 ).
    !
    !    For any I4, it should be the case that
    !
    !     -1 < I4 * I4_HUGE_NORMALIZER < 1.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    25 January 2007
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Output, real ( kind = 8 ) I4_HUGE_NORMALIZER, the "normalizer"
    !    for I4_HUGE.
    !
      implicit none
    
      real ( kind = 8 ) i4_huge_normalizer
    
      i4_huge_normalizer = 4.656612873077392578125D-10
    
      return
    end
    function i4_is_power_of_2 ( n )
    
    !*****************************************************************************80
    !
    !! I4_IS_POWER_OF_2 reports whether an I4 is a power of 2.
    !
    !  Discussion:
    !
    !    The powers of 2 are 1, 2, 4, 8, 16, and so on.
    !
    !    An I4 is an integer ( kind = 4 ) value.
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
    !    Input, integer ( kind = 4 ) N, the integer to be tested.
    !
    !    Output, logical I4_IS_POWER_OF_2, is TRUE if N is a power of 2.
    !
      implicit none
    
      logical i4_is_power_of_2
      integer ( kind = 4 ) n
      integer ( kind = 4 ) n_copy
    
      n_copy = n
      i4_is_power_of_2 = .false.
    
      if ( n_copy <= 0 ) then
        return
      end if
    
      do while ( n_copy /= 1 )
    
        if ( mod ( n_copy, 2 ) == 1 ) then
          return
        end if
    
        n_copy = n_copy / 2
    
      end do
    
      i4_is_power_of_2 = .true.
    
      return
    end
    function i4_is_prime ( n )
    
    !*****************************************************************************80
    !
    !! I4_IS_PRIME reports whether an I4 is prime.
    !
    !  Discussion:
    !
    !    A simple, unoptimized sieve of Erasthosthenes is used to
    !    check whether N can be divided by any integer between 2
    !    and SQRT(N).
    !
    !    Note that negative numbers, 0 and 1 are not considered prime.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    28 March 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer N, the integer to be tested.
    !
    !    Output, logical I4_IS_PRIME, is TRUE if N is prime, and FALSE
    !    otherwise.
    !
      implicit none
    
      integer ( kind = 4 ) i
      logical i4_is_prime
      integer ( kind = 4 ) n
      integer ( kind = 4 ) nhi
    
      if ( n <= 0 ) then
        i4_is_prime = .false.
        return
      end if
    
      if ( n == 1 ) then
        i4_is_prime = .false.
        return
      end if
    
      if ( n <= 3 ) then
        i4_is_prime = .true.
        return
      end if
    
      nhi = int ( sqrt ( real ( n ) ) )
    
      do i = 2, nhi
        if ( mod ( n, i ) == 0 ) then
          i4_is_prime = .false.
          return
        end if
      end do
    
      i4_is_prime = .true.
    
      return
    end
    function i4_lcm ( i, j )
    
    !*****************************************************************************80
    !
    !! I4_LCM computes the least common multiple of two I4's.
    !
    !  Discussion:
    !
    !    The least common multiple may be defined as
    !
    !      LCM(I,J) = ABS ( I * J ) / GCD(I,J)
    !
    !    where GCD(I,J) is the greatest common divisor of I and J.
    !
    !    An I4 is an integer ( kind = 4 ) value.
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
    !    Input, integer ( kind = 4 ) I, J, the integers whose I4_LCM is desired.
    !
    !    Output, integer ( kind = 4 ) I4_LCM, the least common multiple of I and J.
    !    I4_LCM is never negative.  I4_LCM is 0 if either I or J is zero.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_gcd
      integer ( kind = 4 ) j
      integer ( kind = 4 ) i4_lcm
    
      i4_lcm = abs ( i * ( j / i4_gcd ( i, j ) ) )
    
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
    function i4_log_2 ( i )
    
    !*****************************************************************************80
    !
    !! I4_LOG_2 returns the integer part of the logarithm base 2 of an I4.
    !
    !  Discussion:
    !
    !    For positive I4_LOG_2(I), it should be true that
    !      2^I4_LOG_2(X) <= |I| < 2^(I4_LOG_2(I)+1).
    !    The special case of I4_LOG_2(0) returns -HUGE().
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Example:
    !
    !     I  I4_LOG_2
    !
    !     0  -1
    !     1,  0
    !     2,  1
    !     3,  1
    !     4,  2
    !     5,  2
    !     6,  2
    !     7,  2
    !     8,  3
    !     9,  3
    !    10,  3
    !   127,  6
    !   128,  7
    !   129,  7
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    13 January 2003
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, the number whose logarithm base 2
    !    is desired.
    !
    !    Output, integer ( kind = 4 ) I4_LOG_2, the integer part of the
    !    logarithm base 2 of the absolute value of I.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i_abs
      integer ( kind = 4 ) i4_log_2
      integer ( kind = 4 ), parameter :: i4_huge = 2147483647
    
      if ( i == 0 ) then
    
        i4_log_2 = - i4_huge
    
      else
    
        i4_log_2 = 0
    
        i_abs = abs ( i )
    
        do while ( 2 <= i_abs )
          i_abs = i_abs / 2
          i4_log_2 = i4_log_2 + 1
        end do
    
      end if
    
      return
    end
    function i4_log_i4 ( i4, j4 )
    
    !*****************************************************************************80
    !
    !! I4_LOG_I4 returns the logarithm of an I4 to an I4 base.
    !
    !  Discussion:
    !
    !    Only the integer part of the logarithm is returned.
    !
    !    If
    !
    !      K4 = I4_LOG_J4 ( I4, J4 ),
    !
    !    then we ordinarily have
    !
    !      J4^(K4-1) < I4 <= J4^K4.
    !
    !    The base J4 should be positive, and at least 2.  If J4 is negative,
    !    a computation is made using the absolute value of J4.  If J4 is
    !    -1, 0, or 1, the logarithm is returned as 0.
    !
    !    The number I4 should be positive and at least 2.  If I4 is negative,
    !    a computation is made using the absolute value of I4.  If I4 is
    !    -1, 0, or 1, then the logarithm is returned as 0.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Example:
    !
    !    I4  J4  K4
    !
    !     0   3   0
    !     1   3   0
    !     2   3   0
    !     3   3   1
    !     4   3   1
    !     8   3   1
    !     9   3   2
    !    10   3   2
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    09 June 2007
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I4, the number whose logarithm is desired.
    !
    !    Input, integer ( kind = 4 ) J4, the base of the logarithms.
    !
    !    Output, integer ( kind = 4 ) I4_LOG_I4, the integer part of the logarithm
    !    base abs(J4) of abs(I4).
    !
      implicit none
    
      integer ( kind = 4 ) i4
      integer ( kind = 4 ) i4_abs
      integer ( kind = 4 ) i4_log_i4
      integer ( kind = 4 ) j4
      integer ( kind = 4 ) j4_abs
      integer ( kind = 4 ) value
    
      value = 0
    
      i4_abs = abs ( i4 )
    
      if ( 2 <= i4_abs ) then
    
        j4_abs = abs ( j4 )
    
        if ( 2 <= j4_abs ) then
    
          do while ( j4_abs <= i4_abs )
            i4_abs = i4_abs / j4_abs
            value = value + 1
          end do
    
        end if
    
      end if
    
      i4_log_i4 = value
    
      return
    end
    function i4_log_r8 ( x, b )
    
    !*****************************************************************************80
    !
    !! I4_LOG_R8 returns the logarithm of an I4 to an R8 base.
    !
    !  Discussion:
    !
    !    The base B should be positive, but in any case only the absolute
    !    value of B is considered.
    !
    !    The number X whose logarithm is desired should be positive, but
    !    in any case only the absolute value of X is considered.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !    An R8 is a real ( kind = 8 ) value.
    !
    !  Example:
    !
    !    If B is greater than 1, and X is positive:
    !
    !    if 1/B^2  <  X <= 1/B   I4_LOG_R8(X) = -1,
    !    if 1/B    <  X <= 1     I4_LOG_R8(X) = 0,
    !    if 1      <= X <  B,    I4_LOG_R8(X) = 0,
    !    if B      <= X <  B^2  I4_LOG_R8(X) = 1,
    !    if B^2    <= X <  B^3  I4_LOG_R8(X) = 2.
    !
    !    For positive I4_LOG_R8(X), it should be true that
    !
    !      ABS(B)^I4_LOG_R8(X) <= ABS(X) < ABS(B)^(I4_LOG_R8(X)+1).
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    09 April 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) X, the number whose logarithm base B is
    !    desired.  If X is 0, then I4_LOG_B is returned as -I4_HUGE().
    !
    !    Input, real ( kind = 8 ) B, the absolute value of the base of the
    !    logarithms.  B must not be -1, 0, or 1.
    !
    !    Output, integer ( kind = 4 ) I4_LOG_R8, the integer part of the logarithm
    !    base abs(B) of abs(X).
    !
      implicit none
    
      real ( kind = 8 ) b
      real ( kind = 8 ) b_abs
      integer ( kind = 4 ), parameter :: i4_huge = 2147483647
      integer ( kind = 4 ) i4_log_r8
      integer ( kind = 4 ) value_sign
      integer ( kind = 4 ) x
      real ( kind = 8 ) x_abs
    
      if ( x == 0 ) then
        i4_log_r8 = - i4_huge
        return
      end if
    
      b_abs = abs ( b )
      i4_log_r8 = 0
    
      if ( b_abs == 1.0D+00 ) then
        return
      end if
    
      if ( b == 0.0D+00 ) then
        return
      end if
    
      x_abs = abs ( real ( x ) )
    
      if ( b_abs < 1.0D+00 ) then
        value_sign = -1
        b_abs = 1.0D+00 / b_abs
      else
        value_sign = +1
      end if
    
      if ( 1.0D+00 <= x_abs .and. x_abs < b_abs ) then
        i4_log_r8 = value_sign * i4_log_r8
        return
      end if
    
      do while ( b_abs < x_abs )
        x_abs = x_abs / b_abs
        i4_log_r8 = i4_log_r8 + 1
      end do
    
      do while ( x_abs * b_abs <= 1.0D+00 )
        x_abs = x_abs * b_abs
        i4_log_r8 = i4_log_r8 - 1
      end do
    !
    !  If the absolute value of the base was less than 1, we inverted
    !  earlier.  Now negate the logarithm to account for that.
    !
      i4_log_r8 = value_sign * i4_log_r8
    
      return
    end
    subroutine i4_mant ( x, s, j, k, l )
    
    !*****************************************************************************80
    !
    !! I4_MANT computes the "mantissa" of a double precision number.
    !
    !  Discussion:
    !
    !    I4_MANT computes the "mantissa" or "fraction part" of a real
    !    number X, which it stores as a pair of integers, (J/K).
    !
    !    It also computes the sign, and the integer part of the logarithm
    !    (base 2) of X.
    !
    !    On return:
    !
    !      X = S * (J/K) * 2^L
    !
    !    where
    !
    !      S is +1 or -1,
    !      K is a power of 2,
    !      1 <= (J/K) < 2,
    !      L is an integer.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    15 August 2010
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
    !    Output, integer ( kind = 4 ) J, the top part of the mantissa fraction.
    !
    !    Output, integer ( kind = 4 ) K, the bottom part of the mantissa
    !    fraction.  K is a power of 2.
    !
    !    Output, integer ( kind = 4 ) L, the integer part of the logarithm
    !    (base 2) of X.
    !
      implicit none
    
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
      integer ( kind = 4 ) l
      integer ( kind = 4 ) s
      real ( kind = 8 ) x
      real ( kind = 8 ) xtemp
    !
    !  1: Handle the special case of 0.
    !
      if ( x == 0.0D+00 ) then
        s = 1
        j = 0
        k = 1
        l = 0
        return
      end if
    !
    !  2: Determine the sign S.
    !
      if ( 0.0D+00 < x ) then
        s = + 1
        xtemp = + x
      else
        s = - 1
        xtemp = - x
      end if
    !
    !  3: Force XTEMP to lie between 1 and 2, and compute the logarithm L.
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
    !  4: Now strip out the mantissa as J/K.
    !
      j = 0
      k = 1
    
      do
    
        j = 2 * j
    
        if ( 1.0D+00 <= xtemp ) then
          j = j + 1
          xtemp = xtemp - 1.0D+00
        end if
    
        if ( xtemp == 0.0D+00 ) then
          exit
        end if
    
        k = 2 * k
        xtemp = xtemp * 2.0D+00
    
      end do
    
      return
    end
    subroutine i4_mod_inv ( b, n, y )
    
    !*****************************************************************************80
    !
    !! I4_MOD_INV calculates the inverse of B mod N.
    !
    !  Discussion:
    !
    !    This function uses the extended Euclidean algorithm.
    !
    !    Unless the algorithm fails, the output value Y will satisfy
    !
    !      ( B * Y ) mod N = 1
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    22 August 2011
    !
    !  Author:
    !
    !    Original MATLAB version by Wade Trappe, Lawrence Washington.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    Wade Trappe, Lawrence Washington,
    !    Introduction to Cryptography with Coding Theory,
    !    Prentice Hall, 2005,
    !    ISBN13: 978-0131862395,
    !    LC: QA268.T73.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) B, the value whose inverse is desired.
    !    B must not be 0, or a multiple of N.  However, B can be negative.
    !
    !    Input, integer ( kind = 4 ) N, the value with respect to which the inverse
    !    is desired.  N must be 2 or greater.
    !
    !    Output, integer ( kind = 4 ) Y, the inverse of B mod N.  However, if the
    !    inverse does not exist, Y is returned as 0.
    !
      implicit none
    
      integer ( kind = 4 ) b
      integer ( kind = 4 ) b0
      integer ( kind = 4 ) n
      integer ( kind = 4 ) n0
      integer ( kind = 4 ) q
      integer ( kind = 4 ) r
      integer ( kind = 4 ) t
      integer ( kind = 4 ) t0
      integer ( kind = 4 ) temp
      integer ( kind = 4 ) y
    
      n0 = n
      b0 = abs ( b )
      t0 = 0
      t = 1
    
      q = ( n0 / b0 )
      r = n0 - q * b0
    
      do while ( 0 < r )
    
        temp = t0 - q * t
    
        if ( 0 <= temp ) then
          temp =     mod (   temp, n )
        else
          temp = n - mod ( - temp, n )
        end if
    
        n0 = b0
        b0 = r
        t0 = t
        t = temp
    
        q = ( n0 / b0 )
        r = n0 - q * b0
    
      end do
    
      if ( b0 /= 1 ) then
        y = 0
      else
        y = mod ( t, n )
        if ( b < 0 ) then
          y = - y
        end if
      end if
    
      return
    end
    subroutine i4_moddiv ( n, d, m, r )
    
    !*****************************************************************************80
    !
    !! I4_MODDIV breaks an I4 into a multiple of a divisor and remainder.
    !
    !  Discussion:
    !
    !    The formula used is:
    !
    !      N = M * D + R
    !
    !      0 <= || R || < || D ||
    !
    !    and R has the sign of N.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Example:
    !
    !      N         D       M      R
    !
    !     107       50      2      7
    !     107      -50     -2      7
    !    -107       50     -2     -7
    !    -107      -50      2     -7
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    01 April 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number to be decomposed.
    !
    !    Input, integer ( kind = 4 ) D, the divisor.  D may not be zero.
    !
    !    Output, integer ( kind = 4 ) M, the number of times N
    !    is evenly divided by D.
    !
    !    Output, integer ( kind = 4 ) R, a remainder.
    !
      implicit none
    
      integer ( kind = 4 ) d
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
      integer ( kind = 4 ) r
    
      if ( d == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_MODDIV - Fatal error!'
        write ( *, '(a)' ) '  Input divisor D = 0'
        stop
      end if
    
      m = n / d
      r = n - d * m
    
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
    function i4_mop ( i )
    
    !*****************************************************************************80
    !
    !! I4_MOP returns the I-th power of -1 as an I4.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    16 November 2007
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, the power of -1.
    !
    !    Output, integer ( kind = 4 ) I4_MOP, the I-th power of -1.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_mop
    
      if ( mod ( i, 2 ) == 0 ) then
        i4_mop = 1
      else
        i4_mop = -1
      end if
    
      return
    end
    function i4_odd ( i )
    
    !*****************************************************************************80
    !
    !! I4_ODD returns TRUE if an I4 is odd.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 May 2002
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, the integer to be tested.
    !
    !    Output, logical I4_ODD, is TRUE if I is odd.
    !
      implicit none
    
      integer ( kind = 4 ) i
      logical i4_odd
    
      i4_odd = ( mod ( i + 1, 2 ) == 0 )
    
      return
    end
    function i4_power ( i, j )
    
    !*****************************************************************************80
    !
    !! I4_POWER returns the integer power of an I4.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
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
    !    Input, integer ( kind = 4 ) I, J, the base and the power.
    !    J should be nonnegative.
    !
    !    Output, integer ( kind = 4 ) I4_POWER, the value of I^J.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_power
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
    
      if ( j < 0 ) then
    
        if ( i == 1 ) then
          i4_power = 1
        else if ( i == 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'I4_POWER - Fatal error!'
          write ( *, '(a)' ) '  I^J requested, with I = 0 and J negative.'
          stop
        else
          i4_power = 0
        end if
    
      else if ( j == 0 ) then
    
        if ( i == 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'I4_POWER - Fatal error!'
          write ( *, '(a)' ) '  I^J requested, with I = 0 and J = 0.'
          stop
        else
          i4_power = 1
        end if
    
      else if ( j == 1 ) then
    
        i4_power = i
    
      else
    
        i4_power = 1
        do k = 1, j
          i4_power = i4_power * i
        end do
    
      end if
    
      return
    end
    function i4_sign ( x )
    
    !*****************************************************************************80
    !
    !! I4_SIGN evaluates the sign of an I4.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
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
    !    Input, integer ( kind = 4 ) X, the number whose sign is desired.
    !
    !    Output, integer ( kind = 4 ) I4_SIGN, the sign of X:
    !
      implicit none
    
      integer ( kind = 4 ) i4_sign
      integer ( kind = 4 ) x
    
      if ( x < 0 ) then
        i4_sign = -1
      else
        i4_sign = +1
      end if
    
      return
    end
    subroutine i4_swap ( i, j )
    
    !*****************************************************************************80
    !
    !! I4_SWAP swaps two I4's.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    30 November 1998
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input/output, integer ( kind = 4 ) I, J.  On output, the values of I and
    !    J have been interchanged.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
    
      k = i
      i = j
      j = k
    
      return
    end
    subroutine i4_swap3 ( i, j, k )
    
    !*****************************************************************************80
    !
    !! I4_SWAP3 swaps three I4's.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
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
    !    Input/output, integer ( kind = 4 ) I, J, K.  On output, the
    !    values of I, J, and K have been interchanged.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
      integer ( kind = 4 ) l
    
      l = i
      i = j
      j = k
      k = l
    
      return
    end
    subroutine i4_to_angle ( i, angle )
    
    !*****************************************************************************80
    !
    !! I4_TO_ANGLE maps I4's to points on a circle.
    !
    !  Discussion:
    !
    !    The angles are intended to be used to select colors on a color
    !    hexagon whose 6 vertices are red, yellow, green, cyan, blue,
    !    magenta.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Example:
    !
    !     I   X      ANGLE
    !
    !     0   0/3      0
    !     1   1/3    120
    !     2   2/3    240
    !
    !     3   1/6     60
    !     4   3/6    180
    !     5   5/6    300
    !
    !     6   1/12    30
    !     7   3/12    90
    !     8   5/12   150
    !     9   7/12   210
    !    10   9/12   270
    !    11  11/12   330
    !
    !    12   1/24    15
    !    13   3/24    45
    !    14   5/24    75
    !    etc
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    14 January 2003
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, the index of the desired color.
    !
    !    Output, real ( kind = 8 ) ANGLE, an angle, measured in degrees,
    !    between 0 and 360.
    !
      implicit none
    
      real ( kind = 8 ) angle
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_log_2
      integer ( kind = 4 ) i1
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) i3
      integer ( kind = 4 ) i4
    
      if ( 0 <= abs ( i ) .and. abs ( i ) <= 2 ) then
    
        angle = 120.0D+00 * real ( abs ( i ), kind = 8 )
    
      else
    
        i1 = i4_log_2 ( abs ( i ) / 3 )
        i2 = abs ( i ) + 1 - 3 * 2**i1
        i3 = 2 * ( i2 - 1 ) + 1
        i4 = 3 * 2**( i1 + 1 )
    
        angle = 360.0D+00 * real ( i3, kind = 8 ) / real ( i4, kind = 8 )
    
      end if
    
      return
    end
    subroutine i4_to_digits_binary ( i, n, c )
    
    !*****************************************************************************80
    !
    !! I4_TO_DIGITS_BINARY produces the binary digits of an I4.
    !
    !  Discussion:
    !
    !    An I4 is an integer.
    !
    !  Example:
    !
    !     I    N     C               Binary
    !    --  ---   ---         ------------
    !     0    1   0                      0
    !     0    2   0, 0                  00
    !     1    3   1, 0, 0              100
    !     2    3   0, 1, 0              010
    !     3    3   1, 1, 0              011
    !     4    3   0, 0, 1              100
    !     8    3   0, 0, 0           (1)000
    !     8    5   0, 0, 0, 1, 0      01000
    !    -8    5   0, 0, 0, 1, 0  (-) 01000
    !
    !     0    3   0, 0, 0
    !     1    3   1, 0, 0
    !     2    3   0, 1, 0
    !     3    3   1, 1, 0
    !     4    3   0, 0, 1
    !     5    3   1, 0, 1
    !     6    3   0, 1, 1
    !     7    3   1, 1, 1
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    09 December 2010
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, an integer to be represented.
    !
    !    Input, integer ( kind = 4 ) N, the number of binary digits to produce.
    !
    !    Output, integer ( kind = 4 ) C(N), the first N binary digits of I,
    !    with C(1) being the units digit.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) c(n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i_copy
      integer ( kind = 4 ) j
    
      i_copy = abs ( i )
    
      do j = 1, n
    
        c(j) = mod ( i_copy, 2 )
        i_copy = i_copy / 2
    
      end do
    
      return
    end
    subroutine i4_to_digits_decimal ( i, n, digit )
    
    !*****************************************************************************80
    !
    !! I4_TO_DIGITS_DECIMAL determines the last N decimal digits of an I4.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    08 May 2001
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, the integer to be analyzed.
    !
    !    Input, integer ( kind = 4 ) N, the number of digits to determine.
    !
    !    Output, integer ( kind = 4 ) DIGIT(N), the last N decimal digits of I.
    !    DIGIT(I) is the "coefficient" of 10**(I-1).
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) digit(n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i_copy
      integer ( kind = 4 ) j
    
      i_copy = abs ( i )
    
      do j = 1, n
        digit(j) = mod ( i_copy, 10 )
        i_copy = ( i_copy - digit(j) ) / 10
      end do
    
      return
    end
    subroutine i4_to_fac ( intval, prime_num, npower )
    
    !*****************************************************************************80
    !
    !! I4_TO_FAC converts an I4 into a product of prime factors.
    !
    !  Discussion:
    !
    !    This routine will fail if the input integer is not positive,
    !    or if PRIME_NUM is too small to account for the factors of the integer.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !    The formula is:
    !
    !      INTVAL = Product ( 1 <= I <= PRIME_NUM ) PRIME(I)**NPOWER(I).
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    15 August 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) INTVAL, the integer to be factored.
    !
    !    Input, integer ( kind = 4 ) PRIME_NUM, the number of prime factors for
    !    which storage has been allocated.
    !
    !    Output, integer ( kind = 4 ) NPOWER(PRIME_NUM), the powers of the primes.
    !
      implicit none
    
      integer ( kind = 4 ) prime_num
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) intcopy
      integer ( kind = 4 ) intval
      integer ( kind = 4 ) npower(prime_num)
      integer ( kind = 4 ) p
      integer ( kind = 4 ) prime
    
      if ( intval <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_TO_FAC - Fatal error!'
        write ( *, '(a)' ) '  Input integer is not positive.'
        stop
      end if
    !
    !  Try dividing the remainder by each prime.
    !
      intcopy = intval
    
      do i = 1, prime_num
    
        npower(i) = 0
    
        p = prime ( i )
    
        do while ( mod ( intcopy, p ) == 0 )
          npower(i) = npower(i) + 1
          intcopy = intcopy / p
        end do
    
      end do
    
      return
    end
    subroutine i4_to_halton ( dim_num, step, seed, leap, base, r )
    
    !*****************************************************************************80
    !
    !! I4_TO_HALTON computes one element of a leaped Halton subsequence.
    !
    !  Discussion:
    !
    !    The DIM_NUM-dimensional Halton sequence is really DIM_NUM separate
    !    sequences, each generated by a particular base.
    !
    !    This routine selects elements of a "leaped" subsequence of the
    !    Halton sequence.  The subsequence elements are indexed by a
    !    quantity called STEP, which starts at 0.  The STEP-th subsequence
    !    element is simply element
    !
    !      SEED(1:DIM_NUM) + STEP * LEAP(1:DIM_NUM)
    !
    !    of the original Halton sequence.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 July 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    John Halton,
    !    On the efficiency of certain quasi-random sequences of points
    !    in evaluating multi-dimensional integrals,
    !    Numerische Mathematik,
    !    Volume 2, Number 1, December 1960, pages 84-90
    !
    !    John Halton, GB Smith,
    !    Algorithm 247:
    !    Radical-Inverse Quasi-Random Point Sequence,
    !    Communications of the ACM,
    !    Volume 7, Number 12, December 1964, pages 701-702.
    !
    !    Ladislav Kocis, William Whiten,
    !    Computational Investigations of Low-Discrepancy Sequences,
    !    ACM Transactions on Mathematical Software,
    !    Volume 23, Number 2, June 1997, pages 266-294.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
    !    1 <= DIM_NUM is required.
    !
    !    Input, integer ( kind = 4 ) STEP, the index of the subsequence element.
    !    0 <= STEP is required.
    !
    !    Input, integer ( kind = 4 ) SEED(DIM_NUM), the Halton sequence index
    !    corresponding to STEP = 0.
    !    0 <= SEED(1:DIM_NUM) is required.
    !
    !    Input, integer ( kind = 4 ) LEAP(DIM_NUM), the successive jumps in the
    !    Halton sequence.  1 <= LEAP(1:DIM_NUM) is required.
    !
    !    Input, integer ( kind = 4 ) BASE(DIM_NUM), the Halton bases.
    !    1 < BASE(1:DIM_NUM) is required.
    !
    !    Output, real ( kind = 8 ) R(DIM_NUM), the STEP-th element of the leaped
    !    Halton subsequence.
    !
      implicit none
    
      integer ( kind = 4 ) dim_num
    
      integer ( kind = 4 ) base(dim_num)
      real ( kind = 8 ) base_inv
      integer ( kind = 4 ) digit
      integer ( kind = 4 ) i
      integer ( kind = 4 ) leap(dim_num)
      real ( kind = 8 ) r(dim_num)
      integer ( kind = 4 ) seed(dim_num)
      integer ( kind = 4 ) seed2
      integer ( kind = 4 ) step
    !
    !  Check the input.
    !
      if ( dim_num < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_TO_HALTON - Fatal error!'
        write ( *, '(a)' ) '  DIM_NUM < 1.'
        stop
      end if
    
      if ( step < 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_TO_HALTON - Fatal error!'
        write ( *, '(a)' ) ' STEP < 0.'
        stop
      end if
    
      if ( any ( seed(1:dim_num) < 0 ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_TO_HALTON - Fatal error!'
        write ( *, '(a)' ) '  Some SEED(*) < 0.'
        stop
      end if
    
      if ( any ( leap(1:dim_num) < 1 ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_TO_HALTON - Fatal error!'
        write ( *, '(a)' ) '  Some LEAP < 1.'
        stop
      end if
    
      if ( any ( base(1:dim_num) <= 1 ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_TO_HALTON - Fatal error!'
        write ( *, '(a)' ) '  Some BASE <= 1.'
        stop
      end if
    !
    !  Calculate the data.
    !
      do i = 1, dim_num
    
        seed2 = seed(i) + step * leap(i)
    
        r(i) = 0.0D+00
    
        base_inv = real ( 1.0D+00, kind = 8 ) / real ( base(i), kind = 8 )
    
        do while ( seed2 /= 0 )
          digit = mod ( seed2, base(i) )
          r(i) = r(i) + real ( digit, kind = 8 ) * base_inv
          base_inv = base_inv / real ( base(i), kind = 8 )
          seed2 = seed2 / base(i)
        end do
    
      end do
    
      return
    end
    function i4_to_isbn ( i )
    
    !*****************************************************************************80
    !
    !! I4_TO_ISBN converts an I4 to an ISBN digit.
    !
    !  Discussion:
    !
    !    Only the integers 0 through 10 can be input.  The representation
    !    of 10 is 'X'.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    16 July 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Book Industry Study Group,
    !    The Evolution in Product Identification:
    !    Sunrise 2005 and the ISBN-13,
    !    http://www.bisg.org/docs/The_Evolution_in_Product_ID.pdf
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, an integer between 0 and 10.
    !
    !    Output, character I4_TO_ISBN, the ISBN character code of the integer.
    !    If I is illegal, then I4_TO_ISBN is set to '?'.
    !
      implicit none
    
      integer ( kind = 4 ) i
      character i4_to_isbn
    
           if ( i == 0 ) then
        i4_to_isbn = '0'
      else if ( i == 1 ) then
        i4_to_isbn = '1'
      else if ( i == 2 ) then
        i4_to_isbn = '2'
      else if ( i == 3 ) then
        i4_to_isbn = '3'
      else if ( i == 4 ) then
        i4_to_isbn = '4'
      else if ( i == 5 ) then
        i4_to_isbn = '5'
      else if ( i == 6 ) then
        i4_to_isbn = '6'
      else if ( i == 7 ) then
        i4_to_isbn = '7'
      else if ( i == 8 ) then
        i4_to_isbn = '8'
      else if ( i == 9 ) then
        i4_to_isbn = '9'
      else if ( i == 10 ) then
        i4_to_isbn = 'X'
      else
        i4_to_isbn = '?'
      end if
    
      return
    end
    function i4_to_l ( i4 )
    
    !*****************************************************************************80
    !
    !! I4_TO_L converts an I4 to a logical value.
    !
    !  Discussion:
    !
    !    0 is FALSE, and anything else if TRUE.
    !
    !    An I4 is an integer value.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    10 January 2012
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I4, an integer.
    !
    !    Output, logical I4_TO_L, the logical value of I4.
    !
      implicit none
    
      integer ( kind = 4 ) i4
      logical i4_to_l
      logical value
    
      value = ( i4 /= 0 )
    
      i4_to_l = value
    
      return
    end
    function i4_uniform_ab ( a, b, seed )
    
    !*****************************************************************************80
    !
    !! I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
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
    !    02 October 2012
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
    subroutine i4_unswap3 ( i, j, k )
    
    !*****************************************************************************80
    !
    !! I4_UNSWAP3 unswaps three I4's.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
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
    !    Input/output, integer ( kind = 4 ) I, J, K.  On output, the values
    !    of I, J, and K have been interchanged.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
      integer ( kind = 4 ) l
    
      l = k
      k = j
      j = i
      i = l
    
      return
    end
    function i4_walsh_1d ( x, digit )
    
    !*****************************************************************************80
    !
    !! I4_WALSH_1D evaluates the Walsh function.
    !
    !  Discussion:
    !
    !    Consider the binary representation of X, and number the digits
    !    in descending order, from leading to lowest, with the units digit
    !    being numbered 0.
    !
    !    The Walsh function W(J)(X) is equal to the J-th binary digit of X.
    !
    !    An I4 is an integer ( kind = 4 ) value.
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
    !    Output, integer ( kind = 4 ) I4_WALSH_1D, the value of the Walsh function.
    !
      implicit none
    
      integer ( kind = 4 ) digit
      integer ( kind = 4 ) i4_walsh_1d
      integer ( kind = 4 ) n
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
        i4_walsh_1d = 0
      else
        i4_walsh_1d = 1
      end if
    
      return
    end
    function i4_width ( i )
    
    !*****************************************************************************80
    !
    !! I4_WIDTH returns the "width" of an I4.
    !
    !  Discussion:
    !
    !    The width of an integer is the number of characters necessary to print it.
    !
    !    The width of an integer can be useful when setting the appropriate output
    !    format for a vector or array of values.
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !  Example:
    !
    !        I  I4_WIDTH
    !    -----  -------
    !    -1234    5
    !     -123    4
    !      -12    3
    !       -1    2
    !        0    1
    !        1    1
    !       12    2
    !      123    3
    !     1234    4
    !    12345    5
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 December 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, the number whose width is desired.
    !
    !    Output, integer ( kind = 4 ) I4_WIDTH, the number of characters
    !    necessary to represent the integer in base 10, including a negative
    !    sign if necessary.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_log_10
      integer ( kind = 4 ) i4_width
    
      if ( 0 < i ) then
        i4_width = i4_log_10 ( i ) + 1
      else if ( i == 0 ) then
        i4_width = 1
      else if ( i < 0 ) then
        i4_width = i4_log_10 ( i ) + 2
      end if
    
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
    function i4_xor ( i, j )
    
    !*****************************************************************************80
    !
    !! I4_XOR calculates the exclusive OR of two I4's.
    !
    !  Discussion:
    !
    !    An I4 is an integer ( kind = 4 ) value.
    !
    !    FORTRAN offers the function IEOR ( I, J ) which should be used instead.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    16 February 2005
    !
    !  Author:
    !
    !   John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I, J, two values whose exclusive OR is needed.
    !
    !    Output, integer ( kind = 4 ) I4_XOR, the exclusive OR of I and J.
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i1
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) i4_xor
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) k
      integer ( kind = 4 ) l
    
      i1 = i
      j1 = j
      k = 0
      l = 1
    
      do while ( i1 /= 0 .or. j1 /= 0 )
    
        i2 = i1 / 2
        j2 = j1 / 2
    
        if ( &
          ( ( i1 == 2 * i2 ) .and. ( j1 /= 2 * j2 ) ) .or. &
          ( ( i1 /= 2 * i2 ) .and. ( j1 == 2 * j2 ) ) ) then
          k = k + l
        end if
    
        i1 = i2
        j1 = j2
        l = 2 * l
    
      end do
    
      i4_xor = k
    
      return
    end function i4_xor


    function prime ( n )
    
    !*****************************************************************************80
    !
    !! PRIME returns any of the first PRIME_MAX prime numbers.
    !
    !  Discussion:
    !
    !    PRIME_MAX is 1600, and the largest prime stored is 13499.
    !
    !    Thanks to Bart Vandewoestyne for pointing out a typo, 18 February 2005.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    18 February 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Reference:
    !
    !    Milton Abramowitz, Irene Stegun,
    !    Handbook of Mathematical Functions,
    !    US Department of Commerce, 1964, pages 870-873.
    !
    !    Daniel Zwillinger,
    !    CRC Standard Mathematical Tables and Formulae,
    !    30th Edition,
    !    CRC Press, 1996, pages 95-98.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the index of the desired prime number.
    !    In general, is should be true that 0 <= N <= PRIME_MAX.
    !    N = -1 returns PRIME_MAX, the index of the largest prime available.
    !    N = 0 is legal, returning PRIME = 1.
    !
    !    Output, integer ( kind = 4 ) PRIME, the N-th prime.  If N is out of range,
    !    PRIME is returned as -1.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: prime_max = 1600
    
      integer ( kind = 4 ), save :: icall = 0
      integer ( kind = 4 ) n
      integer ( kind = 4 ), save, dimension ( prime_max ) :: npvec
      integer ( kind = 4 ) prime
    
      if ( icall == 0 ) then
    
        icall = 1
    
        npvec(1:100) = (/ &
            2,    3,    5,    7,   11,   13,   17,   19,   23,   29, &
           31,   37,   41,   43,   47,   53,   59,   61,   67,   71, &
           73,   79,   83,   89,   97,  101,  103,  107,  109,  113, &
          127,  131,  137,  139,  149,  151,  157,  163,  167,  173, &
          179,  181,  191,  193,  197,  199,  211,  223,  227,  229, &
          233,  239,  241,  251,  257,  263,  269,  271,  277,  281, &
          283,  293,  307,  311,  313,  317,  331,  337,  347,  349, &
          353,  359,  367,  373,  379,  383,  389,  397,  401,  409, &
          419,  421,  431,  433,  439,  443,  449,  457,  461,  463, &
          467,  479,  487,  491,  499,  503,  509,  521,  523,  541 /)
    
        npvec(101:200) = (/ &
          547,  557,  563,  569,  571,  577,  587,  593,  599,  601, &
          607,  613,  617,  619,  631,  641,  643,  647,  653,  659, &
          661,  673,  677,  683,  691,  701,  709,  719,  727,  733, &
          739,  743,  751,  757,  761,  769,  773,  787,  797,  809, &
          811,  821,  823,  827,  829,  839,  853,  857,  859,  863, &
          877,  881,  883,  887,  907,  911,  919,  929,  937,  941, &
          947,  953,  967,  971,  977,  983,  991,  997, 1009, 1013, &
         1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, &
         1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151, &
         1153, 1163, 1171, 1181, 1187, 1193, 1201, 1213, 1217, 1223 /)
    
        npvec(201:300) = (/ &
         1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291, &
         1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, &
         1381, 1399, 1409, 1423, 1427, 1429, 1433, 1439, 1447, 1451, &
         1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511, &
         1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, &
         1597, 1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657, &
         1663, 1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, &
         1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811, &
         1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877, 1879, 1889, &
         1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987 /)
    
        npvec(301:400) = (/ &
         1993, 1997, 1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053, &
         2063, 2069, 2081, 2083, 2087, 2089, 2099, 2111, 2113, 2129, &
         2131, 2137, 2141, 2143, 2153, 2161, 2179, 2203, 2207, 2213, &
         2221, 2237, 2239, 2243, 2251, 2267, 2269, 2273, 2281, 2287, &
         2293, 2297, 2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357, &
         2371, 2377, 2381, 2383, 2389, 2393, 2399, 2411, 2417, 2423, &
         2437, 2441, 2447, 2459, 2467, 2473, 2477, 2503, 2521, 2531, &
         2539, 2543, 2549, 2551, 2557, 2579, 2591, 2593, 2609, 2617, &
         2621, 2633, 2647, 2657, 2659, 2663, 2671, 2677, 2683, 2687, &
         2689, 2693, 2699, 2707, 2711, 2713, 2719, 2729, 2731, 2741 /)
    
        npvec(401:500) = (/ &
         2749, 2753, 2767, 2777, 2789, 2791, 2797, 2801, 2803, 2819, &
         2833, 2837, 2843, 2851, 2857, 2861, 2879, 2887, 2897, 2903, &
         2909, 2917, 2927, 2939, 2953, 2957, 2963, 2969, 2971, 2999, &
         3001, 3011, 3019, 3023, 3037, 3041, 3049, 3061, 3067, 3079, &
         3083, 3089, 3109, 3119, 3121, 3137, 3163, 3167, 3169, 3181, &
         3187, 3191, 3203, 3209, 3217, 3221, 3229, 3251, 3253, 3257, &
         3259, 3271, 3299, 3301, 3307, 3313, 3319, 3323, 3329, 3331, &
         3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413, &
         3433, 3449, 3457, 3461, 3463, 3467, 3469, 3491, 3499, 3511, &
         3517, 3527, 3529, 3533, 3539, 3541, 3547, 3557, 3559, 3571 /)
    
        npvec(501:600) = (/ &
         3581, 3583, 3593, 3607, 3613, 3617, 3623, 3631, 3637, 3643, &
         3659, 3671, 3673, 3677, 3691, 3697, 3701, 3709, 3719, 3727, &
         3733, 3739, 3761, 3767, 3769, 3779, 3793, 3797, 3803, 3821, &
         3823, 3833, 3847, 3851, 3853, 3863, 3877, 3881, 3889, 3907, &
         3911, 3917, 3919, 3923, 3929, 3931, 3943, 3947, 3967, 3989, &
         4001, 4003, 4007, 4013, 4019, 4021, 4027, 4049, 4051, 4057, &
         4073, 4079, 4091, 4093, 4099, 4111, 4127, 4129, 4133, 4139, &
         4153, 4157, 4159, 4177, 4201, 4211, 4217, 4219, 4229, 4231, &
         4241, 4243, 4253, 4259, 4261, 4271, 4273, 4283, 4289, 4297, &
         4327, 4337, 4339, 4349, 4357, 4363, 4373, 4391, 4397, 4409 /)
    
        npvec(601:700) = (/ &
         4421, 4423, 4441, 4447, 4451, 4457, 4463, 4481, 4483, 4493, &
         4507, 4513, 4517, 4519, 4523, 4547, 4549, 4561, 4567, 4583, &
         4591, 4597, 4603, 4621, 4637, 4639, 4643, 4649, 4651, 4657, &
         4663, 4673, 4679, 4691, 4703, 4721, 4723, 4729, 4733, 4751, &
         4759, 4783, 4787, 4789, 4793, 4799, 4801, 4813, 4817, 4831, &
         4861, 4871, 4877, 4889, 4903, 4909, 4919, 4931, 4933, 4937, &
         4943, 4951, 4957, 4967, 4969, 4973, 4987, 4993, 4999, 5003, &
         5009, 5011, 5021, 5023, 5039, 5051, 5059, 5077, 5081, 5087, &
         5099, 5101, 5107, 5113, 5119, 5147, 5153, 5167, 5171, 5179, &
         5189, 5197, 5209, 5227, 5231, 5233, 5237, 5261, 5273, 5279 /)
    
        npvec(701:800) = (/ &
         5281, 5297, 5303, 5309, 5323, 5333, 5347, 5351, 5381, 5387, &
         5393, 5399, 5407, 5413, 5417, 5419, 5431, 5437, 5441, 5443, &
         5449, 5471, 5477, 5479, 5483, 5501, 5503, 5507, 5519, 5521, &
         5527, 5531, 5557, 5563, 5569, 5573, 5581, 5591, 5623, 5639, &
         5641, 5647, 5651, 5653, 5657, 5659, 5669, 5683, 5689, 5693, &
         5701, 5711, 5717, 5737, 5741, 5743, 5749, 5779, 5783, 5791, &
         5801, 5807, 5813, 5821, 5827, 5839, 5843, 5849, 5851, 5857, &
         5861, 5867, 5869, 5879, 5881, 5897, 5903, 5923, 5927, 5939, &
         5953, 5981, 5987, 6007, 6011, 6029, 6037, 6043, 6047, 6053, &
         6067, 6073, 6079, 6089, 6091, 6101, 6113, 6121, 6131, 6133 /)
    
        npvec(801:900) = (/ &
         6143, 6151, 6163, 6173, 6197, 6199, 6203, 6211, 6217, 6221, &
         6229, 6247, 6257, 6263, 6269, 6271, 6277, 6287, 6299, 6301, &
         6311, 6317, 6323, 6329, 6337, 6343, 6353, 6359, 6361, 6367, &
         6373, 6379, 6389, 6397, 6421, 6427, 6449, 6451, 6469, 6473, &
         6481, 6491, 6521, 6529, 6547, 6551, 6553, 6563, 6569, 6571, &
         6577, 6581, 6599, 6607, 6619, 6637, 6653, 6659, 6661, 6673, &
         6679, 6689, 6691, 6701, 6703, 6709, 6719, 6733, 6737, 6761, &
         6763, 6779, 6781, 6791, 6793, 6803, 6823, 6827, 6829, 6833, &
         6841, 6857, 6863, 6869, 6871, 6883, 6899, 6907, 6911, 6917, &
         6947, 6949, 6959, 6961, 6967, 6971, 6977, 6983, 6991, 6997 /)
    
        npvec(901:1000) = (/ &
         7001, 7013, 7019, 7027, 7039, 7043, 7057, 7069, 7079, 7103, &
         7109, 7121, 7127, 7129, 7151, 7159, 7177, 7187, 7193, 7207, &
         7211, 7213, 7219, 7229, 7237, 7243, 7247, 7253, 7283, 7297, &
         7307, 7309, 7321, 7331, 7333, 7349, 7351, 7369, 7393, 7411, &
         7417, 7433, 7451, 7457, 7459, 7477, 7481, 7487, 7489, 7499, &
         7507, 7517, 7523, 7529, 7537, 7541, 7547, 7549, 7559, 7561, &
         7573, 7577, 7583, 7589, 7591, 7603, 7607, 7621, 7639, 7643, &
         7649, 7669, 7673, 7681, 7687, 7691, 7699, 7703, 7717, 7723, &
         7727, 7741, 7753, 7757, 7759, 7789, 7793, 7817, 7823, 7829, &
         7841, 7853, 7867, 7873, 7877, 7879, 7883, 7901, 7907, 7919 /)
    
        npvec(1001:1100) = (/ &
         7927, 7933, 7937, 7949, 7951, 7963, 7993, 8009, 8011, 8017, &
         8039, 8053, 8059, 8069, 8081, 8087, 8089, 8093, 8101, 8111, &
         8117, 8123, 8147, 8161, 8167, 8171, 8179, 8191, 8209, 8219, &
         8221, 8231, 8233, 8237, 8243, 8263, 8269, 8273, 8287, 8291, &
         8293, 8297, 8311, 8317, 8329, 8353, 8363, 8369, 8377, 8387, &
         8389, 8419, 8423, 8429, 8431, 8443, 8447, 8461, 8467, 8501, &
         8513, 8521, 8527, 8537, 8539, 8543, 8563, 8573, 8581, 8597, &
         8599, 8609, 8623, 8627, 8629, 8641, 8647, 8663, 8669, 8677, &
         8681, 8689, 8693, 8699, 8707, 8713, 8719, 8731, 8737, 8741, &
         8747, 8753, 8761, 8779, 8783, 8803, 8807, 8819, 8821, 8831 /)
    
        npvec(1101:1200) = (/ &
         8837, 8839, 8849, 8861, 8863, 8867, 8887, 8893, 8923, 8929, &
         8933, 8941, 8951, 8963, 8969, 8971, 8999, 9001, 9007, 9011, &
         9013, 9029, 9041, 9043, 9049, 9059, 9067, 9091, 9103, 9109, &
         9127, 9133, 9137, 9151, 9157, 9161, 9173, 9181, 9187, 9199, &
         9203, 9209, 9221, 9227, 9239, 9241, 9257, 9277, 9281, 9283, &
         9293, 9311, 9319, 9323, 9337, 9341, 9343, 9349, 9371, 9377, &
         9391, 9397, 9403, 9413, 9419, 9421, 9431, 9433, 9437, 9439, &
         9461, 9463, 9467, 9473, 9479, 9491, 9497, 9511, 9521, 9533, &
         9539, 9547, 9551, 9587, 9601, 9613, 9619, 9623, 9629, 9631, &
         9643, 9649, 9661, 9677, 9679, 9689, 9697, 9719, 9721, 9733 /)
    
        npvec(1201:1300) = (/ &
         9739, 9743, 9749, 9767, 9769, 9781, 9787, 9791, 9803, 9811, &
         9817, 9829, 9833, 9839, 9851, 9857, 9859, 9871, 9883, 9887, &
         9901, 9907, 9923, 9929, 9931, 9941, 9949, 9967, 9973,10007, &
        10009,10037,10039,10061,10067,10069,10079,10091,10093,10099, &
        10103,10111,10133,10139,10141,10151,10159,10163,10169,10177, &
        10181,10193,10211,10223,10243,10247,10253,10259,10267,10271, &
        10273,10289,10301,10303,10313,10321,10331,10333,10337,10343, &
        10357,10369,10391,10399,10427,10429,10433,10453,10457,10459, &
        10463,10477,10487,10499,10501,10513,10529,10531,10559,10567, &
        10589,10597,10601,10607,10613,10627,10631,10639,10651,10657 /)
    
        npvec(1301:1400) = (/ &
        10663,10667,10687,10691,10709,10711,10723,10729,10733,10739, &
        10753,10771,10781,10789,10799,10831,10837,10847,10853,10859, &
        10861,10867,10883,10889,10891,10903,10909,10937,10939,10949, &
        10957,10973,10979,10987,10993,11003,11027,11047,11057,11059, &
        11069,11071,11083,11087,11093,11113,11117,11119,11131,11149, &
        11159,11161,11171,11173,11177,11197,11213,11239,11243,11251, &
        11257,11261,11273,11279,11287,11299,11311,11317,11321,11329, &
        11351,11353,11369,11383,11393,11399,11411,11423,11437,11443, &
        11447,11467,11471,11483,11489,11491,11497,11503,11519,11527, &
        11549,11551,11579,11587,11593,11597,11617,11621,11633,11657 /)
    
        npvec(1401:1500) = (/ &
        11677,11681,11689,11699,11701,11717,11719,11731,11743,11777, &
        11779,11783,11789,11801,11807,11813,11821,11827,11831,11833, &
        11839,11863,11867,11887,11897,11903,11909,11923,11927,11933, &
        11939,11941,11953,11959,11969,11971,11981,11987,12007,12011, &
        12037,12041,12043,12049,12071,12073,12097,12101,12107,12109, &
        12113,12119,12143,12149,12157,12161,12163,12197,12203,12211, &
        12227,12239,12241,12251,12253,12263,12269,12277,12281,12289, &
        12301,12323,12329,12343,12347,12373,12377,12379,12391,12401, &
        12409,12413,12421,12433,12437,12451,12457,12473,12479,12487, &
        12491,12497,12503,12511,12517,12527,12539,12541,12547,12553 /)
    
       npvec(1501:1600) = (/ &
        12569,12577,12583,12589,12601,12611,12613,12619,12637,12641, &
        12647,12653,12659,12671,12689,12697,12703,12713,12721,12739, &
        12743,12757,12763,12781,12791,12799,12809,12821,12823,12829, &
        12841,12853,12889,12893,12899,12907,12911,12917,12919,12923, &
        12941,12953,12959,12967,12973,12979,12983,13001,13003,13007, &
        13009,13033,13037,13043,13049,13063,13093,13099,13103,13109, &
        13121,13127,13147,13151,13159,13163,13171,13177,13183,13187, &
        13217,13219,13229,13241,13249,13259,13267,13291,13297,13309, &
        13313,13327,13331,13337,13339,13367,13381,13397,13399,13411, &
        13417,13421,13441,13451,13457,13463,13469,13477,13487,13499 /)
    
      end if
    
      if ( n == -1 ) then
        prime = prime_max
      else if ( n == 0 ) then
        prime = 1
      else if ( n <= prime_max ) then
        prime = npvec(n)
      else
        prime = -1
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PRIME - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal prime index N = ', n
        write ( *, '(a,i8)' ) '  N should be between 1 and PRIME_MAX =', prime_max
        stop
      end if
    
      return
    end

end module jburk_i4lib_i4_
