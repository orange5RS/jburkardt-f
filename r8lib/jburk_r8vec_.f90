module     jburk_r8vec_
implicit none
contains






subroutine r8vec_01_to_ab ( n, a, amax, amin )

!*****************************************************************************80
!
!! R8VEC_01_TO_AB shifts and rescales an R8VEC to lie within given bounds.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    On input, A contains the original data, which is presumed to lie
!    between 0 and 1.  However, it is not necessary that this be so.
!
!    On output, A has been shifted and rescaled so that all entries which
!    on input lay in [0,1] now lie between AMIN and AMAX.  Other entries will
!    be mapped in a corresponding way.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input/output, real ( kind = 8 ) A(N), the vector to be rescaled.
!
!    Input, real ( kind = 8 ) AMAX, AMIN, the maximum and minimum values
!    allowed for A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amax2
  real ( kind = 8 ) amax3
  real ( kind = 8 ) amin
  real ( kind = 8 ) amin2
  real ( kind = 8 ) amin3

  if ( amax == amin ) then
    a(1:n) = amin
    return
  end if

  amax2 = max ( amax, amin )
  amin2 = min ( amax, amin )

  amin3 = minval ( a(1:n) )
  amax3 = maxval ( a(1:n) )

  if ( amax3 /= amin3 ) then

    a(1:n) = ( ( amax3 - a(1:n)         ) * amin2   &
             + (         a(1:n) - amin3 ) * amax2 ) &
             / ( amax3          - amin3 )

  else

    a(1:n) = 0.5D+00 * ( amax2 + amin2 )

  end if

  return
end



subroutine r8vec_ab_to_01 ( n, a )

!*****************************************************************************80
!
!! R8VEC_AB_TO_01 shifts and rescales an R8VEC to lie within [0,1].
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    On input, A contains the original data.  On output, A has been shifted
!    and scaled so that all entries lie between 0 and 1.
!
!  Formula:
!
!    A(I) := ( A(I) - AMIN ) / ( AMAX - AMIN )
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
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input/output, real ( kind = 8 ) A(N), the data to be rescaled.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amin

  amax = maxval ( a(1:n) )
  amin = minval ( a(1:n) )

  if ( amin == amax ) then
    a(1:n) = 0.5D+00
  else
    a(1:n) = ( a(1:n) - amin ) / ( amax - amin )
  end if

  return
end



subroutine r8vec_ab_to_cd ( n, a, bmin, bmax, b )

!*****************************************************************************80
!
!! R8VEC_AB_TO_CD shifts and rescales an R8VEC from one interval to another.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The mininum entry of A is mapped to BMIN, the maximum entry
!    to BMAX, and values in between are mapped linearly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 8 ) A(N), the data to be remapped.
!
!    Input, real ( kind = 8 ) BMIN, BMAX, the values to which min(A) and max(A)
!    are to be assigned.
!
!    Output, real ( kind = 8 ) B(N), the remapped data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amin
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) bmax
  real ( kind = 8 ) bmin

  if ( bmax == bmin ) then
    b(1:n) = bmin
    return
  end if

  amin = minval ( a(1:n) )
  amax = maxval ( a(1:n) )

  if ( amax == amin ) then
    b(1:n) = 0.5D+00 * ( bmax + bmin )
    return
  end if

  b(1:n) = ( ( amax - a(1:n)        ) * bmin   &
         + (          a(1:n) - amin ) * bmax ) &
           / ( amax          - amin )

  return
end
function r8vec_all_nonpositive ( n, a )

!*****************************************************************************80
!
!! R8VEC_ALL_NONPOSITIVE: ( all ( A <= 0 ) ) for R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Input, double ( kind = 8 ) A(N), the vector.
!
!    Output, logical R8VEC_ALL_NONPOSITIVE is TRUE if all entries
!    of A are less than or equal to zero.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical r8vec_all_nonpositive

  r8vec_all_nonpositive = all ( a(1:n) <= 0.0D+00 )

  return
end



subroutine r8vec_amax ( n, a, amax )

!*****************************************************************************80
!
!! R8VEC_AMAX returns the maximum absolute value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) AMAX, the value of the entry
!    of largest magnitude.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax

  amax = maxval ( abs ( a(1:n) ) )

  return
end



subroutine r8vec_amax_index ( n, a, amax_index )

!*****************************************************************************80
!
!! R8VEC_AMAX_INDEX returns the index of the maximum absolute value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, integer ( kind = 4 ) AMAX_INDEX, the index of the entry of
!    largest magnitude.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  integer ( kind = 4 ) amax_index
  integer ( kind = 4 ) i

  if ( n <= 0 ) then

    amax_index = -1

  else

    amax_index = 1
    amax = abs ( a(1) )

    do i = 2, n
      if ( amax < abs ( a(i) ) ) then
        amax_index = i
        amax = abs ( a(i) )
      end if
    end do

  end if

  return
end



subroutine r8vec_amin ( n, a, amin )

!*****************************************************************************80
!
!! R8VEC_AMIN returns the minimum absolute value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 )A(N), the array.
!
!    Output, real ( kind = 8 ) AMIN, the value of the entry
!    of smallest magnitude.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amin

  amin = minval ( abs ( a(1:n) ) )

  return
end



subroutine r8vec_amin_index ( n, a, amin_index )

!*****************************************************************************80
!
!! R8VEC_AMIN_INDEX returns the index of the minimum absolute value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, integer ( kind = 4 ) AMIN_INDEX, the index of the entry of
!    smallest magnitude.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amin
  integer ( kind = 4 ) amin_index
  integer ( kind = 4 ) i

  if ( n <= 0 ) then

    amin_index = 0

  else

    amin_index = 1
    amin = abs ( a(1) )

    do i = 2, n
      if ( abs ( a(i) ) < amin ) then
        amin_index = i
        amin = abs ( a(i) )
      end if
    end do

  end if

  return
end
function r8vec_any_negative ( n, a )

!*****************************************************************************80
!
!! R8VEC_ANY_NEGATIVE: ( any A < 0 ) for R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, logical R8VEC_ANY_NEGATIVE is TRUE if any entry is negative.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical r8vec_any_negative

  r8vec_any_negative = any ( a(1:n) < 0.0D+00 )

  return
end
function r8vec_any_nonzero ( n, a )

!*****************************************************************************80
!
!! R8VEC_ANY_NONZERO: ( any A nonzero ) for R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, logical R8VEC_ANY_NONZERO is TRUE if any entry is nonzero.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical r8vec_any_nonzero

  r8vec_any_nonzero = any ( a(1:n) /= 0.0D+00 )

  return
end



subroutine r8vec_any_normal ( dim_num, v1, v2 )

!*****************************************************************************80
!
!! R8VEC_ANY_NORMAL returns some normal vector to V1.
!
!  Discussion:
!
!    If DIM_NUM < 2, then no normal vector can be returned.
!
!    If V1 is the zero vector, then any unit vector will do.
!
!    No doubt, there are better, more robust algorithms.  But I will take
!    just about ANY reasonable unit vector that is normal to V1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) V1(DIM_NUM), the vector.
!
!    Output, real ( kind = 8 ) V2(DIM_NUM), a vector that is
!    normal to V2, and has unit Euclidean length.
!
  implicit none

  integer ( kind = 4 ) dim_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8vec_norm
  real ( kind = 8 ) v1(dim_num)
  real ( kind = 8 ) v2(dim_num)
  real ( kind = 8 ) vj
  real ( kind = 8 ) vk

  if ( dim_num < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_ANY_NORMAL - Fatal error!'
    write ( *, '(a)' ) '  Called with DIM_NUM < 2.'
    stop
  end if

  if ( r8vec_norm ( dim_num, v1 ) == 0.0D+00 ) then
    v2(1) = 1.0D+00
    v2(2:dim_num) = 0.0D+00
    return
  end if
!
!  Seek the largest entry in V1, VJ = V1(J), and the
!  second largest, VK = V1(K).
!
!  Since V1 does not have zero norm, we are guaranteed that
!  VJ, at least, is not zero.
!
  j = - 1
  vj = 0.0D+00

  k = - 1
  vk = 0.0D+00

  do i = 1, dim_num

    if ( abs ( vk ) < abs ( v1(i) ) .or. k < 1 ) then

      if ( abs ( vj ) < abs ( v1(i) ) .or. j < 1 ) then
        k = j
        vk = vj
        j = i
        vj = v1(i)
      else
        k = i
        vk = v1(i)
      end if

    end if

  end do
!
!  Setting V2 to zero, except that V2(J) = -VK, and V2(K) = VJ,
!  will just about do the trick.
!
  v2(1:dim_num) = 0.0D+00

  v2(j) = - vk / sqrt ( vk * vk + vj * vj )
  v2(k) =   vj / sqrt ( vk * vk + vj * vj )

  return
end
function r8vec_ascends ( n, x )

!*****************************************************************************80
!
!! R8VEC_ASCENDS determines if an R8VEC is (weakly) ascending.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    For example, if:
!
!      X = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.5, 9.8 )
!
!    then
!
!      R8VEC_ASCENDS = TRUE
!
!    The sequence is not required to be strictly ascending.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the array.
!
!    Input, real ( kind = 8 ) X(N), the array to be examined.
!
!    Output, logical R8VEC_ASCENDS, is TRUE if the
!    entries of X ascend.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  logical r8vec_ascends
  real ( kind = 8 ) x(n)

  do i = 1, n - 1
    if ( x(i+1) < x(i) ) then
      r8vec_ascends = .false.
      return
    end if
  end do

  r8vec_ascends = .true.

  return
end
function r8vec_ascends_strictly ( n, x )

!*****************************************************************************80
!
!! R8VEC_ASCENDS_STRICTLY determines if an R8VEC is strictly ascending.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Notice the effect of entry number 6 in the following results:
!
!      X = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.4, 9.8 )
!      Y = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.5, 9.8 )
!      Z = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.6, 9.8 )
!
!      R8VEC_ASCENDS_STRICTLY ( X ) = FALSE
!      R8VEC_ASCENDS_STRICTLY ( Y ) = FALSE
!      R8VEC_ASCENDS_STRICTLY ( Z ) = TRUE
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the array.
!
!    Input, real ( kind = 8 ) X(N), the array to be examined.
!
!    Output, logical R8VEC_ASCENDS_STRICTLY, is TRUE if the
!    entries of X strictly ascend.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  logical r8vec_ascends_strictly
  real ( kind = 8 ) x(n)

  do i = 1, n - 1
    if ( x(i+1) <= x(i) ) then
      r8vec_ascends_strictly = .false.
      return
    end if
  end do

  r8vec_ascends_strictly = .true.

  return
end



subroutine r8vec_bin ( n, x, bin_num, bin_min, bin_max, bin, bin_limit )

!*****************************************************************************80
!
!! R8VEC_BIN computes bins based on a given R8VEC.
!
!  Discussion:
!
!    The user specifies minimum and maximum bin values, BIN_MIN and
!    BIN_MAX, and the number of bins, BIN_NUM.  This determines a
!    "bin width":
!
!      H = ( BIN_MAX - BIN_MIN ) / BIN_NUM
!
!    so that bin I will count all entries X(J) such that
!
!      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
!
!    The array X does NOT have to be sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of X.
!
!    Input, real ( kind = 8 ) X(N), an (unsorted) array to be binned.
!
!    Input, integer ( kind = 4 ) BIN_NUM, the number of bins.  Two extra bins,
!    #0 and #BIN_NUM+1, count extreme values.
!
!    Input, real ( kind = 8 ) BIN_MIN, BIN_MAX, define the range and size
!    of the bins.  BIN_MIN and BIN_MAX must be distinct.
!    Normally, BIN_MIN < BIN_MAX, and the documentation will assume
!    this, but proper results will be computed if BIN_MIN > BIN_MAX.
!
!    Output, integer ( kind = 4 ) BIN(0:BIN_NUM+1).
!    BIN(0) counts entries of X less than BIN_MIN.
!    BIN(BIN_NUM+1) counts entries greater than or equal to BIN_MAX.
!    For 1 <= I <= BIN_NUM, BIN(I) counts the entries X(J) such that
!      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
!    where H is the bin spacing.
!
!    Output, real ( kind = 8 ) BIN_LIMIT(0:BIN_NUM), the "limits" of the bins.
!    BIN(I) counts the number of entries X(J) such that
!      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) bin_num

  integer ( kind = 4 ) bin(0:bin_num+1)
  real ( kind = 8 ) bin_limit(0:bin_num)
  real ( kind = 8 ) bin_max
  real ( kind = 8 ) bin_min
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)

  if ( bin_max == bin_min ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_BIN - Fatal error!'
    write ( *, '(a)' ) '  BIN_MIN = BIN_MAX.'
    stop
  end if

  bin(0:bin_num+1) = 0

  do i = 1, n

    t = ( x(i) - bin_min ) / ( bin_max - bin_min )

    if ( t < 0.0D+00 ) then
      j = 0
    else if ( 1.0D+00 <= t ) then
      j = bin_num + 1
    else
      j = 1 + int ( real ( bin_num, kind = 8 ) * t )
    end if

    bin(j) = bin(j) + 1

  end do
!
!  Compute the bin limits.
!
  do i = 0, bin_num
    bin_limit(i) = (   real ( bin_num - i, kind = 8 ) * bin_min   &
                     + real (           i, kind = 8 ) * bin_max ) &
                     / real ( bin_num,     kind = 8 )
  end do

  return
end



subroutine r8vec_blend ( n, t1, x1, t2, x2, x )

!*****************************************************************************80
!
!! R8VEC_BLEND performs weighted interpolation of two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The formula used is:
!
!      x(i) = t * x1(i) + (1-t) * x2(i)
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
!    Input, integer ( kind = 4 ) N, the number of entries in each  vector.
!
!    Input, real ( kind = 8 ) T1, the weight factor for vector 1.
!
!    Input, real ( kind = 8 ) X1(N), the first vector.
!
!    Input, real ( kind = 8 ) T2, the weight factor for vector 2.
!
!    Input, real ( kind = 8 ) X2(N), the second vector.
!
!    Output, real ( kind = 8 ) X(N), the interpolated or extrapolated value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x1(n)
  real ( kind = 8 ) x2(n)

  x(1:n) = t1 * x1(1:n) + t2 * x2(1:n)

  return
end



subroutine r8vec_bracket ( n, x, xval, left, right )

!*****************************************************************************80
!
!! R8VEC_BRACKET searches a sorted R8VEC for successive brackets of a value.
!
!  Discussion:
!
!    This is an inefficient implementation that uses linear search.
!
!    An R8VEC is a vector of R8's.
!
!    If the values in the vector are thought of as defining intervals
!    on the real line, then this routine searches for the interval
!    nearest to or containing the given value.
!
!    It is always true that RIGHT = LEFT+1.
!
!    If XVAL < X(1), then LEFT = 1, RIGHT = 2, and
!      XVAL   < X(1) < X(2);
!    If X(1) <= XVAL < X(N), then
!      X(LEFT) <= XVAL < X(RIGHT);
!    If X(N) <= XVAL, then LEFT = N-1, RIGHT = N, and
!      X(LEFT) <= X(RIGHT) <= XVAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, length of input array.
!
!    Input, real ( kind = 8 ) X(N), an array that has been sorted into
!    ascending order.
!
!    Input, real ( kind = 8 ) XVAL, a value to be bracketed.
!
!    Output, integer ( kind = 4 ) LEFT, RIGHT, the results of the search.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) left
  integer ( kind = 4 ) right
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xval

  do i = 2, n - 1

    if ( xval < x(i) ) then
      left = i - 1
      right = i
      return
    end if

   end do

  left = n - 1
  right = n

  return
end



subroutine r8vec_bracket2 ( n, x, xval, start, left, right )

!*****************************************************************************80
!
!! R8VEC_BRACKET2 searches a sorted R8VEC for successive brackets of a value.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If the values in the vector are thought of as defining intervals
!    on the real line, then this routine searches for the interval
!    containing the given value.
!
!    R8VEC_BRACKET2 is a variation on R8VEC_BRACKET.  It seeks to reduce
!    the search time by allowing the user to suggest an interval that
!    probably contains the value.  The routine will look in that interval
!    and the intervals to the immediate left and right.  If this does
!    not locate the point, a binary search will be carried out on
!    appropriate subportion of the sorted array.
!
!    In the most common case, 1 <= LEFT < LEFT + 1 = RIGHT <= N,
!    and X(LEFT) <= XVAL <= X(RIGHT).
!
!    Special cases:
!      Value is less than all data values:
!    LEFT = -1, RIGHT = 1, and XVAL < X(RIGHT).
!      Value is greater than all data values:
!    LEFT = N, RIGHT = -1, and X(LEFT) < XVAL.
!      Value is equal to a data value:
!    LEFT = RIGHT, and X(LEFT) = X(RIGHT) = XVAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, length of the input array.
!
!    Input, real ( kind = 8 ) X(N), an array that has been sorted into
!    ascending order.
!
!    Input, real ( kind = 8 ) XVAL, a value to be bracketed by entries of X.
!
!    Input, integer ( kind = 4 ) START, between 1 and N, specifies that XVAL
!    is likely to be in the interval:
!      [ X(START), X(START+1) ]
!    or, if not in that interval, then either
!      [ X(START+1), X(START+2) ]
!    or
!      [ X(START-1), X(START) ].
!
!    Output, integer ( kind = 4 ) LEFT, RIGHT, the results of the search.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) high
  integer ( kind = 4 ) left
  integer ( kind = 4 ) low
  integer ( kind = 4 ) right
  integer ( kind = 4 ) start
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xval
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_BRACKET2 - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    stop
  end if

  if ( start < 1 .or. n < start ) then
    start = ( n + 1 ) / 2
  end if
!
!  XVAL = X(START)?
!
  if ( x(start) == xval ) then

    left = start
    right = start
    return
!
!  X(START) < XVAL?
!
  else if ( x(start) < xval ) then
!
!  X(START) = X(N) < XVAL < oo?
!
    if ( n < start + 1 ) then

      left = start
      right = -1
      return
!
!  XVAL = X(START+1)?
!
    else if ( xval == x(start+1) ) then

      left = start + 1
      right = start + 1
      return
!
!  X(START) < XVAL < X(START+1)?
!
    else if ( xval < x(start+1) ) then

      left = start
      right = start + 1
      return
!
!  X(START+1) = X(N) < XVAL < oo?
!
    else if ( n < start + 2 ) then

      left = start + 1
      right = -1
      return
!
!  XVAL = X(START+2)?
!
    else if ( xval == x(start+2) ) then

      left = start + 2
      right = start + 2
      return
!
!  X(START+1) < XVAL < X(START+2)?
!
    else if ( xval < x(start+2) ) then

      left = start + 1
      right = start + 2
      return
!
!  Binary search for XVAL in [ X(START+2), X(N) ],
!  where XVAL is guaranteed to be greater than X(START+2).
!
    else

      low = start + 2
      high = n
      call r8vec_bracket ( high + 1 - low, x(low), xval, left, right )
      left = left + low - 1
      right = right + low - 1

    end if
!
!  -oo < XVAL < X(START) = X(1).
!
  else if ( start == 1 ) then

    left = -1
    right = start
    return
!
!  XVAL = X(START-1)?
!
  else if ( xval == x(start-1) ) then

    left = start - 1
    right = start - 1
    return
!
!  X(START-1) < XVAL < X(START)?
!
  else if ( x(start-1) <= xval ) then

    left = start - 1
    right = start
    return
!
!  Binary search for XVAL in [ X(1), X(START-1) ],
!  where XVAL is guaranteed to be less than X(START-1).
!
  else

    low = 1
    high = start - 1
    call r8vec_bracket ( high + 1 - low, x(1), xval, left, right )

  end if

  return
end



subroutine r8vec_bracket3 ( n, t, tval, left )

!*****************************************************************************80
!
!! R8VEC_BRACKET3 finds the interval containing or nearest a given value.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine always returns the index LEFT of the sorted array
!    T with the property that either
!    *  T is contained in the interval [ T(LEFT), T(LEFT+1) ], or
!    *  T < T(LEFT) = T(1), or
!    *  T > T(LEFT+1) = T(N).
!
!    The routine is useful for interpolation problems, where
!    the abscissa must be located within an interval of data
!    abscissas for interpolation, or the "nearest" interval
!    to the (extreme) abscissa must be found so that extrapolation
!    can be carried out.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, length of the input array.
!
!    Input, real ( kind = 8 ) T(N), an array that has been sorted
!    into ascending order.
!
!    Input, real ( kind = 8 ) TVAL, a value to be bracketed by entries of T.
!
!    Input/output, integer ( kind = 4 ) LEFT.
!    On input, if 1 <= LEFT <= N-1, LEFT is taken as a suggestion for the
!    interval [ T(LEFT), T(LEFT+1) ] in which TVAL lies.  This interval
!    is searched first, followed by the appropriate interval to the left
!    or right.  After that, a binary search is used.
!    On output, LEFT is set so that the interval [ T(LEFT), T(LEFT+1) ]
!    is the closest to TVAL; it either contains TVAL, or else TVAL
!    lies outside the interval [ T(1), T(N) ].
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) high
  integer ( kind = 4 ) left
  integer ( kind = 4 ) low
  integer ( kind = 4 ) mid
  real ( kind = 8 ) t(n)
  real ( kind = 8 ) tval
!
!  Check the input data.
!
  if ( n < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_BRACKET3 - Fatal error!'
    write ( *, '(a)' ) '  N must be at least 2.'
    stop
  end if
!
!  If LEFT is not between 1 and N-1, set it to the middle value.
!
  if ( left < 1 .or. n - 1 < left ) then
    left = ( n + 1 ) / 2
  end if
!
!  CASE 1: TVAL < T(LEFT):
!  Search for TVAL in [T(I), T(I+1)] for intervals I = 1 to LEFT-1.
!
  if ( tval < t(left) ) then

    if ( left == 1 ) then
      return
    else if ( left == 2 ) then
      left = 1
      return
    else if ( t(left-1) <= tval ) then
      left = left - 1
      return
    else if ( tval <= t(2) ) then
      left = 1
      return
    end if
!
!  ...Binary search for TVAL in [T(I), T(I+1)] for intervals I = 2 to LEFT-2.
!
    low = 2
    high = left - 2

    do

      if ( low == high ) then
        left = low
        return
      end if

      mid = ( low + high + 1 ) / 2

      if ( t(mid) <= tval ) then
        low = mid
      else
        high = mid - 1
      end if

    end do
!
!  CASE2: T(LEFT+1) < TVAL:
!  Search for TVAL in [T(I),T(I+1)] for intervals I = LEFT+1 to N-1.
!
  else if ( t(left+1) < tval ) then

    if ( left == n - 1 ) then
      return
    else if ( left == n - 2 ) then
      left = left + 1
      return
    else if ( tval <= t(left+2) ) then
      left = left + 1
      return
    else if ( t(n-1) <= tval ) then
      left = n - 1
      return
    end if
!
!  ...Binary search for TVAL in [T(I), T(I+1)] for intervals I = LEFT+2 to N-2.
!
    low = left + 2
    high = n - 2

    do

      if ( low == high ) then
        left = low
        return
      end if

      mid = ( low + high + 1 ) / 2

      if ( t(mid) <= tval ) then
        low = mid
      else
        high = mid - 1
      end if

    end do
!
!  CASE3: T(LEFT) <= TVAL <= T(LEFT+1):
!  T is in [T(LEFT), T(LEFT+1)], as the user said it might be.
!
  else

  end if

  return
end



subroutine r8vec_bracket4 ( nt, t, ns, s, left )

!*****************************************************************************80
!
!! R8VEC_BRACKET4 finds the nearest interval to each of a vector of values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine always returns the index LEFT of the sorted array
!    T with the property that either
!    *  T is contained in the interval [ T(LEFT), T(LEFT+1) ], or
!    *  T < T(LEFT) = T(1), or
!    *  T > T(LEFT+1) = T(NT).
!
!    The routine is useful for interpolation problems, where
!    the abscissa must be located within an interval of data
!    abscissas for interpolation, or the "nearest" interval
!    to the (extreme) abscissa must be found so that extrapolation
!    can be carried out.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, length of the input array.
!
!    Input, real ( kind = 8 ) T(NT), an array that has been sorted
!    into ascending order.
!
!    Input, integer ( kind = 4 ) NS, the number of points to be bracketed.
!
!    Input, real ( kind = 8 ) S(NS), values to be bracketed by entries of T.
!
!    Output, integer ( kind = 4 ) LEFT(NS).
!    LEFT(I) is set so that the interval [ T(LEFT(I)), T(LEFT(I)+1) ]
!    is the closest to S(I); it either contains S(I), or else S(I)
!    lies outside the interval [ T(1), T(NT) ].
!
  implicit none

  integer ( kind = 4 ) ns
  integer ( kind = 4 ) nt

  integer ( kind = 4 ) high
  integer ( kind = 4 ) i
  integer ( kind = 4 ) left(ns)
  integer ( kind = 4 ) low
  integer ( kind = 4 ) mid
  real ( kind = 8 ) s(ns)
  real ( kind = 8 ) t(nt)
!
!  Check the input data.
!
  if ( nt < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_BRACKET4 - Fatal error!'
    write ( *, '(a)' ) '  NT must be at least 2.'
    stop
  end if

  do i = 1, ns

    left(i) = ( nt + 1 ) / 2
!
!  CASE 1: S < T(LEFT):
!  Search for S in [T(I), T(I+1)] for intervals I = 1 to LEFT-1.
!
    if ( s(i) < t(left(i)) ) then

      if ( left(i) == 1 ) then
        cycle
      else if ( left(i) == 2 ) then
        left(i) = 1
        cycle
      else if ( t(left(i)-1) <= s(i) ) then
        left(i) = left(i) - 1
        cycle
      else if ( s(i) <= t(2) ) then
        left(i) = 1
        cycle
      end if
!
!  ...Binary search for S in [T(I), T(I+1)] for intervals I = 2 to LEFT-2.
!
      low = 2
      high = left(i) - 2

      do

        if ( low == high ) then
          left(i) = low
          exit
        end if

        mid = ( low + high + 1 ) / 2

        if ( t(mid) <= s(i) ) then
          low = mid
        else
          high = mid - 1
        end if

      end do
!
!  CASE2: T(LEFT+1) < S:
!  Search for S in [T(I),T(I+1)] for intervals I = LEFT+1 to N-1.
!
    else if ( t(left(i)+1) < s(i) ) then

      if ( left(i) == nt - 1 ) then
        cycle
      else if ( left(i) == nt - 2 ) then
        left(i) = left(i) + 1
        cycle
      else if ( s(i) <= t(left(i)+2) ) then
        left(i) = left(i) + 1
        cycle
      else if ( t(nt-1) <= s(i) ) then
        left(i) = nt - 1
        cycle
      end if
!
!  ...Binary search for S in [T(I), T(I+1)] for intervals I = LEFT+2 to NT-2.
!
      low = left(i) + 2
      high = nt - 2

      do

        if ( low == high ) then
          left(i) = low
          exit
        end if

        mid = ( low + high + 1 ) / 2

        if ( t(mid) <= s(i) ) then
          low = mid
        else
          high = mid - 1
        end if

      end do
!
!  CASE3: T(LEFT) <= S <= T(LEFT+1):
!  S is in [T(LEFT), T(LEFT+1)].
!
    else

    end if

  end do

  return
end
function r8vec_bracket5 ( nd, xd, xi )

!*****************************************************************************80
!
!! R8VEC_BRACKET5 brackets data between successive entries of a sorted R8VEC.
!
!  Discussion:
!
!    We assume XD is sorted.
!
!    If XI is contained in the interval [XD(1),XD(N)], then the returned 
!    value B indicates that XI is contained in [ XD(B), XD(B+1) ].
!
!    If XI is not contained in the interval [XD(1),XD(N)], then B = -1.
!
!    This code implements a version of binary search which is perhaps more
!    understandable than the usual ones.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ND, the number of data values.
!
!    Input, real ( kind = 8 ) XD(N), the sorted data.
!
!    Input, real ( kind = 8 ) XD, the query value.
!
!    Output, integer ( kind = 4 ) R8VEC_BRACKET5, the bracket information.
!
  implicit none

  integer ( kind = 4 ) nd

  integer ( kind = 4 ) b
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) r
  integer ( kind = 4 ) r8vec_bracket5
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) xi

  if ( xi < xd(1) .or. xd(nd) < xi ) then

    b = -1

  else

    l = 1
    r = nd

    do while ( l + 1 < r )
      m = ( l + r ) / 2
      if ( xi < xd(m) ) then
        r = m
      else
        l = m
      end if
    end do

    b = l

  end if

  r8vec_bracket5 = b

  return
end



subroutine r8vec_bracket6 ( nd, xd, ni, xi, b )

!*****************************************************************************80
!
!! R8VEC_BRACKET6 brackets data between successive entries of a sorted R8VEC.
!
!  Discussion:
!
!    We assume XD is sorted.
!
!    If XI(I) is contained in the interval [XD(1),XD(N)], then the value of
!    B(I) indicates that XI(I) is contained in [ XD(B(I)), XD(B(I)+1) ].
!
!    If XI(I) is not contained in the interval [XD(1),XD(N)], then B(I) = -1.
!
!    This code implements a version of binary search which is perhaps more
!    understandable than the usual ones.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) ND, the number of data values.
!
!    Input, real ( kind = 8 ) XD(N), the sorted data.
!
!    Input, integer ( kind = 4 ) NI, the number of inquiry values.
!
!    Input, real ( kind = 8 ) XD(NI), the query values.
!
!    Output, integer ( kind = 4 ) B(NI), the bracket information.
!
  implicit none

  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni

  integer ( kind = 4 ) b(ni)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) r
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) xi(ni)

  do i = 1, ni

    if ( xi(i) < xd(1) .or. xd(nd) < xi(i) ) then

      b(i) = -1

    else

      l = 1
      r = nd

      do while ( l + 1 < r )
        m = ( l + r ) / 2
        if ( xi(i) < xd(m) ) then
          r = m
        else
          l = m
        end if
      end do

      b(i) = l

    end if

  end do

  return
end



subroutine r8vec_ceiling ( n, r8vec, ceilingvec )

!*****************************************************************************80
!
!! R8VEC_CEILING rounds "up" (towards +oo) entries of an R8VEC.
!
!  Example:
!
!    R8    Value
!
!   -1.1  -1.0
!   -1.0  -1.0
!   -0.9   0.0
!    0.0   0.0
!    5.0   5.0
!    5.1   6.0
!    5.9   6.0
!    6.0   6.0
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
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Input, real ( kind = 8 ) R8VEC(N), the vector.
!
!    Output, real ( kind = 8 ) CEILINGVEC(N), the rounded values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) ceilingvec(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec(n)
  real ( kind = 8 ) value

  do i = 1, n

    value = real ( int ( r8vec(i) ), kind = 8 )

    if ( value < r8vec(i) ) then
      value = value + 1.0D+00
    end if

    ceilingvec(i) = value

  end do

  return
end



subroutine r8vec_chebyspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_CHEBYSPACE creates a vector of Chebyshev spaced values in [A,B].
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the interval.
!
!    Output, real ( kind = 8 ) X(N), a vector of Chebyshev spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) theta
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n

      theta = real ( n - i, kind = 8 ) * pi &
            / real ( n - 1, kind = 8 )

      c = cos ( theta )

      if ( mod ( n, 2 ) == 1 ) then
        if ( 2 * i - 1 == n ) then
          c = 0.0D+00
        end if
      end if

      x(i) = ( ( 1.0D+00 - c ) * a  &
             + ( 1.0D+00 + c ) * b ) &
             /   2.0D+00

    end do

  end if

  return
end



subroutine r8vec_cheby1space ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_CHEBY1SPACE creates Type 1 Chebyshev spaced values in [A,B].
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of Chebyshev spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) theta
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n

      theta = real ( 2 * ( n - i ) + 1, kind = 8 ) * pi &
        / real ( 2 * n, kind = 8 )

      c = cos ( theta )

      if ( mod ( n, 2 ) == 1 ) then
        if ( 2 * i - 1 == n ) then
          c = 0.0D+00
        end if
      end if

      x(i) = ( ( 1.0D+00 - c ) * a   &
             + ( 1.0D+00 + c ) * b ) &
             /   2.0D+00

    end do

  end if

  return
end



subroutine r8vec_cheby2space ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_CHEBY2SPACE creates Type 2 Chebyshev spaced values in [A,B].
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of Chebyshev spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) theta
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n

      theta = real ( n - i, kind = 8 ) * pi / real ( n - 1, kind = 8 )

      c = cos ( theta )

      if ( mod ( n, 2 ) == 1 ) then
        if ( 2 * i - 1 == n ) then
          c = 0.0D+00
        end if
      end if

      x(i) = ( ( 1.0D+00 - c ) * a   &
             + ( 1.0D+00 + c ) * b ) &
             /   2.0D+00

    end do

  end if

  return
end



subroutine r8vec_circular_variance ( n, x, circular_variance )

!*****************************************************************************80
!
!! R8VEC_CIRCULAR_VARIANCE returns the circular variance of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(N), the vector whose variance is desired.
!
!    Output, real ( kind = 8 ) CIRCULAR VARIANCE, the circular variance
!    of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) circular_variance
  real ( kind = 8 ) mean
  real ( kind = 8 ) x(n)

  call r8vec_mean ( n, x, mean )

  circular_variance = &
      ( sum ( cos ( x(1:n) - mean ) ) )**2 &
    + ( sum ( sin ( x(1:n) - mean ) ) )**2

  circular_variance = sqrt ( circular_variance ) / real ( n, kind = 8 )

  circular_variance = 1.0D+00 - circular_variance

  return
end



subroutine r8vec_compare ( n, a1, a2, isgn )

!*****************************************************************************80
!
!! R8VEC_COMPARE compares two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The lexicographic ordering is used.
!
!  Example:
!
!    Input:
!
!      A1 = ( 2.0, 6.0, 2.0 )
!      A2 = ( 2.0, 8.0, 12.0 )
!
!    Output:
!
!      ISGN = -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), the vectors to be compared.
!
!    Output, integer ( kind = 4 ) ISGN, the results of the comparison:
!    -1, A1 < A2,
!     0, A1 = A2,
!    +1, A1 > A2.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) k

  isgn = 0

  k = 1

  do while ( k <= n )

    if ( a1(k) < a2(k) ) then
      isgn = -1
      return
    else if ( a2(k) < a1(k) ) then
      isgn = + 1
      return
    end if

    k = k + 1

  end do

  return
end



subroutine r8vec_convolution ( m, x, n, y, z )

!*****************************************************************************80
!
!! R8VEC_CONVOLUTION returns the convolution of two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The I-th entry of the convolution can be formed by summing the products 
!    that lie along the I-th diagonal of the following table:
!
!    Y3 | 3   4   5   6   7
!    Y2 | 2   3   4   5   6
!    Y1 | 1   2   3   4   5
!       +------------------
!        X1  X2  X3  X4  X5
!
!    which will result in:
!
!    Z = ( X1 * Y1,
!          X1 * Y2 + X2 * Y1,
!          X1 * Y3 + X2 * Y2 + X3 * Y1,
!                    X2 * Y3 + X3 * Y2 + X4 * Y1,
!                              X3 * Y3 + X4 * Y2 + X5 * Y1,
!                                        X4 * Y3 + X5 * Y2,
!                                                  X5 * Y3 )
!            
!  Example:
!
!    Input:
!
!      X = (/ 1, 2, 3, 4 /)
!      Y = (/ -1, 5, 3 /)
!
!    Output:
!
!      Z = (/ -1, 3, 10, 17, 29, 12 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the dimension of X.
!
!    Input, real ( kind = 8 ) X(M), the first vector to be convolved.
!
!    Input, integer ( kind = 4 ) N, the dimension of Y.
!
!    Input, real ( kind = 8 ) Y(N), the second vector to be convolved.
!
!    Output, real ( kind = 8 ) Z(M+N-1), the convolution of X and Y.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  real ( kind = 8 ) x(m)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(m+n-1)

  z(1:m+n-1) = 0.0D+00

  do j = 1, n
    z(j:j+m-1) = z(j:j+m-1) + x(1:m) * y(j)
  end do

  return
end



subroutine r8vec_convolution_circ ( n, x, y, z )

!*****************************************************************************80
!
!! R8VEC_CONVOLUTION_CIRC: discrete circular convolution of two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The formula used is:
!
!      z(1+m) = xCCy(m) = sum ( 0 <= k <= n-1 ) x(1+k) * y(1+m-k)
!
!    Here, if the index of Y becomes nonpositive, it is "wrapped around"
!    by having N added to it.
!
!    The circular convolution is equivalent to multiplication of Y by a
!    circulant matrix formed from the vector X.
!
!  Example:
!
!    Input:
!
!      X = (/ 1, 2, 3, 4 /)
!      Y = (/ 1, 2, 4, 8 /)
!
!    Output:
!
!      Circulant form:
!
!      Z = ( 1 4 3 2 )   ( 1 )
!          ( 2 1 4 3 )   ( 2 )
!          ( 3 2 1 4 ) * ( 4 )
!          ( 4 3 2 1 )   ( 8 )
!
!      The formula:
!
!      Z = (/ 1*1 + 2*8 + 3*4 + 4*2,
!             1*2 + 2*1 + 3*8 + 4*4,
!             1*4 + 2*2 + 3*1 + 4*8,
!             1*8 + 2*4 + 3*2 + 4*1 /)
!
!      Result:
!
!      Z = (/ 37, 44, 43, 26 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the vectors to be convolved.
!
!    Output, real ( kind = 8 ) Z(N), the circular convolution of X and Y.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) m
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)

  do m = 1, n
    z(m) = dot_product ( x(1:m), y(m:1:-1) ) &
         + dot_product ( x(m+1:n), y(n:m+1:-1) )
  end do

  return
end



subroutine r8vec_copy ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_COPY copies an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the vectors.
!
!    Input, real ( kind = 8 ) A1(N), the vector to be copied.
!
!    Output, real ( kind = 8 ) A2(N), a copy of A1.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)

  a2(1:n) = a1(1:n)

  return
end



subroutine r8vec_correlation ( n, x, y, correlation )

!*****************************************************************************80
!
!! R8VEC_CORRELATION returns the correlation of two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If X and Y are two nonzero vectors of length N, then
!
!      correlation = (x/||x||)' (y/||y||)
!
!    It is the cosine of the angle between the two vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input, real ( kind = 8 ) X(N), Y(N), the vectors to be convolved.
!
!    Output, real ( kind = 8 ) CORRELATION, the correlation of X and Y.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) correlation
  real ( kind = 8 ) r8vec_norm
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_norm
  real ( kind = 8 ) xy_dot
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) y_norm

  x_norm = r8vec_norm ( n, x )
  y_norm = r8vec_norm ( n, y )
  xy_dot = dot_product ( x(1:n), y(1:n) )

  if ( x_norm == 0.0D+00 .or. y_norm == 0.0D+00 ) then
    correlation = 0.0D+00
  else
    correlation = xy_dot / x_norm / y_norm
  end if

  return
end
function r8vec_cross_product_2d ( v1, v2 )

!*****************************************************************************80
!
!! R8VEC_CROSS_PRODUCT_2D finds the cross product of a pair of vectors in 2D.
!
!  Discussion:
!
!    Strictly speaking, the vectors V1 and V2 should be considered
!    to lie in a 3D space, both having Z coordinate zero.  The cross
!    product value V3 then represents the standard cross product vector
!    (0,0,V3).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) V1(2), V2(2), the vectors.
!
!    Output, real ( kind = 8 ) R8VEC_CROSS_PRODUCT_2D, the cross product.
!
  implicit none

  real ( kind = 8 ) r8vec_cross_product_2d
  real ( kind = 8 ) v1(2)
  real ( kind = 8 ) v2(2)

  r8vec_cross_product_2d = v1(1) * v2(2) - v1(2) * v2(1)

  return
end
function r8vec_cross_product_affine_2d ( v0, v1, v2 )

!*****************************************************************************80
!
!! R8VEC_CROSS_PRODUCT_AFFINE_2D finds the affine cross product in 2D.
!
!  Discussion:
!
!    Strictly speaking, the vectors V1 and V2 should be considered
!    to lie in a 3D space, both having Z coordinate zero.  The cross
!    product value V3 then represents the standard cross product vector
!    (0,0,V3).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) V0(2), the base vector.
!
!    Input, real ( kind = 8 ) V1(2), V2(2), the vectors.
!
!    Output, real ( kind = 8 ) R8VEC_CROSS_PRODUCT_AFFINE_2D,
!    the cross product (V1-V0) x (V2-V0).
!
  implicit none

  real ( kind = 8 ) r8vec_cross_product_affine_2d
  real ( kind = 8 ) v0(2)
  real ( kind = 8 ) v1(2)
  real ( kind = 8 ) v2(2)

  r8vec_cross_product_affine_2d = &
      ( v1(1) - v0(1) ) * ( v2(2) - v0(2) ) &
    - ( v2(1) - v0(1) ) * ( v1(2) - v0(2) )

  return
end



subroutine r8vec_cross_product_3d ( v1, v2, v3 )

!*****************************************************************************80
!
!! R8VEC_CROSS_PRODUCT_3D computes the cross product of two R8VEC's in 3D.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The cross product in 3D can be regarded as the determinant of the
!    symbolic matrix:
!
!          |  i  j  k |
!      det | x1 y1 z1 |
!          | x2 y2 z2 |
!
!      = ( y1 * z2 - z1 * y2 ) * i
!      + ( z1 * x2 - x1 * z2 ) * j
!      + ( x1 * y2 - y1 * x2 ) * k
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) V1(3), V2(3), the two vectors.
!
!    Output, real ( kind = 8 ) V3(3), the cross product vector.
!
  implicit none

  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)

  v3(1) = v1(2) * v2(3) - v1(3) * v2(2)
  v3(2) = v1(3) * v2(1) - v1(1) * v2(3)
  v3(3) = v1(1) * v2(2) - v1(2) * v2(1)

  return
end



subroutine r8vec_cross_product_affine_3d ( v0, v1, v2, v3 )

!*****************************************************************************80
!
!! R8VEC_CROSS_PRODUCT_AFFINE_3D computes the affine cross product in 3D.
!
!  Discussion:
!
!    The cross product in 3D can be regarded as the determinant of the
!    symbolic matrix:
!
!          |  i  j  k |
!      det | x1 y1 z1 |
!          | x2 y2 z2 |
!
!      = ( y1 * z2 - z1 * y2 ) * i
!      + ( z1 * x2 - x1 * z2 ) * j
!      + ( x1 * y2 - y1 * x2 ) * k
!
!    Here, we use V0 as the base of an affine system so we compute
!    the cross product of (V1-V0) and (V2-V0).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) V0(3), the base vector.
!
!    Input, real ( kind = 8 ) V1(3), V2(3), the two vectors.
!
!    Output, real ( kind = 8 ) V3(3), the cross product vector
!    ( V1-V0) x (V2-V0).
!
  implicit none

  real ( kind = 8 ) v0(3)
  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)

  v3(1) = ( v1(2) - v0(2) ) * ( v2(3) - v0(3) ) &
        - ( v2(2) - v0(2) ) * ( v1(3) - v0(3) )

  v3(2) = ( v1(3) - v0(3) ) * ( v2(1) - v0(1) ) &
        - ( v2(3) - v0(3) ) * ( v1(1) - v0(1) )

  v3(3) = ( v1(1) - v0(1) ) * ( v2(2) - v0(2) ) &
        - ( v2(1) - v0(1) ) * ( v1(2) - v0(2) )

  return
end



subroutine r8vec_cum ( n, a, a_cum )

!*****************************************************************************80
!
!! R8VEC_CUM computes the cumulutive sums of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Input:
!
!      A = (/ 1.0, 2.0, 3.0, 4.0 /)
!
!    Output:
!
!      A_CUM = (/ 1.0, 3.0, 6.0, 10.0 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be summed.
!
!    Output, real ( kind = 8 ) A_CUM(1:N), the cumulative sums.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_cum(n)
  integer ( kind = 4 ) i

  a_cum(1) = a(1)

  do i = 2, n
    a_cum(i) = a_cum(i-1) + a(i)
  end do

  return
end



subroutine r8vec_cum0 ( n, a, a_cum )

!*****************************************************************************80
!
!! R8VEC_CUM0 computes the cumulutive sums of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Input:
!
!      A = (/ 1.0, 2.0, 3.0, 4.0 /)
!
!    Output:
!
!      A_CUM = (/ 0.0, 1.0, 3.0, 6.0, 10.0 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be summed.
!
!    Output, real ( kind = 8 ) A_CUM(0:N), the cumulative sums.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_cum(0:n)
  integer ( kind = 4 ) i

  a_cum(0) = 0.0D+00

  do i = 1, n
    a_cum(i) = a_cum(i-1) + a(i)
  end do

  return
end



subroutine r8vec_dif ( n, h, cof )

!*****************************************************************************80
!
!! R8VEC_DIF computes coefficients for estimating the N-th derivative.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine computes the N+1 coefficients for a centered finite difference
!    estimate of the N-th derivative of a function.
!
!    The estimate has the form
!
!      FDIF(N,X) = Sum (I = 0 to N) COF(I) * F ( X(I) )
!
!    To understand the computation of the coefficients, it is enough
!    to realize that the first difference approximation is
!
!      FDIF(1,X) = F(X+DX) - F(X-DX) ) / (2*DX)
!
!    and that the second difference approximation can be regarded as
!    the first difference approximation repeated:
!
!      FDIF(2,X) = FDIF(1,X+DX) - FDIF(1,X-DX) / (2*DX)
!         = F(X+2*DX) - 2 F(X) + F(X-2*DX) / (4*DX)
!
!    and so on for higher order differences.
!
!    Thus, the next thing to consider is the integer coefficients of
!    the sampled values of F, which are clearly the Pascal coefficients,
!    but with an alternating negative sign.  In particular, if we
!    consider row I of Pascal's triangle to have entries j = 0 through I,
!    then P(I,J) = P(I-1,J-1) - P(I-1,J), where P(*,-1) is taken to be 0,
!    and P(0,0) = 1.
!
!       1
!      -1  1
!       1 -2   1
!      -1  3  -3   1
!       1 -4   6  -4   1
!      -1  5 -10  10  -5  1
!       1 -6  15 -20  15 -6 1
!
!    Next, note that the denominator of the approximation for the
!    N-th derivative will be (2*DX)^N.
!
!    And finally, consider the location of the N+1 sampling
!    points for F:
!
!      X-N*DX, X-(N-2)*DX, X-(N-4)*DX, ..., X+(N-4)*DX, X+(N-2*DX), X+N*DX.
!
!    Thus, a formula for evaluating FDIF(N,X) is
!
!      fdif = 0.0
!      do i = 0, n
!        xi = x + (2*i-n) * h
!        fdif = fdif + cof(i) * f(xi)
!      end do
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the derivative to be
!    approximated.  N must be 0 or greater.
!
!    Input, real ( kind = 8 ) H, the half spacing between points.
!    H must be positive.
!
!    Output, real ( kind = 8 ) COF(0:N), the coefficients needed to approximate
!    the N-th derivative of a function F.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) cof(0:n)
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_DIF - Fatal error!'
    write ( *, '(a,i8)' ) '  Derivative order N = ', n
    write ( *, '(a)' ) '  but N must be at least 0.'
    stop
  end if

  if ( h <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_DIF - Fatal error!'
    write ( *, '(a,g14.6)' ) '  The half sampling spacing is H = ', h
    write ( *, '(a)' ) '  but H must be positive.'
    stop
  end if

  do i = 0, n

    cof(i) = 1.0D+00

    do j = i - 1, 1, -1
      cof(j) = -cof(j) + cof(j-1)
    end do

    if ( 0 < i ) then
      cof(0) = -cof(0)
    end if

  end do

  cof(0:n) = cof(0:n) / ( 2.0D+00 * h )**n

  return
end
function r8vec_diff_dot_product ( n, u1, v1, u2, v2 )

!*****************************************************************************80
!
!! R8VEC_DIFF_DOT_PRODUCT: dot product of a pair of R8VEC differences.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input, real ( kind = 8 ) U1(N), V1(N), defines the vector U1-V1.
!
!    Input, real ( kind = 8 ) U2(N), V2(N), defines the vector U2-V2.
!
!    Output, real ( kind = 8 ) R8VEC_DIFF_DOT_PRODUCT, the dot product 
!    of (U1-V1)*(U2-V2).
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec_diff_dot_product
  real ( kind = 8 ) u1(n)
  real ( kind = 8 ) u2(n)
  real ( kind = 8 ) v1(n)
  real ( kind = 8 ) v2(n)
  real ( kind = 8 ) value

  value = 0.0D+00
  do i = 1, n
    value = value + ( u1(i) - v1(i) ) * ( u2(i) - v2(i) )
  end do

  r8vec_diff_dot_product = value

  return
end
function r8vec_diff_norm ( n, a, b )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM returns the L2 norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), B(N), the vectors
!
!    Output, real ( kind = 8 ) R8VEC_DIFF_NORM, the L2 norm of A - B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r8vec_diff_norm

  r8vec_diff_norm = sqrt ( sum ( ( a(1:n) - b(1:n) )**2 ) )

  return
end
function r8vec_diff_norm_l1 ( n, a, b )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM_L1 returns the L1 norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L1 norm is defined as:
!
!      R8VEC_NORM_L1 = sum ( 1 <= I <= N ) abs ( A(I) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), B(N), the vectors.
!
!    Output, real ( kind = 8 ) R8VEC_DIFF_NORM_L1, the L1 norm of A - B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r8vec_diff_norm_l1

  r8vec_diff_norm_l1 = sum ( abs ( a(1:n) - b(1:n) ) )

  return
end
function r8vec_diff_norm_l2 ( n, a, b )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM_L2 returns the L2 norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), B(N), the vectors
!
!    Output, real ( kind = 8 ) R8VEC_DIFF_NORM_L2, the L2 norm of A - B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r8vec_diff_norm_l2

  r8vec_diff_norm_l2 = sqrt ( sum ( ( a(1:n) - b(1:n) )**2 ) )

  return
end
function r8vec_diff_norm_li ( n, a, b )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM_LI returns the L-oo norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L-oo norm is defined as:
!
!      R8VEC_NORM_LI = max ( 1 <= I <= N ) abs ( A(I) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), B(N), the vectors
!
!    Output, real ( kind = 8 ) R8VEC_DIFF_NORM_LI, the L-oo norm of A - B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r8vec_diff_norm_li

  r8vec_diff_norm_li = maxval ( abs ( a(1:n) - b(1:n) ) )

  return
end
function r8vec_diff_norm_squared ( n, a, b )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM_SQUARED: square of the L2 norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    R8VEC_DIFF_NORM_SQUARED = sum ( 1 <= I <= N ) ( A(I) - B(I) )^2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), B(N), the vectors
!
!    Output, real ( kind = 8 ) R8VEC_DIFF_NORM_SQUARED, the square of 
!    the L2 norm of A - B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r8vec_diff_norm_squared

  r8vec_diff_norm_squared = sum ( ( a(1:n) - b(1:n) )**2 )

  return
end



subroutine r8vec_direct_product ( factor_index, factor_order, factor_value, &
  factor_num, point_num, x )

!*****************************************************************************80
!
!! R8VEC_DIRECT_PRODUCT creates a direct product of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    To explain what is going on here, suppose we had to construct
!    a multidimensional quadrature rule as the product of K rules
!    for 1D quadrature.
!
!    The product rule will be represented as a list of points and weights.
!
!    The J-th item in the product rule will be associated with
!      item J1 of 1D rule 1,
!      item J2 of 1D rule 2,
!      ...,
!      item JK of 1D rule K.
!
!    In particular,
!      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
!    and
!      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
!
!    So we can construct the quadrature rule if we can properly
!    distribute the information in the 1D quadrature rules.
!
!    This routine carries out that task for the abscissas X.
!
!    Another way to do this would be to compute, one by one, the
!    set of all possible indices (J1,J2,...,JK), and then index
!    the appropriate information.  An advantage of the method shown
!    here is that you can process the K-th set of information and
!    then discard it.
!
!  Example:
!
!    Rule 1:
!      Order = 4
!      X(1:4) = ( 1, 2, 3, 4 )
!
!    Rule 2:
!      Order = 3
!      X(1:3) = ( 10, 20, 30 )
!
!    Rule 3:
!      Order = 2
!      X(1:2) = ( 100, 200 )
!
!    Product Rule:
!      Order = 24
!      X(1:24) =
!        ( 1, 10, 100 )
!        ( 2, 10, 100 )
!        ( 3, 10, 100 )
!        ( 4, 10, 100 )
!        ( 1, 20, 100 )
!        ( 2, 20, 100 )
!        ( 3, 20, 100 )
!        ( 4, 20, 100 )
!        ( 1, 30, 100 )
!        ( 2, 30, 100 )
!        ( 3, 30, 100 )
!        ( 4, 30, 100 )
!        ( 1, 10, 200 )
!        ( 2, 10, 200 )
!        ( 3, 10, 200 )
!        ( 4, 10, 200 )
!        ( 1, 20, 200 )
!        ( 2, 20, 200 )
!        ( 3, 20, 200 )
!        ( 4, 20, 200 )
!        ( 1, 30, 200 )
!        ( 2, 30, 200 )
!        ( 3, 30, 200 )
!        ( 4, 30, 200 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) FACTOR_INDEX, the index of the factor being
!    processed.  The first factor processed must be factor 1!
!
!    Input, integer ( kind = 4 ) FACTOR_ORDER, the order of the factor.
!
!    Input, real ( kind = 8 ) FACTOR_VALUE(FACTOR_ORDER), the factor values
!    for factor FACTOR_INDEX.
!
!    Input, integer ( kind = 4 ) FACTOR_NUM, the number of factors.
!
!    Input, integer ( kind = 4 ) POINT_NUM, the number of elements in the
!    direct product.
!
!    Input/output, real ( kind = 8 ) X(FACTOR_NUM,POINT_NUM), the elements of
!    the direct product, which are built up gradually.
!
!  Local Parameters:
!
!    Local, integer START, the first location of a block of values to set.
!
!    Local, integer CONTIG, the number of consecutive values to set.
!
!    Local, integer SKIP, the distance from the current value of START
!    to the next location of a block of values to set.
!
!    Local, integer REP, the number of blocks of values to set.
!
  implicit none

  integer ( kind = 4 ) factor_num
  integer ( kind = 4 ) factor_order
  integer ( kind = 4 ) point_num

  integer ( kind = 4 ), save :: contig
  integer ( kind = 4 ) factor_index
  real ( kind = 8 ) factor_value(factor_order)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ), save :: rep
  integer ( kind = 4 ), save :: skip
  integer ( kind = 4 ) start
  real ( kind = 8 ) x(factor_num,point_num)

  if ( factor_index == 1 ) then
    contig = 1
    skip = 1
    rep = point_num
    x(1:factor_num,1:point_num) = 0.0D+00
  end if

  rep = rep / factor_order
  skip = skip * factor_order

  do j = 1, factor_order

    start = 1 + ( j - 1 ) * contig

    do k = 1, rep
      x(factor_index,start:start+contig-1) = factor_value(j)
      start = start + skip
    end do

  end do

  contig = contig * factor_order

  return
end



subroutine r8vec_direct_product2 ( factor_index, factor_order, factor_value, &
  factor_num, point_num, w )

!*****************************************************************************80
!
!! R8VEC_DIRECT_PRODUCT2 creates a direct product of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    To explain what is going on here, suppose we had to construct
!    a multidimensional quadrature rule as the product of K rules
!    for 1D quadrature.
!
!    The product rule will be represented as a list of points and weights.
!
!    The J-th item in the product rule will be associated with
!      item J1 of 1D rule 1,
!      item J2 of 1D rule 2,
!      ...,
!      item JK of 1D rule K.
!
!    In particular,
!      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
!    and
!      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
!
!    So we can construct the quadrature rule if we can properly
!    distribute the information in the 1D quadrature rules.
!
!    This routine carries out the task involving the weights W.
!
!    Another way to do this would be to compute, one by one, the
!    set of all possible indices (J1,J2,...,JK), and then index
!    the appropriate information.  An advantage of the method shown
!    here is that you can process the K-th set of information and
!    then discard it.
!
!  Example:
!
!    Rule 1:
!      Order = 4
!      W(1:4) = ( 2, 3, 5, 7 )
!
!    Rule 2:
!      Order = 3
!      W(1:3) = ( 11, 13, 17 )
!
!    Rule 3:
!      Order = 2
!      W(1:2) = ( 19, 23 )
!
!    Product Rule:
!      Order = 24
!      W(1:24) =
!        ( 2 * 11 * 19 )
!        ( 3 * 11 * 19 )
!        ( 4 * 11 * 19 )
!        ( 7 * 11 * 19 )
!        ( 2 * 13 * 19 )
!        ( 3 * 13 * 19 )
!        ( 5 * 13 * 19 )
!        ( 7 * 13 * 19 )
!        ( 2 * 17 * 19 )
!        ( 3 * 17 * 19 )
!        ( 5 * 17 * 19 )
!        ( 7 * 17 * 19 )
!        ( 2 * 11 * 23 )
!        ( 3 * 11 * 23 )
!        ( 5 * 11 * 23 )
!        ( 7 * 11 * 23 )
!        ( 2 * 13 * 23 )
!        ( 3 * 13 * 23 )
!        ( 5 * 13 * 23 )
!        ( 7 * 13 * 23 )
!        ( 2 * 17 * 23 )
!        ( 3 * 17 * 23 )
!        ( 5 * 17 * 23 )
!        ( 7 * 17 * 23 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) FACTOR_INDEX, the index of the factor being
!    processed.  The first factor processed must be factor 1!
!
!    Input, integer ( kind = 4 ) FACTOR_ORDER, the order of the factor.
!
!    Input, real ( kind = 8 ) FACTOR_VALUE(FACTOR_ORDER), the factor values
!    for factor FACTOR_INDEX.
!
!    Input, integer ( kind = 4 ) FACTOR_NUM, the number of factors.
!
!    Input, integer ( kind = 4 ) POINT_NUM, the number of elements in the
!    direct product.
!
!    Input/output, real ( kind = 8 ) W(POINT_NUM), the elements of the
!    direct product, which are built up gradually.
!
!  Local Parameters:
!
!    Local, integer ( kind = 4 ) START, the first location of a block of values
!    to set.
!
!    Local, integer ( kind = 4 ) CONTIG, the number of consecutive values
!    to set.
!
!    Local, integer SKIP, the distance from the current value of START
!    to the next location of a block of values to set.
!
!    Local, integer REP, the number of blocks of values to set.
!
  implicit none

  integer ( kind = 4 ) factor_num
  integer ( kind = 4 ) factor_order
  integer ( kind = 4 ) point_num

  integer ( kind = 4 ), save :: contig
  integer ( kind = 4 ) factor_index
  real ( kind = 8 ) factor_value(factor_order)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ), save :: rep
  integer ( kind = 4 ), save :: skip
  integer ( kind = 4 ) start
  real ( kind = 8 ) w(point_num)

  if ( factor_index == 1 ) then
    contig = 1
    skip = 1
    rep = point_num
    w(1:point_num) = 1.0D+00
  end if

  rep = rep / factor_order
  skip = skip * factor_order

  do j = 1, factor_order

    start = 1 + ( j - 1 ) * contig

    do k = 1, rep
      w(start:start+contig-1) = w(start:start+contig-1) * factor_value(j)
      start = start + skip
    end do

  end do

  contig = contig * factor_order

  return
end
function r8vec_distance ( dim_num, v1, v2 )

!*****************************************************************************80
!
!! R8VEC_DISTANCE returns the Euclidean distance between two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    Input, real ( kind = 8 ) V1(DIM_NUM), V2(DIM_NUM), the vectors.
!
!    Output, real ( kind = 8 ) R8VEC_DISTANCE, the Euclidean distance
!    between the vectors.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ) r8vec_distance
  real ( kind = 8 ) v1(dim_num)
  real ( kind = 8 ) v2(dim_num)

  r8vec_distance = sqrt ( sum ( ( v1(1:dim_num) - v2(1:dim_num) )**2 ) )

  return
end
function r8vec_distinct ( n, a )

!*****************************************************************************80
!
!! R8VEC_DISTINCT is true if the entries in an R8VEC are distinct.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be checked.
!
!    Output, logical R8VEC_DISTINCT is TRUE if the elements of A are distinct.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  logical r8vec_distinct

  r8vec_distinct = .false.

  do i = 2, n
    do j = 1, i - 1
      if ( a(i) == a(j) ) then
        return
      end if
    end do
  end do

  r8vec_distinct = .true.

  return
end
function r8vec_dot_product ( n, v1, v2 )

!*****************************************************************************80
!
!! R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In FORTRAN90, the system routine DOT_PRODUCT should be called
!    directly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input, real ( kind = 8 ) V1(N), V2(N), the vectors.
!
!    Output, real ( kind = 8 ) R8VEC_DOT_PRODUCT, the dot product.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8vec_dot_product
  real ( kind = 8 ) v1(n)
  real ( kind = 8 ) v2(n)

  r8vec_dot_product = dot_product ( v1(1:n), v2(1:n) )

  return
end
function r8vec_dot_product_affine ( n, v0, v1, v2 )

!*****************************************************************************80
!
!! R8VEC_DOT_PRODUCT_AFFINE computes the affine dot product V1-V0 * V2-V0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!
!    Input, real ( kind = 8 ) V0(N), the base vector.
!
!    Input, real ( kind = 8 ) V1(N), V2(N), the vectors.
!
!    Output, real ( kind = 8 ) R8VEC_DOT_PRODUCT_AFFINE, the dot product.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8vec_dot_product_affine
  real ( kind = 8 ) v0(n)
  real ( kind = 8 ) v1(n)
  real ( kind = 8 ) v2(n)

  r8vec_dot_product_affine = dot_product ( &
    v1(1:n) - v0(1:n),  &
    v2(1:n) - v0(1:n) )

  return
end
function r8vec_eq ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_EQ is true if two R8VECs are equal.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
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
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), two vectors to compare.
!
!    Output, logical R8VEC_EQ, is TRUE if every pair of elements A1(I)
!    and A2(I) are equal, and FALSE otherwise.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  logical r8vec_eq

  r8vec_eq = ( all ( a1(1:n) == a2(1:n) ) )

  return
end



subroutine r8vec_even ( n, alo, ahi, a )

!*****************************************************************************80
!
!! R8VEC_EVEN returns an R8VEC of evenly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If N is 1, then the midpoint is returned.
!
!    Otherwise, the two endpoints are returned, and N-2 evenly
!    spaced points between them.
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
!    Input, integer ( kind = 4 ) N, the number of values.
!
!    Input, real ( kind = 8 ) ALO, AHI, the low and high values.
!
!    Output, real ( kind = 8 ) A(N), N evenly spaced values.
!    Normally, A(1) = ALO and A(N) = AHI.
!    However, if N = 1, then A(1) = 0.5*(ALO+AHI).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) ahi
  real ( kind = 8 ) alo
  integer ( kind = 4 ) i

  if ( n == 1 ) then

    a(1) = 0.5D+00 * ( alo + ahi )

  else

    do i = 1, n
      a(i) = ( real ( n - i,     kind = 8 ) * alo   &
             + real (     i - 1, kind = 8 ) * ahi ) &
             / real ( n     - 1, kind = 8 )
    end do

  end if

  return
end



subroutine r8vec_even_select ( n, xlo, xhi, ival, xval )

!*****************************************************************************80
!
!! R8VEC_EVEN_SELECT returns the I-th of N evenly spaced values in [ XLO, XHI ].
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    XVAL = ( (N-IVAL) * XLO + (IVAL-1) * XHI ) / real ( N - 1 )
!
!    Unless N = 1, X(1) = XLO and X(N) = XHI.
!
!    If N = 1, then X(1) = 0.5*(XLO+XHI).
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
!    Input, integer ( kind = 4 ) N, the number of values.
!
!    Input, real ( kind = 8 ) XLO, XHI, the low and high values.
!
!    Input, integer ( kind = 4 ) IVAL, the index of the desired point.
!    IVAL is normally between 1 and N, but may be any integer value.
!
!    Output, real ( kind = 8 ) XVAL, the IVAL-th of N evenly spaced values
!    between XLO and XHI.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ival
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xval

  if ( n == 1 ) then

    xval = 0.5D+00 * ( xlo + xhi )

  else

    xval = ( real ( n - ival,     kind = 8 ) * xlo   &
           + real (     ival - 1, kind = 8 ) * xhi ) &
           / real ( n        - 1, kind = 8 )

  end if

  return
end



subroutine r8vec_even2 ( maxval, nfill, nold, xold, nval, xval )

!*****************************************************************************80
!
!! R8VEC_EVEN2 linearly interpolates new numbers into an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The number of values created between two old values can vary from
!    one pair of values to the next.
!
!    The interpolated values are evenly spaced.
!
!    This routine is a generalization of R8VEC_EVEN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MAXVAL, the size of the XVAL array, as declared
!    by the user.  MAXVAL must be large enough to hold the NVAL values computed
!    by this routine.  In other words, MAXVAL must be at least equal to
!    NOLD + SUM (1 <= I <= NOLD-1) NFILL(I).
!
!    Input, integer ( kind = 4 ) NFILL(NOLD-1), the number of values
!    to be interpolated between XOLD(I) and XOLD(I+1).
!    NFILL(I) does not count the endpoints.  Thus, if
!    NFILL(I) is 1, there will be one new point generated
!    between XOLD(I) and XOLD(I+1).
!    NFILL(I) must be nonnegative.
!
!    Input, integer ( kind = 4 ) NOLD, the number of values XOLD,
!    between which extra values are to be interpolated.
!
!    Input, real ( kind = 8 ) XOLD(NOLD), the original vector of numbers
!    between which new values are to be interpolated.
!
!    Output, integer ( kind = 4 ) NVAL, the number of values computed
!    in the XVAL array.
!    NVAL = NOLD + SUM ( 1 <= I <= NOLD-1 ) NFILL(I)
!
!    Output, real ( kind = 8 ) XVAL(MAXVAL).  On output, XVAL contains the
!    NOLD values of XOLD, as well as the interpolated
!    values, making a total of NVAL values.
!
  implicit none

  integer ( kind = 4 ) maxval
  integer ( kind = 4 ) nold

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nadd
  integer ( kind = 4 ) nfill(nold-1)
  integer ( kind = 4 ) nval
  real ( kind = 8 ) xold(nold)
  real ( kind = 8 ) xval(maxval)

  nval = 1

  do i = 1, nold - 1

    if ( nfill(i) < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_EVEN2 - Fatal error!'
      write ( *, '(a,i8)' ) '  NFILL(I) is negative for I = ', i
      write ( *, '(a,i8)' ) '  NFILL(I) = ', nfill(i)
      stop
    end if

    if ( maxval < nval + nfill(i) + 1 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_EVEN2 - Fatal error!'
      write ( *, '(a)' ) '  MAXVAL is not large enough.  '
      write ( *, '(a,i8)' ) '  MAXVAL = ', maxval
      write ( *, '(a)' ) '  which is exceeded by storage requirements'
      write ( *, '(a,i8)' ) '  for interpolating in interval ', i
      stop
    end if

    nadd = nfill(i) + 2

    do j = 1, nadd
      xval(nval+j-1) = ( real ( nadd - j,     kind = 8 ) * xold(i)   &
                       + real (        j - 1, kind = 8 ) * xold(i+1) ) &
                       / real ( nadd     - 1, kind = 8 )
    end do

    nval = nval + nfill(i) + 1

  end do

  return
end



subroutine r8vec_even2_select ( n, xlo, xhi, ival, xval )

!*****************************************************************************80
!
!! R8VEC_EVEN2_SELECT returns the I-th of N evenly spaced midpoint values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This function returns the I-th of N evenly spaced midpoints of N
!    equal subintervals of [XLO,XHI].
!
!    XVAL = ( ( 2 * N - 2 * IVAL + 1 ) * XLO 
!           + (         2 * IVAL - 1 ) * XHI ) 
!           / ( 2 * N                )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of values.
!
!    Input, real ( kind = 8 ) XLO, XHI, the low and high values.
!
!    Input, integer ( kind = 4 ) IVAL, the index of the desired point.
!    IVAL is normally between 1 and N, but may be any integer value.
!
!    Output, real ( kind = 8 ) XVAL, the IVAL-th of N evenly spaced midpoints
!    between XLO and XHI.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ival
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xval

  xval = ( real ( 2 * n - 2 * ival + 1, kind = 8 ) * xlo   &
         + real (         2 * ival - 1, kind = 8 ) * xhi ) &
         / real ( 2 * n, kind = 8 )

  return
end



subroutine r8vec_even3 ( nold, nval, xold, xval )

!*****************************************************************************80
!
!! R8VEC_EVEN3 evenly interpolates new data into an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine accepts a short vector of numbers, and returns a longer
!    vector of numbers, created by interpolating new values between
!    the given values.
!
!    Between any two original values, new values are evenly interpolated.
!
!    Over the whole vector, the new numbers are interpolated in
!    such a way as to try to minimize the largest distance interval size.
!
!    The algorithm employed is not "perfect".
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
!    Input, integer ( kind = 4 ) NOLD, the number of values XOLD, between
!    which extra values are to be interpolated.
!
!    Input, integer ( kind = 4 ) NVAL, the number of values to be computed
!    in the XVAL array.  NVAL should be at least NOLD.
!
!    Input, real ( kind = 8 ) XOLD(NOLD), the original vector of numbers
!    between which new values are to be interpolated.
!
!    Output, real ( kind = 8 ) XVAL(NVAL).  On output, XVAL contains the
!    NOLD values of XOLD, as well as interpolated
!    values, making a total of NVAL values.
!
  implicit none

  integer ( kind = 4 ) nval
  integer ( kind = 4 ) nold

  real ( kind = 8 ) density
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nmaybe
  integer ( kind = 4 ) npts
  integer ( kind = 4 ) ntemp
  integer ( kind = 4 ) ntot
  real ( kind = 8 ) xlen
  real ( kind = 8 ) xleni
  real ( kind = 8 ) xlentot
  real ( kind = 8 ) xold(nold)
  real ( kind = 8 ) xval(nval)

  xlen = 0.0D+00
  do i = 1, nold - 1
    xlen = xlen + abs ( xold(i+1) - xold(i) )
  end do

  ntemp = nval - nold

  density = real ( ntemp, kind = 8 ) / xlen

  ival = 1
  ntot = 0
  xlentot = 0.0D+00

  do i = 1, nold - 1

    xleni = abs ( xold(i+1) - xold(i) )
    npts = int ( density * xleni )
    ntot = ntot + npts
!
!  Determine if we have enough left-over density that it should
!  be changed into a point.  A better algorithm would agonize
!  more over where that point should go.
!
    xlentot = xlentot + xleni
    nmaybe = nint ( xlentot * density )

    if ( ntot < nmaybe ) then
      npts = npts + nmaybe - ntot
      ntot = nmaybe
    end if

    do j = 1, npts + 2
      xval(ival+j-1) = ( real ( npts+2 - j,     kind = 8 ) * xold(i)   &
                       + real (          j - 1, kind = 8 ) * xold(i+1) ) &
                       / real ( npts+2     - 1, kind = 8 )
    end do

    ival = ival + npts + 1

  end do

  return
end



subroutine r8vec_expand_linear ( n, x, fat, xfat )

!*****************************************************************************80
!
!! R8VEC_EXPAND_LINEAR linearly interpolates new data into an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine copies the old data, and inserts NFAT new values
!    between each pair of old data values.  This would be one way to
!    determine places to evenly sample a curve, given the (unevenly
!    spaced) points at which it was interpolated.
!
!  Example:
!
!    N = 3
!    NFAT = 2
!
!    X(1:N)        = (/ 0.0,           6.0,             7.0 /)
!    XFAT(1:2*3+1) = (/ 0.0, 2.0, 4.0, 6.0, 6.33, 6.66, 7.0 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of input data values.
!
!    Input, real ( kind = 8 ) X(N), the original data.
!
!    Input, integer ( kind = 4 ) FAT, the number of data values to interpolate
!    between each pair of original data values.
!
!    Output, real ( kind = 8 ) XFAT((N-1)*(FAT+1)+1), the "fattened" data.
!
  implicit none

  integer ( kind = 4 ) fat
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xfat((n-1)*(fat+1)+1)

  k = 0

  do i = 1, n - 1

    k = k + 1
    xfat(k) = x(i)

    do j = 1, fat
      k = k + 1
      xfat(k) = ( real ( fat - j + 1, kind = 8 ) * x(i)     &
                + real (       j,     kind = 8 ) * x(i+1) ) &
                / real ( fat     + 1, kind = 8 )
    end do

  end do

  k = k + 1
  xfat(k) = x(n)

  return
end



subroutine r8vec_expand_linear2 ( n, x, before, fat, after, xfat )

!*****************************************************************************80
!
!! R8VEC_EXPAND_LINEAR2 linearly interpolates new data into an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine starts with a vector of data.
!
!    The intent is to "fatten" the data, that is, to insert more points
!    between successive values of the original data.
!
!    There will also be extra points placed BEFORE the first original
!    value and AFTER that last original value.
!
!    The "fattened" data is equally spaced between the original points.
!
!    The BEFORE data uses the spacing of the first original interval,
!    and the AFTER data uses the spacing of the last original interval.
!
!  Example:
!
!    N = 3
!    BEFORE = 3
!    FAT = 2
!    AFTER = 1
!
!    X    = (/                   0.0,           6.0,             7.0       /)
!    XFAT = (/ -6.0, -4.0, -2.0, 0.0, 2.0, 4.0, 6.0, 6.33, 6.66, 7.0, 7.66 /)
!            3 "BEFORE's"        Old  2 "FATS"  Old    2 "FATS"  Old  1 "AFTER"
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of input data values.
!    N must be at least 2.
!
!    Input, real ( kind = 8 ) X(N), the original data.
!
!    Input, integer ( kind = 4 ) BEFORE, the number of "before" values.
!
!    Input, integer ( kind = 4 ) FAT, the number of data values to interpolate
!    between each pair of original data values.
!
!    Input, integer ( kind = 4 ) AFTER, the number of "after" values.
!
!    Output, real ( kind = 8 ) XFAT(BEFORE+(N-1)*(FAT+1)+1+AFTER), the
!    "fattened" data.
!
  implicit none

  integer ( kind = 4 ) after
  integer ( kind = 4 ) before
  integer ( kind = 4 ) fat
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xfat(before+(n-1)*(fat+1)+1+after)

  k = 0
!
!  Points BEFORE.
!
  do j = 1 - before + fat, fat
    k = k + 1
    xfat(k) = ( real ( fat - j + 1, kind = 8 ) * ( x(1) - ( x(2) - x(1) ) ) &
              + real (       j,     kind = 8 ) *   x(1)          ) &
              / real ( fat     + 1, kind = 8 )
  end do
!
!  Original points and FAT points.
!
  do i = 1, n - 1

    k = k + 1
    xfat(k) = x(i)

    do j = 1, fat
      k = k + 1
      xfat(k) = ( real ( fat - j + 1, kind = 8 ) * x(i)     &
                + real (       j,     kind = 8 ) * x(i+1) ) &
                / real ( fat     + 1, kind = 8 )
    end do

  end do

  k = k + 1
  xfat(k) = x(n)
!
!  Points AFTER.
!
  do j = 1, after
    k = k + 1
    xfat(k) = ( real ( fat - j + 1, kind = 8 ) * x(n)     &
              + real (       j,     kind = 8 ) &
              * ( x(n) + ( x(n) - x(n-1) ) ) ) &
              / real ( fat     + 1, kind = 8 )
  end do

  return
end



subroutine r8vec_first_index ( n, a, tol, first_index )

!*****************************************************************************80
!
!! R8VEC_FIRST_INDEX indexes the first occurrence of values in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    For element A(I) of the vector, FIRST_INDEX(I) is the index in A of
!    the first occurrence of the value A(I).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Input, real ( kind = 8 ) TOL, a tolerance for equality.
!
!    Output, integer ( kind = 4 ) FIRST_INDEX(N), the first occurrence index.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) first_index(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) tol

  first_index(1:n) = -1

  do i = 1, n

    if ( first_index(i) == -1 ) then

      first_index(i) = i

      do j = i + 1, n
        if ( abs ( a(i) - a(j) ) <= tol ) then
          first_index(j) = i
        end if
      end do

    end if

  end do

  return
end



subroutine r8vec_floor ( n, r8vec, floorvec )

!*****************************************************************************80
!
!! R8VEC_FLOOR rounds "down" (towards -oo) entries of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Example:
!
!    R8    Value
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
!    20 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Input, real ( kind = 8 ) R8VEC(N), the values to be rounded down.
!
!    Output, integer ( kind = 4 ) FLOORVEC(N), the rounded value.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) floorvec(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec(n)
  integer ( kind = 4 ) value

  do i = 1, n

    value = int ( r8vec(i) )

    if ( r8vec(i) < real ( value, kind = 8 ) ) then
      value = value - 1
    end if

    floorvec(i) = value

  end do

  return
end



subroutine r8vec_frac ( n, a, k, frac )

!*****************************************************************************80
!
!! R8VEC_FRAC searches for the K-th smallest entry in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Hoare's algorithm is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, A is the array to search.
!    On output, the elements of A have been somewhat rearranged.
!
!    Input, integer ( kind = 4 ) K, the fractile to be sought.  If K = 1, the
!    minimum entry is sought.  If K = N, the maximum is sought.  Other values
!    of K search for the entry which is K-th in size.  K must be at
!    least 1, and no greater than N.
!
!    Output, real ( kind = 8 ) FRAC, the value of the K-th fractile of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) frac
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iryt
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) left
  real ( kind = 8 ) temp
  real ( kind = 8 ) x

  if ( n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_FRAC - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal nonpositive value of N = ', n
    stop
  end if

  if ( k <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_FRAC - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal nonpositive value of K = ', k
    stop
  end if

  if ( n < k ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_FRAC - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal N < K, K = ', k
    stop
  end if

  left = 1
  iryt = n

  do

    if ( iryt <= left ) then
      frac = a(k)
      exit
    end if

    x = a(k)
    i = left
    j = iryt

    do

      if ( j < i ) then
        if ( j < k ) then
          left = i
        end if
        if ( k < i ) then
          iryt = j
        end if
        exit
      end if
!
!  Find I so that X <= A(I).
!
      do while ( a(i) < x )
        i = i + 1
      end do
!
!  Find J so that A(J) <= X.
!
      do while ( x < a(j) )
        j = j - 1
      end do

      if ( i <= j ) then

        temp = a(i)
        a(i) = a(j)
        a(j) = temp

        i = i + 1
        j = j - 1
      end if

    end do

  end do

  return
end



subroutine r8vec_fraction ( n, x, fraction )

!*****************************************************************************80
!
!! R8VEC_FRACTION returns the fraction parts of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If we regard a real number as
!
!      R8 = SIGN * ( WHOLE + FRACTION )
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
!     R8    R8_FRACTION
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
!    18 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of arguments.
!
!    Input, real ( kind = 8 ) X(N), the arguments.
!
!    Output, real ( kind = 8 ) FRACTION(N), the fraction parts.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fraction(n)
  real ( kind = 8 ) x(n)

  fraction(1:n) = abs ( x(1:n) ) - real ( int ( abs ( x(1:n) ) ), kind = 8 )

  return
end
function r8vec_gt ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_GT == ( A1 > A2 ) for R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The comparison is lexicographic.
!
!    A1 > A2  <=>                              A1(1) > A2(1) or
!                 ( A1(1)     == A2(1)     and A1(2) > A2(2) ) or
!                 ...
!                 ( A1(1:N-1) == A2(1:N-1) and A1(N) > A2(N)
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
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), the vectors to be compared.
!
!    Output, logical R8VEC_GT, is TRUE if and only if A1 > A2.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  logical r8vec_gt

  r8vec_gt = .false.

  do i = 1, n

    if ( a2(i) < a1(i) ) then
      r8vec_gt = .true.
      exit
    else if ( a1(i) < a2(i) ) then
      r8vec_gt = .false.
      exit
    end if

  end do

  return
end



subroutine r8vec_heap_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_HEAP_A reorders an R8VEC into an ascending heap.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An ascending heap is an array A with the property that, for every index J,
!    A(J) <= A(2*J) and A(J) <= A(2*J+1), (as long as the indices
!    2*J and 2*J+1 are legal).
!
!                  A(1)
!                /      \
!            A(2)         A(3)
!          /     \        /  \
!      A(4)       A(5)  A(6) A(7)
!      /  \       /   \
!    A(8) A(9) A(10) A(11)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 July 2003
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
!    Input, integer ( kind = 4 ) N, the size of the input array.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, an unsorted array.
!    On output, the array has been reordered into a heap.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifree
  real ( kind = 8 ) key
  integer ( kind = 4 ) m
!
!  Only nodes N/2 down to 1 can be "parent" nodes.
!
  do i = n / 2, 1, -1
!
!  Copy the value out of the parent node.
!  Position IFREE is now "open".
!
    key = a(i)
    ifree = i

    do
!
!  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
!  IFREE.  (One or both may not exist because they exceed N.)
!
      m = 2 * ifree
!
!  Does the first position exist?
!
      if ( n < m ) then
        exit
      end if
!
!  Does the second position exist?
!
      if ( m + 1 <= n ) then
!
!  If both positions exist, take the smaller of the two values,
!  and update M if necessary.
!
        if ( a(m+1) < a(m) ) then
          m = m + 1
        end if

      end if
!
!  If the small descendant is smaller than KEY, move it up,
!  and update IFREE, the location of the free position, and
!  consider the descendants of THIS position.
!
      if ( key <= a(m) ) then
        exit
      end if

      a(ifree) = a(m)
      ifree = m

    end do
!
!  Once there is no more shifting to do, KEY moves into the free spot.
!
    a(ifree) = key

  end do

  return
end



subroutine r8vec_heap_d ( n, a )

!*****************************************************************************80
!
!! R8VEC_HEAP_D reorders an R8VEC into an descending heap.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    A descending heap is an array A with the property that, for every index J,
!    A(J) >= A(2*J) and A(J) >= A(2*J+1), (as long as the indices
!    2*J and 2*J+1 are legal).
!
!                  A(1)
!                /      \
!            A(2)         A(3)
!          /     \        /  \
!      A(4)       A(5)  A(6) A(7)
!      /  \       /   \
!    A(8) A(9) A(10) A(11)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 July 2003
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
!    Input, integer ( kind = 4 ) N, the size of the input array.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, an unsorted array.
!    On output, the array has been reordered into a heap.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifree
  real ( kind = 8 ) key
  integer ( kind = 4 ) m
!
!  Only nodes N/2 down to 1 can be "parent" nodes.
!
  do i = n / 2, 1, -1
!
!  Copy the value out of the parent node.
!  Position IFREE is now "open".
!
    key = a(i)
    ifree = i

    do
!
!  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
!  IFREE.  (One or both may not exist because they exceed N.)
!
      m = 2 * ifree
!
!  Does the first position exist?
!
      if ( n < m ) then
        exit
      end if
!
!  Does the second position exist?
!
      if ( m + 1 <= n ) then
!
!  If both positions exist, take the larger of the two values,
!  and update M if necessary.
!
        if ( a(m) < a(m+1) ) then
          m = m + 1
        end if

      end if
!
!  If the large descendant is larger than KEY, move it up,
!  and update IFREE, the location of the free position, and
!  consider the descendants of THIS position.
!
      if ( a(m) <= key ) then
        exit
      end if

      a(ifree) = a(m)
      ifree = m

    end do
!
!  Once there is no more shifting to do, KEY moves into the free spot IFREE.
!
    a(ifree) = key

  end do

  return
end



subroutine r8vec_heap_d_extract ( n, a, value )

!*****************************************************************************80
!
!! R8VEC_HEAP_D_EXTRACT: extract maximum from a heap descending sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In other words, the routine finds the maximum value in the
!    heap, returns that value to the user, deletes that value from
!    the heap, and restores the heap to its proper form.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N, the number of items in the heap.
!
!    Input/output, real ( kind = 8 ) A(N), the heap.
!
!    Output, real ( kind = 8 ) VALUE, the item of maximum value, which has
!    been removed from the heap.
!
  implicit none

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_HEAP_D_EXTRACT - Fatal error!'
    write ( *, '(a)' ) '  The heap is empty.'
    stop
  end if
!
!  Get the maximum value.
!
  value = a(1)

  if ( n == 1 ) then
    n = 0
    return
  end if
!
!  Shift the last value down.
!
  a(1) = a(n)
!
!  Restore the heap structure.
!
  n = n - 1
  call r8vec_sort_heap_d ( n, a )

  return
end



subroutine r8vec_heap_d_insert ( n, a, value )

!*****************************************************************************80
!
!! R8VEC_HEAP_D_INSERT inserts a value into a heap descending sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N, the number of items in the heap.
!
!    Input/output, real ( kind = 8 ) A(N), the heap.
!
!    Input, real ( kind = 8 ) VALUE, the value to be inserted.
!
  implicit none

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) parent
  real ( kind = 8 ) value

  n = n + 1
  i = n

  do while ( 1 < i )

    parent = i / 2

    if ( value <= a(parent) ) then
      exit
    end if

    a(i) = a(parent)
    i = parent

  end do

  a(i) = value

  return
end



subroutine r8vec_heap_d_max ( n, a, value )

!*****************************************************************************80
!
!! R8VEC_HEAP_D_MAX returns the maximum value in a heap descending sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items in the heap.
!
!    Input, real ( kind = 8 ) A(N), the heap.
!
!    Output, real ( kind = 8 ) VALUE, the maximum value in the heap.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) value

  value = a(1)

  return
end



subroutine r8vec_histogram ( n, a, a_lo, a_hi, histo_num, histo_gram )

!*****************************************************************************80
!
!! R8VEC_HISTOGRAM histograms an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Values between A_LO and A_HI will be histogrammed into the bins
!    1 through HISTO_NUM.  Values below A_LO are counted in bin 0,
!    and values greater than A_HI are counted in bin HISTO_NUM+1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input, real ( kind = 8 ) A(N), the array to examine.
!
!    Input, real ( kind = 8 ) A_LO, A_HI, the lowest and highest
!    values to be histogrammed.  These values will also define the bins.
!
!    Input, integer ( kind = 4 ) HISTO_NUM, the number of bins to use.
!
!    Output, integer ( kind = 4 ) HISTO_GRAM(0:HISTO_NUM+1), contains the
!    number of entries of A in each bin.
!
  implicit none

  integer ( kind = 4 ) histo_num
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_hi
  real ( kind = 8 ) a_lo
  real ( kind = 8 ) delta
  integer ( kind = 4 ) histo_gram(0:histo_num+1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  histo_gram(0:histo_num+1) = 0

  delta = ( a_hi - a_lo ) / real ( 2 * histo_num, kind = 8 )

  do i = 1, n

    if ( a(i) < a_lo ) then

      histo_gram(0) = histo_gram(0) + 1

    else if ( a(i) <= a_hi ) then

      j = nint ( &
        ( ( a_hi -           delta - a(i)        ) &
        * real ( 1,         kind = 8 )   &
        + (      -           delta + a(i) - a_lo ) &
        * real ( histo_num, kind = 8 ) ) &
        / ( a_hi - 2.0D+00 * delta        - a_lo ) )

      histo_gram(j) = histo_gram(j) + 1

    else if ( a_hi < a(i) ) then

      histo_gram(histo_num+1) = histo_gram(histo_num+1) + 1

    end if

  end do

  return
end



subroutine r8vec_house_column ( n, a, k, v )

!*****************************************************************************80
!
!! R8VEC_HOUSE_COLUMN defines a Householder premultiplier that "packs" a column.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine returns a vector V that defines a Householder
!    premultiplier matrix H(V) that zeros out the subdiagonal entries of
!    column K of the matrix A.
!
!       H(V) = I - 2 * v * v'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 June 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, real ( kind = 8 ) A(N), column K of the matrix A.
!
!    Input, integer ( kind = 4 ) K, the column of the matrix to be modified.
!
!    Output, real ( kind = 8 ) V(N), a vector of unit L2 norm which defines an
!    orthogonal Householder premultiplier matrix H with the property
!    that the K-th column of H*A is zero below the diagonal.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) k
  real ( kind = 8 ) s
  real ( kind = 8 ) v(n)

  v(1:n) = 0.0D+00

  if ( k < 1 .or. n <= k ) then
    return
  end if

  s = sqrt ( dot_product ( a(k:n), a(k:n) ) )

  if ( s == 0.0D+00 ) then
    return
  end if

  v(k) = a(k) + sign ( s, a(k) )
  v(k+1:n) = a(k+1:n)

  v(k:n) = v(k:n) / sqrt ( dot_product ( v(k:n), v(k:n) ) )

  return
end
function r8vec_i4vec_dot_product ( n, r8vec, i4vec )

!*****************************************************************************80
!
!! R8VEC_I4VEC_DOT_PRODUCT finds the dot product of an R8VEC and an I4VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input, real ( kind = 8 ) R8VEC(N), the first vector.
!
!    Input, integer ( kind = 4 ) I4VEC(N), the second vector.
!
!    Output, real ( kind = 8 ) R8VEC_I4VEC_DOT_PRODUCT, the dot product.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i4vec(n)
  real ( kind = 8 ) r8vec(n)
  real ( kind = 8 ) r8vec_i4vec_dot_product

  r8vec_i4vec_dot_product = dot_product ( r8vec(1:n), &
                                   real ( i4vec(1:n), kind = 8 ) )

  return
end
function r8vec_in_01 ( n, a )

!*****************************************************************************80
!
!! R8VEC_IN_01 is TRUE if the entries of an R8VEC are in the range [0,1].
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
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
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, logical R8VEC_IN_01, is TRUE if every entry of A is
!    between 0 and 1.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical r8vec_in_01

  if ( any ( a(1:n) < 0.0D+00 .or. 1.0D+00 < a(1:n) ) ) then
    r8vec_in_01 = .false.
  else
    r8vec_in_01 = .true.
  end if

  return
end
function r8vec_in_ab ( n, x, a, b )

!*****************************************************************************80
!
!! R8VEC_IN_AB is TRUE if the entries of an R8VEC are in the range [A,B].
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in X.
!
!    Input, real ( kind = 8 ) X(N), the vector.
!
!    Input, real A, B, the limits of the range.
!
!    Output, logical R8VEC_IN_AB, is TRUE if every entry of A is
!    between A and B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical r8vec_in_ab
  real ( kind = 8 ) x(n)

  if ( any ( x(1:n) < a .or. b < x(1:n) ) ) then
    r8vec_in_ab = .false.
  else
    r8vec_in_ab = .true.
  end if

  return
end



subroutine r8vec_index_delete_all ( n, x, indx, xval )

!*****************************************************************************80
!
!! R8VEC_INDEX_DELETE_ALL deletes a value from an indexed sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Note that the value of N is adjusted because of the deletions!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N, the size of the current list.
!
!    Input/output, real ( kind = 8 ) X(N), the list.
!
!    Input/output, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Input, real ( kind = 8 ) XVAL, the value to be sought.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) equal1
  integer ( kind = 4 ) equal2
  integer ( kind = 4 ) get
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(*)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  integer ( kind = 4 ) put
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) xval

  if ( n < 1 ) then
    n = 0
    return
  end if

  call r8vec_index_search ( n, x, indx, xval, less, equal, more )

  if ( equal == 0 ) then
    return
  end if

  equal1 = equal

  do

    if ( equal1 <= 1 ) then
      exit
    end if

    if ( x(indx(equal1-1)) /= xval ) then
      exit
    end if

    equal1 = equal1 - 1

  end do

  equal2 = equal

  do

    if ( n <= equal2 ) then
      exit
    end if

    if ( x(indx(equal2+1)) /= xval ) then
      exit
    end if

    equal2 = equal2 + 1

  end do
!
!  Discard certain X values.
!
  put = 0

  do get = 1, n

    if ( x(get) /= xval ) then
      put = put + 1
      x(put) = x(get)
    end if

  end do

  x(put+1:n) = 0.0D+00
!
!  Adjust the INDX values.
!
  do equal = equal1, equal2
    do i = 1, n
      if ( indx(equal) < indx(i) ) then
        indx(i) = indx(i) - 1
      end if
    end do
  end do
!
!  Discard certain INDX values.
!
  indx(equal1:n+equal1-equal2-1) = indx(equal2+1:n)
  indx(n+equal1-equal2:n) = 0
!
!  Adjust N.
!
  n = put

  return
end



subroutine r8vec_index_delete_dupes ( n, x, indx, n2, x2, indx2 )

!*****************************************************************************80
!
!! R8VEC_INDEX_DELETE_DUPES deletes duplicates from an indexed sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The output quantities N2, X2, and INDX2 are computed from the
!    input quantities by sorting, and eliminating duplicates.
!
!    The output arrays should be dimensioned of size N, unless the user
!    knows in advance what the value of N2 will be.
!
!    The output arrays may be identified with the input arrays.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the input list.
!
!    Input, real ( kind = 8 ) X(N), the list.
!
!    Input, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Output, integer ( kind = 4 ) N2, the number of unique entries in X.
!
!    Output, real ( kind = 8 ) X2(N2), a copy of the list which has
!    been sorted, and made unique.
!
!    Output, integer ( kind = 4 ) INDX2(N2), the sort index of the new list.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indx2(n)
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x2(n)
  real ( kind = 8 ) x3(n)

  i = 0
  n3 = 0

  do

    i = i + 1

    if ( n < i ) then
      exit
    end if

    if ( 1 < i ) then
      if ( x(indx(i)) == x3(n3) ) then
        cycle
      end if
    end if

    n3 = n3 + 1
    x3(n3) = x(indx(i))

  end do
!
!  Copy data into output arrays.
!
  n2 = n3
  x2(1:n2) = x3(1:n3)
  call i4vec_indicator ( n2, indx2 )

  return
end



subroutine r8vec_index_delete_one ( n, x, indx, xval, n2, x2, indx2 )

!*****************************************************************************80
!
!! R8VEC_INDEX_DELETE_ONE deletes one copy of a value from indexed sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If the value occurs in the list more than once, only one copy is deleted.
!
!    Note that the value of N is adjusted because of the deletions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the current list.
!
!    Input, real ( kind = 8 ) X(N), the list.
!
!    Input, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Input, real ( kind = 8 ) XVAL, the value to be sought.
!
!    Output, integer ( kind = 4 ) N2, the size of the current list.
!
!    Output, real ( kind = 8 ) X2(N2), the list.
!
!    Output, integer ( kind = 4 ) INDX2(N2), the sort index of the list.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indx2(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  integer ( kind = 4 ) n2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x2(n)
  real ( kind = 8 ) xval

  if ( n < 1 ) then
    n2 = 0
    return
  end if

  n2 = n
  indx2(1:n2) = indx(1:n2)
  x2(1:n2) = x(1:n2)

  call r8vec_index_search ( n2, x2, indx2, xval, less, equal, more )

  if ( equal /= 0 ) then
    j = indx2(equal)
    x2(j:n2-1) = x2(j+1:n2)
    indx2(equal:n2-1) = indx2(equal+1:n2)
    do i = 1, n2-1
      if ( j < indx2(i) ) then
        indx2(i) = indx2(i) - 1
      end if
    end do
    n2 = n2 - 1
  end if

  return
end



subroutine r8vec_index_insert ( n, x, indx, xval )

!*****************************************************************************80
!
!! R8VEC_INDEX_INSERT inserts a value in an indexed sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N, the size of the current list.
!
!    Input/output, real ( kind = 8 ) X(N), the list.
!
!    Input/output, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Input, real ( kind = 8 ) XVAL, the value to be sought.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) indx(*)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) xval

  if ( n <= 0 ) then
    n = 1
    x(1) = xval
    indx(1) = 1
    return
  end if

  call r8vec_index_search ( n, x, indx, xval, less, equal, more )

  x(n+1) = xval
  indx(n+1:more+1:-1) = indx(n:more:-1)
  indx(more) = n + 1
  n = n + 1

  return
end



subroutine r8vec_index_insert_unique ( n, x, indx, xval )

!*****************************************************************************80
!
!! R8VEC_INDEX_INSERT_UNIQUE inserts a unique value in an indexed sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If the value does not occur in the list, it is included in the list,
!    and N, X and INDX are updated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N, the size of the current list.
!
!    Input/output, real ( kind = 8 ) X(N), the list.
!
!    Input/output, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Input, real ( kind = 8 ) XVAL, the value to be sought.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) indx(*)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) xval

  if ( n <= 0 ) then
    n = 1
    x(1) = xval
    indx(1) = 1
    return
  end if
!
!  Does XVAL already occur in X?
!
  call r8vec_index_search ( n, x, indx, xval, less, equal, more )

  if ( equal == 0 ) then
    x(n+1) = xval
    indx(n+1:more+1:-1) = indx(n:more:-1)
    indx(more) = n + 1
    n = n + 1
  end if

  return
end



subroutine r8vec_index_order ( n, x, indx )

!*****************************************************************************80
!
!! R8VEC_INDEX_ORDER sorts an R8VEC using an index vector.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The index vector itself is not modified.  Therefore, the pair
!    (X,INDX) no longer represents an index sorted vector.  If this
!    relationship is to be preserved, then simply set INDX(1:N)=(1:N).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the current list.
!
!    Input/output, real ( kind = 8 ) X(N), the list.  On output, the list
!    has been sorted.
!
!    Input, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) indx(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  y(1:n) = x(indx(1:n))
  x(1:n) = y(1:n)

  return
end



subroutine r8vec_index_search ( n, x, indx, xval, less, equal, more )

!*****************************************************************************80
!
!! R8VEC_INDEX_SEARCH searches for a value in an indexed sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the current list.
!
!    Input, real ( kind = 8 ) X(N), the list.
!
!    Input, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Input, real ( kind = 8 ) XVAL, the value to be sought.
!
!    Output, integer ( kind = 4 ) LESS, EQUAL, MORE, the indexes in INDX of the
!    entries of X that are just less than, equal to, and just greater
!    than XVAL.  If XVAL does not occur in X, then EQUAL is zero.
!    If XVAL is the minimum entry of X, then LESS is 0.  If XVAL
!    is the greatest entry of X, then MORE is N+1.
!
  implicit none

  integer ( kind = 4 ) n

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

  if ( n <= 0 ) then
    less = 0
    equal = 0
    more = 0
    return
  end if

  lo = 1
  hi = n
  xlo = x(indx(lo))
  xhi = x(indx(hi))

  if ( xval < xlo ) then
    less = 0
    equal = 0
    more = 1
    return
  else if ( xval == xlo ) then
    less = 0
    equal = 1
    more = 2
    return
  end if

  if ( xhi < xval ) then
    less = n
    equal = 0
    more = n + 1
    return
  else if ( xval == xhi ) then
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

    if ( xval == xmid ) then
      equal = mid
      less = equal - 1
      more = equal + 1
      return
    else if ( xval < xmid ) then
      hi = mid
    else if ( xmid < xval ) then
      lo = mid
    end if

  end do

  return
end



subroutine r8vec_index_sort_unique ( n, x, indx, n2 )

!*****************************************************************************80
!
!! R8VEC_INDEX_SORT_UNIQUE creates a sorted unique index for an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the current list.
!
!    Input/output, real ( kind = 8 ) X(N), the list.  On output, X contains only
!    unique elements.
!
!    Output, integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    Output, integer ( kind = 4 ) N2, the number of unique elements in X.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) n2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  n2 = 0

  do i = 1, n
    call r8vec_index_insert_unique ( n2, y, indx, x(i) )
  end do

  x(1:n2) = y(1:n2)

  x(n2+1:n) = 0.0D+00
  indx(n2+1:n) = 0

  return
end



subroutine r8vec_index_sorted_range ( n, r, indx, r_lo, r_hi, i_lo, i_hi )

!*****************************************************************************80
!
!! R8VEC_INDEX_SORTED_RANGE: search index sorted vector for elements in a range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 September 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items in the vector.
!
!    Input, real ( kind = 8 ) R(N), the index sorted vector.
!
!    Input, integer ( kind = 4 ) INDX(N), the vector used to sort R.
!    The vector R(INDX(*)) is sorted.
!
!    Input, real ( kind = 8 ) R_LO, R_HI, the limits of the range.
!
!    Output, integer ( kind = 4 ) I_LO, I_HI, the range of indices
!    so that I_LO <= I <= I_HI => R_LO <= R(INDX(I)) <= R_HI.  If no
!    values in R lie in the range, then I_HI < I_LO will be returned.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i_lo
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) r_hi
  real ( kind = 8 ) r_lo
!
!  Cases we can handle immediately.
!
  if ( r(indx(n)) < r_lo ) then
    i_lo = n + 1
    i_hi = n
    return
  end if

  if ( r_hi < r(indx(1)) ) then
    i_lo = 1
    i_hi = 0
    return
  end if
!
!  Are there are least two intervals?
!
  if ( n == 1 ) then
    if ( r_lo <= r(indx(1)) .and. r(indx(1)) <= r_hi ) then
      i_lo = 1
      i_hi = 1
    else
      i_lo = 0
      i_hi = -1
    end if
    return
  end if
!
!  Bracket R_LO.
!
  if ( r_lo <= r(indx(1)) ) then

    i_lo = 1

  else
!
!  R_LO is in one of the intervals spanned by R(INDX(J1)) to R(INDX(J2)).
!  Examine the intermediate interval [R(INDX(I1)), R(INDX(I1+1))].
!  Does R_LO lie here, or below or above?
!
    j1 = 1
    j2 = n
    i1 = ( j1 + j2 - 1 ) / 2
    i2 = i1 + 1

    do

      if ( r_lo < r(indx(i1)) ) then
        j2 = i1
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else if ( r(indx(i2)) < r_lo ) then
        j1 = i2
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else
        i_lo = i1
        exit
      end if

    end do

  end if
!
!  Bracket R_HI.
!
  if ( r(indx(n)) <= r_hi ) then

    i_hi = n

  else

    j1 = i_lo
    j2 = n
    i1 = ( j1 + j2 - 1 ) / 2
    i2 = i1 + 1

    do

      if ( r_hi < r(indx(i1)) ) then
        j2 = i1
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else if ( r(indx(i2)) < r_hi ) then
        j1 = i2
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else
        i_hi = i2
        exit
      end if

    end do

  end if
!
!  We expect to have computed the largest I_LO and smallest I_HI such that
!    R(INDX(I_LO)) <= R_LO <= R_HI <= R(INDX(I_HI))
!  but what we want is actually
!    R_LO <= R(INDX(I_LO)) <= R(INDX(I_HI)) <= R_HI
!  which we can usually get simply by incrementing I_LO and decrementing I_HI.
!
  if ( r(indx(i_lo)) < r_lo ) then
    i_lo = i_lo + 1
    if ( n < i_lo ) then
      i_hi = i_lo - 1
    end if
  end if

  if ( r_hi < r(indx(i_hi)) ) then
    i_hi = i_hi - 1
    if ( i_hi < 1 ) then
      i_lo = i_hi + 1
    end if
  end if

  return
end



subroutine r8vec_indexed_heap_d ( n, a, indx )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D creates a descending heap from an indexed R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An indexed R8VEC is an R8VEC of data values, and an R8VEC of N indices,
!    each referencing an entry of the data vector.
!
!    The function adjusts the index vector INDX so that, for 1 <= J <= N/2,
!    we have:
!      A(INDX(2*J))   <= A(INDX(J))
!    and
!      A(INDX(2*J+1)) <= A(INDX(J))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
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
!    Input, integer ( kind = 4 ) N, the size of the index array.
!
!    Input, real ( kind = 8 ) A(*), the data vector.
!
!    Input/output, integer ( kind = 4 ) INDX(N), the index array.
!    Each entry of INDX must be a valid index for the array A.
!    On output, the indices have been reordered into a descending heap.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifree
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) key
  integer ( kind = 4 ) m
!
!  Only nodes N/2 down to 1 can be "parent" nodes.
!
  do i = n / 2, 1, -1
!
!  Copy the value out of the parent node.
!  Position IFREE is now "open".
!
    key = indx(i)
    ifree = i

    do
!
!  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
!  IFREE.  (One or both may not exist because they exceed N.)
!
      m = 2 * ifree
!
!  Does the first position exist?
!
      if ( n < m ) then
        exit
      end if
!
!  Does the second position exist?
!
      if ( m + 1 <= n ) then
!
!  If both positions exist, take the larger of the two values,
!  and update M if necessary.
!
        if ( a(indx(m)) < a(indx(m+1)) ) then
          m = m + 1
        end if

      end if
!
!  If the large descendant is larger than KEY, move it up,
!  and update IFREE, the location of the free position, and
!  consider the descendants of THIS position.
!
      if ( a(indx(m)) <= a(key) ) then
        exit
      end if

      indx(ifree) = indx(m)
      ifree = m

    end do
!
!  Once there is no more shifting to do, KEY moves into the free spot IFREE.
!
    indx(ifree) = key

  end do

  return
end



subroutine r8vec_indexed_heap_d_extract ( n, a, indx, indx_extract )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D_EXTRACT: extract from heap descending indexed R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An indexed R8VEC is an R8VEC of data values, and an R8VEC of N indices,
!    each referencing an entry of the data vector.
!
!    The routine finds the maximum value in the heap, returns that value to the
!    user, deletes that value from the heap, and restores the heap to its
!    proper form.
!
!    Note that the argument N must be a variable, which will be decremented
!    before return, and that INDX will hold one less value on output than it
!    held on input.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N, the number of items in the
!    index vector.
!
!    Input, real ( kind = 8 ) A(*), the data vector.
!
!    Input/output, integer ( kind = 4 ) INDX(N), the index vector.
!
!    Output, integer ( kind = 4 ) INDX_EXTRACT, the index in A of the item of
!    maximum value, which has now been removed from the heap.
!
  implicit none

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) indx(*)
  integer ( kind = 4 ) indx_extract
  integer ( kind = 4 ) n

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_INDEXED_HEAP_D_EXTRACT - Fatal error!'
    write ( *, '(a)' ) '  The heap is empty.'
    stop
  end if
!
!  Get the index of the maximum value.
!
  indx_extract = indx(1)

  if ( n == 1 ) then
    n = 0
    return
  end if
!
!  Shift the last index down.
!
  indx(1) = indx(n)
!
!  Restore the heap structure.
!
  n = n - 1
  call r8vec_indexed_heap_d ( n, a, indx )

  return
end



subroutine r8vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D_INSERT: insert value into heap descending indexed R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An indexed R8VEC is an R8VEC of data values, and an R8VEC of N indices,
!    each referencing an entry of the data vector.
!
!    Note that the argument N must be a variable, and will be incremented before
!    return, and that INDX must be able to hold one more entry on output than
!    it held on input.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N, the number of items in the
!    index vector.
!
!    Input, real ( kind = 8 ) A(*), the data vector.
!
!    Input/output, integer ( kind = 4 ) INDX(N), the index vector.
!
!    Input, integer ( kind = 4 ) INDX_INSERT, the index in A of the value
!    to be inserted into the heap.
!
  implicit none

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(*)
  integer ( kind = 4 ) indx_insert
  integer ( kind = 4 ) n
  integer ( kind = 4 ) parent

  n = n + 1
  i = n

  do while ( 1 < i )

    parent = i / 2

    if ( a(indx_insert) <= a(indx(parent)) ) then
      exit
    end if

    indx(i) = indx(parent)
    i = parent

  end do

  indx(i) = indx_insert

  return
end



subroutine r8vec_indexed_heap_d_max ( n, a, indx, indx_max )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D_MAX: maximum value in heap descending indexed R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An indexed R8VEC is an R8VEC of data values, and an R8VEC of N indices,
!    each referencing an entry of the data vector.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items in the index vector.
!
!    Input, real ( kind = 8 ) A(*), the data vector.
!
!    Input, integer ( kind = 4 ) INDX(N), the index vector.
!
!    Output, integer ( kind = 4 ) INDX_MAX, the index in A of the maximum value
!    in the heap.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indx_max

  indx_max = indx(1)

  return
end



subroutine r8vec_indicator ( n, a )

!*****************************************************************************80
!
!! R8VEC_INDICATOR sets an R8VEC to the indicator vector.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 September 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Output, real ( kind = 8 ) A(N), the array to be initialized.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = real ( i, kind = 8 )
  end do

  return
end



subroutine r8vec_insert ( n, a, pos, value )

!*****************************************************************************80
!
!! R8VEC_INSERT inserts a value into an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the array on input.
!
!    Input/output, real ( kind = 8 ) A(N+1), the array.  On input, A is
!    assumed to contain only N entries, while on output, A actually
!    contains N+1 entries.
!
!    Input, integer ( kind = 4 ) POS, the position to be assigned the new entry.
!    1 <= POS <= N+1.
!
!    Input, real ( kind = 8 ) VALUE, the value to be inserted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n+1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) pos
  real ( kind = 8 ) value

  if ( pos < 1 .or. n + 1 < pos ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_INSERT - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal insertion position = ', pos
    stop

  else

    do i = n + 1, pos + 1, -1
      a(i) = a(i-1)
    end do

    a(pos) = value

  end if

  return
end
function r8vec_insignificant ( n, r, s )

!*****************************************************************************80
!
!! R8VEC_INSIGNIFICANT determines if an R8VEC is insignificant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input, real ( kind = 8 ) R(N), the vector to be compared against.
!
!    Input, real ( kind = 8 ) S(N), the vector to be compared.
!
!    Output, logical R8VEC_INSIGNIFICANT, is TRUE if S is insignificant
!    compared to R.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) r(n)
  logical r8vec_insignificant
  real ( kind = 8 ) s(n)
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  logical value

  value = .true.

  do i = 1, n

    t = r(i) + s(i)
    tol = epsilon ( r(i) ) * abs ( r(i) )

    if ( tol < abs ( r(i) - t ) ) then 
      value = .false.
      exit
    end if

  end do
  
  r8vec_insignificant = value

  return
end
function r8vec_is_int ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_INT is TRUE if the entries of an R8VEC are integers.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
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
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, logical R8VEC_IS_INT, is TRUE if every entry of A is
!    integral.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical r8vec_is_int

  r8vec_is_int = all ( a(1:n) == aint ( a(1:n) ) )

  return
end
function r8vec_is_nonnegative ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_NONNEGATIVE is TRUE if all the entries of an R8VEC are nonnegative.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, logical R8VEC_IS_NONNEGATIVE, the value of the condition.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical r8vec_is_nonnegative

  r8vec_is_nonnegative = all ( 0.0D+00 <= a(1:n) )

  return
end
function r8vec_is_zero ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_ZERO is TRUE if all the entries of an R8VEC are zero.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, logical R8VEC_IS_ZERO, the value of the condition.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical r8vec_is_zero

  r8vec_is_zero = all ( a(1:n) == 0.0D+00 )

  return
end



subroutine r8vec_legendre ( n, x_first, x_last, x )

!*****************************************************************************80
!
!! R8VEC_LEGENDRE creates a vector of Legendre-spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
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
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X_FIRST, X_LAST, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of Legendre-spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  call legendre_zeros ( n, x )

  x(1:n) = ( ( 1.0D+00 - x(1:n) ) * x_first  &
           + ( 1.0D+00 + x(1:n) ) * x_last ) &
           /   2.0D+00

  return
end



subroutine r8vec_linspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
!
!    In other words, the interval is divided into N-1 even subintervals,
!    and the endpoints of intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A_FIRST, A_LAST, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n
      x(i) = ( real ( n - i,     kind = 8 ) * a   &
             + real (     i - 1, kind = 8 ) * b ) &
             / real ( n     - 1, kind = 8 )
    end do

  end if

  return
end



subroutine r8vec_linspace2 ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE2 creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 2, 4, 6, 8, 10.
!
!    In other words, the interval is divided into N+1 even subintervals,
!    and the endpoints of internal intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n
    x(i) = ( real ( n  - i + 1, kind = 8 ) * a &
           + real (      i,     kind = 8 ) * b ) &
           / real ( n      + 1, kind = 8 )
  end do

  return
end
function r8vec_lt ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_LT == ( A1 < A2 ) for R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The comparison is lexicographic.
!
!    A1 < A2  <=>                              A1(1) < A2(1) or
!                 ( A1(1)     == A2(1)     and A1(2) < A2(2) ) or
!                 ...
!                 ( A1(1:N-1) == A2(1:N-1) and A1(N) < A2(N)
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
!    Input, integer ( kind = 4 ) N, the dimension of the vectors.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), the vectors to be compared.
!
!    Output, logical R8VEC_LT, is TRUE if and only if A1 < A2.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  logical r8vec_lt
  integer ( kind = 4 ) i

  r8vec_lt = .false.

  do i = 1, n

    if ( a1(i) < a2(i) ) then
      r8vec_lt = .true.
      exit
    else if ( a2(i) < a1(i) ) then
      r8vec_lt = .false.
      exit
    end if

  end do

  return
end



subroutine r8vec_mask_print ( n, a, mask_num, mask, title )

!*****************************************************************************80
!
!! R8VEC_MASK_PRINT prints a masked R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 September 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MASK_NUM, the number of masked elements.
!
!    Input, integer ( kind = 4 ) MASK(MASK_NUM), the indices of the vector
!    to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) mask_num
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) mask(mask_num)
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Masked vector printout:'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, mask_num
    write ( *, '(2x,i8,a,1x,i8,2x,g14.6)' ) i, ':', mask(i), a(mask(i))
  end do

  return
end



subroutine r8vec_max ( n, a, amax )

!*****************************************************************************80
!
!! R8VEC_MAX returns the maximum value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) AMAX, the value of the largest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax

  amax = maxval ( a(1:n) )

  return
end



subroutine r8vec_max_abs_index ( n, a, max_index )

!*****************************************************************************80
!
!! R8VEC_MAX_ABS_INDEX: index of the maximum absolute value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, integer ( kind = 4 ) MAX_INDEX, the index of the largest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_index

  if ( n <= 0 ) then

    max_index = -1

  else

    max_index = 1

    do i = 2, n
      if ( abs ( a(max_index) ) < abs ( a(i) ) ) then
        max_index = i
      end if
    end do

  end if

  return
end



subroutine r8vec_max_index ( n, a, max_index )

!*****************************************************************************80
!
!! R8VEC_MAX_INDEX returns the index of the maximum value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, integer ( kind = 4 ) MAX_INDEX, the index of the largest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_index

  if ( n <= 0 ) then

    max_index = -1

  else

    max_index = 1

    do i = 2, n
      if ( a(max_index) < a(i) ) then
        max_index = i
      end if
    end do

  end if

  return
end



subroutine r8vec_mean ( n, a, mean )

!*****************************************************************************80
!
!! R8VEC_MEAN returns the mean of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector whose mean is desired.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean

  mean = sum ( a(1:n) ) / real ( n, kind = 8 )

  return
end



subroutine r8vec_median ( n, a, median )

!*****************************************************************************80
!
!! R8VEC_MEDIAN returns the median of an unsorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Hoare's algorithm is used.  The values of the vector are
!    rearranged by this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2000
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input/output, real ( kind = 8 ) A(N), the array to search.  On output,
!    the order of the elements of A has been somewhat changed.
!
!    Output, real ( kind = 8 ) MEDIAN, the value of the median of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) k
  real ( kind = 8 ) median

  k = ( n + 1 ) / 2

  call r8vec_frac ( n, a, k, median )

  return
end



subroutine r8vec_midspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_MIDSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This function divides the interval [a,b] into n subintervals, and then
!    returns the midpoints of those subintervals.
!
!  Example:
!
!    N = 5, A = 10, B = 20
!    X = [ 11, 13, 15, 17, 19 ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 June 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the endpoints of the interval.
!
!    Output, real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n
    x(i) = ( real ( 2 * n - 2 * i + 1, kind = 8 ) * a &
           + real (         2 * i - 1, kind = 8 ) * b ) &
           / real ( 2 * n,             kind = 8 )
  end do

  return
end



subroutine r8vec_min ( n, a, amin )

!*****************************************************************************80
!
!! R8VEC_MIN returns the minimum value of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) AMIN, the value of the smallest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amin

  amin = minval ( a(1:n) )

  return
end



subroutine r8vec_min_index ( n, a, min_index )

!*****************************************************************************80
!
!! R8VEC_MIN_INDEX returns the index of the minimum value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, integer ( kind = 4 ) MIN_INDEX, the index of the smallest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) min_index

  if ( n <= 0 ) then

    min_index = -1

  else

    min_index = 1

    do i = 2, n
      if ( a(i) < a(min_index) ) then
        min_index = i
      end if
    end do

  end if

  return
end
function r8vec_min_pos ( n, a )

!*****************************************************************************80
!
!! R8VEC_MIN_POS returns the minimum positive value of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 November 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) R8VEC_MIN_POS, the smallest positive entry,
!    or R8_HUGE if no entry is positive.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_huge = 1.0D+30
  real ( kind = 8 ) r8vec_min_pos
  real ( kind = 8 ) value

  value = r8_huge

  do i = 1, n
    if ( 0.0D+00 < a(i) ) then
      value = min ( value, a(i) )
    end if
  end do

  r8vec_min_pos = value

  return
end



subroutine r8vec_mirror_next ( n, a, done )

!*****************************************************************************80
!
!! R8VEC_MIRROR_NEXT steps through all sign variations of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In normal use, the user would set every element of A to be positive.
!    The routine will take the input value of A, and output a copy in
!    which the signs of one or more entries have been changed.  Repeatedly
!    calling the routine with the output from the previous call will generate
!    every distinct "variation" of A; that is, all possible sign variations.
!
!    When the output variable DONE is TRUE (or equal to 1), then the
!    output value of A_NEW is the last in the series.
!
!    Note that A may have some zero values.  The routine will essentially
!    ignore such entries; more exactly, it will not stupidly assume that -0
!    is a proper "variation" of 0!
!
!    Also, it is possible to call this routine with the signs of A set
!    in any way you like.  The routine will operate properly, but it
!    will nonethess terminate when it reaches the value of A in which
!    every nonzero entry has negative sign.
!
!    More efficient algorithms using the Gray code seem to require internal
!    memory in the routine, which is not one of MATLAB's strong points,
!    or the passing back and forth of a "memory array", or the use of
!    global variables, or unnatural demands on the user.  This form of
!    the routine is about as clean as I can make it.
!
!  Example:
!
!      Input         Output
!    ---------    --------------
!    A            A_NEW     DONE
!    ---------    --------  ----
!     1  2  3     -1  2  3  false
!    -1  2  3      1 -2  3  false
!     1 -2  3     -1 -2  3  false
!    -1 -2  3      1  2 -3  false
!     1  2 -3     -1  2 -3  false
!    -1  2 -3      1 -2 -3  false
!     1 -2 -3     -1 -2 -3  false
!    -1 -2 -3      1  2  3  true
!
!     1  0  3     -1  0  3  false
!    -1  0  3      1  0 -3  false
!     1  0 -3     -1  0 -3  false
!    -1  0 -3      1  0  3  true
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2005
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
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, real ( kind = 8 ) A(N), a vector of real numbers.
!    On output, the signs of some entries have been changed.
!
!    Output, logical DONE, is TRUE if the input vector A was the last element
!    in the series (every entry was nonpositive); the output vector is reset
!    so that all entries are nonnegative, but presumably the ride is over!
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical done
  integer ( kind = 4 ) i
  integer ( kind = 4 ) positive
!
!  Seek the first strictly positive entry of A.
!
  positive = 0
  do i = 1, n
    if ( 0.0D+00 < a(i) ) then
      positive = i
      exit
    end if
  end do
!
!  If there is no strictly positive entry of A, there is no successor.
!
  if ( positive == 0 ) then
    a(1:n) = - a(1:n)
    done = .true.
    return
  end if
!
!  Otherwise, negate A up to the positive entry.
!
  a(1:positive) = - a(1:positive)
  done = .false.

  return
end
function r8vec_negative_strict ( n, a )

!*****************************************************************************80
!
!! R8VEC_NEGATIVE_STRICT: every element of an R8VEC is strictly negative.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A(N).
!
!    Output, logical R8VEC_NEGATIVE_STRICT, is TRUE every entry of the
!    vector is strictly negative.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical r8vec_negative_strict

  r8vec_negative_strict = ( all ( a(1:n) < 0.0D+00 ) )

  return
end



subroutine r8vec_nint ( n, a )

!*****************************************************************************80
!
!! R8VEC_NINT rounds entries of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, real ( kind = 8 ) A(N), the vector to be NINT'ed.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)

  a(1:n) = nint ( real ( a(1:n), kind = 8 ) )

  return
end
function r8vec_norm ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM returns the L2 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector whose L2 norm is desired.
!
!    Output, real ( kind = 8 ) R8VEC_NORM, the L2 norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm

  r8vec_norm = sqrt ( sum ( a(1:n)**2 ) )

  return
end
function r8vec_norm_affine ( n, v0, v1 )

!*****************************************************************************80
!
!! R8VEC_NORM_AFFINE returns the affine norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The affine vector L2 norm is defined as:
!
!      R8VEC_NORM_AFFINE(V0,V1)
!        = sqrt ( sum ( 1 <= I <= N ) ( V1(I) - V0(I) )^2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the vectors.
!
!    Input, real ( kind = 8 ) V0(N), the base vector.
!
!    Input, real ( kind = 8 ) V1(N), the vector whose affine norm is desired.
!
!    Output, real ( kind = 8 ) R8VEC_NORM_AFFINE, the L2 norm of V1-V0.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8vec_norm_affine
  real ( kind = 8 ) v0(n)
  real ( kind = 8 ) v1(n)

  r8vec_norm_affine = sqrt ( sum ( ( v0(1:n) - v1(1:n) )**2 ) )

  return
end
function r8vec_norm_l0 ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_L0 returns the l0 "norm" of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The l0 "norm" simply counts the number of nonzero entries in the vector.
!    It is not a true norm, but has some similarities to one.  It is useful
!    in the study of compressive sensing.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 June 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, integer ( kind = 4 ) R8VEC_NORM_L0, the value of the norm.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) r8vec_norm_l0
  integer ( kind = 4 ) value

  value = 0
  do i = 1, n
    if ( a(i) /= 0.0D+00 ) then
      value = value + 1
    end if
  end do

  r8vec_norm_l0 = value

  return
end
function r8vec_norm_l1 ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_L1 returns the L1 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L1 norm is defined as:
!
!      R8VEC_NORM_L1 = sum ( 1 <= I <= N ) abs ( A(I) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector whose L1 norm is desired.
!
!    Output, real ( kind = 8 ) R8VEC_NORM_L1, the L1 norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm_l1

  r8vec_norm_l1 = sum ( abs ( a(1:n) ) )

  return
end
function r8vec_norm_l2 ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_L2 returns the L2 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector whose L2 norm is desired.
!
!    Output, real ( kind = 8 ) R8VEC_NORM_L2, the L2 norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm_l2

  r8vec_norm_l2 = sqrt ( sum ( a(1:n)**2 ) )

  return
end
function r8vec_norm_li ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_LI returns the L-oo norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L-oo norm is defined as:
!
!      R8VEC_NORM_LI = max ( 1 <= I <= N ) abs ( A(I) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector whose L-oo norm is desired.
!
!    Output, real ( kind = 8 ) R8VEC_NORM_LI, the L-oo norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm_li

  r8vec_norm_li = maxval ( abs ( a(1:n) ) )

  return
end
function r8vec_norm_lp ( n, a, p )

!*****************************************************************************80
!
!! R8VEC_NORM_LP returns the LP norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector LP norm is defined as:
!
!      R8VEC_NORM_LP = ( sum ( 1 <= I <= N ) ( abs ( A(I) ) )^P )^(1/P).
!
!    Usually, the LP norms with
!      1 <= P <= oo
!    are of interest.  This routine allows
!      0 < P <= Huge ( P ).
!    If P = Huge ( P ), then the L-oo norm is returned, which is
!    simply the maximum of the absolute values of the vector components.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector whose LP norm is desired.
!
!    Input, real ( kind = 8 ) P, the index of the norm.
!
!    Output, real ( kind = 8 ) R8VEC_NORM_LP, the LP norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) p
  real ( kind = 8 ) r8vec_norm_lp

  if ( p <= 0.0D+00 ) then
    r8vec_norm_lp = -1.0D+00
  else if ( p == huge ( p ) ) then
    r8vec_norm_lp = maxval ( abs ( a(1:n) ) )
  else if ( p == 1.0D+00 ) then
    r8vec_norm_lp = sum ( abs ( a(1:n) ) )
  else if ( p == 2.0D+00 ) then
    r8vec_norm_lp = sqrt ( sum ( a(1:n)**2 ) )
  else
    r8vec_norm_lp = ( sum ( ( abs ( a(1:n) ) )**p ) )**( 1.0D+00 / p )
  end if

  return
end
function r8vec_norm_squared ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_SQUARED returns the square of the L2 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    R8VEC_NORM_SQUARED = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, real ( kind = 8 ) R8VEC_NORM_SQUARED, the squared L2 norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm_squared

  r8vec_norm_squared = sum ( a(1:n)**2 )

  return
end



subroutine r8vec_normal_01 ( n, seed, x )

!*****************************************************************************80
!
!! R8VEC_NORMAL_01 returns a unit pseudonormal R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    This routine can generate a vector of values on one call.  It
!    has the feature that it should provide the same results
!    in the same order no matter how we break up the task.
!
!    Before calling this routine, the user may call RANDOM_SEED
!    in order to set the seed of the random number generator.
!
!    The Box-Muller method is used, which is efficient, but
!    generates an even number of values each time.  On any call
!    to this routine, an even number of new values are generated.
!    Depending on the situation, one value may be left over.
!    In that case, it is saved for the next call.
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
!    Input, integer ( kind = 4 ) N, the number of values desired.  If N is
!    negative,then the code will flush its internal memory; in particular,
!    if there is a saved value to be used on the next call, it is
!    instead discarded.  This is useful if the user has reset the
!    random number seed, for instance.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), a sample of the standard normal PDF.
!
!  Local parameters:
!
!    Local, integer ( kind = 4 ) MADE, records the number of values that have
!    been computed.  On input with negative N, this value overwrites
!    the return value of N, so the user can get an accounting of
!    how much work has been done.
!
!    Local, real ( kind = 8 ) R(N+1), is used to store some uniform
!    random values.  Its dimension is N+1, but really it is only needed
!    to be the smallest even number greater than or equal to N.
!
!    Local, integer SAVED, is 0 or 1 depending on whether there is a
!    single saved value left over from the previous call.
!
!    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
!    X that we need to compute.  This starts off as 1:N, but is adjusted
!    if we have a saved value that can be immediately stored in X(1),
!    and so on.
!
!    Local, real ( kind = 8 ) Y, the value saved from the previous call, if
!    SAVED is 1.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) m
  integer ( kind = 4 ), save :: made = 0
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) r(n+1)
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ), save :: saved = 0
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  integer ( kind = 4 ) x_hi_index
  integer ( kind = 4 ) x_lo_index
  real ( kind = 8 ), save :: y = 0.0D+00
!
!  I'd like to allow the user to reset the internal data.
!  But this won't work properly if we have a saved value Y.
!  I'm making a crock option that allows the user to signal
!  explicitly that any internal memory should be flushed,
!  by passing in a negative value for N.
!
  if ( n < 0 ) then
    n = made
    made = 0
    saved = 0
    y = 0.0D+00
    return
  else if ( n == 0 ) then
    return
  end if
!
!  Record the range of X we need to fill in.
!
  x_lo_index = 1
  x_hi_index = n
!
!  Use up the old value, if we have it.
!
  if ( saved == 1 ) then
    x(1) = y
    saved = 0
    x_lo_index = 2
  end if
!
!  Maybe we don't need any more values.
!
  if ( x_hi_index - x_lo_index + 1 == 0 ) then
!
!  If we need just one new value, do that here to avoid null arrays.
!
  else if ( x_hi_index - x_lo_index + 1 == 1 ) then

    r(1) = r8_uniform_01 ( seed )

    if ( r(1) == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORMAL_01 - Fatal error!'
      write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
      stop
    end if

    r(2) = r8_uniform_01 ( seed )

    x(x_hi_index) = &
             sqrt ( -2.0D+00 * log ( r(1) ) ) * cos ( 2.0D+00 * pi * r(2) )
    y =      sqrt ( -2.0D+00 * log ( r(1) ) ) * sin ( 2.0D+00 * pi * r(2) )

    saved = 1

    made = made + 2
!
!  If we require an even number of values, that's easy.
!
  else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) == 0 ) then

    m = ( x_hi_index - x_lo_index + 1 ) / 2

    call r8vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( -2.0D+00 * log ( r(1:2*m-1:2) ) ) &
      * cos ( 2.0D+00 * pi * r(2:2*m:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( -2.0D+00 * log ( r(1:2*m-1:2) ) ) &
      * sin ( 2.0D+00 * pi * r(2:2*m:2) )

    made = made + x_hi_index - x_lo_index + 1
!
!  If we require an odd number of values, we generate an even number,
!  and handle the last pair specially, storing one in X(N), and
!  saving the other for later.
!
  else

    x_hi_index = x_hi_index - 1

    m = ( x_hi_index - x_lo_index + 1 ) / 2 + 1

    call r8vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( -2.0D+00 * log ( r(1:2*m-3:2) ) ) &
      * cos ( 2.0D+00 * pi * r(2:2*m-2:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( -2.0D+00 * log ( r(1:2*m-3:2) ) ) &
      * sin ( 2.0D+00 * pi * r(2:2*m-2:2) )

    x(n) = sqrt ( -2.0D+00 * log ( r(2*m-1) ) ) &
      * cos ( 2.0D+00 * pi * r(2*m) )

    y = sqrt ( -2.0D+00 * log ( r(2*m-1) ) ) &
      * sin ( 2.0D+00 * pi * r(2*m) )

    saved = 1

    made = made + x_hi_index - x_lo_index + 2

  end if

  return
end



subroutine r8vec_normalize ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORMALIZE normalizes an R8VEC in the Euclidean norm.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The euclidean norm is also sometimes called the l2 or
!    least squares norm.
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
!    Input, integer ( kind = 4 ) N, the dimension of the vector.
!
!    Input/output, real ( kind = 8 ) A(N), the vector to be normalized.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) norm

  norm = sqrt ( sum ( a(1:n)**2 ) )

  if ( norm /= 0.0D+00 ) then
    a(1:n) = a(1:n) / norm
  end if

  return
end



subroutine r8vec_normalize_l1 ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORMALIZE_L1 normalizes an R8VEC to have unit sum.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, real ( kind = 8 ) A(N), the vector to be normalized.
!    On output, the entries of A should have unit sum.  However, if
!    the input vector has zero sum, the routine halts.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_sum

  a_sum = sum ( a(1:n) )

  if ( a_sum == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_NORMALIZE_L1 - Fatal error!'
    write ( *, '(a)' ) '  The vector entries sum to 0.'
    stop
  end if

  a(1:n) = a(1:n) / a_sum

  return
end
function r8vec_normsq ( n, v )

!*****************************************************************************80
!
!! R8VEC_NORMSQ returns the square of the L2 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The square of the vector L2 norm is defined as:
!
!      R8VEC_NORMSQ = sum ( 1 <= I <= N ) V(I)^2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the vector dimension.
!
!    Input, real ( kind = 8 ) V(N), the vector.
!
!    Output, real ( kind = 8 ) R8VEC_NORMSQ, the squared L2 norm.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8vec_normsq
  real ( kind = 8 ) v(n)

  r8vec_normsq = sum ( v(1:n)**2 )

  return
end
function r8vec_normsq_affine ( n, v0, v1 )

!*****************************************************************************80
!
!! R8VEC_NORMSQ_AFFINE returns the affine squared norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The affine squared vector L2 norm is defined as:
!
!      R8VEC_NORMSQ_AFFINE(V0,V1)
!        = sum ( 1 <= I <= N ) ( V1(I) - V0(I) )^2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the vector dimension.
!
!    Input, real ( kind = 8 ) V0(N), the base vector.
!
!    Input, real ( kind = 8 ) V1(N), the vector.
!
!    Output, real ( kind = 8 ) R8VEC_NORMSQ_AFFINE, the squared affine L2 norm.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8vec_normsq_affine
  real ( kind = 8 ) v0(n)
  real ( kind = 8 ) v1(n)

  r8vec_normsq_affine = sum ( ( v0(1:n) - v1(1:n) )**2 )

  return
end



subroutine r8vec_order_type ( n, a, order )

!*****************************************************************************80
!
!! R8VEC_ORDER_TYPE determines if R8VEC is (non)strictly ascending/descending.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the array.
!
!    Input, real ( kind = 8 ) A(N), the array to be checked.
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

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
!
!  Search for the first value not equal to A(1).
!
  i = 1

  do

    i = i + 1

    if ( n < i ) then
      order = 0
      return
    end if

    if ( a(1) < a(i) ) then

      if ( i == 2 ) then
        order = 2
      else
        order = 1
      end if

      exit

    else if ( a(i) < a(1) ) then

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
  do while ( i < n )

    i = i + 1

    if ( order == 1 ) then

      if ( a(i) < a(i-1) ) then
        order = -1
        exit
      end if

    else if ( order == 2 ) then

      if ( a(i) < a(i-1) ) then
        order = -1
        exit
      else if ( a(i) == a(i-1) ) then
        order = 1
      end if

    else if ( order == 3 ) then

      if ( a(i-1) < a(i) ) then
        order = -1
        exit
      end if

    else if ( order == 4 ) then

      if ( a(i-1) < a(i) ) then
        order = -1
        exit
      else if ( a(i) == a(i-1) ) then
        order = 3
      end if

    end if

  end do

  return
end



subroutine r8vec_part_quick_a ( n, a, l, r )

!*****************************************************************************80
!
!! R8VEC_PART_QUICK_A reorders an R8VEC as part of a quick sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine reorders the entries of A.  Using A(1) as the key,
!    all entries of A that are less than or equal to the key will
!    precede the key which precedes all entries that are greater than the key.
!
!  Example:
!
!    Input:
!
!      N = 8
!
!      A = ( 6, 7, 3, 1, 6, 8, 2, 9 )
!
!    Output:
!
!      L = 3, R = 6
!
!      A = ( 3, 1, 2, 6, 6, 8, 9, 7 )
!            -------        -------
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
!    Input/output, real ( kind = 8 ) A(N).  On input, the array to be checked.
!    On output, A has been reordered as described above.
!
!    Output, integer ( kind = 4 ) L, R, the indices of A that define
!    the three segments.  Let KEY = the input value of A(1).  Then
!    I <= L                 A(I) < KEY;
!         L < I < R         A(I) = KEY;
!                 R <= I    KEY < A(I).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) key
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) r
  real ( kind = 8 ) temp

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_PART_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    stop
  else if ( n == 1 ) then
    l = 0
    r = 2
    return
  end if

  key = a(1)
  m = 1
!
!  The elements of unknown size have indices between L+1 and R-1.
!
  l = 1
  r = n + 1

  do i = 2, n

    if ( key < a(l+1) ) then
      r = r - 1
      temp = a(r)
      a(r) = a(l+1)
      a(l+1) = temp
    else if ( a(l+1) == key ) then
      m = m + 1
      temp = a(m)
      a(m) = a(l+1)
      a(l+1) = temp
      l = l + 1
    else if ( a(l+1) < key ) then
      l = l + 1
    end if

  end do
!
!  Now shift small elements to the left, and KEY elements to center.
!
  do i = 1, l - m
    a(i) = a(i+m)
  end do
!
!  Out of bounds here, occasionally
!
  l = l - m

  a(l+1:l+m) = key

  return
end



subroutine r8vec_permute ( n, p, a )

!*****************************************************************************80
!
!! R8VEC_PERMUTE permutes an R8VEC in place.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine permutes an array of real "objects", but the same
!    logic can be used to permute an array of objects of any arithmetic
!    type, or an array of objects of any complexity.  The only temporary
!    storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!    P(I) = J means that the I-th element of the output array should be
!    the J-th element of the input array.  P must be a legal permutation
!    of the integers from 1 to N, otherwise the algorithm will
!    fail catastrophically.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,   4,   5,   1,   3 )
!      A = ( 1.0, 2.0, 3.0, 4.0, 5.0 )
!
!    Output:
!
!      A    = ( 2.0, 4.0, 5.0, 1.0, 3.0 ).
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
!    Input, integer ( kind = 4 ) N, the number of objects.
!
!    Input, integer ( kind = 4 ) P(N), the permutation.
!
!    Input/output, real ( kind = 8 ) A(N), the array to be permuted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_temp
  integer ( kind = 4 ), parameter :: base = 1
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) p(n)

  call perm_check ( n, p, base, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_PERMUTE - Fatal error!'
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
          write ( *, '(a)' ) 'R8VEC_PERMUTE - Fatal error!'
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



subroutine r8vec_permute_cyclic ( n, k, a )

!*****************************************************************************80
!
!! R8VEC_PERMUTE_CYCLIC performs a cyclic permutation of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    For 0 <= K < N, this function cyclically permutes the input vector
!    to have the form
!
!     ( A(K+1), A(K+2), ..., A(N), A(1), ..., A(K) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects.
!
!    Input, integer ( kind = 4 ) K, the increment used.
!
!    Input/output, real ( kind = 8 ) A(N), the array to be permuted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_wrap
  integer ( kind = 4 ) ipk
  integer ( kind = 4 ) k

  do i = 1, n
    ipk = i4_wrap ( i + k, 1, n )
    b(i) = a(ipk)
  end do

  a(1:n) = b(1:n)

  return
end



subroutine r8vec_permute_uniform ( n, a, seed )

!*****************************************************************************80
!
!! R8VEC_PERMUTE_UNIFORM randomly permutes an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects.
!
!    Input/output, real ( kind = 8 ) A(N), the array to be permuted.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ), parameter :: base = 1
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seed

  call perm_uniform ( n, base, seed, p )

  call r8vec_permute ( n, p, a )

  return
end



subroutine r8vec_polarize ( n, a, p, a_normal, a_parallel )

!*****************************************************************************80
!
!! R8VEC_POLARIZE decomposes an R8VEC into normal and parallel components.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The (nonzero) vector P defines a direction.
!
!    The vector A can be written as the sum
!
!      A = A_normal + A_parallel
!
!    where A_parallel is a linear multiple of P, and A_normal
!    is perpendicular to P.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the vector to be polarized.
!
!    Input, real ( kind = 8 ) P(N), the polarizing direction.
!
!    Output, real ( kind = 8 ) A_NORMAL(N), A_PARALLEL(N), the normal
!    and parallel components of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_dot_p
  real ( kind = 8 ) a_normal(n)
  real ( kind = 8 ) a_parallel(n)
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) p_norm

  p_norm = sqrt ( sum ( p(1:n)**2 ) )

  if ( p_norm == 0.0D+00 ) then
    a_normal(1:n) = a(1:n)
    a_parallel(1:n) = 0.0D+00
    return
  end if

  a_dot_p = dot_product ( a(1:n), p(1:n) ) / p_norm

  a_parallel(1:n) = a_dot_p * p(1:n) / p_norm

  a_normal(1:n) = a(1:n) - a_parallel(1:n)

  return
end
function r8vec_positive_strict ( n, a )

!*****************************************************************************80
!
!! R8VEC_POSITIVE_STRICT: every element of an R8VEC is strictly positive.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A(N).
!
!    Output, logical R8VEC_POSITIVE_STRICT, is TRUE every entry of the
!    vector is strictly positive.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical r8vec_positive_strict

  r8vec_positive_strict = ( all ( 0.0D+00 < a(1:n) ) )

  return
end



subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

  return
end



subroutine r8vec_print_part ( n, a, max_print, title )

!*****************************************************************************80
!
!! R8VEC_PRINT_PART prints "part" of an R8VEC.
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
!    19 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
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
      write ( *, '(2x,i8,a,1x,g14.6)' ) i, ':', a(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6)' ) i, ':', a(i)
    end do
    write ( *, '(a)' ) '  ........  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6)' ) i, ':', a(i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6)' ) i, ':', a(i)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,a)' ) i, ':', a(i), '...more entries...'

  end if

  return
end



subroutine r8vec_print_some ( n, a, i_lo, i_hi, title )

!*****************************************************************************80
!
!! R8VEC_PRINT_SOME prints "some" of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
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
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) I_LO, I_HI, the first and last indices
!    to print.  The routine expects 1 <= I_LO <= I_HI <= N.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i_lo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  write ( *, '(a)' ) ' '
  do i = max ( i_lo, 1 ), min ( i_hi, n )
    write ( *, '(2x,i8,a,1x,g14.8)' ) i, ':', a(i)
  end do

  return
end



subroutine r8vec_print2 ( n, a )

!*****************************************************************************80
!
!! R8VEC_PRINT2 prints out an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of A.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amin
  integer ( kind = 4 ) i
  character ( len = 11 ) iform
  logical integ
  integer ( kind = 4 ) lmax
  real ( kind = 8 ) r8_log_10
!
!  Check if all entries are integral.
!
  integ = .true.

  do i = 1, n

    if ( a(i) /= real ( int ( a(i) ), kind = 8 ) ) then
      integ = .false.
      exit
    end if

  end do
!
!  Find the range of the array.
!
  amax = maxval ( abs ( a(1:n) ) )
  amin = minval ( abs ( a(1:n) ) )
!
!  Use the information about the maximum size of an entry to
!  compute an intelligent format for use with integer entries.
!
!  Later, we might also do this for real vectors.
!
  lmax = int ( r8_log_10 ( amax ) )

  if ( integ ) then
    write ( iform, '( ''(2x,i'', i2, '')'' )' ) lmax + 3
  else
    iform = ' '
  end if

  do i = 1, n

    if ( integ ) then
      write ( *, iform ) int ( a(i) )
    else
      write ( *, '(2x,g14.6)' ) a(i)
    end if

  end do

  return
end
function r8vec_product ( n, a )

!*****************************************************************************80
!
!! R8VEC_PRODUCT returns the product of the entries of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In FORTRAN90, this facility is offered by the built in
!    PRODUCT function:
!
!      R8VEC_PRODUCT ( N, A ) = PRODUCT ( A(1:N) )
!
!    In MATLAB, this facility is offered by the built in
!    PROD function:
!
!      R8VEC_PRODUCT ( N, A ) = PROD ( A(1:N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) R8VEC_PRODUCT, the product of the entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_product

  r8vec_product = product ( a(1:n) )

  return
end



subroutine r8vec_range ( n, x, xmin, xmax, y, ymin, ymax )

!*****************************************************************************80
!
!! R8VEC_RANGE finds the range of Y's within a restricted X range.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine is given a set of pairs of points (X,Y), and a range
!    XMIN to XMAX of valid X values.  Over this range, it seeks
!    YMIN and YMAX, the minimum and maximum values of Y for
!    valid X's.
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
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) X(N), the X array.
!
!    Input, real ( kind = 8 ) XMIN, XMAX, the range of X values to check.
!
!    Input, real ( kind = 8 ) Y(N), the Y array.
!
!    Output, real ( kind = 8 ) YMIN, YMAX, the range of Y values whose
!    X value is within the X range.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) ymax
  real ( kind = 8 ) ymin

  ymin =   huge ( ymin )
  ymax = - huge ( ymax )

  do i = 1, n

    if ( xmin <= x(i) .and. x(i) <= xmax ) then

      ymin = min ( ymin, y(i) )
      ymax = max ( ymax, y(i) )

    end if

  end do

  return
end



subroutine r8vec_range_2 ( n, a, amin, amax )

!*****************************************************************************80
!
!! R8VEC_RANGE_2 updates a range to include a new array.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Given a range AMIN to AMAX, and an array A, the routine will
!    decrease AMIN if necessary, or increase AMAX if necessary, so that
!    every entry of A is between AMIN and AMAX.
!
!    However, AMIN will not be increased, nor AMAX decreased.
!
!    This routine may be used to compute the maximum and minimum of a
!    collection of arrays one at a time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Input/output, real ( kind = 8 ) AMIN, AMAX.  On input, the
!    current legal range of values for A.  On output, AMIN and AMAX
!    are either unchanged, or else "widened" so that all entries
!    of A are within the range.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amin

  amax = max ( amax, maxval ( a(1:n) ) )
  amin = min ( amin, minval ( a(1:n) ) )

  return
end



subroutine r8vec_reverse ( n, a )

!*****************************************************************************80
!
!! R8VEC_REVERSE reverses the elements of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In FORTRAN90, calling R8VEC_REVERSE is equivalent to
!
!      A(1:N) = A(N:1:-1)
!
!  Example:
!
!    Input:
!
!      N = 5,
!      A = ( 11.0, 12.0, 13.0, 14.0, 15.0 ).
!
!    Output:
!
!      A = ( 15.0, 14.0, 13.0, 12.0, 11.0 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, real ( kind = 8 ) A(N), the array to be reversed.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)

  a(1:n) = a(n:1:-1)

  return
end
function r8vec_rms ( n, a )

!*****************************************************************************80
!
!! R8VEC_RMS returns the RMS norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector RMS norm is defined as:
!
!      R8VEC_RMS = sqrt ( sum ( 1 <= I <= N ) A(I)^2 / N ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, real ( kind = 8 ) R8VEC_RMS, the RMS norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_rms

  r8vec_rms = sqrt ( sum ( a(1:n)**2 ) / n )

  return
end



subroutine r8vec_rotate ( n, a, m )

!*****************************************************************************80
!
!! R8VEC_ROTATE "rotates" the entries of an R8VEC in place.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine rotates an array of real "objects", but the same
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
!      N = 5, M = 2
!      A    = ( 1.0, 2.0, 3.0, 4.0, 5.0 )
!
!    Output:
!
!      A    = ( 4.0, 5.0, 1.0, 2.0, 3.0 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects.
!
!    Input, integer ( kind = 4 ) M, the number of positions to the right that
!    each element should be moved.  Elements that shift pass position
!    N "wrap around" to the beginning of the array.
!
!    Input/output, real ( kind = 8 ) A(N), the array to be rotated.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mcopy
  integer ( kind = 4 ) nset
  real ( kind = 8 ) temp
!
!  Force M to be positive, between 0 and N-1.
!
  mcopy = i4_modp ( m, n )

  if ( mcopy == 0 ) then
    return
  end if

  istart = 0
  nset = 0

  do

    istart = istart + 1

    if ( n < istart ) then
      exit
    end if

    temp = a(istart)
    iget = istart
!
!  Copy the new value into the vacated entry.
!
    do

      iput = iget

      iget = iget - mcopy
      if ( iget < 1 ) then
        iget = iget + n
      end if

      if ( iget == istart ) then
        exit
      end if

      a(iput) = a(iget)
      nset = nset + 1

    end do

    a(iput) = temp
    nset = nset + 1

    if ( n <= nset ) then
      exit
    end if

  end do

  return
end
function r8vec_scalar_triple_product ( v1, v2, v3 )

!*****************************************************************************80
!
!! R8VEC_SCALAR_TRIPLE_PRODUCT computes the scalar triple product.
!
!  Discussion:
!
!    STRIPLE = V1 dot ( V2 x V3 ).
!
!    STRIPLE is the volume of the parallelogram whose sides are
!    formed by V1, V2 and V3.
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
!    Input, real ( kind = 8 ) V1(3), V2(3), V3(3), the three vectors.
!
!    Output, real ( kind = 8 ) R8VEC_SCALAR_TRIPLE_PRODUCT, the scalar
!    triple product.
!
  implicit none

  real ( kind = 8 ) r8vec_scalar_triple_product
  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)

  r8vec_scalar_triple_product = &
      v1(1) * ( v2(2) * v3(3) - v2(3) * v3(2) ) &
    + v1(2) * ( v2(3) * v3(1) - v2(1) * v3(3) ) &
    + v1(3) * ( v2(1) * v3(2) - v2(2) * v3(1) )

  return
end



subroutine r8vec_search_binary_a ( n, a, aval, indx )

!*****************************************************************************80
!
!! R8VEC_SEARCH_BINARY_A searches an ascending sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Binary search is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Algorithm 1.9,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 26.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements in the array.
!
!    Input, real ( kind = 8 ) A(N), the array to be searched.  The array must
!    be sorted in ascending order.
!
!    Input, real ( kind = 8 ) AVAL, the value to be searched for.
!
!    Output, integer ( kind = 4 ) INDX, the result of the search.
!    -1, AVAL does not occur in the array.
!    I, A(I) = AVAL.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
  integer ( kind = 4 ) high
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) low
  integer ( kind = 4 ) mid

  indx = -1

  low = 1
  high = n

  do while ( low <= high )

    mid = ( low + high ) / 2

    if ( a(mid) == aval ) then
      indx = mid
      exit
    else if ( a(mid) < aval ) then
      low = mid + 1
    else if ( aval < a(mid) ) then
      high = mid - 1
    end if

  end do

  return
end



subroutine r8vec_shift ( shift, n, x )

!*****************************************************************************80
!
!! R8VEC_SHIFT performs a shift on an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) SHIFT, the amount by which each entry is to
!    be shifted.
!
!    Input, integer ( kind = 4 ) N, the length of the vector.
!
!    Input/output, real ( kind = 8 ) X(N), the vector to be shifted.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) shift
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  y(1:n) = x(1:n)

  x(1:n) = 0.0D+00

  ilo = max ( 1, 1 + shift )
  ihi = min ( n, n + shift )

  x(ilo:ihi) = y(ilo-shift:ihi-shift)

  return
end



subroutine r8vec_shift_circular ( shift, n, x )

!*****************************************************************************80
!
!! R8VEC_SHIFT_CIRCULAR performs a circular shift on an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) SHIFT, the amount by which each entry is to
!    be shifted.
!
!    Input, integer ( kind = 4 ) N, the length of the vector.
!
!    Input/output, real ( kind = 8 ) X(N), the vector to be shifted.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_wrap
  integer ( kind = 4 ) j
  integer ( kind = 4 ) shift
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  y(1:n) = x(1:n)

  do i = 1, n
    j = i4_wrap ( i - shift, 1, n )
    x(i) = y(j)
  end do

  return
end



subroutine r8vec_sort_bubble_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_BUBBLE_A ascending sorts an R8VEC using bubble sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Bubble sort is simple to program, but inefficient.  It should not
!    be used for large arrays.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, an unsorted array.
!    On output, the array has been sorted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) t

  do i = 1, n - 1
    do j = i + 1, n
      if ( a(j) < a(i) ) then
        t    = a(i)
        a(i) = a(j)
        a(j) = t
      end if
    end do
  end do

  return
end



subroutine r8vec_sort_bubble_d ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_BUBBLE_D descending sorts an R8VEC using bubble sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Bubble sort is simple to program, but inefficient.  It should not
!    be used for large arrays.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, an unsorted array.
!    On output, the array has been sorted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) t

  do i = 1, n - 1
    do j = i + 1, n
      if ( a(i) < a(j) ) then
        t    = a(i)
        a(i) = a(j)
        a(j) = t
      end if
    end do
  end do

  return
end



subroutine r8vec_sort_heap_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_A ascending sorts an R8VEC using heap sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 July 2003
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
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, the array to be sorted;
!    On output, the array has been sorted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) n1
  real ( kind = 8 ) temp

  if ( n <= 1 ) then
    return
  end if
!
!  1: Put A into descending heap form.
!
  call r8vec_heap_d ( n, a )
!
!  2: Sort A.
!
!  The largest object in the heap is in A(1).
!  Move it to position A(N).
!
  temp = a(1)
  a(1) = a(n)
  a(n) = temp
!
!  Consider the diminished heap of size N1.
!
  do n1 = n - 1, 2, -1
!
!  Restore the heap structure of A(1) through A(N1).
!
    call r8vec_heap_d ( n1, a )
!
!  Take the largest object from A(1) and move it to A(N1).
!
    temp = a(1)
    a(1) = a(n1)
    a(n1) = temp

  end do

  return
end



subroutine r8vec_sort_heap_d ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_D descending sorts an R8VEC using heap sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 July 2003
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
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, the array to be sorted;
!    On output, the array has been sorted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) n1

  if ( n <= 1 ) then
    return
  end if
!
!  1: Put A into ascending heap form.
!
  call r8vec_heap_a ( n, a )
!
!  2: Sort A.
!
!  The smallest object in the heap is in A(1).
!  Move it to position A(N).
!
  call r8_swap ( a(1), a(n) )
!
!  Consider the diminished heap of size N1.
!
  do n1 = n - 1, 2, -1
!
!  Restore the heap structure of A(1) through A(N1).
!
    call r8vec_heap_a ( n1, a )
!
!  Take the smallest object from A(1) and move it to A(N1).
!
    call r8_swap ( a(1), a(n1) )

  end do

  return
end



subroutine r8vec_sort_heap_index_a ( n, a, indx )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      A(INDX(I:N)) is sorted,
!
!    or explicitly, by the call
!
!      call r8vec_permute ( n, indx, a )
!
!    after which A(1:N) is sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), an array to be index-sorted.
!
!    Output, integer ( kind = 4 ) INDX(N), the sort index.  The
!    I-th element of the sorted array is A(INDX(I)).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
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
      aval = a(indxt)

    else

      indxt = indx(ir)
      aval = a(indxt)
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
        if ( a(indx(j)) < a(indx(j+1)) ) then
          j = j + 1
        end if
      end if

      if ( aval < a(indx(j)) ) then
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



subroutine r8vec_sort_heap_index_d ( n, a, indx )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_INDEX_D does an indexed heap descending sort of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      A(INDX(1:N)) is sorted,
!
!    or explicitly, by the call
!
!      call r8vec_permute ( n, indx, a )
!
!    after which A(1:N) is sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), an array to be index-sorted.
!
!    Output, integer ( kind = 4 ) INDX(N), the sort index.  The
!    I-th element of the sorted array is A(INDX(I)).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
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
      aval = a(indxt)

    else

      indxt = indx(ir)
      aval = a(indxt)
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
        if ( a(indx(j+1)) < a(indx(j)) ) then
          j = j + 1
        end if
      end if

      if ( a(indx(j)) < aval ) then
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



subroutine r8vec_sort_heap_mask_a ( n, a, mask_num, mask, indx )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_MASK_A: indexed heap ascending sort of a masked R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An array A is given.  An array MASK of indices into A is given.
!    The routine produces a vector INDX, which is a permutation of the
!    entries of MASK, so that:
!
!      A(MASK(INDX(I)) <= A(MASK(INDX(J))
!
!    whenever
!
!      I <= J
!
!    In other words, only the elements of A that are indexed by MASK
!    are to be considered, and the only thing that happens is that
!    a rearrangment of the indices in MASK is returned that orders the
!    masked elements.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), an array to be index-sorted.
!
!    Input, integer ( kind = 4 ) MASK_NUM, the number of mask elements.
!
!    Input, integer ( kind = 4 ) MASK(MASK_NUM), the mask array.  This is
!    simply a list of indices of A.  The entries of MASK should
!    be unique, and each one should be between 1 and N.
!
!    Output, integer ( kind = 4 ) INDX(MASK_NUM), the sort index.  There are
!    MASK_NUM elements of A selected by MASK.  If we want to list those
!    elements in order, then the I-th element is A(MASK(INDX(I))).
!
  implicit none

  integer ( kind = 4 ) mask_num
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(mask_num)
  integer ( kind = 4 ) indxt
  integer ( kind = 4 ) ir
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) mask(mask_num)

  if ( n < 1 ) then
    return
  end if

  if ( mask_num < 1 ) then
    return
  end if

  if ( mask_num == 1 ) then
    indx(1) = 1
    return
  end if

  call i4vec_indicator ( mask_num, indx )

  l = mask_num / 2 + 1
  ir = mask_num

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      aval = a(mask(indxt))

    else

      indxt = indx(ir)
      aval = a(mask(indxt))
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
        if ( a(mask(indx(j))) < a(mask(indx(j+1))) ) then
          j = j + 1
        end if
      end if

      if ( aval < a(mask(indx(j))) ) then
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



subroutine r8vec_sort_insert_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_INSERT_A ascending sorts an R8VEC using an insertion sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Algorithm 1.1,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 11.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items in the vector.
!    N must be positive.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, the array to be sorted;
!    On output, the array has been sorted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x

  do i = 2, n

    x = a(i)

    j = i - 1

    do while ( 1 <= j )

      if ( a(j) <= x ) then
        exit
      end if

      a(j+1) = a(j)
      j = j - 1

    end do

    a(j+1) = x

  end do

  return
end



subroutine r8vec_sort_insert_index_a ( n, a, indx )

!*****************************************************************************80
!
!! R8VEC_SORT_INSERT_INDEX_A ascending index sorts an R8VEC using insertion.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
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
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Algorithm 1.1,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 11.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items in the vector.
!    N must be positive.
!
!    Input, real ( kind = 8 ) A(N), the array to be sorted.
!
!    Output, integer ( kind = 4 ) INDX(N), the sorted indices.  The array
!    is sorted when listed from A(INDX(1)) through A(INDX(N)).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) x

  if ( n < 1 ) then
    return
  end if

  do i = 1, n
    indx(i) = i
  end do

  do i = 2, n

    x = a(i)

    j = i - 1

    do while ( 1 <= j )

      if ( a(indx(j)) <= x ) then
        exit
      end if

      indx(j+1) = indx(j)
      j = j - 1

    end do

    indx(j+1) = i

  end do

  return
end



subroutine r8vec_sort_insert_index_d ( n, a, indx )

!*****************************************************************************80
!
!! R8VEC_SORT_INSERT_INDEX_D descending index sorts an R8VEC using insertion.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Algorithm 1.1,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 11.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items in the vector.
!    N must be positive.
!
!    Input, real ( kind = 8 ) A(N), the array to be sorted.
!
!    Output, integer ( kind = 4 ) INDX(N), the sorted indices.  The array
!    is sorted when listed from A(INDX(1)) through A(INDX(N)).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) x

  if ( n < 1 ) then
    return
  end if

  do i = 1, n
    indx(i) = i
  end do

  do i = 2, n

    x = a(i)

    j = i - 1

    do while ( 1 <= j )

      if ( x <= a(indx(j)) ) then
        exit
      end if

      indx(j+1) = indx(j)
      j = j - 1

    end do

    indx(j+1) = i

  end do

  return
end



subroutine r8vec_sort_quick_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_QUICK_A ascending sorts an R8VEC using quick sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Example:
!
!    Input:
!
!      N = 7
!      A = ( 6, 7, 3, 2, 9, 1, 8 )
!
!    Output:
!
!      A = ( 1, 2, 3, 6, 7, 8, 9 )
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
!    Input/output, real ( kind = 8 ) A(N).
!    On input, the array to be sorted.
!    On output, the array has been sorted.
!
  implicit none

  integer ( kind = 4 ), parameter :: level_max = 30
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) base
  integer ( kind = 4 ) l_segment
  integer ( kind = 4 ) level
  integer ( kind = 4 ) n_segment
  integer ( kind = 4 ) rsave(level_max)
  integer ( kind = 4 ) r_segment

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_SORT_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    stop
  end if

  if ( n == 1 ) then
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
    call r8vec_part_quick_a ( n_segment, a(base), l_segment, r_segment )
!
!  If the left segment has more than one element, we need to partition it.
!
    if ( 1 < l_segment ) then

      if ( level_max < level ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_SORT_QUICK_A - Fatal error!'
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



subroutine r8vec_sort_shell_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_SHELL_A ascending sorts an R8VEC using Shell's sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, an array to be sorted.
!    On output, the sorted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) asave
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifree
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) ipow
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) maxpow

  if ( n <= 1 ) then
    return
  end if
!
!  Determine the smallest MAXPOW so that
!    N <= ( 3**MAXPOW - 1 ) / 2
!
  maxpow = 1

  do while ( 3**maxpow < 2 * n + 1 )
    maxpow = maxpow + 1
  end do

  if ( 1 < maxpow ) then
    maxpow = maxpow - 1
  end if
!
!  Now sort groups of size ( 3^IPOW - 1 ) / 2.
!
  do ipow = maxpow, 1, -1

    inc = ( 3**ipow - 1 ) / 2
!
!  Sort the values with indices equal to K mod INC.
!
    do k = 1, inc
!
!  Insertion sort of the items with index
!  INC+K, 2*INC+K, 3*INC+K, ...
!
      do i = inc+k, n, inc

        asave = a(i)
        ifree = i
        j = i - inc

        do

          if ( j < 1 ) then
            exit
          end if

          if ( a(j) <= asave ) then
            exit
          end if

          ifree = j
          a(j+inc) = a(j)
          j = j - inc

        end do

        a(ifree) = asave

      end do

    end do

  end do

  return
end



subroutine r8vec_sort2_a ( n, x, y )

!*****************************************************************************80
!
!! R8VEC_SORT2_A ascending sorts an R8VEC and adjusts an associated R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine sorts the elements of X, and whenever
!    an element of X is moved, the corresponding element of
!    Y is moved in the same way.  This action means that after
!    the sorting, every element of X is still paired to the
!    same Y value.
!
!    If you have more than one array associated with X, or
!    an integer array, or some other complication, you may want to
!    look at doing an "indexed sort" instead.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, length of input array.
!
!    Input/output, real ( kind = 8 ) X(N).  On input, an unsorted array.
!    On output, X has been sorted.
!
!    Input/output, real ( kind = 8 ) Y(N), an array which is to be
!    shifted corresponding to the shifts made in X.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  if ( n <= 1 ) then
    return
  end if

  i = 0
  indx = 0
  isgn = 0
  j = 0

  do

    call sort_heap_external ( n, indx, i, j, isgn )

    if ( 0 < indx ) then

      call r8_swap ( x(i), x(j) )
      call r8_swap ( y(i), y(j) )

    else if ( indx < 0 ) then

      if ( x(i) <= x(j) ) then
        isgn = -1
      else
        isgn = + 1
      end if

    else if ( indx == 0 ) then

      exit

    end if

  end do

  return
end



subroutine r8vec_sorted_merge_a ( na, a, nb, b, nc, c )

!*****************************************************************************80
!
!! R8VEC_SORTED_MERGE_A merges two ascending sorted R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The elements of A and B should be sorted in ascending order.
!
!    The elements in the output array C will also be in ascending order,
!    and unique.
!
!    The output vector C may share storage with A or B.
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
!    Input, integer ( kind = 4 ) NA, the dimension of A.
!
!    Input, real ( kind = 8 ) A(NA), the first sorted array.
!
!    Input, integer ( kind = 4 ) NB, the dimension of B.
!
!    Input, real ( kind = 8 ) B(NB), the second sorted array.
!
!    Output, integer ( kind = 4 ) NC, the number of elements in the output
!    array.  Note that C should usually be dimensioned at least NA+NB in the
!    calling routine.
!
!    Output, real ( kind = 8 ) C(NC), the merged unique sorted array.
!
  implicit none

  integer ( kind = 4 ) na
  integer ( kind = 4 ) nb

  real ( kind = 8 ) a(na)
  real ( kind = 8 ) b(nb)
  real ( kind = 8 ) c(na+nb)
  real ( kind = 8 ) d(na+nb)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) ja
  integer ( kind = 4 ) jb
  integer ( kind = 4 ) na2
  integer ( kind = 4 ) nb2
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) order

  na2 = na
  nb2 = nb

  ja = 0
  jb = 0
  nc = 0

  call r8vec_order_type ( na2, a, order )

  if ( order < 0 .or. 2 < order ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_SORTED_MERGE_A - Fatal error!'
    write ( *, '(a)' ) '  The input array A is not ascending sorted!'
    stop
  end if

  call r8vec_order_type ( nb2, b, order )

  if ( order < 0 .or. 2 < order ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_SORTED_MERGE_A - Fatal error!'
    write ( *, '(a)' ) '  The input array B is not ascending sorted!'
    stop
  end if

  do
!
!  If we've used up all the entries of A, stick the rest of B on the end.
!
    if ( na2 <= ja ) then

      do j = 1, nb2 - jb
        jb = jb + 1
        if ( nc == 0 ) then
          nc = nc + 1
          d(nc) = b(jb)
        else if ( d(nc) < b(jb) ) then
          nc = nc + 1
          d(nc) = b(jb)
        end if
      end do

      c(1:nc) = d(1:nc)

      exit
!
!  If we've used up all the entries of B, stick the rest of A on the end.
!
    else if ( nb2 <= jb ) then

      do j = 1, na2 - ja
        ja = ja + 1
        if ( nc == 0 ) then
          nc = nc + 1
          d(nc) = a(ja)
        else if ( d(nc) < a(ja) ) then
          nc = nc + 1
          d(nc) = a(ja)
        end if
      end do

      c(1:nc) = d(1:nc)

      exit
!
!  Otherwise, if the next entry of A is smaller, that's our candidate.
!
    else if ( a(ja+1) <= b(jb+1) ) then

      ja = ja + 1
      if ( nc == 0 ) then
        nc = nc + 1
        d(nc) = a(ja)
      else if ( d(nc) < a(ja) ) then
        nc = nc + 1
        d(nc) = a(ja)
      end if
!
!  ...or if the next entry of B is the smaller, consider that.
!
    else

      jb = jb + 1
      if ( nc == 0 ) then
        nc = nc + 1
        d(nc) = b(jb)
      else if ( d(nc) < b(jb) ) then
        nc = nc + 1
        d(nc) = b(jb)
      end if
    end if

  end do

  return
end
function r8vec_sorted_nearest ( n, a, value )

!*****************************************************************************80
!
!! R8VEC_SORTED_NEAREST returns the nearest element in a sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input, real ( kind = 8 ) A(N), a sorted vector.
!
!    Input, real ( kind = 8 ) VALUE, the value whose nearest vector
!    entry is sought.
!
!    Output, integer ( kind = 4 ) R8VEC_SORTED_NEAREST, the index of the nearest
!    entry in the vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) r8vec_sorted_nearest
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) mid
  real ( kind = 8 ) value

  if ( n < 1 ) then
    r8vec_sorted_nearest = -1
    return
  end if

  if ( n == 1 ) then
    r8vec_sorted_nearest = 1
    return
  end if

  if ( a(1) < a(n) ) then

    if ( value < a(1) ) then
      r8vec_sorted_nearest = 1
      return
    else if ( a(n) < value ) then
      r8vec_sorted_nearest = n
      return
    end if
!
!  Seek an interval containing the value.
!
    lo = 1
    hi = n

    do while ( lo < hi - 1 )

      mid = ( lo + hi ) / 2

      if ( value == a(mid) ) then
        r8vec_sorted_nearest = mid
        return
      else if ( value < a(mid) ) then
        hi = mid
      else
        lo = mid
      end if

    end do
!
!  Take the nearest.
!
    if ( abs ( value - a(lo) ) < abs ( value - a(hi) ) ) then
      r8vec_sorted_nearest = lo
    else
      r8vec_sorted_nearest = hi
    end if

    return
!
!  A descending sorted vector A.
!
  else

    if ( value < a(n) ) then
      r8vec_sorted_nearest = n
      return
    else if ( a(1) < value ) then
      r8vec_sorted_nearest = 1
      return
    end if
!
!  Seek an interval containing the value.
!
    lo = n
    hi = 1

    do while ( lo < hi - 1 )

      mid = ( lo + hi ) / 2

      if ( value == a(mid) ) then
        r8vec_sorted_nearest = mid
        return
      else if ( value < a(mid) ) then
        hi = mid
      else
        lo = mid
      end if

    end do
!
!  Take the nearest.
!
    if ( abs ( value - a(lo) ) < abs ( value - a(hi) ) ) then
      r8vec_sorted_nearest = lo
    else
      r8vec_sorted_nearest = hi
    end if

    return

  end if

  return
end



subroutine r8vec_sorted_range ( n, r, r_lo, r_hi, i_lo, i_hi )

!*****************************************************************************80
!
!! R8VEC_SORTED_RANGE searches a sorted vector for elements in a range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 September 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items in the vector.
!
!    Input, real ( kind = 8 ) R(N), the sorted vector.
!
!    Input, real ( kind = 8 ) R_LO, R_HI, the limits of the range.
!
!    Output, integer ( kind = 4 ) I_LO, I_HI, the range of indices
!    so that I_LO <= I <= I_HI => R_LO <= R(I) <= R_HI.  If no
!    values in R lie in the range, then I_HI < I_LO will be returned.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i_lo
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) r_hi
  real ( kind = 8 ) r_lo
!
!  Cases we can handle immediately.
!
  if ( r(n) < r_lo ) then
    i_lo = 0
    i_hi = - 1
    return
  end if

  if ( r_hi < r(1) ) then
    i_lo = 0
    i_hi = - 1
    return
  end if
!
!  Are there are least two intervals?
!
  if ( n == 1 ) then
    if ( r_lo <= r(1) .and. r(1) <= r_hi ) then
      i_lo = 1
      i_hi = 1
    else
      i_lo = 0
      i_hi = -1
    end if
    return
  end if
!
!  Bracket R_LO.
!
  if ( r_lo <= r(1) ) then

    i_lo = 1

  else
!
!  R_LO is in one of the intervals spanned by R(J1) to R(J2).
!  Examine the intermediate interval [R(I1), R(I1+1)].
!  Does R_LO lie here, or below or above?
!
    j1 = 1
    j2 = n
    i1 = ( j1 + j2 - 1 ) / 2
    i2 = i1 + 1

    do

      if ( r_lo < r(i1) ) then
        j2 = i1
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else if ( r(i2) < r_lo ) then
        j1 = i2
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else
        i_lo = i1
        exit
      end if

    end do

  end if
!
!  Bracket R_HI
!
  if ( r(n) <= r_hi ) then

    i_hi = n

  else

    j1 = i_lo
    j2 = n
    i1 = ( j1 + j2 - 1 ) / 2
    i2 = i1 + 1

    do

      if ( r_hi < r(i1) ) then
        j2 = i1
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else if ( r(i2) < r_hi ) then
        j1 = i2
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else
        i_hi = i2
        exit
      end if

    end do

  end if
!
!  We expect to have computed the largest I_LO and smallest I_HI such that
!    R(I_LO) <= R_LO <= R_HI <= R(I_HI)
!  but what we want is actually
!    R_LO <= R(I_LO) <= R(I_HI) <= R_HI
!  which we can usually get simply by incrementing I_LO and decrementing I_HI.
!
  if ( r(i_lo) < r_lo ) then
    i_lo = i_lo + 1
    if ( n < i_lo ) then
      i_hi = i_lo - 1
    end if
  end if

  if ( r_hi < r(i_hi) ) then
    i_hi = i_hi - 1
    if ( i_hi < 1 ) then
      i_lo = i_hi + 1
    end if
  end if

  return
end



subroutine r8vec_sorted_split ( n, a, split, i_lt, i_gt )

!*****************************************************************************80
!
!! R8VEC_SORTED_SPLIT "splits" a sorted R8VEC, given a splitting value.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Given a splitting value SPLIT, the routine seeks indices
!    I_LT and I_GT so that
!
!      A(I_LT) < SPLIT < A(I_GT),
!
!    and if there are intermediate index values between I_LT and
!    I_GT, then those entries of A are exactly equal to SPLIT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), a sorted array.
!
!    Input, real ( kind = 8 ) SPLIT, a value to which the entries in A are
!    to be compared.
!
!    Output, integer ( kind = 4 ) I_LT:
!    0 if no entries are less than SPLIT;
!    N if all entries are less than SPLIT;
!    otherwise, the index of the last entry in A less than SPLIT.
!
!    Output, integer ( kind = 4 ) I_GT:
!    1 if all entries are greater than SPLIT;
!    N+1 if no entries are greater than SPLIT;
!    otherwise the index of the first entry in A greater than SPLIT.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_gt
  integer ( kind = 4 ) i_lt
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) mid
  real ( kind = 8 ) split

  if ( n < 1 ) then
    i_lt = -1
    i_gt = -1
    return
  end if

  if ( split < a(1) ) then
    i_lt = 0
    i_gt = 1
    return
  end if

  if ( a(n) < split ) then
    i_lt = n
    i_gt = n + 1
    return
  end if

  lo = 1
  hi = n

  do

    if ( lo + 1 == hi ) then
      i_lt = lo
      exit
    end if

    mid = ( lo + hi ) / 2

    if ( split <= a(mid) ) then
      hi = mid
    else
      lo = mid
    end if

  end do

  do i = i_lt + 1, n
    if ( split < a(i) ) then
      i_gt = i
      return
    end if
  end do

  i_gt = n + 1

  return
end



subroutine r8vec_sorted_undex ( x_num, x_val, x_unique_num, tol, undx, xdnu )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNDEX returns unique sorted indexes for a sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The goal of this routine is to determine a vector UNDX,
!    which points, to the unique elements of X, in sorted order,
!    and a vector XDNU, which identifies, for each entry of X, the index of
!    the unique sorted element of X.
!
!    This is all done with index vectors, so that the elements of
!    X are never moved.
!
!    Assuming X is already sorted, we examine the entries of X in order,
!    noting the unique entries, creating the entries of XDNU and
!    UNDX as we go.
!
!    Once this process has been completed, the vector X could be
!    replaced by a compressed vector XU, containing the unique entries
!    of X in sorted order, using the formula
!
!      XU(I) = X(UNDX(I)).
!
!    We could then, if we wished, reconstruct the entire vector X, or
!    any element of it, by index, as follows:
!
!      X(I) = XU(XDNU(I)).
!
!    We could then replace X by the combination of XU and XDNU.
!
!    Later, when we need the I-th entry of X, we can locate it as
!    the XDNU(I)-th entry of XU.
!
!    Here is an example of a vector X, the sort and inverse sort
!    index vectors, and the unique sort and inverse unique sort vectors
!    and the compressed unique sorted vector.
!
!    Here is an example of a vector X, the unique sort and
!    inverse unique sort vectors and the compressed unique sorted vector.
!
!      I      X      XU  Undx  Xdnu
!    ----+------+------+-----+-----+
!      1 | 11.0 |  11.0    1     1
!      2 | 11.0 |  22.0    5     1
!      3 | 11.0 |  33.0    8     1
!      4 | 11.0 |  55.0    9     1
!      5 | 22.0 |                2
!      6 | 22.0 |                2
!      7 | 22.0 |                2
!      8 | 33.0 |                3
!      9 | 55.0 |
!
!    INDX(2) = 3 means that sorted item(2) is X(3).
!    XDNI(2) = 5 means that X(2) is sorted item(5).
!
!    UNDX(3) = 4 means that unique sorted item(3) is at X(4).
!    XDNU(8) = 2 means that X(8) is at unique sorted item(2).
!
!    XU(XDNU(I))) = X(I).
!    XU(I)        = X(UNDX(I)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X_NUM, the number of data values.
!
!    Input, real ( kind = 8 ) X_VAL(X_NUM), the data values.
!
!    Input, integer ( kind = 4 ) X_UNIQUE_NUM, the number of unique values
!    in X_VAL.  This value is only required for languages in which the size of
!    UNDX must be known in advance.
!
!    Input, real ( kind = 8 ) TOL, a tolerance for equality.
!
!    Output, integer ( kind = 4 ) UNDX(X_UNIQUE_NUM), the UNDX vector.
!
!    Output, integer ( kind = 4 ) XDNU(X_NUM), the XDNU vector.
!
  implicit none

  integer ( kind = 4 ) x_num
  integer ( kind = 4 ) x_unique_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) tol
  integer ( kind = 4 ) undx(x_unique_num)
  real ( kind = 8 ) x_val(x_num)
  integer ( kind = 4 ) xdnu(x_num)
!
!  Walk through the sorted array X.
!
  i = 1

  j = 1
  undx(j) = i

  xdnu(i) = j

  do i = 2, x_num

    if ( tol < abs ( x_val(i) - x_val(undx(j)) ) ) then
      j = j + 1
      undx(j) = i
    end if

    xdnu(i) = j

  end do

  return
end



subroutine r8vec_sorted_unique ( n, a, tol, unique_num )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNIQUE keeps the unique elements in a sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, the sorted array of N elements;
!    On output, the sorted unique array of UNIQUE_NUM elements.
!
!    Input, real ( kind = 8 ) TOL, a nonnegative tolerance for equality.
!    Set it to 0.0 for the strictest test.
!
!    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) unique_num
  real ( kind = 8 ) tol

  if ( n <= 0 ) then
    unique_num = 0
    return
  end if

  unique_num = 1

  do i = 2, n

    if ( tol < abs ( a(i) - a(unique_num) ) ) then
      unique_num = unique_num + 1
      a(unique_num) = a(i)
    end if

  end do

  return
end



subroutine r8vec_sorted_unique_count ( n, a, tol, unique_num )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNIQUE_COUNT counts the unique elements in a sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Because the array is sorted, this algorithm is O(N).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input, real ( kind = 8 ) A(N), the sorted array to examine.
!
!    Input, real ( kind = 8 ) TOL, a nonnegative tolerance for equality.
!    Set it to 0.0 for the strictest test.
!
!    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) unique_num
  real ( kind = 8 ) tol

  if ( n < 1 ) then
    unique_num = 0
    return
  end if

  unique_num = 1

  do i = 2, n

    if ( tol < abs ( a(i-1) - a(i) ) ) then
      unique_num = unique_num + 1
    end if

  end do

  return
end



subroutine r8vec_sorted_unique_hist ( n, a, tol, maxuniq, unique_num, &
  auniq, acount )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNIQUE_HIST histograms the unique elements of a sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input, real ( kind = 8 ) A(N), the array to examine.  The elements of A
!    should have been sorted.
!
!    Input, real ( kind = 8 ) TOL, a nonnegative tolerance for equality.
!    Set it to 0.0 for the strictest test.
!
!    Input, integer ( kind = 4 ) MAXUNIQ, the maximum number of unique elements
!    that can be handled.  If there are more than MAXUNIQ unique
!    elements in A, the excess will be ignored.
!
!    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    of A.
!
!    Output, real ( kind = 8 ) AUNIQ(UNIQUE_NUM), the unique elements of A.
!
!    Output, integer ( kind = 4 ) ACOUNT(UNIQUE_NUM), the number of times
!    each element of AUNIQ occurs in A.
!
  implicit none

  integer ( kind = 4 ) maxuniq
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) acount(maxuniq)
  real ( kind = 8 ) auniq(maxuniq)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) unique_num
  real ( kind = 8 ) tol
!
!  Start taking statistics.
!
  unique_num = 0

  do i = 1, n

    if ( i == 1 ) then

      unique_num = 1
      auniq(unique_num) = a(1)
      acount(unique_num) = 1

    else if ( abs ( a(i) - auniq(unique_num) ) <= tol ) then

      acount(unique_num) = acount(unique_num) + 1

    else if ( unique_num < maxuniq ) then

      unique_num = unique_num + 1
      auniq(unique_num) = a(i)
      acount(unique_num) = 1

    end if

  end do

  return
end



subroutine r8vec_split ( n, a, split, isplit )

!*****************************************************************************80
!
!! R8VEC_SPLIT "splits" an unsorted R8VEC based on a splitting value.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If the vector is already sorted, it is simpler to do a binary search
!    on the data than to call this routine.
!
!    The vector is not assumed to be sorted before input, and is not
!    sorted during processing.  If sorting is not needed, then it is
!    more efficient to use this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input/output, real ( kind = 8 ) A(N), the array to split.  On output,
!    all the entries of A that are less than or equal to SPLIT
!    are in A(1:ISPLIT).
!
!    Input, real ( kind = 8 ) SPLIT, the value used to split the vector.
!    It is not necessary that any value of A actually equal SPLIT.
!
!    Output, integer ( kind = 4 ) ISPLIT, indicates the position of the last
!    entry of the split vector that is less than or equal to SPLIT.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) isplit
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j3
  real ( kind = 8 ) split
!
!  Partition the vector into A1, A2, A3, where
!    A1 = A(I1:J1) holds values <= SPLIT,
!    A2 = A(I2:J2) holds untested values,
!    A3 = A(I3:J3) holds values > SPLIT.
!
  i1 = 1
  j1 = 0

  i2 = 1
  j2 = n

  i3 = n + 1
  j3 = n
!
!  Pick the next item from A2, and move it into A1 or A3.
!  Adjust indices appropriately.
!
  do i = 1, n

    if ( a(i2) <= split ) then
      i2 = i2 + 1
      j1 = j1 + 1
    else
      call r8_swap ( a(i2), a(i3-1) )
      i3 = i3 - 1
      j2 = j2 - 1
    end if

  end do

  isplit = j1

  return
end



subroutine r8vec_std ( n, a, std )

!*****************************************************************************80
!
!! R8VEC_STD returns the standard deviation of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The standard deviation of a vector X of length N is defined as
!
!      mean ( X(1:n) ) = sum ( X(1:n) ) / n
!
!      std ( X(1:n) ) = sqrt ( sum ( ( X(1:n) - mean )^2 ) / ( n - 1 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!    N should be at least 2.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, real ( kind = 8 ) STD, the standard deviation of the vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean
  real ( kind = 8 ) std

  if ( n < 2 ) then

    std = 0.0D+00

  else

    mean = sum ( a(1:n) ) / real ( n, kind = 8 )

    std = sum ( ( a(1:n) - mean )**2 )

    std = sqrt ( std / real ( n - 1, kind = 8 ) )

  end if

  return
end



subroutine r8vec_stutter ( n, a, m, am )

!*****************************************************************************80
!
!! R8VEC_STUTTER makes a "stuttering" copy of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Applying a stuttering factor M of 3, the vector A = ( 1, 5, 8 ) becomes
!    AM = ( 1, 1, 1, 5, 5, 5, 8, 8, 8 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the size of the input vector.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Input, integer ( kind = 4 ) M, the "stuttering factor".
!
!    Output, real ( kind = 8 ) AM(M*N), the stuttering vector.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) am(m*n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo

  do i = 1, n
    jlo = m * ( i - 1 ) + 1
    jhi = m *   i
    am(jlo:jhi) = a(i)
  end do

  return
end
function r8vec_sum ( n, a )

!*****************************************************************************80
!
!! R8VEC_SUM returns the sum of the entries of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In FORTRAN90, this facility is offered by the built in
!    SUM function:
!
!      R8VEC_SUM ( N, A ) = SUM ( A(1:N) )
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
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) R8VEC_SUM, the sum of the entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_sum

  r8vec_sum = sum ( a(1:n) )

  return
end



subroutine r8vec_swap ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_SWAP swaps the entries of two R8VECs.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
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
!    Input, integer ( kind = 4 ) N, the number of entries in the arrays.
!
!    Input/output, real ( kind = 8 ) A1(N), A2(N), the vectors to swap.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) a3(n)

  a3(1:n) = a1(1:n)
  a1(1:n) = a2(1:n)
  a2(1:n) = a3(1:n)

  return
end



subroutine r8vec_transpose_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_TRANSPOSE_PRINT prints an R8VEC "transposed".
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Example:
!
!    A = (/ 1.0, 2.1, 3.2, 4.3, 5.4, 6.5, 7.6, 8.7, 9.8, 10.9, 11.0 /)
!    TITLE = 'My vector:  '
!
!    My vector:
!
!        1.0    2.1    3.2    4.3    5.4
!        6.5    7.6    8.7    9.8   10.9
!       11.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do ilo = 1, n, 5
    ihi = min ( ilo + 5 - 1, n )
    write ( *, '(5g14.6)' ) a(ilo:ihi)
  end do

  return
end



subroutine r8vec_undex ( x_num, x_val, x_unique_num, tol, undx, xdnu )

!*****************************************************************************80
!
!! R8VEC_UNDEX returns unique sorted indexes for an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The goal of this routine is to determine a vector UNDX,
!    which points, to the unique elements of X, in sorted order,
!    and a vector XDNU, which identifies, for each entry of X, the index of
!    the unique sorted element of X.
!
!    This is all done with index vectors, so that the elements of
!    X are never moved.
!
!    The first step of the algorithm requires the indexed sorting
!    of X, which creates arrays INDX and XDNI.  (If all the entries
!    of X are unique, then these arrays are the same as UNDX and XDNU.)
!
!    We then use INDX to examine the entries of X in sorted order,
!    noting the unique entries, creating the entries of XDNU and
!    UNDX as we go.
!
!    Once this process has been completed, the vector X could be
!    replaced by a compressed vector XU, containing the unique entries
!    of X in sorted order, using the formula
!
!      XU(1:X_UNIQUE_NUM) = X(UNDX(1:X_UNIQUE_NUM)).
!
!    We could then, if we wished, reconstruct the entire vector X, or
!    any element of it, by index, as follows:
!
!      X(I) = XU(XDNU(I)).
!
!    We could then replace X by the combination of XU and XDNU.
!
!    Later, when we need the I-th entry of X, we can locate it as
!    the XDNU(I)-th entry of XU.
!
!    Here is an example of a vector X, the sort and inverse sort
!    index vectors, and the unique sort and inverse unique sort vectors
!    and the compressed unique sorted vector.
!
!      I    X   Indx  Xdni      XU   Undx  Xdnu
!    ----+-----+-----+-----+--------+-----+-----+
!      1 | 11.     1     1 |    11,     1     1
!      2 | 22.     3     5 |    22,     2     2
!      3 | 11.     6     2 |    33,     4     1
!      4 | 33.     9     8 |    55,     5     3
!      5 | 55.     2     9 |                  4
!      6 | 11.     7     3 |                  1
!      7 | 22.     8     6 |                  2
!      8 | 22.     4     7 |                  2
!      9 | 11.     5     4 |                  1
!
!    INDX(2) = 3 means that sorted item(2) is X(3).
!    XDNI(2) = 5 means that X(2) is sorted item(5).
!
!    UNDX(3) = 4 means that unique sorted item(3) is at X(4).
!    XDNU(8) = 2 means that X(8) is at unique sorted item(2).
!
!    XU(XDNU(I))) = X(I).
!    XU(I)        = X(UNDX(I)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X_NUM, the number of data values.
!
!    Input, real ( kind = 8 ) X_VAL(X_NUM), the data values.
!
!    Input, integer ( kind = 4 ) X_UNIQUE_NUM, the number of unique values
!    in X_VAL.  This value is only required for languages in which the size of
!    UNDX must be known in advance.
!
!    Input, real ( kind = 8 ) TOL, a tolerance for equality.
!
!    Output, integer ( kind = 4 ) UNDX(X_UNIQUE_NUM), the UNDX vector.
!
!    Output, integer ( kind = 4 ) XDNU(X_NUM), the XDNU vector.
!
  implicit none

  integer ( kind = 4 ) x_num
  integer ( kind = 4 ) x_unique_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(x_num)
  integer ( kind = 4 ) j
  real ( kind = 8 ) tol
  integer ( kind = 4 ) undx(x_unique_num)
  real ( kind = 8 ) x_val(x_num)
  integer ( kind = 4 ) xdnu(x_num)
!
!  Implicitly sort the array.
!
  call r8vec_sort_heap_index_a ( x_num, x_val, indx )
!
!  Walk through the implicitly sorted array X.
!
  i = 1

  j = 1
  undx(j) = indx(i)

  xdnu(indx(i)) = j

  do i = 2, x_num

    if ( tol < abs ( x_val(indx(i)) - x_val(undx(j)) ) ) then
      j = j + 1
      undx(j) = indx(i)
    end if

    xdnu(indx(i)) = j

  end do

  return
end



subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    For now, the input quantity SEED is an integer ( kind = 4 ) variable.
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
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + 2147483647
    end if

    r(i) = real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end



subroutine r8vec_uniform_ab ( n, a, b, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Each dimension ranges from A to B.
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
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end



subroutine r8vec_uniform_abvec ( n, a, b, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_ABVEC returns a scaled pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Dimension I ranges from A(I) to B(I).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 October 2012
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
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A(N), B(N), the lower and upper limits
!    for each dimension.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_ABVEC - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = a(i) + ( b(i) - a(i) ) * real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end



subroutine r8vec_unique_count ( n, a, tol, unique_num )

!*****************************************************************************80
!
!! R8VEC_UNIQUE_COUNT counts the unique elements in an unsorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Because the array is unsorted, this algorithm is O(N^2).
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
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input, real ( kind = 8 ) A(N), the unsorted array to examine.
!
!    Input, real ( kind = 8 ) TOL, a nonnegative tolerance for equality.
!    Set it to 0.0 for the strictest test.
!
!    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) unique_num
  real ( kind = 8 ) tol

  unique_num = 0

  do i = 1, n

    unique_num = unique_num + 1

    do j = 1, i - 1

      if ( abs ( a(i) - a(j) ) <= tol ) then
        unique_num = unique_num - 1
        exit
      end if

    end do

  end do

  return
end



subroutine r8vec_unique_index ( n, a, tol, unique_index )

!*****************************************************************************80
!
!! R8VEC_UNIQUE_INDEX indexes the unique occurrence of values in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    For element A(I) of the vector, UNIQUE_INDEX(I) is the uniqueness index
!    of A(I).  That is, if A_UNIQUE contains the unique elements of A,
!    gathered in order, then
!
!      A_UNIQUE ( UNIQUE_INDEX(I) ) = A(I)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Input, real ( kind = 8 ) TOL, a tolerance for equality.
!
!    Output, integer ( kind = 4 ) UNIQUE_INDEX(N), the unique index.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) tol
  integer ( kind = 4 ) unique_index(n)
  integer ( kind = 4 ) unique_num

  unique_index(1:n) = -1
  unique_num = 0

  do i = 1, n

    if ( unique_index(i) == -1 ) then

      unique_num = unique_num + 1
      unique_index(i) = unique_num

      do j = i + 1, n
        if ( abs ( a(i) - a(j) ) <= tol ) then
          unique_index(j) = unique_num
        end if
      end do

    end if

  end do

  return
end



subroutine r8vec_variance ( n, a, variance )

!*****************************************************************************80
!
!! R8VEC_VARIANCE returns the variance of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The variance of a vector X of length N is defined as
!
!      mean ( X(1:n) ) = sum ( X(1:n) ) / n
!
!      var ( X(1:n) ) = sum ( ( X(1:n) - mean )^2 ) / ( n - 1 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!    N should be at least 2.
!
!    Input, real ( kind = 8 ) A(N), the vector.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance

  if ( n < 2 ) then

    variance = 0.0D+00

  else

    mean = sum ( a(1:n) ) / real ( n, kind = 8 )

    variance = sum ( ( a(1:n) - mean )**2 )

    variance = variance / real ( n - 1, kind = 8 )

  end if

  return
end



subroutine r8vec_vector_triple_product ( v1, v2, v3, v )

!*****************************************************************************80
!
!! R8VEC_VECTOR_TRIPLE_PRODUCT computes the vector triple product.
!
!  Discussion:
!
!    VTRIPLE = V1 x ( V2 x V3 )
!
!    VTRIPLE is a vector perpendicular to V1, lying in the plane
!    spanned by V2 and V3.  The norm of VTRIPLE is the product
!    of the norms of V1, V2 and V3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) V1(3), V2(3), V3(3), three vectors.
!
!    Output, real ( kind = 8 ) V(3), the vector triple product.
!
  implicit none

  real ( kind = 8 ) v(3)
  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)
  real ( kind = 8 ) v4(3)

  call r8vec_cross_product_3d ( v2, v3, v4 )

  call r8vec_cross_product_3d ( v1, v4, v )

  return
end



subroutine r8vec_write ( n, r, output_file )

!*****************************************************************************80
!
!! R8VEC_WRITE writes an R8VEC to a file.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) R(N), the vector to be written.
!
!    Input, character ( len = * ) OUTPUT_FILE, the name of the file to which
!    the information is to be written.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  character ( len = * ) output_file
  integer ( kind = 4 ) output_unit
  real ( kind = 8 ) r(n)

  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_file, status = 'replace' )

  do i = 1, n
    write ( output_unit, '(2x,g16.8)' ) r(i)
  end do

  close ( unit = output_unit )

  return
end



subroutine r8vec_zero ( n, a )

!*****************************************************************************80
!
!! R8VEC_ZERO zeroes out an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Output, real ( kind = 8 ) A(N), the vector to be zeroed.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)

  a(1:n) = 0.0D+00

  return
end



subroutine r8vec2_compare ( n, a1, a2, i, j, isgn )

!*****************************************************************************80
!
!! R8VEC2_COMPARE compares two entries in an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    The lexicographic ordering is used.
!
!  Example:
!
!    A1(I) A2(I)   A1(J) A2(J)  ISGN
!    -----------   -----------  ----
!    1.0   5.0  <  1.0   6.0     -1
!    1.0   5.0  <  2.0   8.0     -1
!    1.0   5.0  <  9.0   1.0     -1
!    1.0   5.0  =  1.0   5.0      0
!    1.0   5.0  >  0.0   2.0     +1
!    1.0   5.0  >  0.0   5.0     +1
!    1.0   5.0  >  1.0   3.0     +1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data items.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), the two components of each item.
!
!    Input, integer ( kind = 4 ) I, J, the items to be compared.
!
!    Output, integer ( kind = 4 ) ISGN, the results of the comparison:
!    -1, item I < item J,
!     0, item I = item J,
!    +1, item I > item J.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j

  isgn = 0

       if ( a1(i) < a1(j) ) then

    isgn = -1

  else if ( a1(i) == a1(j) ) then

         if ( a2(i) < a2(j) ) then
      isgn = -1
    else if ( a2(i) < a2(j) ) then
      isgn = 0
    else if ( a2(j) < a2(i) ) then
      isgn = +1
    end if

  else if ( a1(j) < a1(i) ) then

    isgn = +1

  end if

  return
end



subroutine r8vec2_print ( n, a1, a2, title )

!*****************************************************************************80
!
!! R8VEC2_PRINT prints an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), the vectors to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, a1(i), a2(i)
  end do

  return
end



subroutine r8vec2_print_some ( n, x1, x2, max_print, title )

!*****************************************************************************80
!
!! R8VEC2_PRINT_SOME prints "some" of an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vectors, is no more than MAX_PRINT, then
!    the entire vectors are printed, one entry of each per line.
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
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vectors.
!
!    Input, real ( kind = 8 ) X1(N), X2(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title
  real ( kind = 8 ) x1(n)
  real ( kind = 8 ) x2(n)

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
      write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) i, x1(i), x2(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) i, x1(i), x2(i)
    end do
    write ( *, '(a)' ) '  ......  ..............  ..............'
    i = n
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) i, x1(i), x2(i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) i, x1(i), x2(i)
    end do
    i = max_print
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6,2x,a)' ) i, x1(i), x2(i), &
      '...more entries...'

  end if

  return
end



subroutine r8vec2_sort_a ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC2_SORT_A ascending sorts an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    Each item to be sorted is a pair (I,J), with the I
!    and J values stored in separate vectors A1 and A2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items of data.
!
!    Input/output, real ( kind = 8 ) A1(N), A2(N), the data to be sorted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j

  if ( n <= 1 ) then
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

    call sort_heap_external ( n, indx, i, j, isgn )
!
!  Interchange the I and J objects.
!
    if ( 0 < indx ) then

      call r8_swap ( a1(i), a1(j) )
      call r8_swap ( a2(i), a2(j) )
!
!  Compare the I and J objects.
!
    else if ( indx < 0 ) then

      call r8vec2_compare ( n, a1, a2, i, j, isgn )

    else if ( indx == 0 ) then

      exit

    end if

  end do

  return
end



subroutine r8vec2_sort_d ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC2_SORT_D descending sorts an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    Each item to be sorted is a pair (I,J), with the I
!    and J values stored in separate vectors A1 and A2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items of data.
!
!    Input/output, real ( kind = 8 ) A1(N), A2(N), the data to be sorted.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j

  if ( n <= 1 ) then
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

    call sort_heap_external ( n, indx, i, j, isgn )
!
!  Interchange the I and J objects.
!
    if ( 0 < indx ) then

      call r8_swap ( a1(i), a1(j) )
      call r8_swap ( a2(i), a2(j) )
!
!  Compare the I and J objects.
!  Reverse the value of ISGN to effect a descending sort.
!
    else if ( indx < 0 ) then

      call r8vec2_compare ( n, a1, a2, i, j, isgn )

      isgn = -isgn

    else if ( indx == 0 ) then

      exit

    end if

  end do

  return
end



subroutine r8vec2_sort_heap_index_a ( n, x, y, indx )

!*****************************************************************************80
!
!! R8VEC2_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    ( X(I), Y(I) ) < ( X(J), Y(J) ) if:
!
!    * X(I) < X(J), or
!
!    * X(I) = X(J), and Y(I) < Y(J).
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      ( X(INDX(1:N)), Y(INDX(1:N) ), is sorted,
!
!    or explicitly, by the call
!
!      call r8vec_permute ( n, indx, x )
!      call r8vec_permute ( n, indx, y )
!
!    after which ( X(1:N), Y(1:N) ), is sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) X(N),Y(N), pairs of X, Y coordinates of points.
!
!    Output, integer ( kind = 4 ) INDX(N), the sort index.  The
!    I-th element of the sorted array has coordinates ( X(INDX(I)), Y(INDX(I) ).
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indxt
  integer ( kind = 4 ) ir
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) yval

  if ( n < 1 ) then
    return
  end if

  call i4vec_indicator ( n, indx )

  if ( n == 1 ) then
    return
  end if

  l = n / 2 + 1
  ir = n

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      xval = x(indxt)
      yval = y(indxt)

    else

      indxt = indx(ir)
      xval = x(indxt)
      yval = y(indxt)
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

        if ( x(indx(j)) < x(indx(j+1)) .or. &
          ( x(indx(j)) == x(indx(j+1)) .and. y(indx(j)) < y(indx(j+1)) ) ) then
          j = j + 1
        end if

      end if

      if ( xval < x(indx(j)) .or. &
          ( xval == x(indx(j)) .and. yval < y(indx(j)) ) ) then
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



subroutine r8vec2_sorted_unique ( n, a1, a2, unique_num )

!*****************************************************************************80
!
!! R8VEC2_SORTED_UNIQUE keeps unique elements in a sorted R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    Item I is stored as the pair A1(I), A2(I).
!
!    The items must have been sorted, or at least it must be the
!    case that equal items are stored in adjacent vector locations.
!
!    If the items were not sorted, then this routine will only
!    replace a string of equal values by a single representative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items.
!
!    Input/output, real ( kind = 8 ) A1(N), A2(N).
!    On input, the array of N items.
!    On output, an array of unique items.
!
!    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique items.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) itest
  integer ( kind = 4 ) unique_num

  if ( n <= 0 ) then
    unique_num = 0
    return
  end if

  unique_num = 1

  do itest = 2, n

    if ( a1(itest) /= a1(unique_num) .or. a2(itest) /= a2(unique_num) ) then

      unique_num = unique_num + 1

      a1(unique_num) = a1(itest)
      a2(unique_num) = a2(itest)

    end if

  end do

  return
end



subroutine r8vec2_sorted_unique_index ( n, a1, a2, unique_num, indx )

!*****************************************************************************80
!
!! R8VEC2_SORTED_UNIQUE_INDEX indexes unique elements in a sorted R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    Item I is stored as the pair A1(I), A2(I).
!
!    The items must have been sorted, or at least it should be the
!    case that equal items are stored in adjacent vector locations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), the array of N items.
!
!    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique items.
!
!    Output, integer ( kind = 4 ) INDX(N), contains in entries 1 through
!    UNIQUE_NUM an index array of the unique items.  To build new arrays
!    with no repeated elements:
!      B1(1:UNIQUE_NUM) = A1(INDX(1:UNIQUE_NUM))
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) itest
  integer ( kind = 4 ) unique_num

  if ( n <= 0 ) then
    unique_num = 0
    return
  end if

  unique_num = 1
  indx(1) = 1

  do itest = 2, n

    if ( a1(itest-1) /= a1(itest) .or. a2(itest-1) /= a2(itest) ) then

      unique_num = unique_num + 1

      indx(unique_num) = itest

    end if

  end do

  indx(unique_num+1:n) = 0

  return
end



subroutine r8vec2_sum_max_index ( n, a, b, sum_max_index )

!*****************************************************************************80
!
!! R8VEC2_SUM_MAX_INDEX returns the index of the maximum sum of two R8VEC's.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), B(N), two arrays whose sum
!    is to be examined.
!
!    Output, integer ( kind = 4 ) SUM_MAX_INDEX, the index of the largest
!    entry in A+B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) sum_max
  integer ( kind = 4 ) sum_max_index

  if ( n <= 0 ) then

    sum_max_index = -1

  else

    sum_max_index = 1
    sum_max = a(1) + b(1)

    do i = 2, n
      if ( sum_max < a(i) + b(i) ) then
        sum_max = a(i) + b(i)
        sum_max_index = i
      end if
    end do

  end if

  return
end



subroutine r8vec3_print ( n, a1, a2, a3, title )

!*****************************************************************************80
!
!! R8VEC3_PRINT prints an R8VEC3.
!
!  Discussion:
!
!    An R8VEC3 is a dataset consisting of N triples of R8's, stored
!    as three separate vectors A1, A2, A3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A1(N), A2(N), A3(N), the vectors to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) a3(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(i8,3g14.6)' ) i, a1(i), a2(i), a3(i)
  end do

  return
end



subroutine r8vecs_print ( m, nvec, na, a, title )

!*****************************************************************************80
!
!! R8VECS_PRINT prints a packed R8VEC.
!
!  Example:
!
!    M = 5
!    NVEC = (/ 1, 4, 6, 11, 13, 14 /)
!    A = (/ 11, 12, 13, 21, 22, 31, 32, 33, 34, 35, 41, 42, 51 /)
!
!    11 12 13
!    21 22
!    31 32 33 34 35
!    41 42
!    51
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of vectors packed into A.
!
!    Input, integer ( kind = 4 ) NVEC(M+1), pointers to the first entry 
!    in each vector.
!
!    Input, integer ( kind = 4 ) NA, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(NA), the packed vector.  The I-th vector
!    extends from A(NVEC(I)) to A(NVEC(I+1)-1).
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) na

  real ( kind = 8 ) a(na)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) khi
  integer ( kind = 4 ) klo
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nvec(m+1)
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  khi = 0

  do i = 1, m

    n = nvec(i+1) - nvec(i)

    do jlo = 1, n, 5
      jhi = min ( jlo + 5 - 1, n )
      klo = khi + 1
      khi = klo + ( jhi - jlo )
      if ( jlo == 1 ) then
        write ( *, '(2x,i3,2x,5g14.6)' ) i, a(klo:khi)
      else
        write ( *, '(7x,5g14.6)' ) a(klo:khi)
      end if
    end do

  end do

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


end module jburk_r8vec_
