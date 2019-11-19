module     jburk_r4row_
use, intrinsic :: iso_fortran_env
implicit none

contains

subroutine r4row_max ( m, n, a, amax )

!*****************************************************************************80
!
!! R4ROW_MAX returns the maximums of an R4ROW.
!
!  Discussion:
!
!    An R4ROW is an M by N array of R4 values, regarded
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
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns
!    in the array.
!
!    Input, real (kind=4) :: A(M,N), the array to be examined.
!
!    Output, real (kind=4) :: AMAX(M), the maximums of the rows.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  real (kind=4) :: amax(m)
  integer (kind=4) :: i
  integer (kind=4) :: j

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
subroutine r4row_mean ( m, n, a, mean )

!*****************************************************************************80
!
!! R4ROW_MEAN returns the means of an R4ROW.
!
!  Discussion:
!
!    An R4ROW is an M by N array of R4 values, regarded
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
!    Input, real (kind=4) :: A(M,N), the array to be examined.
!
!    Output, real (kind=4) :: MEAN(M), the means, or averages, of the rows.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  real (kind=4) :: mean(m)

  do i = 1, m
    mean(i) = sum ( a(i,1:n) ) / real ( n, kind = 4 )
  end do

  return
end
subroutine r4row_min ( m, n, a, amin )

!*****************************************************************************80
!
!! R4ROW_MIN returns the minimums of an R4ROW.
!
!  Discussion:
!
!    An R4ROW is an M by N array of R4 values, regarded
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
!    03 May 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: M, N, the number of rows and columns
!    in the array.
!
!    Input, real (kind=4) :: A(M,N), the array to be examined.
!
!    Output, real (kind=4) :: AMIN(M), the minimums of the rows.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  real (kind=4) :: amin(m)
  integer (kind=4) :: i
  integer (kind=4) :: j

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
subroutine r4row_sorted_unique_count ( m, n, a, unique_num )

!*****************************************************************************80
!
!! R4ROW_SORTED_UNIQUE_COUNT counts unique elements in an R4ROW.
!
!  Discussion:
!
!    An R4ROW is an M by N array of R4 values, regarded
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
!    Input, real (kind=4) :: A(M,N), a sorted array, containing
!    M rows of data.
!
!    Output, integer (kind=4) :: UNIQUE_NUM, the number of unique rows.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i1
  integer (kind=4) :: i2
  integer (kind=4) :: unique_num

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
subroutine r4row_sum ( m, n, a, rowsum )

!*****************************************************************************80
!
!! R4ROW_SUM returns the sums of the rows of an R4ROW.
!
!  Discussion:
!
!    An R4ROW is an M by N array of R4 values, regarded
!    as an array of M rows of length N.
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
!    Input, real (kind=4) :: A(M,N), the M by N array.
!
!    Output, real (kind=4) :: ROWSUM(M), the sum of the entries of
!    each row.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  real (kind=4) :: rowsum(m)

  do i = 1, m
    rowsum(i) = sum ( a(i,1:n) )
  end do

  return
end
subroutine r4row_swap ( m, n, a, i1, i2 )

!*****************************************************************************80
!
!! R4ROW_SWAP swaps two rows of an R4ROW.
!
!  Discussion:
!
!    An R4ROW is an M by N array of R4 values, regarded
!    as an array of M rows of length N.
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
!    Input/output, real (kind=4) :: A(M,N), the M by N array.
!
!    Input, integer (kind=4) :: I1, I2, the two rows to swap.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i1
  integer (kind=4) :: i2
  real (kind=4) :: row(n)

  if ( i1 < 1 .or. m < i1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4ROW_SWAP - Fatal error!'
    write ( *, '(a)' ) '  I1 is out of range.'
    write ( *, '(a,i8)' ) '  I1 = ', i1
    stop
  end if

  if ( i2 < 1 .or. m < i2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4ROW_SWAP - Fatal error!'
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
subroutine r4row_to_r4vec ( m, n, a, x )

!*****************************************************************************80
!
!! R4ROW_TO_R4VEC converts an R4ROW into an R4VEC.
!
!  Discussion:
!
!    An R4ROW is an M by N array of R4 values, regarded
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
!    Input, real (kind=4) :: A(M,N), the M by N array.
!
!    Output, real (kind=4) :: X(M*N), a vector containing the M rows of A.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: x(m*n)

  j = 1
  do i = 1, m
    x(j:j+n-1) = a(i,1:n)
    j = j + n
  end do

  return
end
subroutine r4row_variance ( m, n, a, variance )

!*****************************************************************************80
!
!! R4ROW_VARIANCE returns the variances of an R4ROW.
!
!  Discussion:
!
!    An R4ROW is an M by N array of R4 values, regarded
!    as an array of M rows of length N.
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
!    Input, integer (kind=4) :: M, N, the number of rows and columns
!    in the array.
!
!    Input, real (kind=4) :: A(M,N), the array whose variances are desired.
!
!    Output, real (kind=4) :: VARIANCE(M), the variances of the rows.
!
  implicit none

  integer (kind=4) :: m
  integer (kind=4) :: n

  real (kind=4) :: a(m,n)
  integer (kind=4) :: i
  integer (kind=4) :: j
  real (kind=4) :: mean
  real (kind=4) :: variance(m)

  do i = 1, m

    mean = sum ( a(i,1:n) ) / real ( n, kind = 4 )

    variance(i) = 0.0E+00
    do j = 1, n
      variance(i) = variance(i) + ( a(i,j) - mean )**2
    end do

    if ( 1 < n ) then
      variance(i) = variance(i) / real ( n - 1, kind = 4 )
    else
      variance(i) = 0.0E+00
    end if

  end do

  return
end
subroutine r4slmat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R4SLMAT_PRINT prints a strict lower triangular R4MAT.
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
!    03 May 2011
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
!    Input, real (kind=4) :: A(*), the M by N matrix.  Only the strict
!    lower triangular elements are stored, in column major order.
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

  else if ( maxval ( abs ( a(1:size) ) ) < 1000000.0E+00 ) then

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

end module jburk_r4row_
