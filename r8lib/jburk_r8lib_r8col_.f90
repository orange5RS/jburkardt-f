!> @author [P. Jang](orange224factory@gmail.com)
!> @date   2020-02-06
!> @see    [Documenting Fortran with Doxygen](https://github.com/Mohid-Water-Modelling-System/Mohid/wiki/Documenting-Fortran-with-Doxygen)
! 
!> @definition
!  row vector: 1 x n; a matrix with one row
!  col vector: m x 1; a matrix with one col
!      matrix: m rows x n columns

!> @example: A is 2 by 3 matrix (2 rows and 3 columns).
!  A[i,j] = | a11, a12, a13 |
!           | a21, a22, a23 |

!> @note
!  column-major order, e.g. Fortran (one-indexed)
!  [1, 2, 3, 4, 5, 6] = [a11, a21, a12, a22, a13, a23]
!  
!  row-major order, e.g. C (zero-indexed)
!  [0, 1, 2, 3, 4, 5] = [a11, a12, a13, a21, a22, a23]
!  
!  row vector
!  A[1,:] = [a11, a12, a13]
!  
!  column vector
!  A[:,1] = [a11, a21]^T
module     jburk_r8lib_r8col_
use, intrinsic :: iso_fortran_env
implicit none

interface        r8col_compare
module procedure r8col_compare
end interface    r8col_compare
public           r8col_compare

interface        r8col_duplicates
module procedure r8col_duplicates
end interface    r8col_duplicates
public           r8col_duplicates

interface        r8col_find
module procedure r8col_find
end interface    r8col_find
public           r8col_find

interface        r8col_first_index
module procedure r8col_first_index
end interface    r8col_first_index
public           r8col_first_index

interface        r8col_insert
module procedure r8col_insert
end interface    r8col_insert
public           r8col_insert

interface        r8col_max
module procedure r8col_max
end interface    r8col_max
public           r8col_max

interface        r8col_max_index
module procedure r8col_max_index
end interface    r8col_max_index
public           r8col_max_index

interface        r8col_max_one
module procedure r8col_max_one
end interface    r8col_max_one
public           r8col_max_one

interface        r8col_mean
module procedure r8col_mean
end interface    r8col_mean
public           r8col_mean

interface        r8col_min
module procedure r8col_min
end interface    r8col_min
public           r8col_min

interface        r8col_min_index
module procedure r8col_min_index
end interface    r8col_min_index
public           r8col_min_index

interface        r8col_normalize_li
module procedure r8col_normalize_li
end interface    r8col_normalize_li
public           r8col_normalize_li

interface        r8col_part_quick_a
module procedure r8col_part_quick_a
end interface    r8col_part_quick_a
public           r8col_part_quick_a

interface        r8col_permute
module procedure r8col_permute
end interface    r8col_permute
public           r8col_permute

interface        r8col_sort_heap_a
module procedure r8col_sort_heap_a
end interface    r8col_sort_heap_a
public           r8col_sort_heap_a

interface        r8col_sort_heap_index_a
module procedure r8col_sort_heap_index_a
end interface    r8col_sort_heap_index_a
public           r8col_sort_heap_index_a

interface        r8col_sort_quick_a
module procedure r8col_sort_quick_a
end interface    r8col_sort_quick_a
public           r8col_sort_quick_a

interface        r8col_sorted_tol_undex
module procedure r8col_sorted_tol_undex
end interface    r8col_sorted_tol_undex
public           r8col_sorted_tol_undex

interface        r8col_sorted_tol_unique
module procedure r8col_sorted_tol_unique
end interface    r8col_sorted_tol_unique
public           r8col_sorted_tol_unique

interface        r8col_sorted_tol_unique_count
module procedure r8col_sorted_tol_unique_count
end interface    r8col_sorted_tol_unique_count
public           r8col_sorted_tol_unique_count

interface        r8col_sorted_undex
module procedure r8col_sorted_undex
end interface    r8col_sorted_undex
public           r8col_sorted_undex

interface        r8col_sorted_unique
module procedure r8col_sorted_unique
end interface    r8col_sorted_unique
public           r8col_sorted_unique

interface        r8col_sorted_unique_count
module procedure r8col_sorted_unique_count
end interface    r8col_sorted_unique_count
public           r8col_sorted_unique_count

interface        r8col_sortr_a
module procedure r8col_sortr_a
end interface    r8col_sortr_a
public           r8col_sortr_a

interface        r8col_sum
module procedure r8col_sum
end interface    r8col_sum
public           r8col_sum

interface        r8col_swap
module procedure r8col_swap
end interface    r8col_swap
public           r8col_swap

interface        r8col_to_r8vec
module procedure r8col_to_r8vec
end interface    r8col_to_r8vec
public           r8col_to_r8vec

interface        r8col_tol_undex
module procedure r8col_tol_undex
end interface    r8col_tol_undex
public           r8col_tol_undex

interface        r8col_tol_unique_count
module procedure r8col_tol_unique_count
end interface    r8col_tol_unique_count
public           r8col_tol_unique_count

interface        r8col_tol_unique_index
module procedure r8col_tol_unique_index
end interface    r8col_tol_unique_index
public           r8col_tol_unique_index

interface        r8col_undex
module procedure r8col_undex
end interface    r8col_undex
public           r8col_undex

interface        r8col_uniform_abvec
module procedure r8col_uniform_abvec
end interface    r8col_uniform_abvec
public           r8col_uniform_abvec

interface        r8col_unique_count
module procedure r8col_unique_count
end interface    r8col_unique_count
public           r8col_unique_count

interface        r8col_unique_index
module procedure r8col_unique_index
end interface    r8col_unique_index
public           r8col_unique_index

interface        r8col_variance
module procedure r8col_variance
end interface    r8col_variance
public           r8col_variance




contains

subroutine r8col_compare ( m, n, a, i, j, value )

!*****************************************************************************80
!
!! R8COL_COMPARE compares columns in an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Example:
!
!    Input:
!
!      M = 3, N = 4, I = 2, J = 4
!
!      A = (
!        1.  2.  3.  4.
!        5.  6.  7.  8.
!        9. 10. 11. 12. )
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
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the M by N array.
!
!    Input, integer :: I, J, the columns to be compared.
!    I and J must be between 1 and N.
!
!    Output, integer :: VALUE, the results of the comparison:
!    -1, column I < column J,
!     0, column I = column J,
!    +1, column J < column I.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i
  integer :: j
  integer :: k
  integer :: value
!
!  Check.
!
  if ( i < 1 .or. n < i ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8COL_COMPARE - Fatal error!'
    write ( *, '(a)' ) '  Column index I is out of bounds.'
    write ( *, '(a,i8)' ) '  I = ', i
    stop
  end if

  if ( j < 1 .or. n < j ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8COL_COMPARE - Fatal error!'
    write ( *, '(a)' ) '  Column index J is out of bounds.'
    write ( *, '(a,i8)' ) '  J = ', j
    stop
  end if

  value = 0

  if ( i == j ) then
    return
  end if

  k = 1

  do while ( k <= m )

    if ( a(k,i) < a(k,j) ) then
      value = -1
      return
    else if ( a(k,j) < a(k,i) ) then
      value = +1
      return
    end if

    k = k + 1

  end do

  return
end



subroutine r8col_duplicates ( m, n, n_unique, seed, a )

!*****************************************************************************80
!
!! R8COL_DUPLICATES generates an R8COL with some duplicate columns.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    This routine generates a random R8COL with a specified number of
!    duplicate columns.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, the number of rows in each column of A.
!
!    Input, integer :: N, the number of columns in A.
!
!    Input, integer :: N_UNIQUE, the number of unique columns in A.
!    1 <= N_UNIQUE <= N.
!
!    Input/output, integer :: SEED, a seed for the random
!    number generator.
!
!    Output, real(kind=8) :: A(M,N), the array.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i4_uniform_ab
  integer :: j1
  integer :: j2
  integer :: n_unique
  integer :: seed
  real(kind=8) :: temp(m)

  if ( n_unique < 1 .or. n < n_unique ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8COL_DUPLICATES - Fatal error!'
    write ( *, '(a)' ) '  1 <= N_UNIQUE <= N is required.'
    stop
  end if

  call r8mat_uniform_01 ( m, n_unique, seed, a )
!
!  Randomly copy unique columns.
!
  do j1 = n_unique + 1, n
    j2 = i4_uniform_ab ( 1, n_unique, seed )
    a(1:m,j1) = a(1:m,j2)
  end do
!
!  Permute the columns.
!
  do j1 = 1, n
    j2 = i4_uniform_ab ( j1, n, seed )
    temp(1:m) = a(1:m,j1)
    a(1:m,j1) = a(1:m,j2)
    a(1:m,j2) = temp(1:m)
  end do

  return
end



subroutine r8col_find ( m, n, a, x, col )

!*****************************************************************************80
!
!! R8COL_FIND seeks a column value in an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Example:
!
!    Input:
!
!      M = 3,
!      N = 4,
!
!      A = (
!        1.  2.  3.  4.
!        5.  6.  7.  8.
!        9. 10. 11. 12. )
!
!      x = ( 3.,
!            7.,
!           11. )
!
!    Output:
!
!      COL = 3
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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), a table of numbers, regarded as
!    N columns of vectors of length M.
!
!    Input, real(kind=8) :: X(M), a vector to be matched with a column of A.
!
!    Output, integer :: COL, the index of the first column of A
!    which exactly matches every entry of X, or -1 if no match
!    could be found.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: col
  integer :: i
  integer :: j
  real(kind=8) :: x(m)

  col = -1

  do j = 1, n

    col = j

    do i = 1, m
      if ( x(i) /= a(i,j) ) then
        col = -1
        exit
      end if
    end do

    if ( col /= -1 ) then
      return
    end if

  end do

  return
end



subroutine r8col_first_index ( m, n, a, tol, first_index )

!*****************************************************************************80
!
!! R8COL_FIRST_INDEX indexes the first occurrence of values in an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    For element A(1:M,J) of the matrix, FIRST_INDEX(J) is the index in A of
!    the first column whose entries are equal to A(1:M,J).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 November 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns of A.
!    The length of an "element" of A, and the number of "elements".
!
!    Input, real(kind=8) :: A(M,N), the array.
!
!    Input, real(kind=8) :: TOL, a tolerance for equality.
!
!    Output, integer :: FIRST_INDEX(N), the first occurrence index.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: first_index(n)
  integer :: j1
  integer :: j2
  real(kind=8) :: tol

  first_index(1:n) = -1

  do j1 = 1, n

    if ( first_index(j1) == -1 ) then

      first_index(j1) = j1

      do j2 = j1 + 1, n
        if ( maxval ( abs ( a(1:m,j1) - a(1:m,j2) ) ) <= tol ) then
          first_index(j2) = j1
        end if
      end do

    end if

  end do

  return
end



subroutine r8col_insert ( n_max, m, n, a, x, col )

!*****************************************************************************80
!
!! R8COL_INSERT inserts a column into an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Example:
!
!    Input:
!
!      N_MAX = 10,
!      M = 3,
!      N = 4,
!
!      A = (
!        1.  2.  3.  4.
!        5.  6.  7.  8.
!        9. 10. 11. 12. )
!
!      X = ( 3., 4., 18. )
!
!    Output:
!
!      N = 5,
!
!      A = (
!        1.  2.  3.  3.  4.
!        5.  6.  4.  7.  8.
!        9. 10. 18. 11. 12. )
!
!      COL = 3
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
!    Input, integer :: N_MAX, the maximum number of columns in A.
!
!    Input, integer :: M, the number of rows.
!
!    Input/output, integer :: N, the number of columns.
!    If the new column is inserted into the table, then the output
!    value of N will be increased by 1.
!
!    Input/output, real(kind=8) :: A(M,N_MAX), a table of numbers, regarded
!    as an array of columns.  The columns must have been sorted
!    lexicographically.
!
!    Input, real(kind=8) :: X(M), a vector of data which will be inserted
!    into the table if it does not already occur.
!
!    Output, integer :: COL.
!    I, X was inserted into column I.
!    -I, column I was already equal to X.
!    0, N = N_MAX.
!
  implicit none

  integer :: m
  integer :: n_max

  real(kind=8) :: a(m,n_max)
  integer :: col
  integer :: high
  integer :: isgn
  integer :: j
  integer :: low
  integer :: mid
  integer :: n
  real(kind=8) :: x(m)
!
!  Refuse to work if N_MAX <= N.
!
  if ( n_max <= n ) then
    col = 0
    return
  end if
!
!  Stick X temporarily in column N+1, just so it's easy to use R8COL_COMPARE.
!
  a(1:m,n+1) = x(1:m)
!
!  Do a binary search.
!
  low = 1
  high = n

  do

    if ( high < low ) then
      col = low
      exit
    end if

    mid = ( low + high ) / 2

    call r8col_compare ( m, n + 1, a, mid, n + 1, isgn )

    if ( isgn == 0 ) then
      col = -mid
      return
    else if ( isgn == -1 ) then
      low = mid + 1
    else if ( isgn == +1 ) then
      high = mid - 1
    end if

  end do
!
!  Shift part of the table up to make room.
!
  do j = n, col, -1
    a(1:m,j+1) = a(1:m,j)
  end do
!
!  Insert the new column.
!
  a(1:m,col) = x(1:m)

  n = n + 1

  return
end



!> @author John Burkardt
!> @brief  R8COL_MAX returns the maximums in an R8COL.
!> @date   2004-10-10
!> @date   2020-02-06
!> @see    
subroutine     r8col_max (m, n, a, amax)
implicit none
   integer, intent(in) :: m
   integer, intent(in) :: n
   real(kind=8), intent(in) :: a(m,n)
   real(kind=8), intent(out) :: amax(n)
   integer :: j

   amax = 0.0D+0
   do j = 1, n
      amax(j) = maxval (a(1:m,j))
   end do
end subroutine r8col_max

!*****************************************************************************80
!
!! R8COL_MAX returns the maximums in an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the array to be examined.
!
!    Output, real(kind=8) :: AMAX(N), the maximums of the columns.
!



subroutine r8col_max_index ( m, n, a, imax )

!*****************************************************************************80
!
!! R8COL_MAX_INDEX returns the indices of column maximums in an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the array to be examined.
!
!    Output, integer :: IMAX(N); IMAX(I) is the row of A in which
!    the maximum for column I occurs.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: amax
  integer :: i
  integer :: imax(n)
  integer :: j

  do j = 1, n

    imax(j) = 1
    amax = a(1,j)
    do i = 2, m
      if ( amax < a(i,j) ) then
        imax(j) = i
        amax = a(i,j)
      end if
    end do

  end do

  return
end


subroutine r8col_max_one ( m, n, a )

!*****************************************************************************80
!
!! R8COL_MAX_ONE rescales an R8COL so each column maximum is 1.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input/output, real(kind=8) :: A(M,N), the array to be rescaled.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i
  integer :: i_big
  integer :: j

  do j = 1, n

    i_big = 1
    do i = 2, m
      if ( abs ( a(i_big,j) ) < abs ( a(i,j) ) ) then
        i_big = i
      end if
    end do

    if ( a(i_big,j) /= 0.0D+00 ) then
      a(1:m,j) = a(1:m,j) / a(i_big,j)
    end if

  end do

  return
end



subroutine r8col_mean ( m, n, a, mean )

!*****************************************************************************80
!
!! R8COL_MEAN returns the column means of an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Example:
!
!    A =
!      1  2  3
!      2  6  7
!
!    MEAN =
!      1.5  4.0  5.0
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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the array to be examined.
!
!    Output, real(kind=8) :: MEAN(N), the means, or averages, of the columns.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: j
  real(kind=8) :: mean(n)

  do j = 1, n
    mean(j) = sum ( a(1:m,j) )
  end do

  mean(1:n) = mean(1:n) / real ( m, kind = 8  )

  return
end



subroutine r8col_min ( m, n, a, amin )

!*****************************************************************************80
!
!! R8COL_MIN returns the column minimums of an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the array to be examined.
!
!    Output, real(kind=8) :: AMIN(N), the minimums of the columns.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: amin(n)
  integer :: j

  do j = 1, n

    amin(j) = minval ( a(1:m,j) )

  end do

  return
end



subroutine r8col_min_index ( m, n, a, imin )

!*****************************************************************************80
!
!! R8COL_MIN_INDEX returns the indices of column minimums in an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the array to be examined.
!
!    Output, integer :: IMIN(N); IMIN(I) is the row of A in which
!    the minimum for column I occurs.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: amin
  integer :: i
  integer :: imin(n)
  integer :: j

  do j = 1, n

    imin(j) = 1
    amin = a(1,j)
    do i = 2, m
      if ( a(i,j) < amin ) then
        imin(j) = i
        amin = a(i,j)
      end if
    end do

  end do

  return
end



subroutine r8col_normalize_li ( m, n, a )

!*****************************************************************************80
!
!! R8COL_NORMALIZE_LI normalizes an R8COL with the column infinity norm.
!
!  Discussion:
!
!    Each column is scaled so that the entry of maximum norm has the value 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 February 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input/output, real(kind=8) :: A(M,N), the array to be normalized.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: c
  integer :: i
  integer :: j

  do j = 1, n

    c = a(1,j)

    do i = 2, m
      if ( abs ( c ) < abs ( a(i,j) ) ) then
        c = a(i,j)
      end if
    end do

    if ( c /= 0.0D+00 ) then
      a(1:m,j) = a(1:m,j) / c
    end if

  end do

  return
end



!*****************************************************************************80
!
!! R8COL_PART_QUICK_A reorders the columns of an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    The routine reorders the columns of A.  Using A(1:M,1) as a
!    key, all entries of A that are less than or equal to the key will
!    precede the key, which precedes all entries that are greater than the key.
!
!  Example:
!
!    Input:
!
!      M = 2, N = 8
!      A = ( 2  8  6  0 10 10  0  5
!            4  8  2  2  6  0  6  8 )
!
!    Output:
!
!      L = 2, R = 4
!
!      A = (  0  0  2  8  6 10 10  5
!             2  6  4  8  2  6  0  8 )
!             ----     -------------
!             LEFT KEY     RIGHT
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
!    Input, integer :: M, the row dimension of A, and the length of
!    a column.
!
!    Input, integer :: N, the column dimension of A.
!
!    Input/output, real(kind=8) :: A(M,N).  On input, the array to be checked.
!    On output, A has been reordered as described above.
!
!    Output, integer :: L, R, the indices of A that define the three
!    segments.  Let KEY = the input value of A(1:M,1).  Then
!    I <= L                 A(1:M,I) < KEY;
!         L < I < R         A(1:M,I) = KEY;
!                 R <= I    KEY < A(1:M,I).
!
subroutine r8col_part_quick_a ( m, n, a, l, r )
use jburk_r8lib_r8vec_, only: r8vec_swap
use jburk_r8lib_r8vec_, only: r8vec_eq
use jburk_r8lib_r8vec_, only: r8vec_gt
use jburk_r8lib_r8vec_, only: r8vec_lt
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: j
  integer :: k
  real(kind=8) :: key(m)
  integer :: l
  integer :: r

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8COL_PART_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    return
  end if

  if ( n == 1 ) then
    l = 0
    r = 2
    return
  end if

  key(1:m) = a(1:m,1)
  k = 1
!
!  The elements of unknown size have indices between L+1 and R-1.
!
  l = 1
  r = n + 1

  do j = 2, n

    if ( r8vec_gt ( m, a(1:m,l+1), key(1:m) ) ) then
      r = r - 1
      call r8vec_swap ( m, a(1:m,r), a(1:m,l+1) )
    else if ( r8vec_eq ( m, a(1:m,l+1), key(1:m) ) ) then
      k = k + 1
      call r8vec_swap ( m, a(1:m,k), a(1:m,l+1) )
      l = l + 1
    else if ( r8vec_lt ( m, a(1:m,l+1), key(1:m) ) ) then
      l = l + 1
    end if

  end do
!
!  Shift small elements to the left.
!
  do j = 1, l - k
    a(1:m,j) = a(1:m,j+k)
  end do
!
!  Shift KEY elements to center.
!
  do j = l - k + 1, l
    a(1:m,j) = key(1:m)
  end do
!
!  Update L.
!
  l = l - k

  return
end



!*****************************************************************************80
!
!! R8COL_PERMUTE permutes an R8COL in place.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
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
!      M = 2
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
!    09 December 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, the dimension of objects.
!
!    Input, integer :: N, the number of objects.
!
!    Input, integer :: P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.
!
!    Input/output, real(kind=8) :: A(M,N), the array to be permuted.
!
subroutine r8col_permute ( m, n, p, a )
use jburk_r8lib_i4vec_, only: perm_check
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: a_temp(m)
  integer ::  parameter :: base = 1
  integer :: ierror
  integer :: iget
  integer :: iput
  integer :: istart
  integer :: p(n)

  call perm_check ( n, p, base, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8COL_PERMUTE - Fatal error!'
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

      a_temp(1:m) = a(1:m,istart)
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
          write ( *, '(a)' ) 'R8COL_PERMUTE - Fatal error!'
          write ( *, '(a)' ) '  A permutation index is out of range.'
          write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
          stop
        end if

        if ( iget == istart ) then
          a(1:m,iput) = a_temp(1:m)
          exit
        end if

        a(1:m,iput) = a(1:m,iget)

      end do

    end if

  end do
!
!  Restore the signs of the entries.
!
  p(1:n) = - p(1:n)

  return
end



subroutine r8col_sort_heap_a ( m, n, a )

!*****************************************************************************80
!
!! R8COL_SORT_HEAP_A ascending heapsorts an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
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
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input/output, real(kind=8) :: A(M,N).
!    On input, the array of N columns of M-vectors.
!    On output, the columns of A have been sorted in lexicographic order.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i
  integer :: indx
  integer :: isgn
  integer :: j

  if ( m <= 0 ) then
    return
  end if

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

      call r8col_swap ( m, n, a, i, j )
!
!  Compare the I and J objects.
!
    else if ( indx < 0 ) then

      call r8col_compare ( m, n, a, i, j, isgn )

    else if ( indx == 0 ) then

      exit

    end if

  end do

  return
end



!*****************************************************************************80
!
!! R8COL_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    A(*,J1) < A(*,J2) if the first nonzero entry of A(*,J1)-A(*,J2)
!    is negative.
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      A(*,INDX(*)) is sorted,
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
!    Input, integer :: M, the number of rows in each column of A.
!
!    Input, integer :: N, the number of columns in A.
!
!    Input, real(kind=8) :: A(M,N), the array.
!
!    Output, integer :: INDX(N), the sort index.  The I-th element
!    of the sorted array is column INDX(I).
!
subroutine r8col_sort_heap_index_a ( m, n, a, indx )
use jburk_r8lib_r8vec_, only: r8vec_compare
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: column(m)
  integer :: i
  integer :: indx(n)
  integer :: indxt
  integer :: ir
  integer :: isgn
  integer :: j
  integer :: l

  if ( n < 1 ) then
    return
  end if

  do i = 1, n
    indx(i) = i
  end do

  if ( n == 1 ) then
    return
  end if

  l = ( n / 2 ) + 1
  ir = n

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      column(1:m) = a(1:m,indxt)

    else

      indxt = indx(ir)
      column(1:m) = a(1:m,indxt)
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

        call r8vec_compare ( m, a(1:m,indx(j)), a(1:m,indx(j+1)), isgn )

        if ( isgn < 0 ) then
          j = j + 1
        end if

      end if

      call r8vec_compare ( m, column, a(1:m,indx(j)), isgn )

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



subroutine r8col_sort_quick_a ( m, n, a )

!*****************************************************************************80
!
!! R8COL_SORT_QUICK_A ascending quick sorts an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
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
!    Input, integer :: M, the row order of A, and the length of
!    a column.
!
!    Input, integer :: N, the number of columns of A.
!
!    Input/output, real(kind=8) :: A(M,N).
!    On input, the array to be sorted.
!    On output, the array has been sorted.
!
  implicit none

  integer ::  parameter :: level_max = 30
  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: base
  integer :: l_segment
  integer :: level
  integer :: n_segment
  integer :: rsave(level_max)
  integer :: r_segment

  if ( m <= 0 ) then
    return
  end if

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8COL_SORT_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    write ( *, '(a,i8)' ) '  N = ', n
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
    call r8col_part_quick_a ( m, n_segment, a(1:m,base:base+n_segment-1), &
      l_segment, r_segment )
!
!  If the left segment has more than one element, we need to partition it.
!
    if ( 1 < l_segment ) then

      if ( level_max < level ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8COL_SORT_QUICK_A - Fatal error!'
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



subroutine r8col_sorted_tol_undex ( m, n, a, unique_num, tol, undx, xdnu )

!*****************************************************************************80
!
!! R8COL_SORTED_TOL_UNDEX indexes tolerably unique entries in a sorted R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    The goal of this routine is to determine a vector UNDX,
!    which points, to the tolerably unique elements of A, in sorted order,
!    and a vector XDNU, which identifies, for each entry of A, the index of
!    the unique sorted element of A.
!
!    This is all done with index vectors, so that the elements of
!    A are never moved.
!
!    Assuming A is already sorted, we examine the entries of A in order,
!    noting the unique entries, creating the entries of XDNU and
!    UNDX as we go.
!
!    Once this process has been completed, the vector A could be
!    replaced by a compressed vector XU, containing the unique entries
!    of A in sorted order, using the formula
!
!      XU(*) = A(UNDX(*)).
!
!    We could then, if we wished, reconstruct the entire vector A, or
!    any element of it, by index, as follows:
!
!      A(I) = XU(XDNU(I)).
!
!    We could then replace A by the combination of XU and XDNU.
!
!    Later, when we need the I-th entry of A, we can locate it as
!    the XDNU(I)-th entry of XU.
!
!    Here is an example of a vector A, the unique sort and
!    inverse unique sort vectors and the compressed unique sorted vector.
!
!      I      A      XU  Undx  Xdnu
!    ----+------+------+-----+-----+
!      1 | 11.0 |  11.0    1     1
!      2 | 11.0 |  22.0    5     1
!      3 | 11.0 |  33.0    8     1
!      4 | 11.0 |  55.0    9     1
!      5 | 22.0 |                2
!      6 | 22.0 |                2
!      7 | 22.0 |                2
!      8 | 33.0 |                3
!      9 | 55.0 |                4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, the dimension of the data values.
!
!    Input, integer :: N, the number of data values.
!
!    Input, real(kind=8) :: A(M,N), the data values.
!
!    Input, integer :: UNIQUE_NUM, the number of unique values
!    in A.  This value is only required for languages in which the size of
!    UNDX must be known in advance.
!
!    Input, real(kind=8) :: TOL, a tolerance for equality.
!
!    Output, integer :: UNDX(UNIQUE_NUM), the UNDX vector.
!
!    Output, integer :: XDNU(N), the XDNU vector.
!
  implicit none

  integer :: m
  integer :: n
  integer :: unique_num

  real(kind=8) :: a(m,n)
  real(kind=8) :: diff
  integer :: i
  integer :: i2
  integer :: j
  integer :: k
  real(kind=8) :: tol
  integer :: undx(unique_num)
  logical unique
  integer :: xdnu(n)
!
!  Consider entry I = 1.
!  It is unique, so set the number of unique items to K.
!  Set the K-th unique item to I.
!  Set the representative of item I to the K-th unique item.
!
  i = 1
  k = 1
  undx(k) = i
  xdnu(i) = k
!
!  Consider entry I.
!
!  If it is unique, increase the unique count K, set the
!  K-th unique item to I, and set the representative of I to K.
!
!  If it is not unique, set the representative of item I to a
!  previously determined unique item that is close to it.
!
  do i = 2, n

    unique = .true.

    do j = 1, k
      i2 = undx(j)
      diff = maxval ( abs ( a(1:m,i) - a(1:m,i2) ) )
      if ( diff <= tol ) then
        unique = .false.
        xdnu(i) = j
        exit
      end if
    end do

    if ( unique ) then
      k = k + 1
      undx(k) = i
      xdnu(i) = k
    end if

  end do

  return
end



subroutine r8col_sorted_tol_unique ( m, n, a, tol, unique_num )

!*****************************************************************************80
!
!! R8COL_SORTED_TOL_UNIQUE keeps tolerably unique elements in a sorted R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    The columns of the array may be ascending or descending sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input/output, real(kind=8) :: A(M,N).
!    On input, the sorted array of N columns of M-vectors.
!    On output, a sorted array of columns of M-vectors.
!
!    Input, real(kind=8) :: TOL, a tolerance for equality.
!
!    Output, integer :: UNIQUE_NUM, the number of unique columns.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: diff
  integer :: i
  integer :: j
  real(kind=8) :: tol
  logical unique
  integer :: unique_num

  if ( n <= 0 ) then
    unique_num = 0
    return
  end if

  unique_num = 1

  do i = 2, n

    unique = .true.

    do j = 1, unique_num
      diff = maxval ( abs ( a(1:m,j) - a(1:m,i) ) )
      if ( diff <= tol ) then
        unique = .false.
        exit
      end if
    end do

    if ( unique ) then
      unique_num = unique_num + 1
      a(1:m,unique_num) = a(1:m,i)
    end if

  end do

  return
end



subroutine r8col_sorted_tol_unique_count ( m, n, a, tol, unique_num )

!*****************************************************************************80
!
!! R8COL_SORTED_TOL_UNIQUE_COUNT: tolerably unique elements in a sorted R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    The columns of the array may be ascending or descending sorted.
!
!    If the tolerance is large enough, then the concept of uniqueness
!    can become ambiguous.  If we have a tolerance of 1.5, then in the
!    list ( 1, 2, 3, 4, 5, 6, 7, 8, 9 ) is it fair to say we have only
!    one unique entry?  That would be because 1 may be regarded as unique,
!    and then 2 is too close to 1 to be unique, and 3 is too close to 2 to
!    be unique and so on.
!
!    This seems wrongheaded.  So I prefer the idea that an item is not
!    unique under a tolerance only if it is close to something that IS unique.
!    Thus, the unique items are guaranteed to cover the space if we include
!    a disk of radius TOL around each one.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), a sorted array, containing
!    N columns of data.
!
!    Input, real(kind=8) :: TOL, a tolerance for equality.
!
!    Output, integer :: UNIQUE_NUM, the number of unique columns.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: diff
  integer :: i
  integer :: i2
  integer :: j
  integer :: k
  real(kind=8) :: tol
  integer :: undx(n)
  logical unique
  integer unique_num
!
!  Consider entry I = 1.
!  It is unique, so set the number of unique items to K.
!  Set the K-th unique item to I.
!
  i = 1
  k = 1
  undx(k) = i
!
!  Consider entry I.
!
!  If it is unique, increase the unique count K and set the
!  K-th unique item to I.
!
  do i = 2, n

    unique = .true.

    do j = 1, k
      i2 = undx(j)
      diff = maxval ( abs ( a(1:m,i) - a(1:m,i2) ) )
      if ( diff <= tol ) then
        unique = .false.
        exit
      end if
    end do

    if ( unique ) then
      k = k + 1
      undx(k) = i
    end if

  end do

  unique_num = k

  return
end



subroutine r8col_sorted_undex ( m, n, a, unique_num, undx, xdnu )

!*****************************************************************************80
!
!! R8COL_SORTED_UNDEX returns unique sorted indexes for a sorted R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    The goal of this routine is to determine a vector UNDX,
!    which points, to the unique elements of A, in sorted order,
!    and a vector XDNU, which identifies, for each entry of A, the index of
!    the unique sorted element of A.
!
!    This is all done with index vectors, so that the elements of
!    A are never moved.
!
!    Assuming A is already sorted, we examine the entries of A in order,
!    noting the unique entries, creating the entries of XDNU and
!    UNDX as we go.
!
!    Once this process has been completed, the vector A could be
!    replaced by a compressed vector XU, containing the unique entries
!    of A in sorted order, using the formula
!
!      XU(*) = A(UNDX(*)).
!
!    We could then, if we wished, reconstruct the entire vector A, or
!    any element of it, by index, as follows:
!
!      A(I) = XU(XDNU(I)).
!
!    We could then replace A by the combination of XU and XDNU.
!
!    Later, when we need the I-th entry of A, we can locate it as
!    the XDNU(I)-th entry of XU.
!
!    Here is an example of a vector A, the sort and inverse sort
!    index vectors, and the unique sort and inverse unique sort vectors
!    and the compressed unique sorted vector.
!
!      I      A      XU  Undx  Xdnu
!    ----+------+------+-----+-----+
!      1 | 11.0 |  11.0    1     1
!      2 | 11.0 |  22.0    5     1
!      3 | 11.0 |  33.0    8     1
!      4 | 11.0 |  55.0    9     1
!      5 | 22.0 |                2
!      6 | 22.0 |                2
!      7 | 22.0 |                2
!      8 | 33.0 |                3
!      9 | 55.0 |                4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, the dimension of the data values.
!
!    Input, integer :: N, the number of data values.
!
!    Input, real(kind=8) :: AL(M,N), the data values.
!
!    Input, integer :: UNIQUE_NUM, the number of unique values
!    in A.  This value is only required for languages in which the size of
!    UNDX must be known in advance.
!
!    Output, integer :: UNDX(UNIQUE_NUM), the UNDX vector.
!
!    Output, integer :: XDNU(N), the XDNU vector.
!
  implicit none

  integer :: m
  integer :: n
  integer :: unique_num

  real(kind=8) :: a(m,n)
  integer :: i
  integer :: j
  integer :: undx(unique_num)
  integer :: xdnu(n)
!
!  Walk through the sorted array.
!
  i = 1
  j = 1
  undx(j) = i
  xdnu(i) = j

  do i = 2, n

    if ( any ( a(1:m,i) /= a(1:m,j) ) ) then
      j = j + 1
      undx(j) = i
    end if

    xdnu(i) = j

  end do

  return
end



subroutine r8col_sorted_unique ( m, n, a, unique_num )

!*****************************************************************************80
!
!! R8COL_SORTED_UNIQUE keeps unique elements in a sorted R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    The columns of the array may be ascending or descending sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input/output, real(kind=8) :: A(M,N).
!    On input, the sorted array of N columns of M-vectors.
!    On output, a sorted array of columns of M-vectors.
!
!    Output, integer :: UNIQUE_NUM, the number of unique columns.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: j1
  integer :: j2
  integer :: unique_num

  if ( n <= 0 ) then
    unique_num = 0
    return
  end if

  j1 = 1

  do j2 = 2, n

    if ( any ( a(1:m,j1) /= a(1:m,j2) ) ) then
      j1 = j1 + 1
      a(1:m,j1) = a(1:m,j2)
    end if

  end do

  unique_num = j1

  return
end



subroutine r8col_sorted_unique_count ( m, n, a, unique_num )

!*****************************************************************************80
!
!! R8COL_SORTED_UNIQUE_COUNT counts unique elements in a sorted R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    The columns of the array may be ascending or descending sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), a sorted array, containing
!    N columns of data.
!
!    Output, integer :: UNIQUE_NUM, the number of unique columns.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: j1
  integer :: j2
  integer :: unique_num

  unique_num = 0

  if ( n <= 0 ) then
    return
  end if

  unique_num = 1
  j1 = 1

  do j2 = 2, n

    if ( any ( a(1:m,j1) /= a(1:m,j2) ) ) then
      unique_num = unique_num + 1
      j1 = j2
    end if

  end do

  return
end



!*****************************************************************************80
!
!! R8COL_SORTR_A ascending sorts one column of an R8COL, adjusting all columns.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input/output, real(kind=8) :: A(M,N).
!    On input, an unsorted M by N array.
!    On output, rows of the array have been shifted in such
!    a way that column KEY of the array is in nondecreasing order.
!
!    Input, integer :: KEY, the column in which the "key" value
!    is stored.  On output, column KEY of the array will be
!    in nondecreasing order.
!
subroutine r8col_sortr_a ( m, n, a, key )
use jburk_r8lib_r8row_, only: r8row_swap
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i
  integer :: indx
  integer :: isgn
  integer :: j
  integer :: key

  if ( m <= 0 ) then
    return
  end if

  if ( key < 1 .or. n < key ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8COL_SORTR_A - Fatal error!'
    write ( *, '(a)' ) '  The value of KEY is not a legal column index.'
    write ( *, '(a,i8)' ) '  KEY = ', key
    write ( *, '(a,i8)' ) '  N = ', n
    stop
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

      if ( a(i,key) < a(j,key) ) then
        isgn = -1
      else
        isgn = +1
      end if

    else if ( indx == 0 ) then

      exit

    end if

  end do

  return
end



subroutine r8col_sum ( m, n, a, colsum )

!*****************************************************************************80
!
!! R8COL_SUM sums the columns of an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the array to be examined.
!
!    Output, real(kind=8) :: COLSUM(N), the sums of the columns.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: colsum(n)
  integer :: j

  do j = 1, n
    colsum(j) = sum ( a(1:m,j) )
  end do

  return
end



subroutine r8col_swap ( m, n, a, j1, j2 )

!*****************************************************************************80
!
!! R8COL_SWAP swaps columns I and J of an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!  Example:
!
!    Input:
!
!      M = 3, N = 4, J1 = 2, J2 = 4
!
!      A = (
!        1.  2.  3.  4.
!        5.  6.  7.  8.
!        9. 10. 11. 12. )
!
!    Output:
!
!      A = (
!        1.  4.  3.  2.
!        5.  8.  7.  6.
!        9. 12. 11. 10. )
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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input/output, real(kind=8) :: A(M,N), the M by N array.
!
!    Input, integer :: J1, J2, the columns to be swapped.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: col(m)
  integer :: j1
  integer :: j2

  if ( j1 < 1 .or. n < j1 .or. j2 < 1 .or. n < j2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8COL_SWAP - Fatal error!'
    write ( *, '(a)' ) '  J1 or J2 is out of bounds.'
    write ( *, '(a,i8)' ) '  J1 =    ', j1
    write ( *, '(a,i8)' ) '  J2 =    ', j2
    write ( *, '(a,i8)' ) '  NCOL = ', n
    stop
  end if

  if ( j1 == j2 ) then
    return
  end if

  col(1:m) = a(1:m,j1)
  a(1:m,j1) = a(1:m,j2)
  a(1:m,j2) = col(1:m)

  return
end



subroutine r8col_to_r8vec ( m, n, a, x )

!*****************************************************************************80
!
!! R8COL_TO_R8VEC converts an R8COL to an R8VEC.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    An R8VEC is a vector of R8's.
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
!    X = ( 11, 21, 31, 12, 22, 32, 13, 23, 33, 14, 24, 34 )
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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the array.
!
!    Output, real(kind=8) :: X(M*N), a vector containing the N columns of A.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: j
  integer :: k
  real(kind=8) :: x(m*n)

  k = 1
  do j = 1, n
    x(k:k+m-1) = a(1:m,j)
    k = k + m
  end do

  return
end



!*****************************************************************************80
!
!! R8COL_TOL_UNDEX indexes tolerably unique entries of an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    The goal of this routine is to determine a vector UNDX,
!    which points, to the unique elements of A, in sorted order,
!    and a vector XDNU, which identifies, for each entry of A, the index of
!    the unique sorted element of A.
!
!    This is all done with index vectors, so that the elements of
!    A are never moved.
!
!    The first step of the algorithm requires the indexed sorting
!    of A, which creates arrays INDX and XDNI.  (If all the entries
!    of A are unique, then these arrays are the same as UNDX and XDNU.)
!
!    We then use INDX to examine the entries of A in sorted order,
!    noting the unique entries, creating the entries of XDNU and
!    UNDX as we go.
!
!    Once this process has been completed, the object X could be
!    replaced by a compressed object XU, containing the unique entries
!    of X in sorted order, using the formula
!
!      XU(*) = A(UNDX(*)).
!
!    We could then, if we wished, reconstruct the entire vector A, or
!    any element of it, by index, as follows:
!
!      A(I) = XU(XDNU(I)).
!
!    We could then replace A by the combination of XU and XDNU.
!
!    Later, when we need the I-th entry of A, we can locate it as
!    the XDNU(I)-th entry of XU.
!
!    Here is an example of a vector A, the sort and inverse sort
!    index vectors, and the unique sort and inverse unique sort vectors
!    and the compressed unique sorted vector.
!
!      I    A   Indx  Xdni      XU   Undx  Xdnu
!    ----+-----+-----+-----+--------+-----+-----+
!      1 | 11.     1     1 |    11.     1     1
!      2 | 22.     3     5 |    22.     2     2
!      3 | 11.     6     2 |    33.     4     1
!      4 | 33.     9     8 |    55.     5     3
!      5 | 55.     2     9 |                  4
!      6 | 11.     7     3 |                  1
!      7 | 22.     8     6 |                  2
!      8 | 22.     4     7 |                  2
!      9 | 11.     5     4 |                  1
!
!    INDX(2) = 3 means that sorted item(2) is A(3).
!    XDNI(2) = 5 means that A(2) is sorted item(5).
!
!    UNDX(3) = 4 means that unique sorted item(3) is at A(4).
!    XDNU(8) = 2 means that A(8) is at unique sorted item(2).
!
!    XU(XDNU(I))) = A(I).
!    XU(I)        = A(UNDX(I)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, the dimension of the data values.
!
!    Input, integer :: N, the number of data values.
!
!    Input, real(kind=8) :: A(M,N), the data values.
!
!    Input, integer :: UNIQUE_NUM, the number of unique values
!    in A.  This value is only required for languages in which the size of
!    UNDX must be known in advance.
!
!    Input, real(kind=8) :: TOL, a tolerance for equality.
!
!    Output, integer :: UNDX(UNIQUE_NUM), the UNDX vector.
!
!    Output, integer :: XDNU(N), the XDNU vector.
!
subroutine r8col_tol_undex ( m, n, a, unique_num, tol, undx, xdnu )
  implicit none

  integer :: m
  integer :: n
  integer :: unique_num

  real(kind=8) :: a(m,n)
  real(kind=8) :: diff
  integer :: i
  integer :: indx(n)
  integer :: j
  integer :: k
  real(kind=8) :: tol
  integer :: undx(unique_num)
  logical unique
  integer :: xdnu(n)
!
!  Implicitly sort the array.
!
  call r8col_sort_heap_index_a ( m, n, a, indx )
!
!  Consider entry I = 1.
!  It is unique, so set the number of unique items to K.
!  Set the K-th unique item to I.
!  Set the representative of item I to the K-th unique item.
!
  i = 1
  k = 1
  undx(k) = indx(i)
  xdnu(indx(i)) = k
!
!  Consider entry I.
!
!  If it is unique, increase the unique count K, set the
!  K-th unique item to I, and set the representative of I to K.
!
!  If it is not unique, set the representative of item I to a
!  previously determined unique item that is close to it.
!
  do i = 2, n

    unique = .true.

    do j = 1, k
      diff = maxval ( abs ( a(1:m,indx(i)) - a(1:m,undx(j)) ) )
      if ( diff <= tol ) then
        unique = .false.
        xdnu(indx(i)) = j
        exit
      end if
    end do

    if ( unique ) then
      k = k + 1
      undx(k) = indx(i)
      xdnu(indx(i)) = k
    end if

  end do

  return
end



subroutine r8col_tol_unique_count ( m, n, a, tol, unique_num )

!*****************************************************************************80
!
!! R8COL_TOL_UNIQUE_COUNT counts tolerably unique entries in an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    If the tolerance is large enough, then the concept of uniqueness
!    can become ambiguous.  If we have a tolerance of 1.5, then in the
!    list ( 1, 2, 3, 4, 5, 6, 7, 8, 9 ) is it fair to say we have only
!    one unique entry?  That would be because 1 may be regarded as unique,
!    and then 2 is too close to 1 to be unique, and 3 is too close to 2 to
!    be unique and so on.
!
!    This seems wrongheaded.  So I prefer the idea that an item is not
!    unique under a tolerance only if it is close to something that IS unique.
!    Thus, the unique items are guaranteed to cover the space if we include
!    a disk of radius TOL around each one.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, the number of rows.
!
!    Input, integer :: N, the number of columns.
!
!    Input, real(kind=8) :: A(M,N), the array of N columns of data.
!
!    Input, real(kind=8) :: TOL, a nonnegative tolerance for equality.
!
!    Output, integer :: UNIQUE_NUM, the number of unique columns.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: diff
  integer :: i
  integer :: indx(n)
  integer :: j
  integer :: k
  real(kind=8) :: tol
  integer :: undx(n)
  logical unique
  integer :: unique_num
!
!  Implicitly sort the array.
!
  call r8col_sort_heap_index_a ( m, n, a, indx )
!
!  Consider entry I = 1.
!  It is unique, so set the number of unique items to K.
!  Set the K-th unique item to I.
!  Set the representative of item I to the K-th unique item.
!
  i = 1
  k = 1
  undx(k) = indx(i)
!
!  Consider entry I.
!
!  If it is unique, increase the unique count K, set the
!  K-th unique item to I, and set the representative of I to K.
!
!  If it is not unique, set the representative of item I to a
!  previously determined unique item that is close to it.
!
  do i = 2, n

    unique = .true.

    do j = 1, k
      diff = maxval ( abs ( a(1:m,indx(i)) - a(1:m,undx(j)) ) )
      if ( diff <= tol ) then
        unique = .false.
        exit
      end if
    end do

    if ( unique ) then
      k = k + 1
      undx(k) = indx(i)
    end if

  end do

  unique_num = k

  return
end



subroutine r8col_tol_unique_index ( m, n, a, tol, unique_index )

!*****************************************************************************80
!
!! R8COL_TOL_UNIQUE_INDEX indexes tolerably unique entries in an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    For element A(1:M,J) of the matrix, UNIQUE_INDEX(J) is the uniqueness index
!    of A(1:M,J).  That is, if A_UNIQUE contains the unique elements of A,
!    gathered in order, then
!
!      A_UNIQUE ( 1:M, UNIQUE_INDEX(J) ) = A(1:M,J)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns of A.
!
!    Input, real(kind=8) :: A(M,N), the array.
!
!    Input, real(kind=8) :: TOL, a tolerance for equality.
!
!    Output, integer :: UNIQUE_INDEX(N), the first occurrence index.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: diff
  integer :: j1
  integer :: j2
  real(kind=8) :: tol
  integer :: unique_index(n)
  integer :: unique_num

  unique_index(1:n) = -1
  unique_num = 0

  do j1 = 1, n

    if ( unique_index(j1) == -1 ) then

      unique_num = unique_num + 1
      unique_index(j1) = unique_num

      do j2 = j1 + 1, n
        diff = maxval ( abs ( a(1:m,j1) - a(1:m,j2) ) )
        if ( diff <= tol ) then
          unique_index(j2) = unique_num
        end if
      end do

    end if

  end do

  return
end



subroutine r8col_undex ( m, n, a, unique_num, undx, xdnu )

!*****************************************************************************80
!
!! R8COL_UNDEX returns unique sorted indexes for an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    The goal of this routine is to determine a vector UNDX,
!    which points, to the unique elements of A, in sorted order,
!    and a vector XDNU, which identifies, for each entry of A, the index of
!    the unique sorted element of A.
!
!    This is all done with index vectors, so that the elements of
!    A are never moved.
!
!    The first step of the algorithm requires the indexed sorting
!    of A, which creates arrays INDX and XDNI.  (If all the entries
!    of A are unique, then these arrays are the same as UNDX and XDNU.)
!
!    We then use INDX to examine the entries of A in sorted order,
!    noting the unique entries, creating the entries of XDNU and
!    UNDX as we go.
!
!    Once this process has been completed, the object X could be
!    replaced by a compressed object XU, containing the unique entries
!    of X in sorted order, using the formula
!
!      XU(*) = A(UNDX(*)).
!
!    We could then, if we wished, reconstruct the entire vector A, or
!    any element of it, by index, as follows:
!
!      A(I) = XU(XDNU(I)).
!
!    We could then replace A by the combination of XU and XDNU.
!
!    Later, when we need the I-th entry of A, we can locate it as
!    the XDNU(I)-th entry of XU.
!
!    Here is an example of a vector A, the sort and inverse sort
!    index vectors, and the unique sort and inverse unique sort vectors
!    and the compressed unique sorted vector.
!
!      I    A   Indx  Xdni      XU   Undx  Xdnu
!    ----+-----+-----+-----+--------+-----+-----+
!      1 | 11.     1     1 |    11.     1     1
!      2 | 22.     3     5 |    22.     2     2
!      3 | 11.     6     2 |    33.     4     1
!      4 | 33.     9     8 |    55.     5     3
!      5 | 55.     2     9 |                  4
!      6 | 11.     7     3 |                  1
!      7 | 22.     8     6 |                  2
!      8 | 22.     4     7 |                  2
!      9 | 11.     5     4 |                  1
!
!    INDX(2) = 3 means that sorted item(2) is A(3).
!    XDNI(2) = 5 means that A(2) is sorted item(5).
!
!    UNDX(3) = 4 means that unique sorted item(3) is at A(4).
!    XDNU(8) = 2 means that A(8) is at unique sorted item(2).
!
!    XU(XDNU(I))) = A(I).
!    XU(I)        = A(UNDX(I)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, the dimension of the data values.
!
!    Input, integer :: N, the number of data values.
!
!    Input, real(kind=8) :: A(M,N), the data values.
!
!    Input, integer :: UNIQUE_NUM, the number of unique values
!    in A.  This value is only required for languages in which the size of
!    UNDX must be known in advance.
!
!    Output, integer :: UNDX(UNIQUE_NUM), the UNDX vector.
!
!    Output, integer :: XDNU(N), the XDNU vector.
!
  implicit none

  integer :: m
  integer :: n
  integer :: unique_num

  real(kind=8) :: a(m,n)
  real(kind=8) :: diff
  integer :: i
  integer :: indx(n)
  integer :: j
  integer :: undx(unique_num)
  integer :: xdnu(n)
!
!  Implicitly sort the array.
!
  call r8col_sort_heap_index_a ( m, n, a, indx )
!
!  Walk through the implicitly sorted array.
!
  i = 1
  j = 1
  undx(j) = indx(i)
  xdnu(indx(i)) = j

  do i = 2, n

    diff = maxval ( abs ( a(1:m,indx(i)) - a(1:m,undx(j)) ) )

    if ( 0.0D+00 < diff ) then
      j = j + 1
      undx(j) = indx(i)
    end if

    xdnu(indx(i)) = j

  end do

  return
end



subroutine r8col_uniform_abvec ( m, n, a, b, seed, r )

!*****************************************************************************80
!
!! R8COL_UNIFORM_ABVEC fills an R8COL with scaled pseudorandom numbers.
!
!  Discussion:
!
!    An R8COL is an array of R8 values, regarded as a set of column vectors.
!
!    The user specifies a minimum and maximum value for each row.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 2011
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
!    Input, integer :: M, N, the number of rows and columns in
!    the array.
!
!    Input, real(kind=8) :: A(M), B(M), the lower and upper limits.
!
!    Input/output, integer :: SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real(kind=8) :: R(M,N), the array of pseudorandom values.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m)
  real(kind=8) :: b(m)
  integer :: i
  integer ::  parameter :: i4_huge = 2147483647
  integer :: j
  integer :: k
  integer :: seed
  real(kind=8) :: r(m,n)

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = a(i) &
        + ( b(i) - a(i) ) * real ( seed, kind = 8 ) * 4.656612875D-10

    end do
  end do

  return
end



subroutine r8col_unique_count ( m, n, a, unique_num )

!*****************************************************************************80
!
!! R8COL_UNIQUE_COUNT counts the unique columns in an unsorted R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    Because the array is unsorted, this algorithm is O(N^2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, the number of rows.
!
!    Input, integer :: N, the number of columns.
!
!    Input, real(kind=8) :: A(M,N), the array of N columns of data.
!
!    Input, real(kind=8) :: TOL, a nonnegative tolerance for equality.
!
!    Output, integer :: UNIQUE_NUM, the number of unique columns.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: diff
  integer :: j1
  integer :: j2
  logical unique(n)
  integer :: unique_num

  unique_num = 0

  do j1 = 1, n

    unique_num = unique_num + 1
    unique(j1) = .true.

    do j2 = 1, j1 - 1

      if ( unique(j2) ) then
        diff = maxval ( abs ( a(1:m,j1) - a(1:m,j2) ) )
        if ( diff == 0.0D+00 ) then
          unique_num = unique_num - 1
          unique(j1) = .false.
          exit
        end if
      end if

    end do

  end do

  return
end



subroutine r8col_unique_index ( m, n, a, unique_index )

!*****************************************************************************80
!
!! R8COL_UNIQUE_INDEX indexes the unique occurrence of values in an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
!
!    For element A(1:M,J) of the matrix, UNIQUE_INDEX(J) is the uniqueness index
!    of A(1:M,J).  That is, if A_UNIQUE contains the unique elements of A,
!    gathered in order, then
!
!      A_UNIQUE ( 1:M, UNIQUE_INDEX(J) ) = A(1:M,J)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer :: M, N, the number of rows and columns of A.
!    The length of an "element" of A, and the number of "elements".
!
!    Input, real(kind=8) :: A(M,N), the array.
!
!    Output, integer :: UNIQUE_INDEX(N), the first occurrence index.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: diff
  integer :: j1
  integer :: j2
  integer :: unique_index(n)
  integer :: unique_num

  unique_index(1:n) = -1
  unique_num = 0

  do j1 = 1, n

    if ( unique_index(j1) == -1 ) then

      unique_num = unique_num + 1
      unique_index(j1) = unique_num

      do j2 = j1 + 1, n
        diff = maxval ( abs ( a(1:m,j1) - a(1:m,j2) ) )
        if ( diff == 0.0D+00 ) then
          unique_index(j2) = unique_num
        end if
      end do

    end if

  end do

  return
end


subroutine     r8col_variance ( m, n, a, variance )

!*****************************************************************************80
!
!! R8COL_VARIANCE returns the variances of an R8COL.
!
!  Discussion:
!
!    An R8COL is an M by N array of R8's, regarded as an array of N columns,
!    each of length M.
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
!    Input, integer :: M, N, the number of rows and columns in
!    the array.
!
!    Input, real(kind=8) :: A(M,N), the array whose variances are desired.
!
!    Output, real(kind=8) :: VARIANCE(N), the variances of the rows.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i
  integer :: j
  real(kind=8) :: mean
  real(kind=8) :: variance(n)

  do j = 1, n

    mean = sum ( a(1:m,j) ) / real ( m, kind = 8  )

    variance(j) = 0.0D+00
    do i = 1, m
      variance(j) = variance(j) + ( a(i,j) - mean )**2
    end do

    if ( 1 < m ) then
      variance(j) = variance(j) / real ( m - 1, kind = 8 )
    else
      variance(j) = 0.0D+00
    end if

  end do

  return
end subroutine r8col_variance

end module jburk_r8lib_r8col_
