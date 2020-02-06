!> @author [P. Jang](orange224factory@gmail.com)
!> @date   2020-02-06
!> @see    [Documenting Fortran with Doxygen](https://github.com/Mohid-Water-Modelling-System/Mohid/wiki/Documenting-Fortran-with-Doxygen)
module     jburk_r8lib_r8row_
use, intrinsic :: iso_fortran_env
implicit none

interface        r8row_compare
module procedure r8row_compare
end interface    r8row_compare
public           r8row_compare

interface        r8row_max
module procedure r8row_max
end interface    r8row_max
public           r8row_max

contains



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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the M by N array.
!
!    Input, integer :: I, J, the rows to be compared.
!    I and J must be between 1 and M.
!
!    Output, integer :: VALUE, the results of the comparison:
!    -1, row I < row J,
!     0, row I = row J,
!    +1, row J < row I.
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
  if ( i < 1 .or. m < i ) then
    write (unit=*, fmt='(a)') ' '
    write (unit=*, fmt='(a)') 'R8ROW_COMPARE - Fatal error!'
    write (unit=*, fmt='(a)') '  Row index I is out of bounds.'
    write (unit=*, fmt='(a,i8)') '  I = ', i
    stop
  end if

  if ( j < 1 .or. m < j ) then
    write (unit=*, fmt='(a)') ' '
    write (unit=*, fmt='(a)') 'R8ROW_COMPARE - Fatal error!'
    write (unit=*, fmt='(a)') '  Row index J is out of bounds.'
    write (unit=*, fmt='(a,i8)') '  J = ', j
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



!> @author John Burkardt
!> @brief  R8ROW_MAX returns the maximums of an R8ROW.
!> @date   2004-10-10
!> @date   2020-02-06
!> @see    
subroutine     r8row_max (m, n, a, amax)
implicit none
   integer, intent(in) :: m
   integer, intent(in) :: n
   real(kind=8), intent(in) :: a(m,n)
   real(kind=8), intent(out) :: amax(n)
   integer :: i, j

   amax = 0.0D+0
   do i = 1, m
      amax(i) = a(i,1)
      do j = 2, n
         if (amax(i) < a(i,j)) then
             amax(i) = a(i,j)
         end if
      end do
   end do
end subroutine r8row_max
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
!    Input, integer :: M, N, the number of rows and columns
!    in the array.
!
!    Input, real(kind=8) :: A(M,N), the array to be examined.
!
!    Output, real(kind=8) :: AMAX(M), the maximums of the rows.
!



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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the array to be examined.
!
!    Output, real(kind=8) :: MEAN(M), the means, or averages, of the rows.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i
  real(kind=8) :: mean(m)

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
!    Input, integer :: M, N, the number of rows and columns
!    in the array.
!
!    Input, real(kind=8) :: A(M,N), the array to be examined.
!
!    Output, real(kind=8) :: AMIN(M), the minimums of the rows.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  real(kind=8) :: amin(m)
  integer :: i
  integer :: j

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
!    Input, integer :: M, the row dimension of A.
!
!    Input, integer :: N, the column dimension of A, and the
!    length of a row.
!
!    Input/output, real(kind=8) :: A(M,N).  On input, the array to be checked.
!    On output, A has been reordered as described above.
!
!    Output, integer :: L, R, the indices of A that define the three
!    segments.  Let KEY = the input value of A(1,1:N).  Then
!    I <= L                 A(I,1:N) < KEY;
!         L < I < R         A(I,1:N) = KEY;
!                 R <= I    KEY < A(I,1:N).
!
subroutine r8row_part_quick_a ( m, n, a, l, r )
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
  real(kind=8) :: key(n)
  integer :: l
  integer :: r
  ! logical r8vec_eq
  ! logical r8vec_gt
  ! logical r8vec_lt

  if ( m < 1 ) then
    write (unit=*, fmt='(a)') ' '
    write (unit=*, fmt='(a)') 'R8ROW_PART_QUICK_A - Fatal error!'
    write (unit=*, fmt='(a)') '  M < 1.'
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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input/output, real(kind=8) :: A(M,N).
!    On input, the array of M rows of N-vectors.
!    On output, the rows of A have been sorted in lexicographic order.
!
subroutine r8row_sort_heap_a ( m, n, a )
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
!    Input, integer :: M, the number of rows in each column of A.
!
!    Input, integer :: N, the number of columns in A.
!
!    Input, real(kind=8) :: A(M,N), the array.
!
!    Output, integer :: INDX(M), the sort index.  The I-th element
!    of the sorted array is row INDX(I).
!
subroutine r8row_sort_heap_index_a ( m, n, a, indx )
use jburk_r8lib_r8vec_, only: r8vec_compare
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i
  integer :: indx(m)
  integer :: indxt
  integer :: ir
  integer :: isgn
  integer :: j
  integer :: l
  real(kind=8) :: row(n)

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



!> @author John Burkardt
!> @brief  R8ROW_SORT_QUICK_A ascending quick sorts an R8ROW.
!> @date   2012-05-21
!> @date   2020-02-06
!> @see    
subroutine r8row_sort_quick_a (m, n, a)
implicit none
   integer, intent(in) :: m              !< M, the number of rows of A.
   integer, intent(in) :: n              !< N, the number of columns of A, and the length of a row.
   real(kind=8), intent(inout) :: a(m,n) !< A(M,N). On input, the array to be sorted.

   integer, parameter :: level_max = 30
   integer :: base
   integer :: l_segment
   integer :: level
   integer :: m_segment
   integer :: rsave(level_max)
   integer :: r_segment

   if (m .lt. 1) then
      write (unit=*, fmt='(a)')    ' '
      write (unit=*, fmt='(a)')    'R8ROW_SORT_QUICK_A - Fatal error!'
      write (unit=*, fmt='(a)')    '  M < 1.'
      write (unit=*, fmt='(a,i8)') '  M = ', m
      stop
   end if

   if (n .le. 0) return
   if (m .eq. 1) return

   level = 1
   rsave(level) = m + 1
   base = 1
   m_segment = m

   do
      ! Partition the segment.
      call r8row_part_quick_a (m_segment, n, a(base:base+m_segment-1,1:n), l_segment, r_segment)

      ! If the left segment has more than one element, we need to partition it.
      if (1 < l_segment) then
         if (level_max < level) then
            write (unit=*, fmt='(a)') ' '
            write (unit=*, fmt='(a)') 'R8ROW_SORT_QUICK_A - Fatal error!'
            write (unit=*, fmt='(a,i8)') '  Exceeding recursion maximum of ', level_max
            stop
         end if

         level = level + 1
         m_segment = l_segment
         rsave(level) = r_segment + base - 1

      ! The left segment and the middle segment are sorted.
      ! Must the right segment be partitioned?
      else if ( r_segment < m_segment ) then
         m_segment = m_segment + 1 - r_segment
         base = base + r_segment - 1

      ! Otherwise, we back up a level if there is an earlier one.
      else
         do
            if (level <= 1) return

            base = rsave(level)
            m_segment = rsave(level-1) - rsave(level)
            level = level - 1

            if (0 < m_segment) exit
         end do

      end if
  end do

end subroutine r8row_sort_quick_a
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
!    Input, integer :: M, the number of rows of A.
!
!    Input, integer :: N, the number of columns of A,
!    and the length of a row.
!
!    Input/output, real(kind=8) :: A(M,N).
!    On input, the array to be sorted.
!    On output, the array has been sorted.
!



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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), a sorted array, containing
!    M rows of data.
!
!    Output, integer :: UNIQUE_NUM, the number of unique rows.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i1
  integer :: i2
  integer :: unique_num

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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the M by N array.
!
!    Output, real(kind=8) :: ROWSUM(M), the sum of the entries of
!    each row.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i
  real(kind=8) :: rowsum(m)

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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input/output, real(kind=8) :: A(M,N), the M by N array.
!
!    Input, integer :: I1, I2, the two rows to swap.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i1
  integer :: i2
  real(kind=8) :: row(n)

  if ( i1 < 1 .or. m < i1 ) then
    write (unit=*, fmt='(a)') ' '
    write (unit=*, fmt='(a)') 'R8ROW_SWAP - Fatal error!'
    write (unit=*, fmt='(a)') '  I1 is out of range.'
    write (unit=*, fmt='(a,i8)') '  I1 = ', i1
    stop
  end if

  if ( i2 < 1 .or. m < i2 ) then
    write (unit=*, fmt='(a)') ' '
    write (unit=*, fmt='(a)') 'R8ROW_SWAP - Fatal error!'
    write (unit=*, fmt='(a)') '  I2 is out of range.'
    write (unit=*, fmt='(a,i8)') '  I2 = ', i2
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
!    Input, integer :: M, N, the number of rows and columns.
!
!    Input, real(kind=8) :: A(M,N), the M by N array.
!
!    Output, real(kind=8) :: X(M*N), a vector containing the M rows of A.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i
  integer :: j
  real(kind=8) :: x(m*n)

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
!    Input, integer :: M, N, the number of rows and columns
!    in the array.
!
!    Input, real(kind=8) :: A(M,N), the array whose variances are desired.
!
!    Output, real(kind=8) :: VARIANCE(M), the variances of the rows.
!
  implicit none

  integer :: m
  integer :: n

  real(kind=8) :: a(m,n)
  integer :: i
  integer :: j
  real(kind=8) :: mean
  real(kind=8) :: variance(m)

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
end module jburk_r8lib_r8row_
