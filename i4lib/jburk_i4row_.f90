module     jburk_i4row_
use, intrinsic :: iso_fortran_env
implicit none

contains
!   private
    subroutine i4row_compare ( m, n, a, i, j, isgn )
    
    !*****************************************************************************80
    !
    !! I4ROW_COMPARE compares two rows of an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
    !
    !  Example:
    !
    !    Input:
    !
    !    M = 3, N = 4, I = 2, J = 3
    !
    !    A = (
    !    1  2  3  4
    !    5  6  7  8
    !    9 10 11 12 )
    !
    !    Output:
    !
    !    ISGN = -1
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    14 November 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), an array of M rows of vectors
    !    of length N.
    !
    !    Input, integer ( kind = 4 ) I, J, the rows to be compared.
    !    I and J must be between 1 and M.
    !
    !    Output, integer ( kind = 4 ) ISGN, the results of the comparison:
    !    -1, row I < row J,
    !     0, row I = row J,
    !    +1, row J < row I.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) isgn
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
    !
    !  Check that I and J are legal.
    !
      if ( i < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4ROW_COMPARE - Fatal error!'
        write ( *, '(a)' ) '  Row index I is less than 1.'
        write ( *, '(a,i8)' ) '  I = ', i
        stop
      else if ( m < i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4ROW_COMPARE - Fatal error!'
        write ( *, '(a)' ) '  Row index I is out of bounds.'
        write ( *, '(a,i8)' ) '  I = ', i
        write ( *, '(a,i8)' ) '  Maximum legal value is M = ', m
        stop
      end if
    
      if ( j < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4ROW_COMPARE - Fatal error!'
        write ( *, '(a)' ) '  Row index J is less than 1.'
        write ( *, '(a,i8)' ) '  J = ', j
        stop
      else if ( m < j ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4ROW_COMPARE - Fatal error!'
        write ( *, '(a)' ) '  Row index J is out of bounds.'
        write ( *, '(a,i8)' ) '  J = ', j
        write ( *, '(a,i8)' ) '  Maximum legal value is M = ', m
        stop
      end if
    
      isgn = 0
    
      if ( i == j ) then
        return
      end if
    
      k = 1
    
      do while ( k <= n )
    
        if ( a(i,k) < a(j,k) ) then
          isgn = -1
          return
        else if ( a(j,k) < a(i,k) ) then
          isgn = +1
          return
        end if
    
        k = k + 1
    
      end do
    
      return
    end
    subroutine i4row_find_item ( m, n, a, item, row, col )
    
    !*****************************************************************************80
    !
    !! I4ROW_FIND_ITEM searches the rows of an I4ROW for a given value.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    13 November 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the table to search.
    !
    !    Input, integer ( kind = 4 ) ITEM, the value to search for.
    !
    !    Output, integer ( kind = 4 ) ROW, COL, the row and column indices
    !    of the first occurrence of the value ITEM.  The search
    !    is conducted by rows.  If the item is not found, then
    !    ROW = COL = -1.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) col
      integer ( kind = 4 ) i
      integer ( kind = 4 ) item
      integer ( kind = 4 ) j
      integer ( kind = 4 ) row
    
      row = -1
      col = -1
    
      do i = 1, m
        do j = 1, n
          if ( a(i,j) == item ) then
            row = i
            col = j
            return
          end if
        end do
      end do
    
      return
    end
    subroutine i4row_find_pair_wrap ( m, n, a, item1, item2, row, col )
    
    !*****************************************************************************80
    !
    !! I4ROW_FIND_PAIR_WRAP searches rows of an I4ROW for a pair of items.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
    !
    !    The items must occur consecutively, with ITEM1 occurring
    !    first.  However, wrapping is allowed.  That is, if ITEM1
    !    occurs in the last column, and ITEM2 in the first, this
    !    is also regarded as a match.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    13 November 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the table to search.
    !
    !    Input, integer ( kind = 4 ) ITEM1, ITEM2, the values to search for.
    !
    !    Output, integer ( kind = 4 ) ROW, COL, the row and column indices
    !    of the first occurrence of the value ITEM1 followed immediately
    !    by ITEM2.  The search is conducted by rows.  If the pair of
    !    items is not found, then ROW = COL = -1.  If COL = N,
    !    the ITEM1 occurs in column N and ITEM2 occurs in column 1.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) col
      integer ( kind = 4 ) i
      integer ( kind = 4 ) item1
      integer ( kind = 4 ) item2
      integer ( kind = 4 ) j
      integer ( kind = 4 ) jp1
      integer ( kind = 4 ) row
    
      row = -1
      col = -1
    
      do i = 1, m
        do j = 1, n
    
          if ( a(i,j) == item1 ) then
    
            if ( j < n ) then
              jp1 = j + 1
            else
              jp1 = 1
            end if
    
            if ( a(i,jp1) == item2 ) then
              row = i
              col = j
              return
            end if
    
          end if
    
        end do
      end do
    
      return
    end
    subroutine i4row_max ( m, n, a, amax )
    
    !*****************************************************************************80
    !
    !! I4ROW_MAX returns the maximums of the rows of an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    13 November 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the array to be examined.
    !
    !    Output, integer ( kind = 4 ) AMAX(M), the maximums of the rows
    !    of the array.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) amax(m)
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
    subroutine i4row_mean ( m, n, a, mean )
    
    !*****************************************************************************80
    !
    !! I4ROW_MEAN returns the means of the rows of an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    13 November 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), an array of data.
    !
    !    Output, real ( kind = 8 ) MEAN(M), the mean of each row.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      real ( kind = 8 ) mean(m)
    
      do i = 1, m
        mean(i) = sum ( a(i,1:n) ) / real ( n, kind = 8 )
      end do
    
      return
    end
    subroutine i4row_min ( m, n, a, amin )
    
    !*****************************************************************************80
    !
    !! I4ROW_MIN returns the minimums of the rows of an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    13 November 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the array to be examined.
    !
    !    Output, integer ( kind = 4 ) AMIN(M), the minimums of the rows.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) amin(m)
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
    subroutine i4row_sort_a ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4ROW_SORT_A ascending sorts the rows of an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
    !
    !    In lexicographic order, the statement "X < Y", applied to two
    !    vectors X and Y of length M, means that there is some index I, with
    !    1 <= I <= M, with the property that
    !
    !      X(J) = Y(J) for J < I,
    !    and
    !      X(I) < Y(I).
    !
    !    In other words, X is less than Y if, at the first index where they
    !    differ, the X value is less than the Y value.
    !
    !  Example:
    !
    !    Input:
    !
    !      M = 5, N = 3
    !
    !      A =
    !        3  2  1
    !        2  4  3
    !        3  1  8
    !        2  4  2
    !        1  9  9
    !
    !    Output:
    !
    !      A =
    !        1  9  9
    !        2  4  2
    !        2  4  3
    !        3  1  8
    !        3  2  1
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
    !    Input, integer ( kind = 4 ) M, the number of rows of A.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns of A.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N).
    !    On input, the array of M rows of N-vectors.
    !    On output, the rows of A have been sorted in ascending
    !    lexicographic order.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) indx
      integer ( kind = 4 ) isgn
      integer ( kind = 4 ) j
    
      if ( m <= 1 ) then
        return
      end if
    
      if ( n <= 0 ) then
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
    
          call i4row_swap ( m, n, a, i, j )
    !
    !  Compare the I and J objects.
    !
        else if ( indx < 0 ) then
    
          call i4row_compare ( m, n, a, i, j, isgn )
    
        else if ( indx == 0 ) then
    
          exit
    
        end if
    
      end do
    
      return
    end
    subroutine i4row_sort_d ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4ROW_SORT_D descending sorts the rows of an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
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
    !    25 September 2001
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows and columns of A.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N).
    !    On input, the array of M rows of N-vectors.
    !    On output, the rows of A have been sorted in descending
    !    lexicographic order.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) indx
      integer ( kind = 4 ) isgn
      integer ( kind = 4 ) j
    
      if ( m <= 1 ) then
        return
      end if
    
      if ( n <= 0 ) then
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
    
          call i4row_swap ( m, n, a, i, j )
    !
    !  Compare the I and J objects.
    !
        else if ( indx < 0 ) then
    
          call i4row_compare ( m, n, a, i, j, isgn )
          isgn = -isgn
    
        else if ( indx == 0 ) then
    
          exit
    
        end if
    
      end do
    
      return
    end
    subroutine i4row_sort2_d ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4ROW_SORT2_D descending sorts the elements of each row of an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
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
    !    Input, integer ( kind = 4 ) M, the number of rows of A.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns of A, and the length
    !    of a vector of data.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N).
    !    On input, the array of M rows of N-vectors.
    !    On output, the elements of each row of A have been sorted in descending
    !    order.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) indx
      integer ( kind = 4 ) isgn
      integer ( kind = 4 ) j
      integer ( kind = 4 ) row
      integer ( kind = 4 ) t
    
      if ( m <= 1 ) then
        return
      end if
    
      if ( n <= 0 ) then
        return
      end if
    !
    !  Initialize.
    !
      do row = 1, m
    
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
    
            t        = a(row,i)
            a(row,i) = a(row,j)
            a(row,j) = t
    !
    !  Compare the I and J objects.
    !
          else if ( indx < 0 ) then
    
            if ( a(row,i) < a(row,j) ) then
              isgn = +1
            else
              isgn = -1
            end if
    
          else if ( indx == 0 ) then
    
            exit
    
          end if
    
        end do
    
      end do
    
      return
    end
    subroutine i4row_sorted_unique ( m, n, a, unique_num )
    
    !*****************************************************************************80
    !
    !! I4ROW_SORTED_UNIQUE keeps unique elements in an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
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
    !    Input/output, integer ( kind = 4 ) A(M,N), a sorted array, containing
    !    M rows of data.  On output, the first UNIQUE_NUM rows
    !    contain the unique rows.
    !
    !    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique rows.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i1
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) unique_num
    
      if ( n <= 0 ) then
        unique_num = 0
        return
      end if
    
      i1 = 1
    
      do i2 = 2, m
    
        if ( any ( a(i1,1:n) /= a(i2,1:n) ) ) then
          i1 = i1 + 1
          a(i1,1:n) = a(i2,1:n)
        end if
    
      end do
    
      unique_num = i1
    
      return
    end
    subroutine i4row_sorted_unique_count ( m, n, a, unique_num )
    
    !*****************************************************************************80
    !
    !! I4ROW_SORTED_UNIQUE_COUNT counts unique elements in an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
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
    !    Input, integer ( kind = 4 ) A(M,N), a sorted array, containing
    !    M rows of data.
    !
    !    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique rows.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
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
    subroutine i4row_sum ( m, n, a, rsum )
    
    !*****************************************************************************80
    !
    !! I4ROW_SUM returns the sums of the rows of an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    05 April 2001
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), an array of data.
    !
    !    Output, integer ( kind = 4 ) RSUM(M), the sum of the entries
    !    of each row of TABLE.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) rsum(m)
    
      do i = 1, m
        rsum(i) = sum ( a(i,1:n) )
      end do
    
      return
    end
    subroutine i4row_swap ( m, n, a, i1, i2 )
    
    !*****************************************************************************80
    !
    !! I4ROW_SWAP swaps two rows of an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 April 2001
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N), an array of data.
    !
    !    Input, integer ( kind = 4 ) I1, I2, the two rows to swap.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i1
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) row(n)
    !
    !  Check.
    !
      if ( i1 < 1 .or. m < i1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4ROW_SWAP - Fatal error!'
        write ( *, '(a)' ) '  I1 is out of range.'
        stop
      end if
    
      if ( i2 < 1 .or. m < i2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4ROW_SWAP - Fatal error!'
        write ( *, '(a)' ) '  I2 is out of range.'
        stop
      end if
    
      if ( i1 == i2 ) then
        return
      end if
    
      row(1:n)  = a(i1,1:n)
      a(i1,1:n) = a(i2,1:n)
      a(i2,1:n) = row(1:n)
    
      return
    end
    subroutine i4row_variance ( m, n, a, variance )
    
    !*****************************************************************************80
    !
    !! I4ROW_VARIANCE returns the variances of an I4ROW.
    !
    !  Discussion:
    !
    !    An I4ROW is an M by N array of I4's, regarded
    !    as an array of M rows of length N.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    13 November 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the array of data.
    !
    !    Output, real ( kind = 8 ) VARIANCE(M), the variance of each row.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      real ( kind = 8 ) mean
      real ( kind = 8 ) variance(m)
    
      if ( n < 2 ) then
    
        variance(1:m) = 0.0D+00
    
      else
    
        do i = 1, m
    
          mean = sum ( a(i,1:n) ) / real ( n, kind = 8 )
    
          variance(i) = 0.0D+00
          do j = 1, n
            variance(i) = variance(i) + ( real ( a(i,j), kind = 8 ) - mean )**2
          end do
    
          variance(i) = variance(i) / real ( n - 1, kind = 8 )
    
        end do
    
      end if
    
      return
    end

end module jburk_i4row_
