module     jburk_i4col_
use, intrinsic :: iso_fortran_env
implicit none
!   private

contains
    subroutine i4col_compare ( m, n, a, i, j, isgn )
    
    !*****************************************************************************80
    !
    !! I4COL_COMPARE compares columns I and J of an I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
    !
    !  Example:
    !
    !    Input:
    !
    !      M = 3, N = 4, I = 2, J = 4
    !
    !      A = (
    !        1  2  3  4
    !        5  6  7  8
    !        9 10 11 12 )
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
    !    12 June 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), an array of N columns of
    !    vectors of length M.
    !
    !    Input, integer ( kind = 4 ) I, J, the columns to be compared.
    !    I and J must be between 1 and N.
    !
    !    Output, integer ( kind = 4 ) ISGN, the results of the comparison:
    !    -1, column I < column J,
    !     0, column I = column J,
    !    +1, column J < column I.
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
    !  Check.
    !
      if ( i < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4COL_COMPARE - Fatal error!'
        write ( *, '(a,i8,a)' ) '  Column index I = ', i, ' is less than 1.'
        stop
      end if
    
      if ( n < i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4COL_COMPARE - Fatal error!'
        write ( *, '(a,i8,a)' ) '  N = ', n, ' is less than column index I = ', i
        stop
      end if
    
      if ( j < 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4COL_COMPARE - Fatal error!'
        write ( *, '(a,i8,a)' ) '  Column index J = ', j, ' is less than 1.'
        stop
      end if
    
      if ( n < j ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4COL_COMPARE - Fatal error!'
        write ( *, '(a,i8,a)' ) '  N = ', n, ' is less than column index J = ', j
        stop
      end if
    
      isgn = 0
    
      if ( i == j ) then
        return
      end if
    
      k = 1
    
      do while ( k <= m )
    
        if ( a(k,i) < a(k,j) ) then
          isgn = -1
          return
        else if ( a(k,j) < a(k,i) ) then
          isgn = +1
          return
        end if
    
        k = k + 1
    
      end do
    
      return
    end
    subroutine i4col_find ( m, n, a, ivec, col )
    
    !*****************************************************************************80
    !
    !! I4COL_FIND searches an I4COL for a particular column value.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
    !
    !  Example:
    !
    !    M = 3, N = 4,
    !
    !    A = (
    !      1  2  3  4
    !      5  6  7  8
    !      9 10 11 12 )
    !
    !    IVEC = ( 3, 7, 11 )
    !
    !    COL = 3
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
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns in
    !    the table.  M is also the length of IVEC.
    !
    !    Input, integer ( kind = 4 ) A(M,N), an array of N columns of vectors
    !    of length M.
    !
    !    Input, integer ( kind = 4 ) IVEC(M), a vector to be matched with the data
    !    in the array.
    !
    !    Output, integer ( kind = 4 ) COL, the index of the first column of
    !    the table which exactly matches every entry of IVEC, or -1 if no match
    !    could be found.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) col
      integer ( kind = 4 ) ivec(m)
      integer ( kind = 4 ) j
    
      if ( m <= 0 ) then
        col = -1
        return
      end if
    
      do j = 1, n
    
        i = 1
    
        do while ( ivec(i) == a(i,j) )
    
          if ( i == m ) then
            col = j
            return
          end if
    
          i = i + 1
    
        end do
    
      end do
    
      col = -1
    
      return
    end
    subroutine i4col_find_item ( m, n, a, item, row, col )
    
    !*****************************************************************************80
    !
    !! I4COL_FIND_ITEM searches an I4COL for a given scalar value.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    12 November 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns in
    !    the table.
    !
    !    Input, integer ( kind = 4 ) A(M,N), an array of N columns of vectors
    !    of length M.
    !
    !    Input, integer ( kind = 4 ) ITEM, the value to search for.
    !
    !    Output, integer ( kind = 4 ) ROW, COL, the row and column indices
    !    of the first occurrence of the value ITEM.  The search
    !    is conducted by columns.  If the item is not found, then
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
    
      do j = 1, n
        do i = 1, m
          if ( a(i,j) == item ) then
            row = i
            col = j
            return
          end if
        end do
      end do
    
      row = -1
      col = -1
    
      return
    end
    subroutine i4col_find_pair_wrap ( m, n, a, item1, item2, row, col )
    
    !*****************************************************************************80
    !
    !! I4COL_FIND_PAIR_WRAP searches an I4COL for a pair of items.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
    !
    !    The items (ITEM1, ITEM2) must occur consecutively.
    !    However, wrapping is allowed, that is, if ITEM1 occurs
    !    in the last row, and ITEM2 "follows" it in the first row
    !    of the same column, a match is declared.
    !
    !    If the pair of items is not found, then ROW = COL = -1.
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
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns
    !    in the array.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the array to search.
    !
    !    Input, integer ( kind = 4 ) ITEM1, ITEM2, the values to search for.
    !
    !    Output, integer ( kind = 4 ) ROW, COL, the row and column indices
    !    of the first occurrence of the value ITEM1 followed immediately
    !    by ITEM2.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) col
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) item1
      integer ( kind = 4 ) item2
      integer ( kind = 4 ) j
      integer ( kind = 4 ) row
    
      do j = 1, n
        do i = 1, m
    
          if ( a(i,j) == item1 ) then
    
            i2 = i + 1
    
            if ( m < i2 ) then
              i2 = 1
            end if
    
            if ( a(i2,j) == item2 ) then
              row = i
              col = j
              return
            end if
    
          end if
    
        end do
      end do
    
      row = -1
      col = -1
    
      return
    end
    subroutine i4col_first_index ( m, n, a, first_index )
    
    !*****************************************************************************80
    !
    !! I4COL_FIRST_INDEX indexes the first occurrence of values in an I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's.
    !    It is regarded as an array of N columns of length M.
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
    !    24 August 2008
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns of A.
    !    The length of an "element" of A, and the number of "elements".
    !
    !    Input, integer ( kind = 4 ) A(M,N), the array.
    !
    !    Output, integer ( kind = 4 ) FIRST_INDEX(N), the first occurrence index.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) first_index(n)
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
    
      first_index(1:n) = -1
    
      do j1 = 1, n
    
        if ( first_index(j1) == -1 ) then
    
          first_index(j1) = j1
    
          do j2 = j1 + 1, n
            if ( all ( a(1:m,j1) == a(1:m,j2) ) ) then
              first_index(j2) = j1
            end if
          end do
    
        end if
    
      end do
    
      return
    end
    subroutine i4col_sort_a ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4COL_SORT_A ascending sorts an I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
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
    !    Input, integer ( kind = 4 ) M, the number of rows of A, and the length of
    !    a vector of data.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns of A.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N).
    !    On input, the array of N columns of M-vectors.
    !    On output, the columns of A have been sorted in ascending
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
    
          call i4col_swap ( m, n, a, i, j )
    !
    !  Compare the I and J objects.
    !
        else if ( indx < 0 ) then
    
          call i4col_compare ( m, n, a, i, j, isgn )
    
        else if ( indx == 0 ) then
    
          exit
    
        end if
    
      end do
    
      return
    end
    subroutine i4col_sort_d ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4COL_SORT_D descending sorts an I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
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
    !    Input, integer ( kind = 4 ) M, the number of rows of A, and the length of
    !    a vector of data.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns of A.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N).
    !    On input, the array of N columns of M-vectors.
    !    On output, the columns of A have been sorted in descending
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
    
          call i4col_swap ( m, n, a, i, j )
    !
    !  Compare the I and J objects.
    !
        else if ( indx < 0 ) then
    
          call i4col_compare ( m, n, a, i, j, isgn )
          isgn = -isgn
    
        else if ( indx == 0 ) then
    
          exit
    
        end if
    
      end do
    
      return
    end
    subroutine i4col_sort2_a ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4COL_SORT2_A ascending sorts the elements of each column of an I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
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
    !    On input, the array of N columns of M vectors.
    !    On output, the elements of each column of A have been sorted in ascending
    !    order.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) col
      integer ( kind = 4 ) i
      integer ( kind = 4 ) indx
      integer ( kind = 4 ) isgn
      integer ( kind = 4 ) j
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
      do col = 1, n
    
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
    
            t        = a(i,col)
            a(i,col) = a(j,col)
            a(j,col) = t
    !
    !  Compare the I and J objects.
    !
          else if ( indx < 0 ) then
    
            if ( a(j,col) < a(i,col) ) then
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
    subroutine i4col_sort2_d ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4COL_SORT2_D descending sorts elements of each column of an I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
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
    !    On input, the array of N columns of M vectors.
    !    On output, the elements of each column of A have been sorted in descending
    !    order.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) col
      integer ( kind = 4 ) i
      integer ( kind = 4 ) indx
      integer ( kind = 4 ) isgn
      integer ( kind = 4 ) j
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
      do col = 1, n
    
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
    
            t        = a(i,col)
            a(i,col) = a(j,col)
            a(j,col) = t
    !
    !  Compare the I and J objects.
    !
          else if ( indx < 0 ) then
    
            if ( a(i,col) < a(j,col) ) then
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
    subroutine i4col_sorted_singleton_count ( m, n, a, singleton_num )
    
    !*****************************************************************************80
    !
    !! I4COL_SORTED_SINGLETON_COUNT counts singletons in an I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
    !
    !    The columns of the array may be ascending or descending sorted.
    !
    !    A "singleton" is an item that occurs exactly once.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    26 October 2005
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
    !    N columns of data.
    !
    !    Output, integer ( kind = 4 ) SINGLETON_NUM, the number of singletons.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      logical differ_from_next
      logical differ_from_previous
      integer ( kind = 4 ) j
      integer ( kind = 4 ) singleton_num
    
      singleton_num = 0
    
      if ( n <= 0 ) then
        return
      end if
    
      differ_from_next = .true.
    
      do j = 1, n
    
        differ_from_previous = differ_from_next
    
        if ( j < n ) then
          differ_from_next = any ( a(1:m,j) /= a(1:m,j+1) )
        else
          differ_from_next = .true.
        end if
    
        if ( differ_from_previous .and. differ_from_next ) then
          singleton_num = singleton_num + 1
        end if
    
      end do
    
      return
    end
    subroutine i4col_sorted_unique ( m, n, a, unique_num )
    
    !*****************************************************************************80
    !
    !! I4COL_SORTED_UNIQUE keeps unique elements in a sorted I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
    !
    !    The array can be sorted into ascending or descending order.
    !    The important point is that identical elements must be stored
    !    in adjacent positions.
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
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows of A, and the length of
    !    a vector of data.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns of A.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N).
    !    On input, the sorted array of N columns of M-vectors.
    !    On output, a sorted array of columns of M-vectors.
    !
    !    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique columns of A.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) unique_num
    
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
    subroutine i4col_sorted_unique_count ( m, n, a, unique_num )
    
    !*****************************************************************************80
    !
    !! I4COL_SORTED_UNIQUE_COUNT counts unique elements in an I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
    !
    !    The columns of the array may be ascending or descending sorted.
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
    !    N columns of data.
    !
    !    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique columns.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) unique_num
    
      if ( n <= 0 ) then
        unique_num = 0
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
    subroutine i4col_swap ( m, n, a, j1, j2 )
    
    !*****************************************************************************80
    !
    !! I4COL_SWAP swaps columns J1 and J2 of an I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's, regarded
    !    as an array of N columns of length M.
    !
    !  Example:
    !
    !    Input:
    !
    !      M = 3, N = 4, J1 = 2, J2 = 4
    !
    !      A = (
    !        1  2  3  4
    !        5  6  7  8
    !        9 10 11 12 )
    !
    !    Output:
    !
    !      A = (
    !        1  4  3  2
    !        5  8  7  6
    !        9 12 11 10 )
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
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns
    !    in the array.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N), an array of N columns
    !    of length M.
    !
    !    Input, integer ( kind = 4 ) J1, J2, the columns to be swapped.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) col(m)
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
    
      if ( j1 < 1 .or. n < j1 .or. j2 < 1 .or. n < j2 ) then
    
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4COL_SWAP - Fatal error!'
        write ( *, '(a)' ) '  J1 or J2 is out of bounds.'
        write ( *, '(a,i8)' ) '  J1 =    ', j1
        write ( *, '(a,i8)' ) '  J2 =    ', j2
        write ( *, '(a,i8)' ) '  N =     ', n
        stop
    
      end if
    
      if ( j1 == j2 ) then
        return
      end if
    
      col(1:m)  = a(1:m,j1)
      a(1:m,j1) = a(1:m,j2)
      a(1:m,j2) = col(1:m)
    
      return
    end
    subroutine i4col_unique_index ( m, n, a, unique_index )
    
    !*****************************************************************************80
    !
    !! I4COL_UNIQUE_INDEX indexes the unique occurrence of values in an I4COL.
    !
    !  Discussion:
    !
    !    An I4COL is an M by N array of I4's.
    !    It is regarded as an array of N columns of length M.
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
    !    24 August 2008
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns of A.
    !    The length of an "element" of A, and the number of "elements".
    !
    !    Input, integer ( kind = 4 ) A(M,N), the array.
    !
    !    Output, integer ( kind = 4 ) UNIQUE_INDEX(N), the unique index.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) unique_index(n)
      integer ( kind = 4 ) unique_num
    
      unique_index(1:n) = -1
      unique_num = 0
    
      do j1 = 1, n
    
        if ( unique_index(j1) == -1 ) then
    
          unique_num = unique_num + 1
          unique_index(j1) = unique_num
    
          do j2 = j1 + 1, n
            if ( all ( a(1:m,j1) == a(1:m,j2) ) ) then
              unique_index(j2) = unique_num
            end if
          end do
    
        end if
    
      end do
    
      return
    end
    subroutine i4i4_sort_a ( i1, i2, j1, j2 )
    
    !*****************************************************************************80
    !
    !! I4I4_SORT_A ascending sorts a pair of integers.
    !
    !  Discussion:
    !
    !    An I4I4 is a pair of integers, regarded as a single data item.
    !
    !    The program allows the reasonable call:
    !
    !      call i4i4_sort_a ( i1, i2, i1, i2 )
    !
    !    and this will return the reasonable result.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    11 October 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I1, I2, the values to sort.
    !
    !    Output, integer ( kind = 4 ) J1, J2, the sorted values.
    !
      implicit none
    
      integer ( kind = 4 ) i1
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) k1
      integer ( kind = 4 ) k2
    !
    !  Copy arguments, so that the user can make "reasonable" calls like:
    !
    !    call i4i4_sort_a ( i1, i2, i1, i2 )
    !
      k1 = i1
      k2 = i2
    
      j1 = min ( k1, k2 )
      j2 = max ( k1, k2 )
    
      return
    end
    subroutine i4i4i4_sort_a ( i1, i2, i3, j1, j2, j3 )
    
    !*****************************************************************************80
    !
    !! I4I4I4_SORT_A ascending sorts a triple of integers.
    !
    !  Discussion:
    !
    !    An I4I4I4 is a triple of integers, regarded as a single data item.
    !
    !    The program allows the reasonable call:
    !
    !      call i4i4i4_sort_a ( i1, i2, i3, i1, i2, i3 )
    !
    !    and this will return the reasonable result.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    13 October 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) I1, I2, I3, the values to sort.
    !
    !    Output, integer ( kind = 4 ) J1, J2, J3, the sorted values.
    !
      implicit none
    
      integer ( kind = 4 ) i1
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) i3
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) j3
      integer ( kind = 4 ) k1
      integer ( kind = 4 ) k2
      integer ( kind = 4 ) k3
    !
    !  Copy arguments, so that the user can make "reasonable" calls like:
    !
    !    call i4i4i4_sort_a ( i1, i2, i3, i1, i2, i3 )
    !
      k1 = i1
      k2 = i2
      k3 = i3
    
      j1 = min ( min ( k1, k2 ), min ( k2, k3 ) )
      j2 = min ( max ( k1, k2 ), &
           min ( max ( k2, k3 ), max ( k3, k1 ) ) )
      j3 = max ( max ( k1, k2 ), max ( k2, k3 ) )
    
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
    !    Input, integer ( kind = 4 ) IMIN, IMAX, the range.
    !
    !    Input, integer ( kind = 4 ) I, the integer to be converted.
    !
    !    Input, real ( kind = 4 ) RMIN, RMAX, the range.
    !
    !    Output, real ( kind = 4 ) R, the corresponding value in [RMIN,RMAX].
    !
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) imax
      integer ( kind = 4 ) imin
      real ( kind = 4 ) r
      real ( kind = 4 ) rmax
      real ( kind = 4 ) rmin
    
      if ( imax == imin ) then
    
        r = 0.5E+00 * ( rmin + rmax )
    
      else
    
        r = ( real ( imax - i,        kind = 4 ) * rmin   &
            + real (        i - imin, kind = 4 ) * rmax ) &
            / real ( imax     - imin, kind = 4 )
    
      end if
    
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

    subroutine i4list_print ( n, first, list_num, list, title )
    
    !*****************************************************************************80
    !
    !! I4LIST_PRINT prints an I4LIST.
    !
    !  Discussion:
    !
    !    An I4LIST is a list of integers grouped into N segments.
    !    An index vector locates the first entry of each segment.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    01 May 2010
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the number of segments.
    !
    !    Input, integer ( kind = 4 ) FIRST(N+1), indexes the first entry
    !    of each segment.
    !
    !    Input, integer ( kind = 4 ) LIST_NUM, the number of entries.
    !
    !    Input, integer ( kind = 4 ) LIST(LIST_NUM), the data.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) list_num
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) first(n+1)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      integer ( kind = 4 ) list(list_num)
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
    
      do i = 1, n
    
        do jlo = first(i), first(i+1) - 1, 5
          jhi = min ( jlo + 4, first(i+1) - 1 )
          if ( jlo == first(i) ) then
            write ( *, '(i5,a,5(2x,i8))' ) i, ':', list(jlo:jhi)
          else
            write ( *, '(6x,  5(2x,i8))' )         list(jlo:jhi)
          end if
        end do
    
      end do
    
      return
    end
end module jburk_i4col_
