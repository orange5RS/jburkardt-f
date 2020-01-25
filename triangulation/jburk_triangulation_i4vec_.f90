

module     jburk_triangulation_i4vec_
implicit none

   interface        i4vec_heap_d
   module procedure i4vec_heap_d
   end interface    i4vec_heap_d
   public           i4vec_heap_d

   interface        i4vec_indicator
   module procedure i4vec_indicator
   end interface    i4vec_indicator
   public           i4vec_indicator

   interface        i4vec_print
   module procedure i4vec_print
   end interface    i4vec_print
   public           i4vec_print

   interface        i4vec_sort_heap_a
   module procedure i4vec_sort_heap_a
   end interface    i4vec_sort_heap_a
   public           i4vec_sort_heap_a

   interface        i4vec_sorted_unique
   module procedure i4vec_sorted_unique
   end interface    i4vec_sorted_unique
   public           i4vec_sorted_unique

   interface        i4vec2_compare
   module procedure i4vec2_compare
   end interface    i4vec2_compare
   public           i4vec2_compare

   interface        i4vec2_sort_a
   module procedure i4vec2_sort_a
   end interface    i4vec2_sort_a
   public           i4vec2_sort_a

   interface        i4vec2_sorted_unique
   module procedure i4vec2_sorted_unique
   end interface    i4vec2_sorted_unique
   public           i4vec2_sorted_unique

contains


!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   I4VEC_HEAP_D reorders an I4VEC into a descending heap.
!> @date    1999-04-15
!> @see     Albert Nijenhuis, Herbert Wilf, 
!!          Combinatorial Algorithms, 
!!          Academic Press, 1978, second edition, ISBN 0-12-519260-6.
!----------------------------------------------------------------------
subroutine i4vec_heap_d ( n, a )

!*****************************************************************************80
!
!! I4VEC_HEAP_D reorders an I4VEC into a descending heap.
!
!  Discussion:
!
!    A descending heap is an array A with the property that, for every index J,
!    A(2*J) <= A(J) and A(2*J+1) <= A(J), (as long as the indices
!    2*J and 2*J+1 are legal).
!
!  Diagram:
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
!    15 April 1999
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
!    Input, integer(kind=4) N, the size of the input array.
!
!    Input/output, integer(kind=4) A(N).
!    On input, an unsorted array.
!    On output, the array has been reordered into a heap.
!
  implicit none

  integer(kind=4) n

  integer(kind=4) a(n)
  integer(kind=4) i
  integer(kind=4) ifree
  integer(kind=4) key
  integer(kind=4) m
!
!  Only nodes N/2 down to 1 can be "parent" nodes.
!
  do i = n/2, 1, -1
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



!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   I4VEC_INDICATOR sets an I4VEC to the indicator vector.
!> @date    2000-11-09
!----------------------------------------------------------------------
subroutine i4vec_indicator ( n, a )

!*****************************************************************************80
!
!! I4VEC_INDICATOR sets an I4VEC to the indicator vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(kind=4) N, the number of elements of A.
!
!    Output, integer(kind=4) A(N), the array to be initialized.
!
  implicit none

  integer(kind=4) n

  integer(kind=4) a(n)
  integer(kind=4) i

  do i = 1, n
    a(i) = i
  end do

  return
end


!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   I4VEC_MIN computes the minimum element of an I4VEC.
!> @date    1999-01-30
!----------------------------------------------------------------------
subroutine i4vec_min ( n, a, amin )

!*****************************************************************************80
!
!! I4VEC_MIN computes the minimum element of an I4VEC.
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
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(kind=4) N, the number of entries in the array.
!
!    Input, integer(kind=4) A(N), the array.
!
!    Output, integer(kind=4) AMIN, the value of the smallest entry.
!
  implicit none

  integer(kind=4), intent(in) :: n
  integer(kind=4), intent(in) :: a(n)
  integer(kind=4), intent(out) :: amin

  amin = minval ( a(1:n) )

  return
end subroutine i4vec_min



!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   I4VEC_PRINT prints an I4VEC.
!> @date    2000-11-28
!----------------------------------------------------------------------
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(kind=4) N, the number of components of the vector.
!
!    Input, integer(kind=4) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed first.
!    TITLE may be blank.
!
  implicit none

  integer(kind=4) n

  integer(kind=4) a(n)
  integer(kind=4) big
  integer(kind=4) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  big = maxval ( abs ( a(1:n) ) )

  write ( *, '(a)' ) ' '
  if ( big < 1000 ) then
    do i = 1, n
      write ( *, '(i8,1x,i4)' ) i, a(i)
    end do
  else if ( big < 1000000 ) then
    do i = 1, n
      write ( *, '(i8,1x,i7)' ) i, a(i)
    end do
  else
    do i = 1, n
      write ( *, '(i8,i11)' ) i, a(i)
    end do
  end if

  return
end subroutine i4vec_print



!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
!> @date    1999-04-15
!> @see     Albert Nijenhuis, Herbert Wilf, 
!!          Combinatorial Algorithms, 
!!          Academic Press, 1978, second edition, ISBN 0-12-519260-6.
!----------------------------------------------------------------------
subroutine i4vec_sort_heap_a ( n, a )

!*****************************************************************************80
!
!! I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 1999
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
!    Input, integer(kind=4) N, the number of entries in the array.
!
!    Input/output, integer(kind=4) A(N).
!    On input, the array to be sorted;
!    On output, the array has been sorted.
!
  implicit none

  integer(kind=4) n

  integer(kind=4) a(n)
  integer(kind=4) n1

  if ( n <= 1 ) then
    return
  end if
!
!  1: Put A into descending heap form.
!
  call i4vec_heap_d ( n, a )
!
!  2: Sort A.
!
!  The largest object in the heap is in A(1).
!  Move it to position A(N).
!
  call i4_swap ( a(1), a(n) )
!
!  Consider the diminished heap of size N1.
!
  do n1 = n-1, 2, -1
!
!  Restore the heap structure of A(1) through A(N1).
!
    call i4vec_heap_d ( n1, a )
!
!  Take the largest object from A(1) and move it to A(N1).
!
    call i4_swap ( a(1), a(n1) )

  end do

  return
end



!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   I4VEC_SORTED_UNIQUE gets the unique elements in a sorted I4VEC.
!> @date    2000-07-09
!----------------------------------------------------------------------
subroutine i4vec_sorted_unique ( n, a, unique_num )

!*****************************************************************************80
!
!! I4VEC_SORTED_UNIQUE gets the unique elements in a sorted I4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(kind=4) N, the number of elements in A.
!
!    Input/output, integer(kind=4) A(N).  On input, the sorted
!    integer array.  On output, the unique elements in A.
!
!    Output, integer(kind=4) UNIQUE_NUM, the number of unique elements
!    in A.
!
  implicit none

  integer(kind=4) n

  integer(kind=4) a(n)
  integer(kind=4) itest
  integer(kind=4) unique_num

  unique_num = 0

  if ( n <= 0 ) then
    return
  end if

  unique_num = 1

  do itest = 2, n

    if ( a(itest) /= a(unique_num) ) then
      unique_num = unique_num + 1
      a(unique_num) = a(itest)
    end if

  end do

  return
end



!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   I4VEC2_COMPARE compares pairs of integers stored in two vectors.
!> @date    1999-10-22
!----------------------------------------------------------------------
subroutine i4vec2_compare ( n, a1, a2, i, j, isgn )

!*****************************************************************************80
!
!! I4VEC2_COMPARE compares pairs of integers stored in two vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 October 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(kind=4) N, the number of data items.
!
!    Input, integer(kind=4) A1(N), A2(N), contain the two components
!    of each item.
!
!    Input, integer(kind=4) I, J, the items to be compared.
!
!    Output, integer(kind=4) ISGN, the results of the comparison:
!    -1, item I < item J,
!     0, item I = item J,
!    +1, item J < item I.
!
  implicit none

  integer(kind=4) n

  integer(kind=4) a1(n)
  integer(kind=4) a2(n)
  integer(kind=4) i
  integer(kind=4) isgn
  integer(kind=4) j

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



!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   I4VEC2_SORT_A ascending sorts a vector of pairs of integers.
!> @date    2001-09-25
!> @see     subroutine i4vec2_compare
!> @see     subroutine sort_heap_external 
!----------------------------------------------------------------------
subroutine i4vec2_sort_a ( n, a1, a2 )

!*****************************************************************************80
!
!! I4VEC2_SORT_A ascending sorts a vector of pairs of integers.
!
!  Discussion:
!
!    Each item to be sorted is a pair of integers (I,J), with the I
!    and J values stored in separate vectors A1 and A2.
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
!    Input, integer(kind=4) N, the number of items of data.
!
!    Input/output, integer(kind=4) A1(N), A2(N), the data to be sorted.
!
  implicit none

  integer(kind=4) n

  integer(kind=4) a1(n)
  integer(kind=4) a2(n)
  integer(kind=4) i
  integer(kind=4) indx
  integer(kind=4) isgn
  integer(kind=4) j
  integer(kind=4) temp

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

      temp  = a1(i)
      a1(i) = a1(j)
      a1(j) = temp

      temp  = a2(i)
      a2(i) = a2(j)
      a2(j) = temp
!
!  Compare the I and J objects.
!
    else if ( indx < 0 ) then

      call i4vec2_compare ( n, a1, a2, i, j, isgn )

    else if ( indx == 0 ) then

      exit

    end if

  end do

  return
end



!----------------------------------------------------------------------
!> @author  John Burkardt
!> @brief   I4VEC2_SORTED_UNIQUE gets the unique elements in a sorted I4VEC2.
!> @date    2000-07-09
!----------------------------------------------------------------------
subroutine i4vec2_sorted_unique ( n, a1, a2, unique_num )

!*****************************************************************************80
!
!! I4VEC2_SORTED_UNIQUE gets the unique elements in a sorted I4VEC2.
!
!  Discussion:
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
!    09 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer(kind=4) N, the number of items.
!
!    Input/output, integer(kind=4) A1(N), A2(N).
!    On input, the array of N items.
!    On output, an array of unique items.
!
!    Output, integer(kind=4) UNIQUE_NUM, the number of unique items.
!
  implicit none

  integer(kind=4) n

  integer(kind=4) a1(n)
  integer(kind=4) a2(n)
  integer(kind=4) itest
  integer(kind=4) unique_num

  unique_num = 0

  if ( n <= 0 ) then
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


end module jburk_triangulation_i4vec_