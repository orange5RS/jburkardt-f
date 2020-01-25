

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

   interface        perm_check2
   module procedure perm_check2
   end interface    perm_check2
   public           perm_check2

   interface        perm_inverse
   module procedure perm_inverse
   end interface    perm_inverse
   public           perm_inverse

   interface        sort_heap_external
   module procedure sort_heap_external
   end interface    sort_heap_external
   public           sort_heap_external

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


subroutine perm_check2 ( n, p, base, ierror )

!*****************************************************************************80
!
!! PERM_CHECK2 checks that a vector represents a permutation.
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
      write ( *, '(a)' ) 'PERM_CHECK2 - Fatal error!'
      write ( *, '(a)' ) '  The input array does not represent'
      write ( *, '(a)' ) '  a proper permutation.'
      stop
    end if

  end do

  return
end



subroutine perm_inverse ( n, p )

!*****************************************************************************80
!
!! PERM_INVERSE inverts a permutation "in place".
!
!  Discussion:
!
!    This algorithm assumes that the entries in the permutation vector are
!    strictly positive.  In particular, the value 0 must not occur.
!
!    When necessary, this function shifts the data temporarily so that
!    this requirement is satisfied.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 June 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects being permuted.
!
!    Input/output, integer ( kind = 4 ) P(N), the permutation, in standard
!    index form.  On output, P describes the inverse permutation
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) base
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i0
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i4_sign
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) is
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) p_min

  if ( n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PERM_INVERSE - Fatal error!'
    write ( *, '(a,i8)' ) '  Input value of N = ', n
    stop
  end if
!
!  Find the least value, and shift data so it begins at 1.
!
  call i4vec_min ( n, p, p_min )
  base = 1
  p(1:n) = p(1:n) - p_min + base
!
!  Check the permutation.
!
  call perm_check2 ( n, p, base, ierror )

  if ( ierror /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PERM_INVERSE - Fatal error!'
    write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
    stop
  end if
!
!  Invert the permutation.
!
  is = 1

  do i = 1, n

    i1 = p(i)

    do while ( i < i1 )
      i2 = p(i1)
      p(i1) = -i2
      i1 = i2
    end do

    is = - i4_sign ( p(i) )
    p(i) = is * abs ( p(i) )

  end do

  do i = 1, n

    i1 = - p(i)

    if ( 0 <= i1 ) then

      i0 = i

      do

        i2 = p(i1)
        p(i1) = i0

        if ( i2 < 0 ) then
          exit
        end if

        i0 = i1
        i1 = i2

      end do

    end if

  end do
!
!  Reverse the shift.
!
  p(1:n) = p(1:n) + p_min - base

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
!    routine may be used to sort integers, real ( kind = 8 )s, numbers, names,
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
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
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
!    Input, integer ( kind = 4 ) ISGN, results of comparison of elements I
!    and J.  (Used only when the previous call returned INDX less than 0).
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



end module jburk_triangulation_i4vec_