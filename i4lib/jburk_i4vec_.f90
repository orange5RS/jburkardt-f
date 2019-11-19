module    jburk_i4vec_
use, intrinsic :: iso_fortran_env
implicit none
   !private

   interface          i4vec_add
      module procedure i4vec_add
   end interface       i4vec_add
   public             i4vec_add

   interface          i4vec_all_nonpositive
      module procedure i4vec_all_nonpositive
   end interface       i4vec_all_nonpositive
   public             i4vec_all_nonpositive

   interface          i4vec_amax
      module procedure i4vec_amax
   end interface       i4vec_amax
   public             i4vec_amax

   interface          i4vec_amax_index
      module procedure i4vec_amax_index
   end interface       i4vec_amax_index
   public             i4vec_amax_index

   interface          i4vec_amin
      module procedure i4vec_amin
   end interface       i4vec_amin
   public             i4vec_amin

   interface          i4vec_amin_index
      module procedure i4vec_amin_index
   end interface       i4vec_amin_index
   public             i4vec_amin_index

   interface          i4vec_aminz
      module procedure i4vec_aminz
   end interface       i4vec_aminz
   public             i4vec_aminz

   interface          i4vec_aminz_index
      module procedure i4vec_aminz_index
   end interface       i4vec_aminz_index
   public             i4vec_aminz_index

   interface          i4vec_any_lt
      module procedure i4vec_any_lt
   end interface       i4vec_any_lt
   public             i4vec_any_lt

   interface          i4vec_any_negative
      module procedure i4vec_any_negative
   end interface       i4vec_any_negative
   public             i4vec_any_negative

   interface          i4vec_any_nonzero
      module procedure i4vec_any_nonzero
   end interface       i4vec_any_nonzero
   public             i4vec_any_nonzero

   interface          i4vec_ascend_sub
      module procedure i4vec_ascend_sub
   end interface       i4vec_ascend_sub
   public             i4vec_ascend_sub

   interface          i4vec_ascends
      module procedure i4vec_ascends
   end interface       i4vec_ascends
   public             i4vec_ascends

   interface          i4vec_axpy
      module procedure i4vec_axpy
   end interface       i4vec_axpy
   public             i4vec_axpy

   interface          i4vec_bracket
      module procedure i4vec_bracket
   end interface       i4vec_bracket
   public             i4vec_bracket

   interface          i4vec_compare
      module procedure i4vec_compare
   end interface       i4vec_compare
   public             i4vec_compare

   interface          i4vec_copy
      module procedure i4vec_copy
   end interface       i4vec_copy
   public             i4vec_copy

   interface          i4vec_cum
      module procedure i4vec_cum
   end interface       i4vec_cum
   public             i4vec_cum

   interface          i4vec_cum0
      module procedure i4vec_cum0
   end interface       i4vec_cum0
   public             i4vec_cum0

   interface          i4vec_descends
      module procedure i4vec_descends
   end interface       i4vec_descends
   public             i4vec_descends

   interface          i4vec_direct_product
      module procedure i4vec_direct_product
   end interface       i4vec_direct_product
   public             i4vec_direct_product

   interface          i4vec_dot_product
      module procedure i4vec_dot_product
   end interface       i4vec_dot_product
   public             i4vec_dot_product




   interface          i4vec_frac
      module procedure i4vec_frac
   end interface       i4vec_frac
   public             i4vec_frac

   interface          i4vec_heap_a
      module procedure i4vec_heap_a
   end interface       i4vec_heap_a
   public             i4vec_heap_a

   interface          i4vec_heap_d
      module procedure i4vec_heap_d
   end interface       i4vec_heap_d
   public             i4vec_heap_d

   interface          i4vec_heap_d_extract
      module procedure i4vec_heap_d_extract
   end interface       i4vec_heap_d_extract
   public             i4vec_heap_d_extract

   interface          i4vec_heap_d_insert
      module procedure i4vec_heap_d_insert
   end interface       i4vec_heap_d_insert
   public             i4vec_heap_d_insert

   interface           i4vec_heap_d_max
      module procedure i4vec_heap_d_max
   end interface       i4vec_heap_d_max
   public              i4vec_heap_d_max

   interface          i4vec_indicator
      module procedure i4vec_indicator
   end interface       i4vec_indicator
   public             i4vec_indicator

   interface          i4vec_median
      module procedure i4vec_median
   end interface       i4vec_median
   public             i4vec_median

   ! interface           i4vec_pop
   !    module procedure i4vec_pop
   ! end interface       i4vec_pop
   ! public              i4vec_pop

   interface           i4vec_print
      module procedure i4vec_print
   end interface       i4vec_print
   public              i4vec_print

   ! interface          i4vec_push
   !    module procedure i4vec_push
   ! end interface       i4vec_push
   ! public             i4vec_push

   interface           i4vec_swap
      module procedure i4vec_swap
   end interface       i4vec_swap
   public              i4vec_swap

   interface           i4vec_sort_heap_d
      module procedure i4vec_sort_heap_d
   end interface       i4vec_sort_heap_d
   public              i4vec_sort_heap_d

   interface           i4vec_split_unsort
      module procedure i4vec_split_unsort
   end interface       i4vec_split_unsort
   public              i4vec_split_unsort


contains

   subroutine i4vec_add ( n, a, b, c )
   
   !*****************************************************************************80
   !
   !! I4VEC_ADD computes C = A + B for I4VEC's.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   28 April 2010
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries.
   !
   !   Input, integer (kind=4) :: A(N), the first vector.
   !
   !   Input, integer (kind=4) :: B(N), the second vector.
   !
   !   Output, integer (kind=4) :: C(N), the sum of the vectors.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: b(n)
      integer (kind=4) :: c(n)
   
      c(1:n) = a(1:n) + b(1:n)
   
      return
   end subroutine i4vec_add


   function i4vec_all_nonpositive ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_ALL_NONPOSITIVE: ( all ( A <= 0 ) ) for I4VEC's.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   08 October 2011
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries.
   !
   !   Input, integer (kind=4) :: A(N), the vector.
   !
   !   Output, logical I4VEC_ALL_NONPOSITIVE is TRUE if all entries
   !   of A are less than or equal to zero.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      logical i4vec_all_nonpositive
   
      i4vec_all_nonpositive = all ( a(1:n) <= 0 )
   
      return
   end
   subroutine i4vec_amax ( n, a, aamax )
   
   !*****************************************************************************80
   !
   !! I4VEC_AMAX returns the largest magnitude in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 November 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be searched.
   !
   !   Output, integer (kind=4) :: AAMAX, the value of the entry of
   !   largest magnitude.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: aamax
      integer (kind=4) :: i
   
      if ( n <= 0 ) then
   
       aamax = 0
   
      else
   
       aamax = abs ( a(1) )
   
       do i = 2, n
         aamax = max ( aamax, abs ( a(i) ) )
       end do
   
      end if
   
      return
   end
   subroutine i4vec_amax_index ( n, a, amax_index )
   
   !*****************************************************************************80
   !
   !! I4VEC_AMAX_INDEX returns the index of the largest magnitude in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 November 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be searched.
   !
   !   Output, integer (kind=4) :: AMAX_INDEX, the index of the entry
   !   of largest magnitude.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: aamax
      integer (kind=4) :: i
      integer (kind=4) :: amax_index
   
      if ( n <= 0 ) then
   
       amax_index = 0
   
      else
   
       aamax = abs ( a(1) )
       amax_index = 1
   
       do i = 2, n
   
         if ( aamax < abs ( a(i) ) ) then
          aamax = abs ( a(i) )
          amax_index = i
         end if
   
       end do
   
      end if
   
      return
   end
   subroutine i4vec_amin ( n, a, aamin )
   
   !*****************************************************************************80
   !
   !! I4VEC_AMIN returns the smallest magnitude in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   29 May 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries to be checked.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be checked.
   !
   !   Output, integer AAMIN, the value of the smallest magnitude.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: aamin
      integer (kind=4) :: i
   
      if ( n <= 0 ) then
   
       aamin = 0
   
      else
   
       aamin = abs ( a(1) )
   
       do i = 2, n
         aamin = min ( aamin, abs ( a(i) ) )
       end do
   
      end if
   
      return
   end
   subroutine i4vec_amin_index ( n, a, amin_index )
   
   !*****************************************************************************80
   !
   !! I4VEC_AMIN_INDEX returns the index of the smallest magnitude in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 November 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries to be checked.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be checked.
   !
   !   Output, integer (kind=4) :: AMIN_INDEX, the entry of the smallest
   !   magnitude.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: aamin
      integer (kind=4) :: i
      integer (kind=4) :: amin_index
   
      if ( n <= 0 ) then
   
       amin_index = 0
   
      else
   
       aamin = a(1)
       amin_index = 1
   
       do i = 2, n
   
         if ( abs ( a(i) ) < aamin ) then
          aamin = abs ( a(i) )
          amin_index = i
         end if
   
       end do
   
      end if
   
      return
   end
   subroutine i4vec_aminz ( n, a, aminz )
   
   !*****************************************************************************80
   !
   !! I4VEC_AMINZ returns the smallest nonzero magnitude in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 January 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries to be checked.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be checked.
   !
   !   Output, integer (kind=4) :: AMINZ, the value of the smallest nonzero
   !   magnitude.  If all entries are zero, AMINZ is 0.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: aminz
      integer (kind=4) :: i
      integer (kind=4) :: iset
   
      aminz = 0
      iset = 0
   
      do i = 1, n
   
       if ( a(i) /= 0 ) then
   
         if ( iset == 0 ) then
          aminz = abs ( a(i) )
          iset = 1
         else
          aminz = min ( aminz, abs ( a(i) ) )
         end if
   
       end if
   
      end do
   
      return
   end
   subroutine i4vec_aminz_index ( n, a, aminz_index )
   
   !*****************************************************************************80
   !
   !! I4VEC_AMINZ_INDEX returns the smallest nonzero magnitude in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   18 November 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries to be checked.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be checked.
   !
   !   Output, integer (kind=4) :: AMINZ_INDEX, the entry of the smallest
   !   nonzero magnitude.  If all entries are zero, AMINZ_INDEX is 0.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: aminz
      integer (kind=4) :: i
      integer (kind=4) :: aminz_index
   
      aminz = 0
      aminz_index = 0
   
      do i = 1, n
   
       if ( a(i) /= 0 ) then
   
         if ( aminz_index == 0 .or. abs ( a(i) ) < aminz ) then
          aminz = abs ( a(i) )
          aminz_index = i
         end if
   
       end if
   
      end do
   
      return
   end
   function i4vec_any_lt ( n, a, b )
   
   !*****************************************************************************80
   !
   !! I4VEC_ANY_LT: ( any ( A < B ) ) for I4VEC's.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   28 April 2010
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries.
   !
   !   Input, integer (kind=4) :: A(N), the first vector.
   !
   !   Input, integer (kind=4) :: B(N), the second vector.
   !
   !   Output, logical I4VEC_ANY_LT is TRUE if any entry
   !   of A is less than the corresponding entry of B.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: b(n)
      logical i4vec_any_lt
   
      i4vec_any_lt = any ( a(1:n) < b(1:n) )
   
      return
   end
   function i4vec_any_negative ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_ANY_NEGATIVE: ( any A < 0 ) for I4VEC's.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   08 October 2011
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries.
   !
   !   Input, integer (kind=4) :: A(N), the vector.
   !
   !   Output, logical I4VEC_ANY_NEGATIVE is TRUE if any entry is negative.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      logical i4vec_any_negative
   
      i4vec_any_negative = any ( a(1:n) < 0 )
   
      return
   end
   function i4vec_any_nonzero ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_ANY_NONZERO: ( any A nonzero ) for I4VEC's.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   25 December 2011
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries.
   !
   !   Input, integer (kind=4) :: A(N), the vector.
   !
   !   Output, logical I4VEC_ANY_NONZERO is TRUE if any entry is nonzero.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      logical i4vec_any_nonzero
   
      i4vec_any_nonzero = any ( a(1:n) /= 0 )
   
      return
   end
   subroutine i4vec_ascend_sub ( n, a, length, sub )
   
   !*****************************************************************************80
   !
   !! I4VEC_ASCEND_SUB computes the longest ascending subsequence of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The subsequence is required to be strictly increasing.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   07 May 2001
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the length of the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be examined.
   !
   !   Output, integer (kind=4) :: LENGTH, the length of the longest
   !   increasing subsequence.
   !
   !   Output, integer (kind=4) :: SUB(N), contains in entries 1 through LENGTH
   !   a longest increasing subsequence of A.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: j
      integer (kind=4) :: k
      integer (kind=4) :: length
      integer (kind=4) :: sub(n)
      integer (kind=4) :: top(n)
      integer (kind=4) :: top_prev(n)
   
      top(1:n) = 0
      top_prev(1:n) = 0
      sub(1:n) = 0
   
      if ( n <= 0 ) then
       length = 0
       return
      end if
   
      length = 0
   
      do i = 1, n
   
       k = -1
   
       do j = 1, length
         if ( a(i) <= a(top(j)) ) then
          k = j
          exit
         end if
       end do
   
       if ( k == -1 ) then
         length = length + 1
         k = length
       end if
   
       top(k) = i
   
       if ( 1 < k ) then
         top_prev(i) = top(k-1)
       else
         top_prev(i) = 0
       end if
   
      end do
   !
   !  Extract the subsequence.
   !
      j = top(length)
      sub(length) = a(j)
   
      do i = length - 1, 1, -1
       j = top_prev(j)
       sub(i) = a(j)
      end do
   
      return
   end
   function i4vec_ascends ( n, x )
   
   !*****************************************************************************80
   !
   !! I4VEC_ASCENDS determines if an I4VEC is (weakly) ascending.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Example:
   !
   !   X = ( -8, 1, 2, 3, 7, 7, 9 )
   !
   !   I4VEC_ASCENDS = TRUE
   !
   !   The sequence is not required to be strictly ascending.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   07 May 2001
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the array.
   !
   !   Input, integer (kind=4) :: X(N), the array to be examined.
   !
   !   Output, logical I4VEC_ASCENDS, is TRUE if the entries of X ascend.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: i
      logical i4vec_ascends
      integer (kind=4) :: x(n)
   
      i4vec_ascends = .false.
   
      do i = 1, n - 1
       if ( x(i+1) < x(i) ) then
         return
       end if
      end do
   
      i4vec_ascends = .true.
   
      return
   end
   subroutine i4vec_axpy ( n, ia, x, incx, y, incy )
   
   !*****************************************************************************80
   !
   !! I4VEC_AXPY adds a scaled multiple of one I4VEC to another.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   If X and Y are simple vectors, then IAXPY is equivalent to:
   !
   !      DO I = 1, N
   !       Y(I) = Y(I) + IA * X(I)
   !      END DO
   !
   !   However, by using the increments correctly, IAXPY can also be used
   !   to manipulate rows or columns of matrices.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   03 March 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries of X and Y.
   !
   !   Input, integer (kind=4) :: IA, the scalar value by which each entry
   !   of X is multiplied before being added to Y.
   !
   !   Input, integer (kind=4) :: X(*), the vector, a multiple of which is to be
   !   added to Y.
   !
   !   Input, integer (kind=4) :: INCX, the increment between successive
   !   entries of X.
   !
   !   Input/output, integer (kind=4) :: Y(*).
   !   On output, each entry of Y has been increased by
   !   IA times the corresponding entry of X.
   !
   !   Input, integer (kind=4) :: INCY, the increment between successive
   !   entries of Y.
   !
      implicit none
   
      integer (kind=4) :: i
      integer (kind=4) :: ia
      integer (kind=4) :: incx
      integer (kind=4) :: incy
      integer (kind=4) :: indx
      integer (kind=4) :: indy
      integer (kind=4) :: n
      integer (kind=4) :: x(*)
      integer (kind=4) :: y(*)
   
      indx = 1
      indy = 1
   
      do i = 1, n
   
       y(indy) = y(indy) + ia * x(indx)
   
       indx = indx + incx
       indy = indy + incy
   
      end do
   
      return
   end
   subroutine i4vec_bracket ( n, a, xval, left, right )
   
   !*****************************************************************************80
   !
   !! I4VEC_BRACKET searches a sorted I4VEC for successive brackets of a value.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   If the values in the vector are thought of as defining intervals
   !   on the number line, then this routine searches for the interval
   !   containing the given value.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   25 February 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, length of input array.
   !
   !   Input, integer (kind=4) :: A(N), an array that has been sorted
   !   into ascending order.
   !
   !   Input, integer (kind=4) :: XVAL, a value to be bracketed.
   !
   !   Output, integer (kind=4) :: LEFT, RIGHT, the results of the search.
   !   In the most common case, 1 <= LEFT < LEFT + 1 = RIGHT <= N,
   !   and A(LEFT) <= XVAL <= A(RIGHT).
   !
   !   Special cases:
   !      Value is less than all data values:
   !       LEFT = -1, RIGHT = 1, and XVAL < A(RIGHT).
   !      Value is greater than all data values:
   !       LEFT = N, RIGHT = -1, and A(LEFT) < XVAL.
   !      Value is equal to a data value:
   !       LEFT = RIGHT, and A(LEFT) = A(RIGHT) = XVAL.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: high
      integer (kind=4) :: left
      integer (kind=4) :: low
      integer (kind=4) :: mid
      integer (kind=4) :: right
      integer (kind=4) :: xval
   !
   !  XVAL < A(1).
   !
      if ( xval < a(1) ) then
       left = -1
       right = 1
   !
   !  A(N) < XVAL.
   !
      else if ( a(n) < xval ) then
       left = n
       right = -1
   !
   !  N = 1
   !
      else if ( n == 1 ) then
       left = 1
       right = 1
   !
   !  A(1) <= XVAL <= A(N).
   !
      else
   
       low = 1
       high = n - 1
   
       do
   
         mid = ( low + high ) / 2
   
         if ( high < low ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'I4VEC_BRACKET - Fatal error!'
          write ( *, '(a)' ) '  Algorithm or data failure.'
          stop
         end if
   
         if ( a(mid) == xval ) then
          left = mid
          right = mid
          exit
         else if ( a(mid+1) == xval ) then
          left = mid + 1
          right = mid + 1
          exit
         else if ( a(mid) < xval .and. xval < a(mid+1) ) then
          left = mid
          right = mid + 1
          exit
         else if ( a(mid+1) < xval ) then
          low = mid + 1
         else if ( xval < a(mid) ) then
          high = mid - 1
         end if
   
       end do
   
      end if
   
      return
   end
   subroutine i4vec_compare ( n, a1, a2, isgn )
   
   !*****************************************************************************80
   !
   !! I4VEC_COMPARE compares two I4VEC's.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The lexicographic ordering is used.
   !
   !  Example:
   !
   !   Input:
   !
   !      A1 = ( 2, 6, 2 )
   !      A2 = ( 2, 8, 12 )
   !
   !   Output:
   !
   !      ISGN = -1
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   23 February 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vectors.
   !
   !   Input, integer (kind=4) :: A1(N), A2(N), the vectors to be compared.
   !
   !   Output, integer (kind=4) :: ISGN, the results of the comparison:
   !   -1, A1 < A2,
   !    0, A1 = A2,
   !   +1, A2 < A1.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a1(n)
      integer (kind=4) :: a2(n)
      integer (kind=4) :: isgn
      integer (kind=4) :: k
   
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
   subroutine i4vec_copy ( n, a1, a2 )
   
   !*****************************************************************************80
   !
   !! I4VEC_COPY copies an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   23 September 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the length of the vectors.
   !
   !   Input, integer (kind=4) :: A1(N), the vector to be copied.
   !
   !   Output, integer (kind=4) :: A2(N), a copy of A1.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a1(n)
      integer (kind=4) :: a2(n)
   
      a2(1:n) = a1(1:n)
   
      return
   end
   subroutine i4vec_cum ( n, a, a_cum )
   
   !*****************************************************************************80
   !
   !! I4VEC_CUM computes the cumulutive sum of the entries of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Example:
   !
   !   Input:
   !
   !      A = (/ 1, 2, 3, 4 /)
   !
   !   Output:
   !
   !      A_CUM = (/ 1, 3, 6, 10 /)
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   22 December 2010
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be summed.
   !
   !   Output, integer (kind=4) :: A_CUM(N), the cumulative sum of the
   !   entries of A.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: a_cum(n)
      integer (kind=4) :: i
   
      a_cum(1) = a(1)
   
      do i = 2, n
       a_cum(i) = a_cum(i-1) + a(i)
      end do
   
      return
   end
   subroutine i4vec_cum0 ( n, a, a_cum )
   
   !*****************************************************************************80
   !
   !! I4VEC_CUM0 computes the cumulutive sum of the entries of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   This routine returns a vector of length N+1, with the first value
   !   being 0.
   !
   !  Example:
   !
   !   Input:
   !
   !      A = (/ 1, 2, 3, 4 /)
   !
   !   Output:
   !
   !      A_CUM = (/ 0, 1, 3, 6, 10 /)
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   22 December 2010
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be summed.
   !
   !   Output, integer (kind=4) :: A_CUM(0:N), the cumulative sum of the
   !   entries of A.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: a_cum(0:n)
      integer (kind=4) :: i
   
      a_cum(0) = 0
   
      do i = 1, n
       a_cum(i) = a_cum(i-1) + a(i)
      end do
   
      return
   end
   function i4vec_descends ( n, x )
   
   !*****************************************************************************80
   !
   !! I4VEC_DESCENDS determines if an I4VEC is decreasing.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Example:
   !
   !   X = ( 9, 7, 7, 3, 2, 1, -8 )
   !
   !   I4VEC_DESCENDS = TRUE
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 July 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the array.
   !
   !   Input, integer (kind=4) :: X(N), the array to be examined.
   !
   !   Output, logical I4VEC_DESCENDS, is TRUE if the entries of X descend.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: i
      logical i4vec_descends
      integer (kind=4) :: x(n)
   
      i4vec_descends = .false.
   
      do i = 1, n - 1
       if ( x(i) < x(i+1) ) then
         return
       end if
      end do
   
      i4vec_descends = .true.
   
      return
   end
   subroutine i4vec_direct_product ( factor_index, factor_order, factor_value, &
      factor_num, point_num, x )
   
   !*****************************************************************************80
   !
   !! I4VEC_DIRECT_PRODUCT creates a direct product of I4VEC's.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   To explain what is going on here, suppose we had to construct
   !   a multidimensional quadrature rule as the product of K rules
   !   for 1D quadrature.
   !
   !   The product rule will be represented as a list of points and weights.
   !
   !   The J-th item in the product rule will be associated with
   !      item J1 of 1D rule 1,
   !      item J2 of 1D rule 2,
   !      ...,
   !      item JK of 1D rule K.
   !
   !   In particular,
   !      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
   !   and
   !      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
   !
   !   So we can construct the quadrature rule if we can properly
   !   distribute the information in the 1D quadrature rules.
   !
   !   This routine carries out that task for the abscissas X.
   !
   !   Another way to do this would be to compute, one by one, the
   !   set of all possible indices (J1,J2,...,JK), and then index
   !   the appropriate information.  An advantage of the method shown
   !   here is that you can process the K-th set of information and
   !   then discard it.
   !
   !  Example:
   !
   !   Rule 1:
   !      Order = 4
   !      X(1:4) = ( 1, 2, 3, 4 )
   !
   !   Rule 2:
   !      Order = 3
   !      X(1:3) = ( 10, 20, 30 )
   !
   !   Rule 3:
   !      Order = 2
   !      X(1:2) = ( 100, 200 )
   !
   !   Product Rule:
   !      Order = 24
   !      X(1:24) =
   !       ( 1, 10, 100 )
   !       ( 2, 10, 100 )
   !       ( 3, 10, 100 )
   !       ( 4, 10, 100 )
   !       ( 1, 20, 100 )
   !       ( 2, 20, 100 )
   !       ( 3, 20, 100 )
   !       ( 4, 20, 100 )
   !       ( 1, 30, 100 )
   !       ( 2, 30, 100 )
   !       ( 3, 30, 100 )
   !       ( 4, 30, 100 )
   !       ( 1, 10, 200 )
   !       ( 2, 10, 200 )
   !       ( 3, 10, 200 )
   !       ( 4, 10, 200 )
   !       ( 1, 20, 200 )
   !       ( 2, 20, 200 )
   !       ( 3, 20, 200 )
   !       ( 4, 20, 200 )
   !       ( 1, 30, 200 )
   !       ( 2, 30, 200 )
   !       ( 3, 30, 200 )
   !       ( 4, 30, 200 )
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   18 April 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: FACTOR_INDEX, the index of the factor being
   !   processed.  The first factor processed must be factor 1!
   !
   !   Input, integer (kind=4) :: FACTOR_ORDER, the order of the factor.
   !
   !   Input, integer (kind=4) :: FACTOR_VALUE(FACTOR_ORDER), the factor values
   !   for factor FACTOR_INDEX.
   !
   !   Input, integer (kind=4) :: FACTOR_NUM, the number of factors.
   !
   !   Input, integer (kind=4) :: POINT_NUM, the number of elements in the
   !   direct product.
   !
   !   Input/output, integer X(FACTOR_NUM,POINT_NUM), the elements of the
   !   direct product, which are built up gradually.
   !
   !  Local Parameters:
   !
   !   Local, integer (kind=4) :: START, the first location of a block of
   !   values to set.
   !
   !   Local, integer (kind=4) :: CONTIG, the number of consecutive values
   !   to set.
   !
   !   Local, integer (kind=4) :: SKIP, the distance from the current value
   !   of START to the next location of a block of values to set.
   !
   !   Local, integer (kind=4) :: REP, the number of blocks of values to set.
   !
      implicit none
   
      integer (kind=4) :: factor_num
      integer (kind=4) :: factor_order
      integer (kind=4) :: point_num
   
      integer (kind=4), save :: contig
      integer (kind=4) :: factor_index
      integer (kind=4) :: factor_value(factor_order)
      integer (kind=4) :: j
      integer (kind=4) :: k
      integer (kind=4), save :: rep
      integer (kind=4), save :: skip
      integer (kind=4) :: start
      integer (kind=4) :: x(factor_num,point_num)
   
      if ( factor_index == 1 ) then
       contig = 1
       skip = 1
       rep = point_num
       x(1:factor_num,1:point_num) = 0
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
   subroutine i4vec_direct_product2 ( factor_index, factor_order, factor_value, &
      factor_num, point_num, w )
   
   !*****************************************************************************80
   !
   !! I4VEC_DIRECT_PRODUCT2 creates a direct product of I4VEC's.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   To explain what is going on here, suppose we had to construct
   !   a multidimensional quadrature rule as the product of K rules
   !   for 1D quadrature.
   !
   !   The product rule will be represented as a list of points and weights.
   !
   !   The J-th item in the product rule will be associated with
   !      item J1 of 1D rule 1,
   !      item J2 of 1D rule 2,
   !      ...,
   !      item JK of 1D rule K.
   !
   !   In particular,
   !      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
   !   and
   !      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
   !
   !   So we can construct the quadrature rule if we can properly
   !   distribute the information in the 1D quadrature rules.
   !
   !   This routine carries out the task involving the weights W.
   !
   !   Another way to do this would be to compute, one by one, the
   !   set of all possible indices (J1,J2,...,JK), and then index
   !   the appropriate information.  An advantage of the method shown
   !   here is that you can process the K-th set of information and
   !   then discard it.
   !
   !  Example:
   !
   !   Rule 1:
   !      Order = 4
   !      W(1:4) = ( 2, 3, 5, 7 )
   !
   !   Rule 2:
   !      Order = 3
   !      W(1:3) = ( 11, 13, 17 )
   !
   !   Rule 3:
   !      Order = 2
   !      W(1:2) = ( 19, 23 )
   !
   !   Product Rule:
   !      Order = 24
   !      W(1:24) =
   !       ( 2 * 11 * 19 )
   !       ( 3 * 11 * 19 )
   !       ( 4 * 11 * 19 )
   !       ( 7 * 11 * 19 )
   !       ( 2 * 13 * 19 )
   !       ( 3 * 13 * 19 )
   !       ( 5 * 13 * 19 )
   !       ( 7 * 13 * 19 )
   !       ( 2 * 17 * 19 )
   !       ( 3 * 17 * 19 )
   !       ( 5 * 17 * 19 )
   !       ( 7 * 17 * 19 )
   !       ( 2 * 11 * 23 )
   !       ( 3 * 11 * 23 )
   !       ( 5 * 11 * 23 )
   !       ( 7 * 11 * 23 )
   !       ( 2 * 13 * 23 )
   !       ( 3 * 13 * 23 )
   !       ( 5 * 13 * 23 )
   !       ( 7 * 13 * 23 )
   !       ( 2 * 17 * 23 )
   !       ( 3 * 17 * 23 )
   !       ( 5 * 17 * 23 )
   !       ( 7 * 17 * 23 )
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   18 April 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: FACTOR_INDEX, the index of the factor being
   !   processed.  The first factor processed must be factor 1!
   !
   !   Input, integer (kind=4) :: FACTOR_ORDER, the order of the factor.
   !
   !   Input, integer (kind=4) :: FACTOR_VALUE(FACTOR_ORDER), the factor values
   !   for factor FACTOR_INDEX.
   !
   !   Input, integer (kind=4) :: FACTOR_NUM, the number of factors.
   !
   !   Input, integer (kind=4) :: POINT_NUM, the number of elements in the
   !   direct product.
   !
   !   Input/output, integer (kind=4) :: W(POINT_NUM), the elements of the
   !   direct product, which are built up gradually.
   !
   !  Local Parameters:
   !
   !   Local, integer (kind=4) :: START, the first location of a block of
   !   values to set.
   !
   !   Local, integer (kind=4) :: CONTIG, the number of consecutive values to
   !   set.
   !
   !   Local, integer (kind=4) :: SKIP, the distance from the current value
   !   of START to the next location of a block of values to set.
   !
   !   Local, integer (kind=4) :: REP, the number of blocks of values to set.
   !
      implicit none
   
      integer (kind=4) :: factor_num
      integer (kind=4) :: factor_order
      integer (kind=4) :: point_num
   
      integer (kind=4), save :: contig
      integer (kind=4) :: factor_index
      integer (kind=4) :: factor_value(factor_order)
      integer (kind=4) :: j
      integer (kind=4) :: k
      integer (kind=4), save :: rep
      integer (kind=4), save :: skip
      integer (kind=4) :: start
      integer (kind=4) :: w(point_num)
   
      if ( factor_index == 1 ) then
       contig = 1
       skip = 1
       rep = point_num
       w(1:point_num) = 1
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
   function i4vec_dot_product ( n, x, y )
   
   !*****************************************************************************80
   !
   !! I4VEC_DOT_PRODUCT computes the dot product of two I4VEC's.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   19 December 2011
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the array.
   !
   !   Input, integer (kind=4) :: X(N), Y(N), the arrays.
   !
   !   Output, integer (kind=4) :: I4VEC_DOT_PRODUCT, the dot product of X and Y.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: i4vec_dot_product
      integer (kind=4) :: x(n)
      integer (kind=4) :: y(n)
   
      i4vec_dot_product = dot_product ( x(1:n), y(1:n) )
   
      return
   end
   function i4vec_eq ( n, a1, a2 )
   
   !*****************************************************************************80
   !
   !! I4VEC_EQ is true if two I4VECs are equal.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   13 May 2012
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vectors.
   !
   !   Input, integer (kind=4) :: A1(N), A2(N), two vectors to compare.
   !
   !   Output, logical I4VEC_EQ, is TRUE if every pair of elements A1(I)
   !   and A2(I) are equal, and FALSE otherwise.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a1(n)
      integer (kind=4) :: a2(n)
      logical i4vec_eq
   
      i4vec_eq = ( all ( a1(1:n) == a2(1:n) ) )
   
      return
   end
   function i4vec_even_all ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_EVEN_ALL is TRUE if all entries of an I4VEC are even.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 April 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector.
   !
   !   Output, logical I4VEC_EVEN_ALL, TRUE if all entries are even.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      logical i4vec_even_all
   
      i4vec_even_all = all ( mod ( a(1:n), 2 ) == 0 )
   
      return
   end
   function i4vec_even_any ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_EVEN_ANY is TRUE if any entry of an I4VEC is even.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 April 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector.
   !
   !   Output, logical I4VEC_EVEN_ANY, TRUE if any entry is even.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      logical i4vec_even_any
   
      i4vec_even_any = any ( mod ( a(1:n), 2 ) == 0 )
   
      return
   end
   subroutine i4vec_find ( n, a, value, location )
   
   !*****************************************************************************80
   !
   !! I4VEC_FIND finds the first occurrence of a value in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 August 2011
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Input, integer (kind=4) :: A(N), the array.
   !
   !   Input, integer (kind=4) :: VALUE, the value being sought.
   !
   !   Output, integer (kind=4) :: LOCATION, the first location in A where 
   !   VALUE occurs, or -1 if VALUE never occurs.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: location
      integer (kind=4) :: value
   
      location = -1
   
      do i = 1, n
   
       if ( a(i) == value ) then
         location = i
         return
       end if
   
      end do
   
      return
   end
   subroutine i4vec_first_index ( n, a, first_index )
   
   !*****************************************************************************80
   !
   !! I4VEC_FIRST_INDEX indexes the first occurrence of values in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   For element A(I) of the vector, FIRST_INDEX(I) is the index in A of
   !   the first occurrence of the value A(I).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   24 August 2008
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Input, integer (kind=4) :: A(N), the array.
   !
   !   Output, integer (kind=4) :: FIRST_INDEX(N), the first occurrence index.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: first_index(n)
      integer (kind=4) :: i
      integer (kind=4) :: j
   
      first_index(1:n) = -1
   
      do i = 1, n
   
       if ( first_index(i) == -1 ) then
   
         first_index(i) = i
   
         do j = i + 1, n
          if ( a(i) == a(j) ) then
             first_index(j) = i
          end if
         end do
   
       end if
   
      end do
   
      return
   end

subroutine i4vec_frac_2000 ( n, a, k, iafrac )

!*****************************************************************************80
!
!! I4VEC_FRAC searches for the K-th smallest element in an N-vector.
!
!  Discussion:
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
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer (kind=4) :: N, the number of elements of A.
!
!    Input/output, integer (kind=4) :: A(N), array to search.  On output,
!    the elements of A have been somewhat rearranged.
!
!    Input, integer (kind=4) :: K, the fractile to be sought.  If K = 1, the
!    minimum entry is sought.  If K = N, the maximum is sought.
!    Other values of K search for the entry which is K-th in size.
!    K must be at least 1, and no greater than N.
!
!    Output, integer (kind=4) :: IAFRAC, the value of the K-th fractile of A.
!
!> @see dutch.f90

  implicit none

  integer (kind=4) :: n

  integer (kind=4) :: i
  integer (kind=4) :: a(n)
  integer (kind=4) :: iafrac
  integer (kind=4) :: iryt
  integer (kind=4) :: ix
  integer (kind=4) :: j
  integer (kind=4) :: k
  integer (kind=4) :: left

  if ( n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_FRAC  - Fatal error!'
    write ( *, '(a,i6)' ) '  Illegal nonpositive value of N = ', n
    stop
  end if

  if ( k <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_FRAC  - Fatal error!'
    write ( *, '(a,i6)' ) '  Illegal nonpositive value of K = ', k
    stop
  end if

  if ( n < k ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_FRAC  - Fatal error!'
    write ( *, '(a,i6)' ) '  Illegal N < K, K = ', k
    stop
  end if

  left = 1
  iryt = n

  do

    if ( iryt <= left ) then
      iafrac = a(k)
      exit
    end if

    ix = a(k)
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
!  Find I so that IX <= A(I).
!
      do while ( a(i) < ix )
        i = i + 1
      end do
!
!  Find J so that A(J) <= IX.
!
      do while ( ix < a(j) )
        j = j - 1
      end do

      if ( i <= j ) then
        call i4_swap ( a(i), a(j) )
        i = i + 1
        j = j - 1
      end if

    end do

  end do

  return
end subroutine i4vec_frac_2000

   subroutine i4vec_frac ( n, a, k, frac )
   !*****************************************************************************80
   !
   !! I4VEC_FRAC searches for the K-th smallest element in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   Hoare's algorithm is used.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 September 2009
   !
   !  Author:
   !
   !   FORTRAN90 version by John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Input/output, integer (kind=4) :: A(N), array to search.  On output,
   !   the elements of A have been somewhat rearranged.
   !
   !   Input, integer (kind=4) :: K, the fractile to be sought.  If K = 1, the
   !   minimum entry is sought.  If K = N, the maximum is sought.
   !   Other values of K search for the entry which is K-th in size.
   !   K must be at least 1, and no greater than N.
   !
   !   Output, integer (kind=4) :: FRAC, the value of the K-th fractile of A.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: frac
      integer (kind=4) :: i
      integer (kind=4) :: iryt
      integer (kind=4) :: ix
      integer (kind=4) :: j
      integer (kind=4) :: k
      integer (kind=4) :: left
      integer (kind=4) :: t
   
      if ( n <= 0 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_FRAC  - Fatal error!'
       write ( *, '(a,i8)' ) '  Illegal nonpositive value of N = ', n
       stop
      end if
   
      if ( k <= 0 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_FRAC  - Fatal error!'
       write ( *, '(a,i8)' ) '  Illegal nonpositive value of K = ', k
       stop
      end if
   
      if ( n < k ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_FRAC  - Fatal error!'
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
   
       ix = a(k)
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
   !  Find I so that IX <= A(I).
   !
         do while ( a(i) < ix )
          i = i + 1
         end do
   !
   !  Find J so that A(J) <= IX.
   !
         do while ( ix < a(j) )
          j = j - 1
         end do
   
         if ( i <= j ) then
   
          t   = a(i)
          a(i) = a(j)
          a(j) = t
   
          i = i + 1
          j = j - 1
   
         end if
   
       end do
   
      end do
   
      return
   end subroutine i4vec_frac

   subroutine i4vec_gcd ( n, v, gcd )
   
   !*****************************************************************************80
   !
   !! I4VEC_GCD returns the greatest common divisor of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The value GCD returned has the property that it is the greatest integer
   !   which evenly divides every entry of V.
   !
   !   The entries in V may be negative.
   !
   !   Any zero entries in V are ignored.  If all entries of V are zero,
   !   GCD is returned as 1.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   02 July 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the order of V.
   !
   !   Input, integer (kind=4) :: V(N), the vector.
   !
   !   Output, integer (kind=4) :: GCD, the greatest common divisor of V.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: gcd
      integer (kind=4) :: i
      integer (kind=4) :: i4_gcd
      integer (kind=4) :: v(n)
   
      gcd = 0
   
      do i = 1, n
   
       if ( v(i) /= 0 ) then
         if ( gcd == 0 ) then
          gcd = abs ( v(i) )
         else
          gcd = i4_gcd ( gcd, v(i) )
         end if
       end if
   
      end do
   !
   !  If GCD is 0, that can only happen because all entries of V are zero.
   !
      if ( gcd == 0 ) then
       gcd = 1
      end if
   
      return
   end


   subroutine i4vec_heap_a ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_HEAP_A reorders an I4VEC into an ascending heap.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   An ascending heap is an array A with the property that, for every index J,
   !   A(J) <= A(2*J) and A(J) <= A(2*J+1), (as long as the indices
   !   2*J and 2*J+1 are legal).
   !
   !                A(1)
   !              /      \
   !          A(2)        A(3)
   !         /    \       /  \
   !      A(4)       A(5)  A(6) A(7)
   !      /  \       /   \
   !   A(8) A(9) A(10) A(11)
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   20 March 2001
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Albert Nijenhuis, Herbert Wilf,
   !   Combinatorial Algorithms for Computers and Calculators,
   !   Academic Press, 1978,
   !   ISBN: 0-12-519260-6,
   !   LC: QA164.N54.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the input array.
   !
   !   Input/output, integer (kind=4) :: A(N).
   !   On input, an unsorted array.
   !   On output, the array has been reordered into a heap.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: ifree
      integer (kind=4) :: key
      integer (kind=4) :: m
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
   end subroutine i4vec_heap_a



   subroutine i4vec_heap_d ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_HEAP_D reorders an I4VEC into an descending heap.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   A descending heap is an array A with the property that, for every index J,
   !   A(J) >= A(2*J) and A(J) >= A(2*J+1), (as long as the indices
   !   2*J and 2*J+1 are legal).
   !
   !                A(1)
   !              /      \
   !          A(2)        A(3)
   !         /    \       /  \
   !      A(4)       A(5)  A(6) A(7)
   !      /  \       /   \
   !   A(8) A(9) A(10) A(11)
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   15 April 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Albert Nijenhuis, Herbert Wilf,
   !   Combinatorial Algorithms for Computers and Calculators,
   !   Academic Press, 1978,
   !   ISBN: 0-12-519260-6,
   !   LC: QA164.N54.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the input array.
   !
   !   Input/output, integer (kind=4) :: A(N).
   !   On input, an unsorted array.
   !   On output, the array has been reordered into a heap.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: ifree
      integer (kind=4) :: key
      integer (kind=4) :: m
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
   end subroutine i4vec_heap_d



   subroutine i4vec_heap_d_extract ( n, a, value )
   !*****************************************************************************80
   !
   !! I4VEC_HEAP_D_EXTRACT extracts the maximum value from a heap descending I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   In other words, the routine finds the maximum value in the
   !   heap, returns that value to the user, deletes that value from
   !   the heap, and restores the heap to its proper form.
   !
   !   This is one of three functions needed to model a priority queue.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   28 November 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Thomas Cormen, Charles Leiserson, Ronald Rivest,
   !   Introduction to Algorithms,
   !   MIT Press, 2001,
   !   ISBN: 0262032937,
   !   LC: QA76.C662.
   !
   !  Parameters:
   !
   !   Input/output, integer (kind=4) :: N, the number of items in the heap.
   !
   !   Input/output, integer (kind=4) :: A(N), the heap.
   !
   !   Output, integer (kind=4) :: VALUE, the item of maximum value, which has
   !   been removed from the heap.
   !
      implicit none
   
      integer (kind=4) :: a(*)
      integer (kind=4) :: n
      integer (kind=4) :: value
   
      if ( n < 1 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_HEAP_D_EXTRACT - Fatal error!'
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
      call i4vec_sort_heap_d ( n, a )
   
      return
   end subroutine i4vec_heap_d_extract



   subroutine     i4vec_heap_d_insert ( n, a, value )
   
   !*****************************************************************************80
   !
   !! I4VEC_HEAP_D_INSERT inserts a new I4 into a heap descending I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   This is one of three functions needed to model a priority queue.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   28 November 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Thomas Cormen, Charles Leiserson, Ronald Rivest,
   !   Introduction to Algorithms,
   !   MIT Press, 2001,
   !   ISBN: 0262032937,
   !   LC: QA76.C662.
   !
   !  Parameters:
   !
   !   Input/output, integer (kind=4) :: N, the number of items in the heap.
   !
   !   Input/output, integer (kind=4) :: A(N), the heap.
   !
   !   Input, integer (kind=4) :: VALUE, the value to be inserted.
   !
      implicit none
   
      integer (kind=4) :: a(*)
      integer (kind=4) :: i
      integer (kind=4) :: n
      integer (kind=4) :: parent
      integer (kind=4) :: value
   
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
   end subroutine i4vec_heap_d_insert



   subroutine     i4vec_heap_d_max ( n, a, value )
   !*****************************************************************************80
   !
   !! I4VEC_HEAP_D_MAX returns the maximum value in a heap descending I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   This is one of three functions needed to model a priority queue.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   28 November 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Thomas Cormen, Charles Leiserson, Ronald Rivest,
   !   Introduction to Algorithms,
   !   MIT Press, 2001,
   !   ISBN: 0262032937,
   !   LC: QA76.C662.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of items in the heap.
   !
   !   Input, integer (kind=4) :: A(N), the heap.
   !
   !   Output, integer (kind=4) :: VALUE, the maximum value in the heap.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: value
   
      value = a(1)
   
      return
   end subroutine i4vec_heap_d_max


   subroutine i4vec_histogram ( n, a, histo_num, histo_gram )
   
   !*****************************************************************************80
   !
   !! I4VEC_HISTOGRAM computes a histogram of the elements of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   It is assumed that the entries in the vector A are nonnegative.
   !   Only values between 0 and HISTO_NUM will be histogrammed.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   29 August 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Input, integer (kind=4) :: A(N), the array to examine.
   !
   !   Input, integer (kind=4) :: HISTO_NUM, the maximum value for which a
   !   histogram entry will be computed.
   !
   !   Output, integer (kind=4) :: HISTO_GRAM(0:HISTO_NUM), contains the
   !   number of entries of A with the values of 0 through HISTO_NUM.
   !
      implicit none
   
      integer (kind=4) :: histo_num
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: histo_gram(0:histo_num)
      integer (kind=4) :: i
   
      histo_gram(0:histo_num) = 0
   
      do i = 1, n
   
       if ( 0 <= a(i) .and. a(i) <= histo_num ) then
         histo_gram(a(i)) = histo_gram(a(i)) + 1
       end if
   
      end do
   
      return
   end
   function i4vec_index ( n, a, aval )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEX returns the first location of a given value in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   01 February 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be searched.
   !
   !   Input, integer (kind=4) :: AVAL, the value to be indexed.
   !
   !   Output, integer (kind=4) :: I4VEC_INDEX, the first location in A which
   !   has the value AVAL, or -1 if no such index exists.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: aval
      integer (kind=4) :: i
      integer (kind=4) :: i4vec_index
   
      do i = 1, n
       if ( a(i) == aval ) then
         i4vec_index = i
         return
       end if
      end do
   
      i4vec_index = -1
   
      return
   end
   subroutine i4vec_index_delete_all ( n, x, indx, xval )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEX_DELETE_ALL deletes a value in an indexed sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   16 October 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input/output, integer (kind=4) :: N, the size of the current list.
   !
   !   Input/output, integer (kind=4) :: X(N), the list.
   !
   !   Input/output, integer (kind=4) :: INDX(N), the sort index of the list.
   !
   !   Input, integer (kind=4) :: XVAL, the value to be sought.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: equal
      integer (kind=4) :: equal1
      integer (kind=4) :: equal2
      integer (kind=4) :: get
      integer (kind=4) :: i
      integer (kind=4) :: indx(*)
      integer (kind=4) :: less
      integer (kind=4) :: more
      integer (kind=4) :: put
      integer (kind=4) :: x(*)
      integer (kind=4) :: xval
   
      if ( n < 1 ) then
       n = 0
       return
      end if
   
      call i4vec_index_search ( n, x, indx, xval, less, equal, more )
   
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
   
      x(put+1:n) = 0
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
   subroutine i4vec_index_delete_dupes ( n, x, indx, n2, x2, indx2 )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEX_DELETE_DUPES deletes duplicates from an indexed sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The output quantities N2, X2, and INDX2 are computed from the
   !   input quantities by sorting, and eliminating duplicates.
   !
   !   The output arrays should be dimensioned of size N, unless the user
   !   knows in advance what the value of N2 will be.
   !
   !   The output arrays may be identified with the input arrays.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   15 October 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) ::N, the size of the input list.
   !
   !   Input, integer (kind=4) :: X(N), the list.
   !
   !   Input, integer (kind=4) :: INDX(N), the sort index of the list.
   !
   !   Output, integer (kind=4) :: N2, the number of unique entries in X.
   !
   !   Output, integer (kind=4) :: X2(N2), a copy of the list which has
   !   been sorted, and made unique.
   !
   !   Output, integer  ( kind = 4 ) INDX2(N2), the sort index of the new list.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: i
      integer (kind=4) :: indx(n)
      integer (kind=4) :: indx2(n)
      integer (kind=4) :: n2
      integer (kind=4) :: n3
      integer (kind=4) :: x(n)
      integer (kind=4) :: x2(n)
      integer (kind=4) :: x3(n)
   
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
   subroutine i4vec_index_delete_one ( n, x, indx, xval, n2, x2, indx2 )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEX_DELETE_ONE deletes one copy of I4 from an indexed sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   If the value occurs in the list more than once, only one copy is deleted.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   24 October 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the current list.
   !
   !   Input, integer (kind=4) :: X(N), the list.
   !
   !   Input, integer (kind=4) :: INDX(N), the sort index of the list.
   !
   !   Input, integer (kind=4) :: XVAL, the value to be sought.
   !
   !   Output, integer (kind=4) :: N2, the size of the current list.
   !
   !   Output, integer (kind=4) :: X2(N2), the list.
   !
   !   Output, integer (kind=4) :: INDX2(N2), the sort index of the list.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: equal
      integer (kind=4) :: i
      integer (kind=4) :: indx(n)
      integer (kind=4) :: indx2(n)
      integer (kind=4) :: j
      integer (kind=4) :: less
      integer (kind=4) :: more
      integer (kind=4) :: n2
      integer (kind=4) :: x(n)
      integer (kind=4) :: x2(n)
      integer (kind=4) :: xval
   
      if ( n < 1 ) then
       n2 = 0
       return
      end if
   
      n2 = n
      indx2(1:n2) = indx(1:n2)
      x2(1:n2) = x(1:n2)
   
      call i4vec_index_search ( n2, x2, indx2, xval, less, equal, more )
   
      if ( equal /= 0 ) then
       j = indx2(equal)
       x2(j:n2-1) = x2(j+1:n2)
       indx2(equal:n2-1) = indx2(equal+1:n2)
       do i = 1, n2 - 1
         if ( j < indx2(i) ) then
          indx2(i) = indx2(i) - 1
         end if
       end do
       n2 = n2 - 1
      end if
   
      return
   end
   subroutine i4vec_index_insert ( n, x, indx, xval )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEX_INSERT inserts an I4 into an indexed sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   11 October 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input/output, integer (kind=4) :: N, the size of the current list.
   !
   !   Input/output, integer (kind=4) :: X(N), the list.
   !
   !   Input/output, integer (kind=4) :: INDX(N), the sort index of the list.
   !
   !   Input, integer (kind=4) :: XVAL, the value to be sought.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: equal
      integer (kind=4) :: indx(*)
      integer (kind=4) :: less
      integer (kind=4) :: more
      integer (kind=4) :: x(*)
      integer (kind=4) :: xval
   
      if ( n <= 0 ) then
       n = 1
       x(1) = xval
       indx(1) = 1
       return
      end if
   
      call i4vec_index_search ( n, x, indx, xval, less, equal, more )
   
      x(n+1) = xval
      indx(n+1:more+1:-1) = indx(n:more:-1)
      indx(more) = n + 1
      n = n + 1
   
      return
   end
   subroutine i4vec_index_insert_unique ( n, x, indx, xval )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEX_INSERT_UNIQUE inserts a unique I4 into an indexed sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   11 October 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input/output, integer (kind=4) :: N, the size of the current list.
   !   If the input value XVAL does not already occur in X, then N is increased.
   !
   !   Input/output, integer (kind=4) :: X(N), the list.
   !   If the input value XVAL does not already occur in X, then it is added
   !   to X.
   !
   !   Input/output, integer (kind=4) :: INDX(N), the sort index of the list.
   !   If the input value XVAL does not already occur in X, then INDX is updated.
   !
   !   Input, integer (kind=4) :: XVAL, the value which will be inserted into
   !   the X vector if it is not there already.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: equal
      integer (kind=4) :: indx(*)
      integer (kind=4) :: less
      integer (kind=4) :: more
      integer (kind=4) :: x(*)
      integer (kind=4) :: xval
   
      if ( n <= 0 ) then
       n = 1
       x(1) = xval
       indx(1) = 1
       return
      end if
   !
   !  Does XVAL already occur in X?
   !
      call i4vec_index_search ( n, x, indx, xval, less, equal, more )
   
      if ( equal == 0 ) then
       x(n+1) = xval
       indx(n+1:more+1:-1) = indx(n:more:-1)
       indx(more) = n + 1
       n = n + 1
      end if
   
      return
   end
   subroutine i4vec_index_order ( n, x, indx )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEX_ORDER sorts an I4VEC using an index vector.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The index vector itself is not modified.  Therefore, the pair
   !   (X,INDX) no longer represents an index sorted vector.  If this
   !   relationship is to be preserved, then simply set INDX(1:N)=(1:N).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   11 October 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the current list.
   !
   !   Input/output, integer (kind=4) :: X(N), the list.  On output, the list
   !   has been sorted.
   !
   !   Input, integer (kind=4) :: INDX(N), the sort index of the list.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: indx(n)
      integer (kind=4) :: x(n)
      integer (kind=4) :: y(n)
   
      y(1:n) = x(indx(1:n))
      x(1:n) = y(1:n)
   
      return
   end
   subroutine i4vec_index_search ( n, x, indx, xval, less, equal, more )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEX_SEARCH searches for an I4 in an indexed sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   11 October 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the current list.
   !
   !   Input, integer (kind=4) :: X(N), the list.
   !
   !   Input, integer (kind=4) :: INDX(N), the sort index of the list.
   !
   !   Input, integer (kind=4) :: XVAL, the value to be sought.
   !
   !   Output, integer LESS, EQUAL, MORE, the indexes in INDX of the
   !   entries of X that are just less than, equal to, and just greater
   !   than XVAL.  If XVAL does not occur in X, then EQUAL is zero.
   !   If XVAL is the minimum entry of X, then LESS is 0.  If XVAL
   !   is the greatest entry of X, then MORE is N+1.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: equal
      integer (kind=4) :: hi
      integer (kind=4) :: indx(n)
      integer (kind=4) :: less
      integer (kind=4) :: lo
      integer (kind=4) :: mid
      integer (kind=4) :: more
      integer (kind=4) :: x(n)
      integer (kind=4) :: xhi
      integer (kind=4) :: xlo
      integer (kind=4) :: xmid
      integer (kind=4) :: xval
   
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
         less = mid - 1
         more = mid + 1
         return
       else if ( xval < xmid ) then
         hi = mid
       else if ( xmid < xval ) then
         lo = mid
       end if
   
      end do
   
      return
   end
   subroutine i4vec_index_sort_unique ( n, x, n2, x2, indx2 )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEX_SORT_UNIQUE creates a sorted unique index for an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   16 October 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the current list.
   !
   !   Input, integer (kind=4) :: X(N), the list.
   !
   !   Output, integer (kind=4) :: N2, the number of unique elements in X.
   !
   !   Output, integer (kind=4) :: X2(N2), a list of the unique elements of X.
   !
   !   Output, integer (kind=4) :: INDX2(N2), the sort index of the list.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: i
      integer (kind=4) :: indx2(n)
      integer (kind=4) :: n2
      integer (kind=4) :: x(n)
      integer (kind=4) :: x2(n)
   
      n2 = 0
   
      do i = 1, n
       call i4vec_index_insert_unique ( n2, x2, indx2, x(i) )
      end do
   
      x2(n2+1:n) = -1
      indx2(n2+1:n) = -1
   
      return
   end
   subroutine i4vec_indexed_heap_d ( n, a, indx )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEXED_HEAP_D creates a descending heap from an indexed I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   An indexed I4VEC is an I4VEC of data values, and an I4VEC of N indices,
   !   each referencing an entry of the data vector.
   !
   !   The function adjusts the index vector INDX so that, for 1 <= J <= N/2,
   !   we have:
   !      A(INDX(2*J))   <= A(INDX(J))
   !   and
   !      A(INDX(2*J+1)) <= A(INDX(J))
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   16 August 2010
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Albert Nijenhuis, Herbert Wilf,
   !   Combinatorial Algorithms for Computers and Calculators,
   !   Academic Press, 1978,
   !   ISBN: 0-12-519260-6,
   !   LC: QA164.N54.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the index array.
   !
   !   Input, integer (kind=4) :: A(*), the data vector.
   !
   !   Input/output, integer (kind=4) :: INDX(N), the index array.
   !   Each entry of INDX must be a valid index for the array A.
   !   On output, the indices have been reordered into a descending heap.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(*)
      integer (kind=4) :: i
      integer (kind=4) :: ifree
      integer (kind=4) :: indx(n)
      integer (kind=4) :: key
      integer (kind=4) :: m
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
   subroutine i4vec_indexed_heap_d_extract ( n, a, indx, indx_extract )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEXED_HEAP_D_EXTRACT: extract from heap descending indexed I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   An indexed I4VEC is an I4VEC of data values, and an I4VEC of N indices,
   !   each referencing an entry of the data vector.
   !
   !   The routine finds the maximum value in the heap, returns that value to the
   !   user, deletes that value from the heap, and restores the heap to its
   !   proper form.
   !
   !   Note that the argument N must be a variable, which will be decremented
   !   before return, and that INDX will hold one less value on output than it
   !   held on input.
   !
   !   This is one of three functions needed to model a priority queue.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   16 August 2010
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Thomas Cormen, Charles Leiserson, Ronald Rivest,
   !   Introduction to Algorithms,
   !   MIT Press, 2001,
   !   ISBN: 0262032937,
   !   LC: QA76.C662.
   !
   !  Parameters:
   !
   !   Input/output, integer (kind=4) :: N, the number of items in the
   !   index vector.
   !
   !   Input, integer (kind=4) :: A(*), the data vector.
   !
   !   Input/output, integer (kind=4) :: INDX(N), the index vector.
   !
   !   Output, integer (kind=4) :: INDX_EXTRACT, the index in A of the item of
   !   maximum value, which has now been removed from the heap.
   !
      implicit none
   
      integer (kind=4) :: a(*)
      integer (kind=4) :: indx(*)
      integer (kind=4) :: indx_extract
      integer (kind=4) :: n
   
      if ( n < 1 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_INDEXED_HEAP_D_EXTRACT - Fatal error!'
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
      call i4vec_indexed_heap_d ( n, a, indx )
   
      return
   end
   subroutine i4vec_indexed_heap_d_insert ( n, a, indx, indx_insert )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEXED_HEAP_D_INSERT: insert value into heap descending indexed I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   An indexed I4VEC is an I4VEC of data values, and an I4VEC of N indices,
   !   each referencing an entry of the data vector.
   !
   !   Note that the argument N must be a variable, and will be incremented before
   !   return, and that INDX must be able to hold one more entry on output than
   !   it held on input.
   !
   !   This is one of three functions needed to model a priority queue.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   16 August 2010
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Thomas Cormen, Charles Leiserson, Ronald Rivest,
   !   Introduction to Algorithms,
   !   MIT Press, 2001,
   !   ISBN: 0262032937,
   !   LC: QA76.C662.
   !
   !  Parameters:
   !
   !   Input/output, integer (kind=4) :: N, the number of items in the
   !   index vector.
   !
   !   Input, integer (kind=4) :: A(*), the data vector.
   !
   !   Input/output, integer (kind=4) :: INDX(N), the index vector.
   !
   !   Input, integer (kind=4) :: INDX_INSERT, the index in A of the value
   !   to be inserted into the heap.
   !
      implicit none
   
      integer (kind=4) :: a(*)
      integer (kind=4) :: i
      integer (kind=4) :: indx(*)
      integer (kind=4) :: indx_insert
      integer (kind=4) :: n
      integer (kind=4) :: parent
   
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
   subroutine i4vec_indexed_heap_d_max ( n, a, indx, indx_max )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDEXED_HEAP_D_MAX: maximum value in heap descending indexed I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   An indexed I4VEC is an I4VEC of data values, and an I4VEC of N indices,
   !   each referencing an entry of the data vector.
   !
   !   This is one of three functions needed to model a priority queue.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   16 August 2010
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Thomas Cormen, Charles Leiserson, Ronald Rivest,
   !   Introduction to Algorithms,
   !   MIT Press, 2001,
   !   ISBN: 0262032937,
   !   LC: QA76.C662.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of items in the index vector.
   !
   !   Input, integer (kind=4) :: A(*), the data vector.
   !
   !   Input, integer (kind=4) :: INDX(N), the index vector.
   !
   !   Output, integer (kind=4) :: INDX_MAX, the index in A of the maximum value
   !   in the heap.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(*)
      integer (kind=4) :: indx(n)
      integer (kind=4) :: indx_max
   
      indx_max = indx(1)
   
      return
   end

   subroutine     i4vec_indicator ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_INDICATOR sets an I4VEC to the indicator vector.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   01 May 2007
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Output, integer (kind=4) :: A(N), the array to be initialized.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
   
      do i = 1, n
       a(i) = i
      end do
   
      return
   end subroutine i4vec_indicator



   subroutine i4vec_insert ( n, a, pos, value )
   !*****************************************************************************80
   !
   !! I4VEC_INSERT inserts a value into an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 February 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the dimension of the array on input.
   !
   !   Input/output, integer (kind=4) :: A(N+1), the array.  On input, A is
   !   assumed to contain N entries.  On output, A actually contains N+1 entries.
   !
   !   Input, integer (kind=4) :: POS, the position to be assigned the new entry.
   !   1 <= POS <= N+1.
   !
   !   Input, integer (kind=4) :: VALUE, the value to be inserted at the given
   !   position.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n+1)
      integer (kind=4) :: i
      integer (kind=4) :: pos
      integer (kind=4) :: value
   
      if ( pos < 1 .or. n + 1 < pos ) then
   
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_INSERT - Fatal error!'
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
   function i4vec_lcm ( n, v )
   
   !*****************************************************************************80
   !
   !! I4VEC_LCM returns the least common multiple of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The value LCM returned has the property that it is the smallest integer
   !   which is evenly divisible by every element of V.
   !
   !   The entries in V may be negative.
   !
   !   If any entry of V is 0, then LCM is 0.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   31 July 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the order of V.
   !
   !   Input, integer (kind=4) :: V(N), the vector.
   !
   !   Output, integer (kind=4) :: I4VEC_LCM, the least common multiple of V.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: i
      integer (kind=4) :: i4_lcm
      integer (kind=4) :: i4vec_lcm
      integer (kind=4) :: lcm
      integer (kind=4) :: v(n)
   
      lcm = 1
   
      do i = 1, n
   
       if ( v(i) == 0 ) then
         lcm = 0
         return
       end if
   
       lcm = i4_lcm ( lcm, v(i) )
   
      end do
   
      i4vec_lcm = lcm
   
      return
   end
   subroutine i4vec_mask_print ( n, a, mask_num, mask, title )
   
   !*****************************************************************************80
   !
   !! I4VEC_MASK_PRINT prints a masked I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   24 September 2001
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of components of the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be printed.
   !
   !   Input, integer (kind=4) :: MASK_NUM, the number of masked elements.
   !
   !   Input, integer (kind=4) :: MASK(MASK_NUM), the indices of the vector
   !   to be printed.
   !
   !   Input, character ( len = * ) TITLE, a title.
   !
      implicit none
   
      integer (kind=4) :: mask_num
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: mask(mask_num)
      character ( len = * ) title
   
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Masked vector printout:'
   
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, mask_num
       write ( *, '(2x,i8,a,1x,i8,2x,i10)' ) i, ':', mask(i), a(mask(i))
      end do
   
      return
   end
   subroutine i4vec_max ( n, a, amax )
   
   !*****************************************************************************80
   !
   !! I4VEC_MAX computes the maximum element of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 January 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input, integer (kind=4) :: A(N), the array.
   !
   !   Output, integer (kind=4) :: AMAX, the value of the largest entry.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: amax
   
      amax = maxval ( a(1:n) )
   
      return
   end
   subroutine i4vec_max_index ( n, a, max_index )
   
   !*****************************************************************************80
   !
   !! I4VEC_MAX_INDEX computes the index of a maximum element of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   If more than one element has the maximum value, this routine returns
   !   the index of the first such element.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 November 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input, integer (kind=4) :: A(N), the array.
   !
   !   Output, integer (kind=4) :: MAX_INDEX, the index of the largest entry.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: amax
      integer (kind=4) :: i
      integer (kind=4) :: max_index
   
      if ( n <= 0 ) then
   
       max_index = 0
   
      else
   
       amax = a(1)
       max_index = 1
   
       do i = 2, n
   
         if ( amax < a(i) ) then
          amax = a(i)
          max_index = i
         end if
   
       end do
   
      end if
   
      return
   end
   function i4vec_max_index_last ( n, x )
   
   !*****************************************************************************80
   !
   !! I4VEC_MAX_INDEX_LAST returns the last maximal element location in an I4VEC
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Example:
   !
   !   X = ( 5, 1, 2, 5, 0, 5, 3 )
   !
   !   I4VEC_MAX_INDEX_LAST = 6
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 July 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the array.
   !
   !   Input, integer (kind=4) :: X(N), the array to be examined.
   !
   !   Output, integer (kind=4) :: I4VEC_MAX_INDEX_LAST, the index of the
   !   last element of X of maximal value.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: i
      integer (kind=4) :: i4vec_max_index_last
      integer (kind=4) :: max_last
      integer (kind=4) :: x(n)
   
      i4vec_max_index_last = 0
   
      do i = 1, n
       if ( i == 1 ) then
         i4vec_max_index_last = 1
         max_last = x(1)
       else if ( max_last <= x(i) ) then
         i4vec_max_index_last = i
         max_last = x(i)
       end if
      end do
   
      return
   end
   subroutine i4vec_mean ( n, a, mean )
   
   !*****************************************************************************80
   !
   !! I4VEC_MEAN returns the mean of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 July 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector whose mean is desired.
   !
   !   Output, real ( kind = 8 ) MEAN, the mean of the vector entries.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      real ( kind = 8 ) mean
   
      mean = real ( sum ( a(1:n) ), kind = 8 ) &
          / real ( n, kind = 8 )
   
      return
   end



   subroutine     i4vec_median ( n, a, median )
   !*****************************************************************************80
   !
   !! I4VEC_MEDIAN returns the median of an unsorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   Hoare's algorithm is used.  The values of the vector are
   !   rearranged by this routine.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   18 September 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Input/output, integer (kind=4) :: A(N), the array to search.  On output,
   !   the order of the elements of A has been somewhat changed.
   !
   !   Output, integer (kind=4) :: MEDIAN, the value of the median of A.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: k
      integer (kind=4) :: median
   
      k = ( n + 1 ) / 2
   
      call i4vec_frac ( n, a, k, median )
   
      return
   end subroutine i4vec_median



   subroutine     i4vec_merge_a ( na, a, nb, b, nc, c )
   
   !*****************************************************************************80
   !
   !! I4VEC_MERGE_A merges two ascending sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The elements of A and B should be sorted in ascending order.
   !
   !   The elements in the output array C will also be in ascending order,
   !   and unique.
   !
   !   The output vector C may share storage with A or B.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 July 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: NA, the dimension of A.
   !
   !   Input, integer (kind=4) :: A(NA), the first sorted array.
   !
   !   Input, integer (kind=4) :: NB, the dimension of B.
   !
   !   Input, integer (kind=4) :: B(NB), the second sorted array.
   !
   !   Output, integer (kind=4) :: NC, the number of elements in the output
   !   array.  Note that C should usually be dimensioned at least NA+NB in the
   !   calling routine.
   !
   !   Output, integer (kind=4) :: C(NC), the merged unique sorted array.
   !
      implicit none
   
      integer (kind=4) :: na
      integer (kind=4) :: nb
   
      integer (kind=4) :: a(na)
      integer (kind=4) :: b(nb)
      integer (kind=4) :: c(na+nb)
      integer (kind=4) :: d(na+nb)
      integer (kind=4) :: j
      integer (kind=4) :: ja
      integer (kind=4) :: jb
      integer (kind=4) :: na2
      integer (kind=4) :: nb2
      integer (kind=4) :: nc
      integer (kind=4) :: order
   
      na2 = na
      nb2 = nb
   
      ja = 0
      jb = 0
      nc = 0
   
      call i4vec_order_type ( na2, a, order )
   
      if ( order < 0 .or. 2 < order ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_MERGE - Fatal error!'
       write ( *, '(a)') '  The input array A is not ascending sorted.'
       stop
      end if
   
      call i4vec_order_type ( nb2, b, order )
   
      if ( order < 0 .or. 2 < order ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_MERGE - Fatal error!'
       write ( *, '(a)' ) '  The input array B is not ascending sorted.'
       stop
      end if
   
      do
   !
   !  If we've used up all the entries of A, stick the rest of B on the end.
   !
       if ( na2 <= ja ) then
   
         do j = 1, nb2 - jb
          jb = jb + 1
          if ( nc == 0 .or. d(nc) < b(jb) ) then
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
          if ( nc == 0 .or. d(nc) < a(ja) ) then
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
         if ( nc == 0 .or. d(nc) < a(ja) ) then
          nc = nc + 1
          d(nc) = a(ja)
         end if
   !
   !  ...or if the next entry of B is the smaller, consider that.
   !
       else
   
         jb = jb + 1
         if ( nc == 0 .or. d(nc) < b(jb) ) then
          nc = nc + 1
          d(nc) = b(jb)
         end if
       end if
   
      end do
   
      return
   end
   subroutine i4vec_min ( n, a, amin )
   
   !*****************************************************************************80
   !
   !! I4VEC_MIN computes the minimum element of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 January 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input, integer (kind=4) :: A(N), the array.
   !
   !   Output, integer (kind=4) :: AMIN, the value of the smallest entry.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: amin
   
      amin = minval ( a(1:n) )
   
      return
   end
   subroutine i4vec_min_index ( n, a, imin )
   
   !*****************************************************************************80
   !
   !! I4VEC_MIN_INDEX computes the index of the minimum element of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 November 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input, integer (kind=4) :: A(N), the array.
   !
   !   Output, integer (kind=4) :: IMIN, the index of the smallest entry.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: amin
      integer (kind=4) :: i
      integer (kind=4) :: imin
   
      if ( n <= 0 ) then
   
       imin = 0
   
      else
   
       amin = a(1)
       imin = 1
   
       do i = 2, n
   
         if ( a(i) < amin ) then
          amin = a(i)
          imin = i
         end if
   
       end do
   
      end if
   
      return
   end
   subroutine i4vec_min_mv ( m, n, u, v, w )
   
   !*****************************************************************************80
   !
   !! I4VEC_MIN_MV determines U(1:N) /\ V for vectors U and a single vector V.
   !
   !  Discussion:
   !
   !   For two vectors U and V, each of length M, we define
   !
   !      ( U /\ V ) (I) = min ( U(I), V(I) ).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   12 January 2011
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: M, the dimension of the vectors.
   !
   !   Input, integer (kind=4) :: N, the number of vectors in U.
   !
   !   Input, integer (kind=4) :: U(M,N), N vectors, each of length M.
   !
   !   Input, integer (kind=4) :: V(M), a vector of length M.
   !
   !   Output, integer (kind=4) :: W(M,N), the value of U /\ W.
   !
      implicit none
   
      integer (kind=4) :: m
      integer (kind=4) :: n
   
      integer (kind=4) :: i
      integer (kind=4) :: j
      integer (kind=4) :: u(m,n)
      integer (kind=4) :: v(m)
      integer (kind=4) :: w(m,n)
   
      do j = 1, n
       do i = 1, m
         w(i,j) = min ( u(i,j), v(i) )
       end do
      end do
   
      return
   end
   function i4vec_nonzero_count ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_NONZERO_COUNT counts the nonzero entries in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   01 November 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the size of the input array.
   !
   !   Input, integer (kind=4) :: A(N), an array.
   !
   !   Output, integer (kind=4) :: I4VEC_NONZERO_COUNT, the number of
   !   nonzero entries.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: i4vec_nonzero_count
   
      i4vec_nonzero_count = 0
   
      do i = 1, n
       if ( a(i) /= 0 ) then
         i4vec_nonzero_count = i4vec_nonzero_count + 1
       end if
      end do
   
      return
   end
   subroutine i4vec_nonzero_first ( n, x, nz, indx )
   
   !*****************************************************************************80
   !
   !! I4VEC_NONZERO_FIRST left-shifts all nonzeros in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The routine preserves the ordering of the nonzero entries.  It counts
   !   the nonzeros, and returns an index vector.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   25 April 2007
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input/output, integer (kind=4) :: X(N), the vector to be shifted.
   !
   !   Output, integer (kind=4) :: NZ, the number of nonzero entries in
   !   the vector.
   !
   !   Output, integer (kind=4) :: INDX(N), contains the original location
   !   of each entry.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: indx(n)
      integer (kind=4) :: j
      integer (kind=4) :: k
      integer (kind=4) :: nz
      integer (kind=4) :: x(n)
   
      nz = 0
   
      do j = 1, n
       indx(j) = j
      end do
   
      j = 0
   
      do while ( j < n )
   
       j = j + 1
   
       if ( x(j) /= 0 ) then
   
         nz = nz + 1
   
         if ( nz /= j ) then
   
          x(nz) = x(j)
          x(j) = 0
   
          k = indx(nz)
          indx(nz) = j
          indx(j) = k
   
         end if
       end if
      end do
   
      return
   end
   function i4vec_norm_l0 ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_NORM_L0 returns the l0 "norm" of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The l0 "norm" simply counts the number of nonzero entries in the vector.
   !   It is not a true norm, but has some similarities to one.  It is useful
   !   in the study of compressive sensing.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   01 June 2012
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector.
   !
   !   Output, integer (kind=4) :: I4VEC_NORM_L0, the value of the norm.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: i4vec_norm_l0
      integer (kind=4) :: value
   
      value = 0
      do i = 1, n
       if ( a(i) /= 0 ) then
         value = value + 1
       end if
      end do
   
      i4vec_norm_l0 = value
   
      return
   end
   function i4vec_odd_all ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_ODD_ALL is TRUE if all entries of an I4VEC are odd.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 April 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector.
   !
   !   Output, logical I4VEC_ODD_ALL, TRUE if all entries are odd.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      logical i4vec_odd_all
   
      i4vec_odd_all = all ( mod ( a(1:n), 2 ) == 1 )
   
      return
   end
   function i4vec_odd_any ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_ODD_ANY is TRUE if any entry of an I4VEC is odd.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 April 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector.
   !
   !   Output, logical I4VEC_ODD_ANY, TRUE if any entry is odd.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      logical i4vec_odd_any
   
      i4vec_odd_any = any ( mod ( a(1:n), 2 ) == 1 )
   
      return
   end
   subroutine i4vec_order_type ( n, a, order )
   
   !*****************************************************************************80
   !
   !! I4VEC_ORDER_TYPE determines if I4VEC is (non)strictly ascending/descending.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   17 July 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries of the array.
   !
   !   Input, integer (kind=4) :: A(N), the array to be checked.
   !
   !   Output, integer (kind=4) :: ORDER, order indicator:
   !   -1, no discernable order;
   !   0, all entries are equal;
   !   1, ascending order;
   !   2, strictly ascending order;
   !   3, descending order;
   !   4, strictly descending order.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: order
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
   function i4vec_pairwise_prime ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_PAIRWISE_PRIME checks whether an I4VEC's entries are pairwise prime.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   Two positive integers I and J are pairwise prime if they have no common
   !   factor greater than 1.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   05 July 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of values to check.
   !
   !   Input, integer (kind=4) :: A(N), the vector of integers.
   !
   !   Output, logical I4VEC_PAIRWISE_PRIME, is TRUE if the vector of integers
   !   is pairwise prime.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: i4_gcd
      logical i4vec_pairwise_prime
      integer (kind=4) :: j
   
      i4vec_pairwise_prime = .false.
   
      do i = 1, n
       do j = i + 1, n
         if ( i4_gcd ( a(i), a(j) ) /= 1 ) then
          return
         end if
       end do
      end do
   
      i4vec_pairwise_prime = .true.
   
      return
   end
   subroutine i4vec_part ( n, nval, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_PART partitions an integer NVAL into N nearly equal parts.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Example:
   !
   !   Input:
   !
   !      N = 5, NVAL = 17
   !
   !   Output:
   !
   !      A = ( 4, 4, 3, 3, 3 ).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   18 January 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input, integer (kind=4) :: NVAL, the integer to be partitioned.
   !   NVAL may be positive, zero, or negative.
   !
   !   Output, integer (kind=4) :: A(N), the partition of NVAL.  The entries of
   !   A add up to NVAL.  The entries of A are either all equal, or
   !   differ by at most 1.  The entries of A all have the same sign
   !   as NVAL, and the "largest" entries occur first.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: j
      integer (kind=4) :: nval
   
      a(1:n) = 0
   
      if ( 0 < nval ) then
   
       j = 1
       do i = 1, nval
         a(j) = a(j) + 1
         j = j + 1
         if ( n < j ) then
          j = 1
         end if
       end do
   
      else if ( nval < 0 ) then
   
       j = 1
       do i = nval, -1
         a(j) = a(j) - 1
         j = j + 1
         if ( n < j ) then
          j = 1
         end if
       end do
   
      end if
   
      return
   end
   subroutine i4vec_part_quick_a ( n, a, l, r )
   
   !*****************************************************************************80
   !
   !! I4VEC_PART_QUICK_A reorders an I4VEC as part of a quick sort.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The routine reorders the entries of A.  Using A(1) as a key,
   !   all entries of A that are less than or equal to the key will
   !   precede the key which precedes all entries that are greater than the key.
   !
   !  Example:
   !
   !   Input:
   !
   !      N = 8
   !
   !      A = ( 6, 7, 3, 1, 6, 8, 2, 9 )
   !
   !   Output:
   !
   !      L = 3, R = 6
   !
   !      A = ( 3, 1, 2, 6, 6, 8, 9, 7 )
   !          -------       -------
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 September 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries of A.
   !
   !   Input/output, integer (kind=4) :: A(N).  On input, the array to be
   !   checked.  On output, A has been reordered as described above.
   !
   !   Output, integer (kind=4) :: L, R, the indices of A that define the
   !   three segments.
   !   Let KEY = the input value of A(1).  Then
   !   I <= L               A(I) < KEY;
   !        L < I < R        A(I) = KEY;
   !               R <= I   KEY < A(I).
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: key
      integer (kind=4) :: l
      integer (kind=4) :: m
      integer (kind=4) :: r
      integer (kind=4) :: t
   
      if ( n < 1 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_PART_QUICK_A - Fatal error!'
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
         t      = a(r)
         a(r)   = a(l+1)
         a(l+2) = t
       else if ( a(l+1) == key ) then
         m = m + 1
         t      = a(m)
         a(m)   = a(l+1)
         a(l+1) = t
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
   !  Out of bounds here, occasionally.
   !
      l = l - m
   
      a(l+1:l+m) = key
   
      return
   end
   subroutine i4vec_permute ( n, p, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_PERMUTE permutes an I4VEC in place.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   This routine permutes an array of integer "objects", but the same
   !   logic can be used to permute an array of objects of any arithmetic
   !   type, or an array of objects of any complexity.  The only temporary
   !   storage required is enough to store a single object.  The number
   !   of data movements made is N + the number of cycles of order 2 or more,
   !   which is never more than N + N/2.
   !
   !  Example:
   !
   !   Input:
   !
   !      N = 5
   !      P = (   2,   4,   5,   1,   3 )
   !      A = (   1,   2,   3,   4,   5 )
   !
   !   Output:
   !
   !      A   = (   2,   4,   5,   1,   3 ).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   20 July 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of objects.
   !
   !   Input, integer (kind=4) :: P(N), the permutation.  P(I) = J means
   !   that the I-th element of the output array should be the J-th
   !   element of the input array.
   !
   !   Input/output, integer (kind=4) :: A(N), the array to be permuted.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: a_temp
      integer (kind=4), parameter :: base = 1
      integer (kind=4) :: ierror
      integer (kind=4) :: iget
      integer (kind=4) :: iput
      integer (kind=4) :: istart
      integer (kind=4) :: p(n)
   
      call perm_check ( n, p, base, ierror )
   
      if ( ierror /= 0 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_PERMUTE - Fatal error!'
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
             write ( *, '(a)' ) 'I4VEC_PERMUTE - Fatal error!'
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
   subroutine i4vec_permute_uniform ( n, a, seed )
   
   !*****************************************************************************80
   !
   !! I4VEC_PERMUTE_UNIFORM randomly permutes an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   01 April 2003
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of objects.
   !
   !   Input/output, integer (kind=4) :: A(N), the array to be permuted.
   !
   !   Input/output, integer (kind=4) :: SEED, a seed for the random number
   !   generator.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4), parameter :: base = 1
      integer (kind=4) :: p(n)
      integer (kind=4) :: seed
   
      call perm_uniform ( n, base, seed, p )
   
      call i4vec_permute ( n, p, a )
   
      return
   end

subroutine i4vec_pop ( n, x, stack1_max, stack1_num, stack1, stack2_max, &
  stack2_num, stack2 )

!*****************************************************************************80
!
!! I4VEC_POP pops an integer vector off of a stack.
!
!  Discussion:
!
!    If there are no more objects in the stack, N is returned as -1.
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
!    Output, integer ( kind = 4 ) N, the dimension of the vector.
!
!    Output, integer ( kind = 4 ) X(*), the value of the vector.
!
!    Input, integer ( kind = 4 ) STACK1_MAX, the maximum size of STACK1.
!
!    Input/output, integer ( kind = 4 ) STACK1_NUM, the current size of STACK1.
!
!    Input/output, integer ( kind = 4 ) STACK1(STACK1_MAX), the vector
!    dimension stack.
!
!    Input, integer ( kind = 4 ) STACK2_MAX, the maximum size of STACK2.
!
!    Input/output, integer ( kind = 4 ) STACK2_NUM, the current size of STACK2.
!
!    Input/output, integer ( kind = 4 ) STACK2(STACK2_MAX), the vector value stack.
!> @see dutch.f90
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) stack1_max
  integer ( kind = 4 ) stack2_max

  integer ( kind = 4 ) stack1(stack1_max)
  integer ( kind = 4 ) stack1_num
  integer ( kind = 4 ) stack2(stack2_max)
  integer ( kind = 4 ) stack2_num
  integer ( kind = 4 ) x(*)

  if ( stack1_num < 1 ) then
    n = -1
    return
  end if

  n = stack1(stack1_num)
  stack1_num = stack1_num - 1

  stack2_num = stack2_num - n
  x(1:n) = stack2(stack2_num+1:stack2_num+n)

  return
end subroutine i4vec_pop


subroutine i4vec_print_1999 ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an integer vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed first.
!    TITLE may be blank.
!> @see dutch.f90
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  if ( title /= ' ' ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(i6,i10)' ) i, a(i)
  end do

  return
end subroutine i4vec_print_1999

   subroutine     i4vec_print ( n, a, title )
   
   !*****************************************************************************80
   !
   !! I4VEC_PRINT prints an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   02 May 2010
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of components of the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be printed.
   !
   !   Input, character ( len = * ) TITLE, a title.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      character ( len = * ) title
   
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
       write ( *, '(2x,i8,a,2x,i12)' ) i, ':', a(i)
      end do
   
      return
   end subroutine i4vec_print

subroutine i4vec_push ( n, x, stack1_max, stack1_num, stack1, stack2_max, &
  stack2_num, stack2 )

!*****************************************************************************80
!
!! I4VEC_PUSH pushes an integer vector onto a stack.
!
!  Discussion:
!
!    STACK1 contains a list of the dimensions of the objects stored.
!    Therefore, STACK1_MAX should be at least as big as the maximum number
!    of objects to be considered.
!
!    STACK2 contains the values of the objects.  Therefore, STACK2_MAX
!    should probably be as big as the maximum total length of the maximum
!    number of objects stored.
!
!    On first call, the user should have set STACK1_NUM and STACK2_NUM to zero.
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
!    Input, integer ( kind = 4 ) N, the dimension of the vector.  N may be zero.
!
!    Input, integer ( kind = 4 ) X(N), the value of the vector.
!
!    Input, integer ( kind = 4 ) STACK1_MAX, the maximum size of STACK1.
!
!    Input/output, integer ( kind = 4 ) STACK1_NUM, the current size of STACK1.
!
!    Input/output, integer ( kind = 4 ) STACK1(STACK1_MAX), the vector
!    dimension stack.
!
!    Input, integer ( kind = 4 ) STACK2_MAX, the maximum size of STACK2.
!
!    Input/output, integer ( kind = 4 ) STACK2_NUM, the current size of STACK2.
!
!    Input/output, integer ( kind = 4 ) STACK2(STACK2_MAX), the vector value stack.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) stack1_max
  integer ( kind = 4 ) stack2_max

  integer ( kind = 4 ) stack1(stack1_max)
  integer ( kind = 4 ) stack1_num
  integer ( kind = 4 ) stack2(stack2_max)
  integer ( kind = 4 ) stack2_num
  integer ( kind = 4 ) x(n)

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_PUSH - Fatal error!'
    write ( *, '(a)' ) '  Input dimension N is negative.'
    stop
  end if

  if ( stack1_max < stack1_num + 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_PUSH - Fatal error!'
    write ( *, '(a)' ) '  Exceeding size of stack #1.'
    stop
  end if

  if ( stack2_max < stack2_num + n ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4VEC_PUSH - Fatal error!'
    write ( *, '(a)' ) '  Exceeding size of stack #2.'
    stop
  end if

  stack1_num = stack1_num + 1
  stack1(stack1_num) = n

  stack2(stack2_num+1:stack2_num+n) = x(1:n)
  stack2_num = stack2_num + n

  return
end subroutine i4vec_push



   subroutine i4vec_print_part ( n, a, max_print, title )
   
   !*****************************************************************************80
   !
   !! I4VEC_PRINT_PART prints "part" of an I4VEC.
   !
   !  Discussion:
   !
   !   The user specifies MAX_PRINT, the maximum number of lines to print.
   !
   !   If N, the size of the vector, is no more than MAX_PRINT, then
   !   the entire vector is printed, one entry per line.
   !
   !   Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
   !   followed by a line of periods suggesting an omission,
   !   and the last entry.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   27 October 2010
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries of the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be printed.
   !
   !   Input, integer (kind=4) :: MAX_PRINT, the maximum number of lines
   !   to print.
   !
   !   Input, character ( len = * ) TITLE, a title.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: max_print
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
         write ( *, '(2x,i8,a,1x,i8)' ) i, ':', a(i)
       end do
   
      else if ( 3 <= max_print ) then
   
       do i = 1, max_print - 2
         write ( *, '(2x,i8,a,1x,i8)' ) i, ':', a(i)
       end do
       write ( *, '(a)' ) '  ........  ........'
       i = n
       write ( *, '(2x,i8,a,1x,i8)' ) i, ':', a(i)
   
      else
   
       do i = 1, max_print - 1
         write ( *, '(2x,i8,a,1x,i8)' ) i, ':', a(i)
       end do
       i = max_print
       write ( *, '(2x,i8,a,1x,i8,2x,a)' ) i, ':', a(i), '...more entries...'
   
      end if
   
      return
   end
   subroutine i4vec_print_some ( n, a, i_lo, i_hi, title )
   
   !*****************************************************************************80
   !
   !! I4VEC_PRINT_SOME prints "some" of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   10 September 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries of the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be printed.
   !
   !   Input, integer (kind=4) :: I_LO, I_HI, the first and last indices
   !   to print.  The routine expects 1 <= I_LO <= I_HI <= N.
   !
   !   Input, character ( len = * ) TITLE, a title.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: i_hi
      integer (kind=4) :: i_lo
      character ( len = * ) title
   
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
   
      do i = max ( i_lo, 1 ), min ( i_hi, n )
       write ( *, '(2x,i8,a,2x,i8)' ) i, ':', a(i)
      end do
   
      return
   end
   function i4vec_product ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_PRODUCT returns the product of the entries of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   In FORTRAN90, this facility is offered by the built in
   !   PRODUCT function:
   !
   !      I4VEC_PRODUCT ( N, A ) = PRODUCT ( A(1:N) )
   !
   !   In MATLAB, this facility is offered by the built in
   !   PROD function:
   !
   !      I4VEC_PRODUCT ( N, A ) = PROD ( A(1:N) )
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   22 September 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input, integer (kind=4) :: A(N), the array.
   !
   !   Output, integer (kind=4) :: I4VEC_PRODUCT, the product of the entries.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i4vec_product
   
      i4vec_product = product ( a(1:n) )
   
      return
   end
   subroutine i4vec_red ( n, a, factor )
   
   !*****************************************************************************80
   !
   !! I4VEC_RED divides out common factors in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   On output, the entries of A have no common factor
   !   greater than 1.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   25 September 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input/output, integer (kind=4) :: A(N), the vector to be reduced.
   !
   !   Output, integer (kind=4) :: FACTOR, the common factor that was divided
   !   out.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: factor
      integer (kind=4) :: i
      integer (kind=4) :: i4_gcd
   !
   !  Find the smallest nonzero value.
   !
      factor = 0
   
      do i = 1, n
   
       if ( a(i) /= 0 ) then
   
         if ( factor == 0 ) then
          factor = abs ( a(i) )
         else
          factor = min ( factor, abs ( a(i) ) )
         end if
   
       end if
   
      end do
   
      if ( factor == 0 ) then
       return
      end if
   !
   !  Find the greatest common factor of the entire vector.
   !
      do i = 1, n
       factor = i4_gcd ( a(i), factor )
      end do
   
      if ( factor == 1 ) then
       return
      end if
   !
   !  Divide out the common factor.
   !
      do i = 1, n
       a(i) = a(i) / factor
      end do
   
      return
   end
   subroutine i4vec_reverse ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_REVERSE reverses the elements of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   In FORTRAN90, call I4VEC_REVERSE is equivalent to:
   !
   !      A(1:N) = A(N:1:-1)
   !
   !  Example:
   !
   !   Input:
   !
   !      N = 5,
   !      A = ( 11, 12, 13, 14, 15 ).
   !
   !   Output:
   !
   !      A = ( 15, 14, 13, 12, 11 ).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 September 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input/output, integer (kind=4) :: A(N), the array to be reversed.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
   
      a(1:n) = a(n:1:-1)
   
      return
   end
   subroutine i4vec_rotate ( n, m, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_ROTATE rotates an I4VEC in place.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Example:
   !
   !   Input:
   !
   !      N = 5, M = 2
   !      A = ( 1, 2, 3, 4, 5 )
   !
   !   Output:
   !
   !      A = ( 4, 5, 1, 2, 3 ).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   07 October 1998
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of objects.
   !
   !   Input, integer (kind=4) :: M, the number of positions to the right that
   !   each element should be moved.  Elements that shift pass position
   !   N "wrap around" to the beginning of the array.
   !
   !   Input/output, integer A(N), the array to be rotated.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i4_modp
      integer (kind=4) :: iget
      integer (kind=4) :: iput
      integer (kind=4) :: istart
      integer (kind=4) :: m
      integer (kind=4) :: mcopy
      integer (kind=4) :: nset
      integer (kind=4) :: temp
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
   subroutine i4vec_run_count ( n, a, run_count )
   
   !*****************************************************************************80
   !
   !! I4VEC_RUN_COUNT counts runs of equal values in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   A run is a sequence of equal values.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   26 January 2007
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be examined.
   !
   !   Output, integer (kind=4) :: RUN_COUNT, the number of runs.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: run_count
      integer (kind=4) :: test
   
      run_count = 0
   
      if ( n < 1 ) then
       return
      end if
   
      test = 0
   
      do i = 1, n
   
       if ( i == 1 .or. a(i) /= test ) then
         run_count = run_count + 1
         test = a(i)
       end if
   
      end do
   
      return
   end
   subroutine i4vec_search_binary_a ( n, a, b, indx )
   
   !*****************************************************************************80
   !
   !! I4VEC_SEARCH_BINARY_A searches an ascending sorted I4VEC for a value.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   Binary search is used.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   16 April 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Donald Kreher, Douglas Simpson,
   !   Algorithm 1.9,
   !   Combinatorial Algorithms,
   !   CRC Press, 1998, page 26.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the array to be searched.  A must
   !   be sorted in ascending order.
   !
   !   Input, integer (kind=4) :: B, the value to be searched for.
   !
   !   Output, integer (kind=4) :: INDX, the result of the search.
   !   -1, B does not occur in A.
   !   I, A(I) = B.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: b
      integer (kind=4) :: high
      integer (kind=4) :: indx
      integer (kind=4) :: low
      integer (kind=4) :: mid
   
      indx = - 1
   
      low = 1
      high = n
   
      do while ( low <= high )
   
       mid = ( low + high ) / 2
   
       if ( a(mid) == b ) then
         indx = mid
         exit
       else if ( a(mid) < b ) then
         low = mid + 1
       else if ( b < a(mid) ) then
         high = mid - 1
       end if
   
      end do
   
      return
   end
   subroutine i4vec_search_binary_d ( n, a, b, indx )
   
   !*****************************************************************************80
   !
   !! I4VEC_SEARCH_BINARY_D searches a descending sorted I4VEC for a value.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   Binary search is used.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   16 April 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Donald Kreher, Douglas Simpson,
   !   Algorithm 1.9,
   !   Combinatorial Algorithms,
   !   CRC Press, 1998, page 26.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the array to be searched.  A must
   !   be sorted in descending order.
   !
   !   Input, integer (kind=4) :: B, the value to be searched for.
   !
   !   Output, integer (kind=4) :: INDX, the result of the search.
   !   -1, B does not occur in A.
   !   I, A(I) = B.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: b
      integer (kind=4) :: high
      integer (kind=4) :: indx
      integer (kind=4) :: low
      integer (kind=4) :: mid
   
      indx = - 1
   
      low = 1
      high = n
   
      do while ( low <= high )
   
       mid = ( low + high ) / 2
   
       if ( a(mid) == b ) then
         indx = mid
         exit
       else if ( b < a(mid) ) then
         low = mid + 1
       else if ( a(mid) < b ) then
         high = mid - 1
       end if
   
      end do
   
      return
   end
   subroutine i4vec_sort_bubble_a ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORT_BUBBLE_A ascending sorts an I4VEC using bubble sort.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   11 April 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input/output, integer (kind=4) :: A(N).
   !   On input, the array to be sorted;
   !   On output, the array has been sorted.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: j
      integer (kind=4) :: k
   
      do i = 1, n - 1
       do j = i + 1, n
         if ( a(j) < a(i) ) then
          k   = a(i)
          a(i) = a(j)
          a(j) = k
         end if
       end do
      end do
   
      return
   end
   subroutine i4vec_sort_bubble_d ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORT_BUBBLE_D descending sorts an I4VEC using bubble sort.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   26 November 2001
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input/output, integer (kind=4) :: A(N).
   !   On input, the array to be sorted;
   !   On output, the array has been sorted.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: j
      integer (kind=4) :: k
   
      do i = 1, n - 1
       do j = i + 1, n
         if ( a(i) < a(j) ) then
          k   = a(i)
          a(i) = a(j)
          a(j) = k
         end if
       end do
      end do
   
      return
   end
   subroutine i4vec_sort_heap_a ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORT_HEAP_A ascending sorts an I4VEC using heap sort.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 September 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Albert Nijenhuis, Herbert Wilf,
   !   Combinatorial Algorithms for Computers and Calculators,
   !   Academic Press, 1978,
   !   ISBN: 0-12-519260-6,
   !   LC: QA164.N54.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input/output, integer (kind=4) :: A(N).
   !   On input, the array to be sorted;
   !   On output, the array has been sorted.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: n1
      integer (kind=4) :: t
   
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
      t   = a(1)
      a(1) = a(n)
      a(n) = t
   !
   !  Consider the diminished heap of size N1.
   !
      do n1 = n - 1, 2, -1
   !
   !  Restore the heap structure of A(1) through A(N1).
   !
       call i4vec_heap_d ( n1, a )
   !
   !  Take the largest object from A(1) and move it to A(N1).
   !
       t   = a(1)
       a(1) = a(n1)
       a(n1) = t
   
      end do
   
      return
   end



   subroutine     i4vec_sort_heap_d ( n, a )
   !*****************************************************************************80
   !
   !! I4VEC_SORT_HEAP_D descending sorts an I4VEC using heap sort.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   15 April 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Albert Nijenhuis, Herbert Wilf,
   !   Combinatorial Algorithms for Computers and Calculators,
   !   Academic Press, 1978,
   !   ISBN: 0-12-519260-6,
   !   LC: QA164.N54.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input/output, integer (kind=4) :: A(N).
   !   On input, the array to be sorted;
   !   On output, the array has been sorted.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: n1
      integer (kind=4) :: t
   
      if ( n <= 1 ) then
       return
      end if
   !
   !  1: Put A into ascending heap form.
   !
      call i4vec_heap_a ( n, a )
   !
   !  2: Sort A.
   !
   !  The smallest object in the heap is in A(1).
   !  Move it to position A(N).
   !
      t   = a(1)
      a(1) = a(n)
      a(n) = t
   !
   !  Consider the diminished heap of size N1.
   !
      do n1 = n - 1, 2, -1
   !
   !  Restore the heap structure of A(1) through A(N1).
   !
       call i4vec_heap_a ( n1, a )
   !
   !  Take the smallest object from A(1) and move it to A(N1).
   !
       t    = a(1)
       a(1)  = a(n1)
       a(n1) = t
   
      end do
   
      return
   end subroutine i4vec_sort_heap_d



   subroutine i4vec_sort_heap_index_a ( n, a, indx )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The sorting is not actually carried out.  Rather an index array is
   !   created which defines the sorting.  This array may be used to sort
   !   or index the array, or to sort or index related arrays keyed on the
   !   original array.
   !
   !   Once the index array is computed, the sorting can be carried out
   !   "implicitly:
   !
   !      A(INDX(1:N)) is sorted,
   !
   !   or explicitly, by the call
   !
   !      call i4vec_permute ( n, indx, a )
   !
   !   after which A(1:N) is sorted.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 March 2004
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input, integer (kind=4) :: A(N), an array to be index-sorted.
   !
   !   Output, integer (kind=4) :: INDX(N), the sort index.  The
   !   I-th element of the sorted array is A(INDX(I)).
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: indx(n)
      integer (kind=4) :: indxt
      integer (kind=4) :: ir
      integer (kind=4) :: j
      integer (kind=4) :: l
      integer (kind=4) :: value
   
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
         value = a(indxt)
   
       else
   
         indxt = indx(ir)
         value = a(indxt)
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
   
         if ( value < a(indx(j)) ) then
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
   subroutine i4vec_sort_heap_index_d ( n, a, indx )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORT_HEAP_INDEX_D does an indexed heap descending sort of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The sorting is not actually carried out.  Rather an index array is
   !   created which defines the sorting.  This array may be used to sort
   !   or index the array, or to sort or index related arrays keyed on the
   !   original array.
   !
   !   Once the index array is computed, the sorting can be carried out
   !   "implicitly:
   !
   !      A(INDX(1:N)) is sorted,
   !
   !   or explicitly, by the call
   !
   !      call i4vec_permute ( n, indx, a )
   !
   !   after which A(1:N) is sorted.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 March 2004
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input, integer (kind=4) :: A(N), an array to be index-sorted.
   !
   !   Output, integer (kind=4) :: INDX(N), the sort index.  The
   !   I-th element of the sorted array is A(INDX(I)).
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: indx(n)
      integer (kind=4) :: indxt
      integer (kind=4) :: ir
      integer (kind=4) :: j
      integer (kind=4) :: l
      integer (kind=4) :: value
   
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
         value = a(indxt)
   
       else
   
         indxt = indx(ir)
         value = a(indxt)
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
   
         if ( a(indx(j)) < value ) then
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
   subroutine i4vec_sort_insert_a ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORT_INSERT_A uses an ascending insertion sort on an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   24 July 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Donald Kreher, Douglas Simpson,
   !   Algorithm 1.1,
   !   Combinatorial Algorithms,
   !   CRC Press, 1998, page 11.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of items in the vector.
   !   N must be positive.
   !
   !   Input/output, integer (kind=4) :: A(N).
   !   On input, A contains data to be sorted.
   !   On output, the entries of A have been sorted in ascending order.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: j
      integer (kind=4) :: x
   
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
   subroutine i4vec_sort_insert_d ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORT_INSERT_D uses a descending insertion sort on an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   24 July 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Reference:
   !
   !   Donald Kreher, Douglas Simpson,
   !   Algorithm 1.1,
   !   Combinatorial Algorithms,
   !   CRC Press, 1998, page 11.
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of items in the vector.
   !   N must be positive.
   !
   !   Input/output, integer (kind=4) :: A(N).
   !   On input, A contains data to be sorted.
   !   On output, the entries of A have been sorted in descending order.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: j
      integer (kind=4) :: x
   
      do i = 2, n
   
       x = a(i)
   
       j = i - 1
   
       do while ( 1 <= j )
   
         if ( x <= a(j) ) then
          exit
         end if
   
         a(j+1) = a(j)
         j = j - 1
   
       end do
   
       a(j+1) = x
   
      end do
   
      return
   end
   subroutine i4vec_sort_quick_a ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORT_QUICK_A ascending sorts an I4VEC using quick sort.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Example:
   !
   !   Input:
   !
   !      N = 7
   !
   !      A = (/ 6, 7, 3, 2, 9, 1, 8 /)
   !
   !   Output:
   !
   !      A = (/ 1, 2, 3, 6, 7, 8, 9 /)
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   25 September 2001
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input/output, integer (kind=4) :: A(N).
   !   On input, the array to be sorted.
   !   On output, the array has been sorted.
   !
      implicit none
   
      integer (kind=4), parameter :: level_max = 30
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: base
      integer (kind=4) :: l_segment
      integer (kind=4) :: level
      integer (kind=4) :: n_segment
      integer (kind=4) :: rsave(level_max)
      integer (kind=4) :: r_segment
   
      if ( n <= 1 ) then
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
       call i4vec_part_quick_a ( n_segment, a(base), l_segment, r_segment )
   !
   !  If the left segment has more than one element, we need to partition it.
   !
       if ( 1 < l_segment ) then
   
         if ( level_max < level ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'I4VEC_SORT_QUICK_A - Fatal error!'
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
   subroutine i4vec_sort_shell_a ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORT_SHELL_A ascending sorts an I4VEC using Shell's sort.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   12 March 2001
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input/output, integer (kind=4) :: A(N).
   !   On input, an array to be sorted.
   !   On output, the sorted array.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: asave
      integer (kind=4) :: i
      integer (kind=4) :: ifree
      integer (kind=4) :: inc
      integer (kind=4) :: ipow
      integer (kind=4) :: j
      integer (kind=4) :: k
      integer (kind=4) :: maxpow
   
      if ( n <= 1 ) then
       return
      end if
   !
   !  Determine the smallest MAXPOW so that
   !   N <= ( 3^MAXPOW - 1 ) / 2
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
         do i = inc + k, n, inc
   
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
   subroutine i4vec_sorted_undex ( x_num, x_val, x_unique_num, undx, xdnu )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORTED_UNDEX returns unique sorted indexes for a sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The goal of this routine is to determine a vector UNDX,
   !   which points, to the unique elements of X, in sorted order,
   !   and a vector XDNU, which identifies, for each entry of X, the index of
   !   the unique sorted element of X.
   !
   !   This is all done with index vectors, so that the elements of
   !   X are never moved.
   !
   !   Assuming X is already sorted, we examine the entries of X in order,
   !   noting the unique entries, creating the entries of XDNU and
   !   UNDX as we go.
   !
   !   Once this process has been completed, the vector X could be
   !   replaced by a compressed vector XU, containing the unique entries
   !   of X in sorted order, using the formula
   !
   !      XU(I) = X(UNDX(I)).
   !
   !   We could then, if we wished, reconstruct the entire vector X, or
   !   any element of it, by index, as follows:
   !
   !      X(I) = XU(XDNU(I)).
   !
   !   We could then replace X by the combination of XU and XDNU.
   !
   !   Later, when we need the I-th entry of X, we can locate it as
   !   the XDNU(I)-th entry of XU.
   !
   !   Here is an example of a vector X, the unique sort and
   !   inverse unique sort vectors and the compressed unique sorted vector.
   !
   !      I   X   XU  Undx  Xdnu
   !   ----+----+----+-----+-----+
   !      1 | 11 |  11   1    1
   !      2 | 11 |  22   5    1
   !      3 | 11 |  33   8    1
   !      4 | 11 |  55   9    1
   !      5 | 22 |             2
   !      6 | 22 |             2
   !      7 | 22 |             2
   !      8 | 33 |             3
   !      9 | 55 |             4
   !
   !   UNDX(3) = 8 means that unique sorted item(3) is at X(8).
   !   XDNU(6) = 2 means that X(6) is at unique sorted item(2).
   !
   !   XU(XDNU(I))) = X(I).
   !   XU(I)       = X(UNDX(I)).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   27 October 2008
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: X_NUM, the number of data values.
   !
   !   Input, integer (kind=4) :: X_VAL(X_NUM), the data values.
   !
   !   Input, integer (kind=4) :: X_UNIQUE_NUM, the number of unique values i
   !   n X_VAL.  This value is only required for languages in which the size of
   !   UNDX must be known in advance.
   !
   !   Output, integer (kind=4) :: UNDX(X_UNIQUE_NUM), the UNDX vector.
   !
   !   Output, integer (kind=4) :: XDNU(X_NUM), the XDNU vector.
   !
      implicit none
   
      integer (kind=4) :: x_num
      integer (kind=4) :: x_unique_num
   
      integer (kind=4) :: i
      integer (kind=4) :: j
      integer (kind=4) :: undx(x_unique_num)
      integer (kind=4) :: x_val(x_num)
      integer (kind=4) :: xdnu(x_num)
   !
   !  Walk through the sorted array.
   !
      i = 1
   
      j = 1
      undx(j) = i
   
      xdnu(i) = j
   
      do i = 2, x_num
   
       if ( x_val(i) /= x_val(undx(j)) ) then
         j = j + 1
         undx(j) = i
       end if
   
       xdnu(i) = j
   
      end do
   
      return
   end
   subroutine i4vec_sorted_unique ( n, a, unique_num )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORTED_UNIQUE finds the unique elements in a sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   09 July 2000
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements in A.
   !
   !   Input/output, integer (kind=4) :: A(N).  On input, the sorted
   !   integer array.  On output, the unique elements in A.
   !
   !   Output, integer (kind=4) :: UNIQUE_NUM, the number of unique elements.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: itest
      integer (kind=4) :: unique_num
   
      if ( n <= 0 ) then
       unique_num = 0
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
   subroutine i4vec_sorted_unique_count ( n, a, unique_num )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORTED_UNIQUE_COUNT counts the unique elements in a sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   Because the array is sorted, this algorithm is O(N).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   29 April 2004
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Input, integer (kind=4) :: A(N), the sorted array to examine.
   !
   !   Output, integer (kind=4) :: UNIQUE_NUM, the number of unique elements.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: unique_num
   
      if ( n < 1 ) then
       unique_num = 0
       return
      end if
   
      unique_num = 1
   
      do i = 2, n
   
       if ( a(i-1) /= a(i) ) then
         unique_num = unique_num + 1
       end if
   
      end do
   
      return
   end
   subroutine i4vec_sorted_unique_hist ( n, a, maxuniq, unique_num, auniq, acount )
   
   !*****************************************************************************80
   !
   !! I4VEC_SORTED_UNIQUE_HIST histograms the unique elements of a sorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   01 September 2003
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Input, integer (kind=4) :: A(N), the array to examine.  The elements of A
   !   should have been sorted.
   !
   !   Input, integer (kind=4) :: MAXUNIQ, the maximum number of unique elements
   !   that can be handled.  If there are more than MAXUNIQ unique
   !   elements in A, the excess will be ignored.
   !
   !   Output, integer (kind=4) :: UNIQUE_NUM, the number of unique elements.
   !
   !   Output, integer (kind=4) :: AUNIQ(UNIQUE_NUM), the unique elements of A.
   !
   !   Output, integer (kind=4) :: ACOUNT(UNIQUE_NUM), the number of times
   !   each element of AUNIQ occurs in A.
   !
      implicit none
   
      integer (kind=4) :: maxuniq
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: acount(maxuniq)
      integer (kind=4) :: auniq(maxuniq)
      integer (kind=4) :: i
      integer (kind=4) :: unique_num
   !
   !  Start taking statistics.
   !
      unique_num = 0
   
      do i = 1, n
   
       if ( i == 1 ) then
   
         unique_num = 1
         auniq(unique_num) = a(1)
         acount(unique_num) = 1
   
       else if ( a(i) == auniq(unique_num) ) then
   
         acount(unique_num) = acount(unique_num) + 1
   
       else if ( unique_num < maxuniq ) then
   
         unique_num = unique_num + 1
         auniq(unique_num) = a(i)
         acount(unique_num) = 1
   
       end if
   
      end do
   
      return
   end
   subroutine i4vec_split ( n, a, split, split_index )
   
   !*****************************************************************************80
   !
   !! I4VEC_SPLIT "splits" an unsorted I4VEC based on a splitting value.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   If the vector is already sorted, it is simpler to do a binary search
   !   on the data than to call this routine.
   !
   !   The vector is not assumed to be sorted before input, and is not
   !   sorted during processing.  If sorting is not needed, then it is
   !   more efficient to use this routine.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 September 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Input/output, integer (kind=4) :: A(N), the array to split.  On output,
   !   all the entries of A that are less than or equal to SPLIT
   !   are in A(1:SPLIT_INDEX).
   !
   !   Input, integer (kind=4) :: SPLIT, the value used to split the vector.
   !   It is not necessary that any value of A actually equal SPLIT.
   !
   !   Output, integer (kind=4) :: SPLIT_INDEX, indicates the position of the
   !   last entry of the split vector that is less than or equal to SPLIT.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: i1
      integer (kind=4) :: i2
      integer (kind=4) :: i3
      integer (kind=4) :: j1
      integer (kind=4) :: j2
      integer (kind=4) :: j3
      integer (kind=4) :: split
      integer (kind=4) :: split_index
      integer (kind=4) :: t
   !
   !  Partition the vector into A1, A2, A3, where
   !   A1 = A(I1:J1) holds values <= SPLIT,
   !   A2 = A(I2:J2) holds untested values,
   !   A3 = A(I3:J3) holds values > SPLIT.
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
   
         t       = a(i2)
         a(i2)   = a(i3-1)
         a(i3-1) = t
   
         i3 = i3 - 1
         j2 = j2 - 1
   
       end if
   
      end do
   
      split_index = j1
   
      return
   end

subroutine i4vec_split_unsort ( n, a, split, isplit )
!*****************************************************************************80
!
!! I4VEC_SPLIT_UNSORT "splits" an unsorted I4VEC based on a splitting value.
!
!  Discussion:
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
!    18 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input/output, integer ( kind = 4 ) A(N), the array to split.  On output,
!    all the entries of A that are less than or equal to SPLIT
!    are in A(1:ISPLIT).
!
!    Input, integer ( kind = 4 ) SPLIT, the value used to split the vector.
!    It is not necessary that any value of A actually equal SPLIT.
!
!    Output, integer ( kind = 4 ) ISPLIT, indicates the position of the last
!    entry of the split vector that is less than or equal to SPLIT.
!> @see: dutch.f90
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) isplit
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j3
  integer ( kind = 4 ) split
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

  i3 = n+1
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
      call i4_swap ( a(i2), a(i3-1) )
      i3 = i3 - 1
      j2 = j2 - 1
    end if

  end do

  isplit = j1

  return
end subroutine i4vec_split_unsort

   subroutine i4vec_std ( n, a, std )
   
   !*****************************************************************************80
   !
   !! I4VEC_STD returns the standard deviation of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The standard deviation of a vector X of length N is defined as
   !
   !      mean ( X(1:n) ) = sum ( X(1:n) ) / n
   !
   !      std ( X(1:n) ) = sqrt ( sum ( ( X(1:n) - mean )^2 ) / ( n - 1 ) )
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   14 August 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector whose variance is desired.
   !
   !   Output, real ( kind = 8 ) STD, the standard deviation of the entries.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      real ( kind = 8 ) mean
      real ( kind = 8 ) std
   
      if ( n < 2 ) then
   
       std = 0.0D+00
   
      else
   
       mean = real ( sum ( a(1:n) ), kind = 8 ) / real ( n, kind = 8 )
   
       std = sum ( ( real ( a(1:n), kind = 8 ) - mean )**2 )
   
       std = sqrt ( std / real ( n - 1, kind = 8 ) )
   
      end if
   
      return
   end
   function i4vec_sum ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_SUM returns the sum of the entries of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   In FORTRAN90, this facility is offered by the built in
   !   SUM function:
   !
   !      I4VEC_SUM ( N, A ) = SUM ( A(1:N) )
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   22 September 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the array.
   !
   !   Input, integer (kind=4) :: A(N), the array.
   !
   !   Output, integer (kind=4) :: I4VEC_SUM, the sum of the entries.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i4vec_sum
   
      i4vec_sum = sum ( a(1:n) )
   
      return
   end
   subroutine i4vec_swap ( n, a1, a2 )
   
   !*****************************************************************************80
   !
   !! I4VEC_SWAP swaps the entries of two I4VEC's.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   04 April 2001
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the arrays.
   !
   !   Input/output, integer (kind=4) :: A1(N), A2(N), the vectors to swap.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a1(n)
      integer (kind=4) :: a2(n)
      integer (kind=4) :: a3(n)
   
      a3(1:n) = a1(1:n)
      a1(1:n) = a2(1:n)
      a2(1:n) = a3(1:n)
   
      return
   end
   subroutine i4vec_transpose_print ( n, a, title )
   
   !*****************************************************************************80
   !
   !! I4VEC_TRANSPOSE_PRINT prints an I4VEC "transposed".
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Example:
   !
   !   A = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 /)
   !   TITLE = 'My vector:  '
   !
   !   My vector:
   !
   !       1   2   3   4   5
   !       6   7   8   9   10
   !       11
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   16 April 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of components of the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector to be printed.
   !
   !   Input, character ( len = * ) TITLE, a title.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: ihi
      integer (kind=4) :: ilo
      character ( len = * ) title
   
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do ilo = 1, n, 5
       ihi = min ( ilo + 5 - 1, n )
       write ( *, '(5i12)' ) a(ilo:ihi)
      end do
   
      return
   end
   subroutine i4vec_undex ( x_num, x_val, x_unique_num, undx, xdnu )
   
   !*****************************************************************************80
   !
   !! I4VEC_UNDEX returns unique sorted indexes for an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The goal of this routine is to determine a vector UNDX,
   !   which points, to the unique elements of X, in sorted order,
   !   and a vector XDNU, which identifies, for each entry of X, the index of
   !   the unique sorted element of X.
   !
   !   This is all done with index vectors, so that the elements of
   !   X are never moved.
   !
   !   The first step of the algorithm requires the indexed sorting
   !   of X, which creates arrays INDX and XDNI.  (If all the entries
   !   of X are unique, then these arrays are the same as UNDX and XDNU.)
   !
   !   We then use INDX to examine the entries of X in sorted order,
   !   noting the unique entries, creating the entries of XDNU and
   !   UNDX as we go.
   !
   !   Once this process has been completed, the vector X could be
   !   replaced by a compressed vector XU, containing the unique entries
   !   of X in sorted order, using the formula
   !
   !      XU(1:X_UNIQUE_NUM) = X(UNDX(1:X_UNIQUE_NUM)).
   !
   !   We could then, if we wished, reconstruct the entire vector X, or
   !   any element of it, by index, as follows:
   !
   !      X(I) = XU(XDNU(I)).
   !
   !   We could then replace X by the combination of XU and XDNU.
   !
   !   Later, when we need the I-th entry of X, we can locate it as
   !   the XDNU(I)-th entry of XU.
   !
   !   Here is an example of a vector X, the sort and inverse sort
   !   index vectors, and the unique sort and inverse unique sort vectors
   !   and the compressed unique sorted vector.
   !
   !      I   X  Indx  Xdni      XU  Undx  Xdnu
   !   ----+----+-----+-----+-------+-----+-----+
   !      1 | 11    1    1 |   11    1    1
   !      2 | 22    3    5 |   22    2    2
   !      3 | 11    6    2 |   33    4    1
   !      4 | 33    9    8 |   55    5    3
   !      5 | 55    2    9 |               4
   !      6 | 11    7    3 |               1
   !      7 | 22    8    6 |               2
   !      8 | 22    4    7 |               2
   !      9 | 11    5    4 |               1
   !
   !   INDX(2) = 3 means that sorted item(2) is X(3).
   !   XDNI(2) = 5 means that X(2) is sorted item(5).
   !
   !   UNDX(3) = 4 means that unique sorted item(3) is at X(4).
   !   XDNU(8) = 2 means that X(8) is at unique sorted item(2).
   !
   !   XU(XDNU(I))) = X(I).
   !   XU(I)       = X(UNDX(I)).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   27 October 2008
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: X_NUM, the number of data values.
   !
   !   Input, integer (kind=4) :: X_VAL(X_NUM), the data values.
   !
   !   Input, integer (kind=4) :: X_UNIQUE_NUM, the number of unique values
   !   in X_VAL.  This value is only required for languages in which the size of
   !   UNDX must be known in advance.
   !
   !   Output, integer (kind=4) :: UNDX(X_UNIQUE_NUM), the UNDX vector.
   !
   !   Output, integer (kind=4) :: XDNU(X_NUM), the XDNU vector.
   !
      implicit none
   
      integer (kind=4) :: x_num
      integer (kind=4) :: x_unique_num
   
      integer (kind=4) :: i
      integer (kind=4) :: indx(x_num)
      integer (kind=4) :: j
      integer (kind=4) :: undx(x_unique_num)
      integer (kind=4) :: x_val(x_num)
      integer (kind=4) :: xdnu(x_num)
   !
   !  Implicitly sort the array.
   !
      call i4vec_sort_heap_index_a ( x_num, x_val, indx )
   !
   !  Walk through the implicitly sorted array.
   !
      i = 1
   
      j = 1
      undx(j) = indx(i)
   
      xdnu(indx(i)) = j
   
      do i = 2, x_num
   
       if ( x_val(indx(i)) /= x_val(undx(j)) ) then
         j = j + 1
         undx(j) = indx(i)
       end if
   
       xdnu(indx(i)) = j
   
      end do
   
      return
   end
   subroutine i4vec_uniform_ab ( n, a, b, seed, x )
   
   !*****************************************************************************80
   !
   !! I4VEC_UNIFORM_AB returns a scaled pseudorandom I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The pseudorandom numbers should be scaled to be uniformly distributed
   !   between A and B.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   27 November 2006
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the dimension of the vector.
   !
   !   Input, integer (kind=4) :: A, B, the limits of the interval.
   !
   !   Input/output, integer (kind=4) :: SEED, the "seed" value, which
   !   should NOT be 0.  On output, SEED has been updated.
   !
   !   Output, integer (kind=4) :: X(N), a vector of numbers between A and B.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a
      integer (kind=4) :: b
      integer (kind=4) :: i
      integer (kind=4), parameter :: i4_huge = 2147483647
      integer (kind=4) :: k
      real ( kind = 4 ) r
      integer (kind=4) :: seed
      integer (kind=4) :: value
      integer (kind=4) :: x(n)
   
      if ( seed == 0 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'I4VEC_UNIFORM_AB - Fatal error!'
       write ( *, '(a)' ) '  Input value of SEED = 0.'
       stop
      end if
   
      do i = 1, n
   
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
         +            r   * ( real ( max ( a, b ), kind = 4 ) + 0.5E+00 )
   !
   !  Use rounding to convert R to an integer between A and B.
   !
       value = nint ( r, kind = 4 )
   
       value = max ( value, min ( a, b ) )
       value = min ( value, max ( a, b ) )
   
       x(i) = value
   
      end do
   
      return
   end
   subroutine i4vec_unique_count ( n, a, unique_num )
   
   !*****************************************************************************80
   !
   !! I4VEC_UNIQUE_COUNT counts the unique elements in an unsorted I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   Because the array is unsorted, this algorithm is O(N^2).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   29 April 2004
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Input, integer (kind=4) :: A(N), the unsorted array to examine.
   !
   !   Output, integer (kind=4) :: UNIQUE_NUM, the number of unique elements
   !   of A.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: j
      integer (kind=4) :: unique_num
   
      unique_num = 0
   
      do i = 1, n
   
       unique_num = unique_num + 1
   
       do j = 1, i - 1
   
         if ( a(i) == a(j) ) then
          unique_num = unique_num - 1
          exit
         end if
   
       end do
   
      end do
   
      return
   end
   subroutine i4vec_unique_index ( n, a, unique_index )
   
   !*****************************************************************************80
   !
   !! I4VEC_UNIQUE_INDEX indexes the unique occurrence of values in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   For element A(I) of the vector, UNIQUE_INDEX(I) is the uniqueness index
   !   of A(I).  That is, if A_UNIQUE contains the unique elements of A,
   !   gathered in order, then
   !
   !      A_UNIQUE ( UNIQUE_INDEX(I) ) = A(I)
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   24 August 2008
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of elements of A.
   !
   !   Input, integer (kind=4) :: A(N), the array.
   !
   !   Output, integer (kind=4) :: UNIQUE_INDEX(N), the unique index.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: j
      integer (kind=4) :: unique_index(n)
      integer (kind=4) :: unique_num
   
      unique_index(1:n) = -1
      unique_num = 0
   
      do i = 1, n
   
       if ( unique_index(i) == -1 ) then
   
         unique_num = unique_num + 1
         unique_index(i) = unique_num
   
         do j = i + 1, n
          if ( a(i) == a(j) ) then
             unique_index(j) = unique_num
          end if
         end do
   
       end if
   
      end do
   
      return
   end
   subroutine i4vec_value_index ( n, a, value, max_index, n_index, value_index )
   
   !*****************************************************************************80
   !
   !! I4VEC_VALUE_INDEX indexes entries equal to a given value in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Example:
   !
   !   Input:
   !
   !      N = 10
   !      A = (  2, 3, 1, 3, 2, 4, 2, 3, 5, 3 )
   !      X_VALUE = 3
   !
   !   Output:
   !
   !      N_INDEX = 4
   !      VALUE_INDEX = ( 2, 4, 8, 10 ).
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   30 July 1999
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of objects.
   !
   !   Input, integer (kind=4) :: A(N), the array to be indexed.
   !
   !   Input, integer (kind=4) :: VALUE, a value to be searched for.
   !
   !   Input, integer (kind=4) :: MAX_INDEX, the maximum number of indices
   !   to find.
   !
   !   Output, integer (kind=4) :: N_INDEX, the number of entries equal to VALUE.
   !
   !   Output, integer (kind=4) :: VALUE_INDEX(MAX_INDEX), the indices of entries
   !   equal to VALUE.
   !
      implicit none
   
      integer (kind=4) :: max_index
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: n_index
      integer (kind=4) :: value
      integer (kind=4) :: value_index(max_index)
   
      n_index = 0
   
      do i = 1, n
   
       if ( a(i) == value ) then
   
         if ( max_index <= n_index ) then
          return
         end if
   
         n_index = n_index + 1
         value_index(n_index) = i
   
       end if
   
      end do
   
      return
   end
   subroutine i4vec_value_num ( n, a, value, value_num )
   
   !*****************************************************************************80
   !
   !! I4VEC_VALUE_NUM counts entries equal to a given value in an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   21 September 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of objects.
   !
   !   Input, integer (kind=4) :: A(N), the array to be indexed.
   !
   !   Input, integer (kind=4) :: VALUE, a value to be searched for.
   !
   !   Input, integer (kind=4) :: VALUE_NUM, the number of times the
   !   value occurs.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: value
      integer (kind=4) :: value_num
   
      value_num = 0
   
      do i = 1, n
   
       if ( a(i) == value ) then
         value_num = value_num + 1
       end if
   
      end do
   
      return
   end
   subroutine i4vec_variance ( n, a, variance )
   
   !*****************************************************************************80
   !
   !! I4VEC_VARIANCE returns the variance of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   13 August 2009
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector whose variance is desired.
   !
   !   Output, real ( kind = 8 ) VARIANCE, the variance of the vector entries.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      real ( kind = 8 ) mean
      real ( kind = 8 ) variance
   
      if ( n < 2 ) then
   
       variance = 0.0D+00
   
      else
   
       mean = real ( sum ( a(1:n) ), kind = 8 ) / real ( n, kind = 8 )
   
       variance = sum ( ( real ( a(1:n), kind = 8 ) - mean )**2 )
   
       variance = variance / real ( n - 1, kind = 8 )
   
      end if
   
      return
   end
   function i4vec_width ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_WIDTH returns the "width" of an I4VEC.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !   The width of an integer vector is simply the maximum of the widths of
   !   its entries.
   !
   !   The width of a single integer is the number of characters
   !   necessary to print it.
   !
   !   The width of an integer vector can be useful when the vector is
   !   to be printed.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   04 December 2004
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Input, integer (kind=4) :: A(N), the vector.
   !
   !   Output, integer (kind=4) :: I4VEC_WIDTH, the width of the vector.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
      integer (kind=4) :: i
      integer (kind=4) :: i4_width
      integer (kind=4) :: i4vec_width
   
      i4vec_width = -1
   
      do i = 1, n
       i4vec_width = max ( i4vec_width, i4_width ( a(i) ) )
      end do
   
      return
   end
   subroutine i4vec_zero ( n, a )
   
   !*****************************************************************************80
   !
   !! I4VEC_ZERO sets the entries of an I4VEC to 0.
   !
   !  Discussion:
   !
   !   An I4VEC is a vector of I4's.
   !
   !  Licensing:
   !
   !   This code is distributed under the GNU LGPL license.
   !
   !  Modified:
   !
   !   22 September 2005
   !
   !  Author:
   !
   !   John Burkardt
   !
   !  Parameters:
   !
   !   Input, integer (kind=4) :: N, the number of entries in the vector.
   !
   !   Output, integer (kind=4) :: A(N), the vector, which has been set to zero.
   !
      implicit none
   
      integer (kind=4) :: n
   
      integer (kind=4) :: a(n)
   
      a(1:n) = 0
   
      return
   end


    subroutine i4vec2_compare ( n, a1, a2, i, j, isgn )
    
    !*****************************************************************************80
    !
    !! I4VEC2_COMPARE compares entries of an I4VEC2.
    !
    !  Discussion:
    !
    !    An I4VEC2 is a pair of I4VEC's.
    !
    !    An I4VEC is a vector of I4's.
    !
    !    Entry K of an I4VEC2 is the pair of values located
    !    at the K-th entries of the two I4VEC's.
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
    !    Input, integer (kind=4) :: N, the number of data items.
    !
    !    Input, integer (kind=4) :: A1(N), A2(N), contain the two components
    !    of each item.
    !
    !    Input, integer (kind=4) :: I, J, the items to be compared.
    !
    !    Output, integer (kind=4) :: ISGN, the results of the comparison:
    !    -1, item I < item J,
    !     0, item I = item J,
    !    +1, item J < item I.
    !
      implicit none
    
      integer (kind=4) :: n
    
      integer (kind=4) :: a1(n)
      integer (kind=4) :: a2(n)
      integer (kind=4) :: i
      integer (kind=4) :: isgn
      integer (kind=4) :: j
    
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
    subroutine i4vec2_print ( n, a, b, title )
    
    !*****************************************************************************80
    !
    !! I4VEC2_PRINT prints a pair of integer vectors.
    !
    !  Discussion:
    !
    !    An I4VEC2 is a pair of I4VEC's.
    !
    !    An I4VEC is a vector of I4's.
    !
    !    Entry K of an I4VEC2 is the pair of values located
    !    at the K-th entries of the two I4VEC's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    09 May 2000
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer (kind=4) :: N, the number of components of the vector.
    !
    !    Input, integer (kind=4) :: A(N), B(N), the vectors to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer (kind=4) :: n
    
      integer (kind=4) :: a(n)
      integer (kind=4) :: b(n)
      integer (kind=4) :: i
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,i10,2x,i10)' ) i, ':', a(i), b(i)
      end do
    
      return
    end
    subroutine i4vec2_sort_a ( n, a1, a2 )
    
    !*****************************************************************************80
    !
    !! I4VEC2_SORT_A ascending sorts a vector of pairs of integers.
    !
    !  Discussion:
    !
    !    An I4VEC2 is a pair of I4VEC's.
    !
    !    An I4VEC is a vector of I4's.
    !
    !    Entry K of an I4VEC2 is the pair of values located
    !    at the K-th entries of the two I4VEC's.
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
    !    Input, integer (kind=4) :: N, the number of items of data.
    !
    !    Input/output, integer (kind=4) :: A1(N), A2(N), the data to be sorted.
    !
      implicit none
    
      integer (kind=4) :: n
    
      integer (kind=4) :: a1(n)
      integer (kind=4) :: a2(n)
      integer (kind=4) :: i
      integer (kind=4) :: indx
      integer (kind=4) :: isgn
      integer (kind=4) :: j
      integer (kind=4) :: t
    
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
    
          t     = a1(i)
          a1(i) = a1(j)
          a1(j) = t
    
          t     = a2(i)
          a2(i) = a2(j)
          a2(j) = t
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
    subroutine i4vec2_sort_d ( n, a1, a2 )
    
    !*****************************************************************************80
    !
    !! I4VEC2_SORT_D descending sorts a vector of pairs of integers.
    !
    !  Discussion:
    !
    !    An I4VEC2 is a pair of I4VEC's.
    !
    !    An I4VEC is a vector of I4's.
    !
    !    Entry K of an I4VEC2 is the pair of values located
    !    at the K-th entries of the two I4VEC's.
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
    !    Input, integer (kind=4) :: N, the number of items of data.
    !
    !    Input/output, integer (kind=4) :: A1(N), A2(N), the data to be sorted.
    !
      implicit none
    
      integer (kind=4) :: n
    
      integer (kind=4) :: a1(n)
      integer (kind=4) :: a2(n)
      integer (kind=4) :: i
      integer (kind=4) :: indx
      integer (kind=4) :: isgn
      integer (kind=4) :: j
      integer (kind=4) :: t
    
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
    
          t     = a1(i)
          a1(i) = a1(j)
          a1(j) = t
    
          t     = a2(i)
          a2(i) = a2(j)
          a2(j) = t
    !
    !  Compare the I and J objects.
    !
        else if ( indx < 0 ) then
    
          call i4vec2_compare ( n, a1, a2, i, j, isgn )
          isgn = -isgn
    
        else if ( indx == 0 ) then
    
          exit
    
        end if
    
      end do
    
      return
    end
    function l_to_i4 ( l )
    
    !*****************************************************************************80
    !
    !! L_TO_I4 converts an L to an I4.
    !
    !  Discussion:
    !
    !    0 is FALSE, and anything else if TRUE.
    !
    !    An I4 is an integer value.
    !    An L is a logical value.
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
    !    Input, logical L, a logical value.
    !
    !    Output, integer (kind=4) :: L_TO_I4, the integer value of L.
    !
      implicit none
    
      logical l
      integer (kind=4) :: l_to_i4
      integer (kind=4) :: value
    
      if ( l ) then
        value = 1
      else
        value = 0
      end if
    
      l_to_i4 = value
    
      return
    end
    subroutine i4vec2_sorted_unique ( n, a1, a2, unique_num )
    
    !*****************************************************************************80
    !
    !! I4VEC2_SORTED_UNIQUE keeps the unique elements in a sorted I4VEC2.
    !
    !  Discussion:
    !
    !    An I4VEC2 is a pair of I4VEC's.
    !
    !    An I4VEC is a vector of I4's.
    !
    !    Entry K of an I4VEC2 is the pair of values located
    !    at the K-th entries of the two I4VEC's.
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
    !    Input, integer (kind=4) :: N, the number of items.
    !
    !    Input/output, integer (kind=4) :: A1(N), A2(N).
    !    On input, the array of N items.
    !    On output, an array of unique items.
    !
    !    Output, integer (kind=4) :: UNIQUE_NUM, the number of unique items.
    !
      implicit none
    
      integer (kind=4) :: n
    
      integer (kind=4) :: a1(n)
      integer (kind=4) :: a2(n)
      integer (kind=4) :: itest
      integer (kind=4) :: unique_num
    
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

    subroutine perm_check ( n, p, base, ierror )
    
    !*****************************************************************************80
    !
    !! PERM_CHECK checks that a vector represents a permutation.
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
    !    Input, integer (kind=4) :: N, the number of entries.
    !
    !    Input, integer (kind=4) :: P(N), the array to check.
    !
    !    Input, integer (kind=4) :: BASE, the index base.
    !
    !    Output, integer (kind=4) :: IERROR, error flag.
    !    0, the array represents a permutation.
    !    nonzero, the array does not represent a permutation.  The smallest
    !    missing value is equal to IERROR.
    !
      implicit none
    
      integer (kind=4) :: n
    
      integer (kind=4) :: base
      integer (kind=4) :: find
      integer (kind=4) :: ierror
      integer (kind=4) :: p(n)
      integer (kind=4) :: seek
    
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
          write ( *, '(a)' ) 'PERM_CHECK - Fatal error!'
          write ( *, '(a)' ) '  The input array does not represent'
          write ( *, '(a)' ) '  a proper permutation.'
          stop
        end if
    
      end do
    
      return
    end
    subroutine perm_cycle ( n, iopt, p, isgn, ncycle )
    
    !*****************************************************************************80
    !
    !! PERM_CYCLE analyzes a permutation.
    !
    !  Discussion:
    !
    !    The routine will count cycles, find the sign of a permutation,
    !    and tag a permutation.
    !
    !  Example:
    !
    !    Input:
    !
    !      N = 9
    !      IOPT = 1
    !      P = 2, 3, 9, 6, 7, 8, 5, 4, 1
    !
    !    Output:
    !
    !      NCYCLE = 3
    !      ISGN = +1
    !      P = -2, 3, 9, -6, -7, 8, 5, 4, 1
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    09 July 2000
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
    !    Input, integer (kind=4) :: N, the number of objects being permuted.
    !
    !    Input, integer (kind=4) :: IOPT, requests tagging.
    !    0, the permutation will not be tagged.
    !    1, the permutation will be tagged.
    !
    !    Input/output, integer (kind=4) :: P(N).  On input, P describes a
    !    permutation, in the sense that entry I is to be moved to P(I).
    !    If IOPT = 0, then P will not be changed by this routine.
    !    If IOPT = 1, then on output, P will be "tagged".  That is,
    !    one element of every cycle in P will be negated.  In this way,
    !    a user can traverse a cycle by starting at any entry I1 of P
    !    which is negative, moving to I2 = ABS(P(I1)), then to
    !    P(I2), and so on, until returning to I1.
    !
    !    Output, integer (kind=4) :: ISGN, the "sign" of the permutation, which is
    !    +1 if the permutation is even, -1 if odd.  Every permutation
    !    may be produced by a certain number of pairwise switches.
    !    If the number of switches is even, the permutation itself is
    !    called even.
    !
    !    Output, integer (kind=4) :: NCYCLE, the number of cycles in the
    !    permutation.
    !
      implicit none
    
      integer (kind=4) :: n
    
      integer (kind=4), parameter :: base = 1
      integer (kind=4) :: i
      integer (kind=4) :: i1
      integer (kind=4) :: i2
      integer (kind=4) :: i4_sign
      integer (kind=4) :: ierror
      integer (kind=4) :: iopt
      integer (kind=4) :: is
      integer (kind=4) :: isgn
      integer (kind=4) :: ncycle
      integer (kind=4) :: p(n)
    
      call perm_check ( n, p, base, ierror )
    
      if ( ierror /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PERM_CYCLE - Fatal error!'
        write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
        stop
      end if
    
      is = 1
      ncycle = n
    
      do i = 1, n
    
        i1 = p(i)
    
        do while ( i < i1 )
          ncycle = ncycle - 1
          i2 = p(i1)
          p(i1) = -i2
          i1 = i2
        end do
    
        if ( iopt /= 0 ) then
          is = - i4_sign ( p(i) )
        end if
    
        p(i) = is * abs ( p(i) )
    
      end do
    
      isgn = 1 - 2 * mod ( n - ncycle, 2 )
    
      return
    end
    subroutine perm_uniform ( n, base, seed, p )
    
    !*****************************************************************************80
    !
    !! PERM_UNIFORM selects a random permutation of N objects.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    18 November 2008
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
    !    Input, integer (kind=4) :: N, the number of objects to be permuted.
    !
    !    Input, integer (kind=4) :: BASE, is 0 for a 0-based permutation and 1 for
    !    a 1-based permutation.
    !
    !    Input/output, integer (kind=4) :: SEED, a seed for the random
    !    number generator.
    !
    !    Output, integer (kind=4) :: P(N), the permutation.  P(I) is the "new"
    !    location of the object originally at I.
    !
      implicit none
    
      integer (kind=4) :: n
    
      integer (kind=4) :: base
      integer (kind=4) :: i
      integer (kind=4) :: i4_uniform_ab
      integer (kind=4) :: j
      integer (kind=4) :: k
      integer (kind=4) :: p(n)
      integer (kind=4) :: seed
    
      do i = 1, n
        p(i) = ( i - 1 ) + base
      end do
    
      do i = 1, n
        j = i4_uniform_ab ( i, n, seed )
        k    = p(i)
        p(i) = p(j)
        p(j) = k
      end do
    
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
    !    routine may be used to sort integers, reals, numbers, names,
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
    !    Combinatorial Algorithms for Computers and Calculators,
    !    Academic Press, 1978,
    !    ISBN: 0-12-519260-6,
    !    LC: QA164.N54.
    !
    !  Parameters:
    !
    !    Input, integer (kind=4) :: N, the number of items to be sorted.
    !
    !    Input/output, integer (kind=4) :: INDX, the main communication signal.
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
    !    Output, integer (kind=4) :: I, J, the indices of two items.
    !    On return with INDX positive, elements I and J should be interchanged.
    !    On return with INDX negative, elements I and J should be compared, and
    !    the result reported in ISGN on the next call.
    !
    !    Input, integer (kind=4) :: ISGN, results of comparison of elements
    !    I and J. (Used only when the previous call returned INDX less than 0).
    !    ISGN <= 0 means I is less than or equal to J;
    !    0 <= ISGN means I is greater than or equal to J.
    !
      implicit none
    
      integer (kind=4) :: i
      integer (kind=4), save :: i_save = 0
      integer (kind=4) :: indx
      integer (kind=4) :: isgn
      integer (kind=4) :: j
      integer (kind=4), save :: j_save = 0
      integer (kind=4), save :: k = 0
      integer (kind=4), save :: k1 = 0
      integer (kind=4) :: n
      integer (kind=4), save :: n1 = 0
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


subroutine perm_print ( n, p, title )

!*****************************************************************************80
!
!! PERM_PRINT prints a permutation.
!
!  Example:
!
!    Input:
!
!      P = 7 2 4 1 5 3 6
!
!    Printed output:
!
!      "This is the permutation:"
!
!      1 2 3 4 5 6 7
!      7 2 4 1 5 3 6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects permuted.
!
!    Input, integer ( kind = 4 ) P(N), the permutation, in standard index form.
!
!    Input, character ( len = * ) TITLE, an optional title.
!    If no title is supplied, then only the permutation is printed.
!> @see dutch.f90

  implicit none

  integer ( kind = 4 ), parameter :: inc = 20
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) p(n)
  character ( len = * ) title

  if ( len_trim ( title ) /= 0 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )

    do ilo = 1, n, inc
      ihi = min ( n, ilo + inc - 1 )
      write ( *, '(a)' ) ' '
      write ( *, '(20i4)' ) ( i, i = ilo, ihi )
      write ( *, '(20i4)' ) p(ilo:ihi)
    end do

  else

    do ilo = 1, n, inc
      ihi = min ( n, ilo + inc - 1 )
      write ( *, '(20i4)' ) p(ilo:ihi)
    end do

  end if

  return
end


subroutine perm_random ( n, seed, p )

!*****************************************************************************80
!
!! PERM_RANDOM returns a random permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Marc de Berg, Marc van Kreveld, Mark Overmars, Otfried Schwarzkopf,
!    Computational Geometry,
!    Second Edition,
!    Springer, 2000, page 78.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items to permute.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) P(N), a permutation of the numbers
!    from 1 to N.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform
  integer ( kind = 4 ) j
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seed

  call i4vec_indicator ( n, p )

  do i = n, 2, -1
    j = i4_uniform ( 1, i, seed )
    call i4_swap ( p(i), p(j) )
  end do

  return
end subroutine perm_random


end module jburk_i4vec_

