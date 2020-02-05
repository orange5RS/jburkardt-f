
!> @see https://github.com/Mohid-Water-Modelling-System/Mohid/wiki/Documenting-Fortran-with-Doxygen
module     jburk_r8lib_i4vec_
use, intrinsic :: iso_fortran_env
implicit none

   interface        i4vec_indicator
   module procedure i4vec_indicator
   end interface    i4vec_indicator
   public           i4vec_indicator

   interface        i4vec_permute
   module procedure i4vec_permute
   end interface    i4vec_permute
   public           i4vec_permute

   interface        i4vec_print
   module procedure i4vec_print
   end interface    i4vec_print
   public           i4vec_print

   interface        perm_check
   module procedure perm_check
   end interface    perm_check
   public           perm_check

   interface        perm_uniform
   module procedure perm_uniform
   end interface    perm_uniform
   public           perm_uniform

contains



!> @author John Burkardt
!> @brief  An I4VEC is a vector of I4's.
!> @date   2007-05-01
!> @see    
subroutine     i4vec_indicator (n, a)
implicit none
   integer(kind=4), intent(in) :: n
   integer(kind=4), intent(out) :: a(n)
   integer(kind=4) :: i

   do i = 1, n
      a(i) = i
   end do
end subroutine i4vec_indicator

!*****************************************************************************80
!
!! I4VEC_INDICATOR sets an I4VEC to the indicator vector.
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
!    01 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Output, integer ( kind = 4 ) A(N), the array to be initialized.
!



!> @author John Burkardt
!> @brief  I4VEC_PERMUTE permutes an I4VEC in place.
!> @date   2000-07-20
!> @see    
subroutine     i4vec_permute (n, p, a)
implicit none
   integer(kind=4), intent(in)    :: n
   integer(kind=4), intent(inout) :: a(n)
   integer(kind=4), intent(inout) :: p(n)

   integer(kind=4), parameter :: base = 1
   integer(kind=4) :: a_temp
   integer(kind=4) :: ierror, iget, iput, istart

   call perm_check (n, p, base, ierror)

   ! Search for the next element of the permutation that has not been used.
   do istart = 1, n
      if (p(istart) < 0) then
         cycle

      else if (p(istart) .eq. istart) then
         p(istart) = - p(istart)
         cycle

      else
         a_temp = a(istart)
         iget = istart

         ! Copy the new value into the vacated entry.
         do
            iput = iget
            iget = p(iget)

            p(iput) = - p(iput)

            if (iget < 1 .or. n < iget) then
               write (unit=*, fmt='(a)')         ' '
               write (unit=*, fmt='(a)')         'I4VEC_PERMUTE - Fatal error!'
               write (unit=*, fmt='(a)')         '  A permutation index is out of range.'
               write (unit=*, fmt='(a,i8,a,i8)') '  P(', iput, ') = ', iget
               stop
            end if

            if (iget .eq. istart) then
               a(iput) = a_temp
               exit
            end if

            a(iput) = a(iget)
         end do
      end if
   end do

   ! Restore the signs of the entries.
   p(1:n) = - p(1:n)

end subroutine i4vec_permute
!*****************************************************************************80
!
!! I4VEC_PERMUTE permutes an I4VEC in place.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    This routine permutes an array of integer "objects", but the same
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
!      N = 5
!      P = (   2,   4,   5,   1,   3 )
!      A = (   1,   2,   3,   4,   5 )
!
!    Output:
!
!      A    = (   2,   4,   5,   1,   3 ).
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
!    Input, integer ( kind = 4 ) P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.
!
!    Input/output, integer ( kind = 4 ) A(N), the array to be permuted.
!



subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
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
!    02 May 2010
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
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,a,2x,i12)' ) i, ':', a(i)
  end do

  return
end



!> @author   John Burkardt
!> @brief    PERM_CHECK checks that a vector represents a permutation.
!> @date     2008-10-31
!> @see      
subroutine perm_check (n, p, base, ierror)
implicit none
   integer(kind=4), intent(in)  :: n      !< N, the number of entries.
   integer(kind=4), intent(in)  :: base   !< BASE, the index base.
   integer(kind=4), intent(out) :: ierror !< IERROR, error flag. 0, the array represents a permutation.
   integer(kind=4), intent(in)  :: p(n)   !< P(N), the array to check.
   integer(kind=4) :: seek, find

   ierror = 0
   do seek = base, base+n-1
      ierror = 1
      do find = 1, n
         if (p(find) .eq. seek) then
            ierror = 0
            exit
         end if
      end do

      if (ierror .ne. 0) then
         write (unit=*, fmt='(a)') ' '
         write (unit=*, fmt='(a)') 'PERM_CHECK - Fatal error!'
         write (unit=*, fmt='(a)') '  The input array does not represent'
         write (unit=*, fmt='(a)') '  a proper permutation.'
         stop
      end if
   end do
end subroutine perm_check
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



!> @author   John Burkardt
!> @brief    PERM_UNIFORM selects a random permutation of N objects.
!> @date     2008-11-18
!> @see      Albert Nijenhuis, Herbert Wilf,
!!           Combinatorial Algorithms for Computers and Calculators,
!!           Academic Press, 1978,
!!           ISBN: 0-12-519260-6,
!!           LC: QA164.N54.
subroutine perm_uniform (n, base, seed, p)
!use jburk_i4lib, only: i4_uniform_ab
implicit none
   integer(kind=4), intent(in)  :: n
   integer(kind=4), intent(in)  :: base
   integer(kind=4), intent(out) :: p(n)
   integer(kind=4), intent(in)  :: seed
   integer(kind=4) :: i4_uniform_ab
   integer(kind=4) :: i, j, k

   p = 0
   do i = 1, n
      p(i) = (i - 1) + base
   end do

   do i = 1, n
      j    = i4_uniform_ab (i, n, seed)
      k    = p(i)
      p(i) = p(j)
      p(j) = k
   end do

end subroutine perm_uniform
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
!    Input, integer ( kind = 4 ) N, the number of objects to be permuted.
!
!    Input, integer ( kind = 4 ) BASE, is 0 for a 0-based permutation and 1 for
!    a 1-based permutation.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) P(N), the permutation.  P(I) is the "new"
!    location of the object originally at I.



end module jburk_r8lib_i4vec_
