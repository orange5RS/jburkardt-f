

!> @author [P. Jang](orange224factory@gmail.com)
!> @date   2020-02-18
!> @see    [Documenting Fortran with Doxygen](https://github.com/Mohid-Water-Modelling-System/Mohid/wiki/Documenting-Fortran-with-Doxygen)
module     jburk_toeplitz_c4vec_
use, intrinsic :: iso_fortran_env
implicit none

contains



subroutine c4vec_indicator ( n, a )

!*****************************************************************************80
!
!! C4VEC_INDICATOR sets a C4VEC to the indicator vector.
!
!  Discussion:
!
!    X(1:N) = ( 1-1i, 2-2i, 3-3i, 4-4i, ... )
!
!  Modified:
!
!    04 January 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Output, complex A(N), the array to be initialized.
!
  implicit none

  integer ( kind = 4 ) n

  complex a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = cmplx ( i, -i )
  end do

  return
end
subroutine c4vec_print ( n, a, title )

!*****************************************************************************80
!
!! C4VEC_PRINT prints a C4VEC, with an optional title.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, complex A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed first.
!    TITLE may be blank.
!
  implicit none

  integer ( kind = 4 ) n

  complex a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(i8,2g14.6)' ) i, a(i)
  end do

  return
end
subroutine c4vec_print_some ( n, x, max_print )

!*****************************************************************************80
!
!! C4VEC_PRINT_SOME prints some of a C4VEC.
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
!  Modified:
!
!    14 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, complex X(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines to print.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  complex x(n)

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print-2
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do
    write ( *, '(a)' ) '......  ..............'
    i = n
    write ( *, '(i8,2x,2g14.6)' ) i, x(i)

  else

    do i = 1, max_print - 1
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do
    i = max_print
    write ( *, '(i8,2x,2g14.6,2x,a)' ) i, x(i), '...more entries...'

  end if

  return
end
subroutine c4vec_random ( alo, ahi, n, a )

!*****************************************************************************80
!
!! C4VEC_RANDOM returns a random C4VEC in a given range.
!
!  Modified:
!
!    08 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) ALO, AHI, the range allowed for the entries.
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Output, complex A(N), the vector of randomly chosen values.
!
  implicit none

  integer ( kind = 4 ) n

  complex a(n)
  real ( kind = 4 ) ahi
  real ( kind = 4 ) ai(n)
  real ( kind = 4 ) alo
  real ( kind = 4 ) ar(n)

  call random_number ( harvest = ai(1:n) )
  ai(1:n) = alo + ai(1:n) * ( ahi - alo )

  call random_number ( harvest = ar(1:n) )
  ar(1:n) = alo + ar(1:n) * ( ahi - alo )

  a(1:n) = cmplx ( ar(1:n), ai(1:n) )

  return
end


end module jburk_toeplitz_c4vec_

