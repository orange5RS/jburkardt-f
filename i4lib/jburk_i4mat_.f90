module     jburk_i4mat_
use, intrinsic :: iso_fortran_env
implicit none
!   private

contains

    subroutine i43mat_flip_cols ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I43MAT_FLIP_COLS swaps the columns of an I43MAT.
    !
    !  Discussion:
    !
    !    An I43MAT is a matrix, each of whose entries is an I43,
    !    a triple of I4's.
    !
    !    An I43MAT can be stored as a 3 x M x N array, where M counts the "columns"
    !    and N counts the "rows".
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    22 June 2006
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input/output, integer ( kind = 4 ) A(3,M,N), the matrix whose columns
    !    are to be flipped.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(3,m,n)
      integer ( kind = 4 ) b(3,m,1)
      integer ( kind = 4 ) j
    
      do j = 1, n / 2
        b(1:3,1:m,    1) = a(1:3,1:m,    j)
        a(1:3,1:m,    j) = a(1:3,1:m,n+1-j)
        a(1:3,1:m,n+1-j) = b(1:3,1:m,    1)
      end do
    
      return
    end
    subroutine i43mat_flip_rows ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I43MAT_FLIP_ROWS swaps the rows of an I43MAT.
    !
    !  Discussion:
    !
    !    An I43MAT is a matrix, each of whose entries is an I43,
    !    a triple of I4's.
    !
    !    An I43MAT can be stored as a 3 x M x N array, where M counts the "columns"
    !    and N counts the "rows".
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    22 June 2006
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input/output, integer ( kind = 4 ) A(3,M,N), the matrix whose rows
    !    are to be flipped.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(3,m,n)
      integer ( kind = 4 ) b(3,1,n)
      integer ( kind = 4 ) i
    
      do i = 1, m / 2
        b(1:3,    1,1:n) = a(1:3,    i,1:n)
        a(1:3,    i,1:n) = a(1:3,m+1-i,1:n)
        a(1:3,m+1-i,1:n) = b(1:3,    1,1:n)
      end do
    
      return
    end
    subroutine i4block_print ( l, m, n, a, title )
    
    !*****************************************************************************80
    !
    !! I4BLOCK_PRINT prints an I4BLOCK.
    !
    !  Discussion:
    !
    !    An I4BLOCK is a 3D array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    11 October 2001
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) L, M, N, the dimensions of the block.
    !
    !    Input, integer ( kind = 4 ) A(L,M,N), the matrix to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) l
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(l,m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      integer ( kind = 4 ) k
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
    
      do k = 1, n
    
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  K = ', k
    
        do jlo = 1, m, 10
          jhi = min ( jlo + 10 - 1, m )
          write ( *, '(a)' ) ' '
          write ( *, '(8x,a2,10(2x,i6))' ) 'J:', ( j, j = jlo, jhi )
          write ( *, '(7x,a2)' ) 'I:'
          do i = 1, l
            write ( *, '(2x,i6,a1,1x,10(2x,i6))' ) i, ':', a(i,jlo:jhi,k)
          end do
        end do
    
      end do
    
      return
    end


    subroutine i4mat_border_add ( m, n, table, table2 )
    
    !*****************************************************************************80
    !
    !! I4MAT_BORDER_ADD adds a "border" to an I4MAT.
    !
    !  Discussion:
    !
    !    We suppose the input data gives values of a quantity on nodes
    !    in the interior of a 2D grid, and we wish to create a new table
    !    with additional positions for the nodes that would be on the
    !    border of the 2D grid.
    !
    !                  0 0 0 0 0 0
    !      * * * *     0 * * * * 0
    !      * * * * --> 0 * * * * 0
    !      * * * *     0 * * * * 0
    !                  0 0 0 0 0 0
    !
    !    The illustration suggests the situation in which a 3 by 4 array
    !    is input, and a 5 by 6 array is to be output.
    !
    !    The old data is shifted to its correct positions in the new array.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    25 January 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the spatial dimension.
    !
    !    Input, integer ( kind = 4 ) N, the number of points.
    !
    !    Input,integer TABLE(M,N), the table data.
    !
    !    Output, integer ( kind = 4 ) TABLE2(M+2,N+2), the augmented table data.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) table(m,n)
      integer ( kind = 4 ) table2(m+2,n+2)
    
      table2(1,1:n+2) = 0
      table2(m+2,1:n+2) = 0
      table2(2:m+1,1) = 0
      table2(2:m+1,n+2) = 0
    
      table2(2:m+1,2:n+1) = table(1:m,1:n)
    
      return
    end
    subroutine i4mat_border_cut ( m, n, table, table2 )
    
    !*****************************************************************************80
    !
    !! I4MAT_BORDER_CUT cuts the "border" of an I4MAT.
    !
    !  Discussion:
    !
    !    We suppose the input data gives values of a quantity on nodes
    !    on a 2D grid, and we wish to create a new table corresponding only
    !    to those nodes in the interior of the 2D grid.
    !
    !      0 0 0 0 0 0
    !      0 * * * * 0    * * * *
    !      0 * * * * 0 -> * * * *
    !      0 * * * * 0    * * * *
    !      0 0 0 0 0 0
    !
    !    The illustration suggests the situation in which a 5 by 6 array
    !    is input, and a 3 by 4 array is to be output.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    25 January 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the spatial dimension.
    !
    !    Input, integer ( kind = 4 ) N, the number of points.
    !
    !    Input, integer ( kind = 4 ) TABLE(M,N), the table data.
    !
    !    Output, integer ( kind = 4 ) TABLE2(M-2,N-2), the new table data.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) table(m,n)
      integer ( kind = 4 ) table2(m-2,n-2)
    
      if ( m <= 2 .or. n <= 2 ) then
        return
      end if
    
      table2(1:m-2,1:n-2) = table(2:m-1,2:n-1)
    
      return
    end
    subroutine i4mat_copy ( m, n, a1, a2 )
    
    !*****************************************************************************80
    !
    !! I4MAT_COPY copies an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    23 October 2010
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A1(M,N), the matrix to be copied.
    !
    !    Output, integer ( kind = 4 ) A2(M,N), the copied matrix.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a1(m,n)
      integer ( kind = 4 ) a2(m,n)
    
      a2(1:m,1:n) = a1(1:m,1:n)
    
      return
    end
    subroutine i4mat_elim ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4MAT_ELIM carries out exact Gauss elimination on an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    03 March 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows in A.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns in A.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N).  On input, the M by N matrix to
    !    be Gauss eliminated.  On output, the Gauss-eliminated matrix.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) amax
      integer ( kind = 4 ) i
      integer ( kind = 4 ) icol(n)
      integer ( kind = 4 ) ifact
      integer ( kind = 4 ) i4_gcd
      integer ( kind = 4 ) imax
      integer ( kind = 4 ) imult
      integer ( kind = 4 ) irow(m)
      integer ( kind = 4 ) iswap
      integer ( kind = 4 ) j
      integer ( kind = 4 ) jcol
      integer ( kind = 4 ) jmult
    !
    !  Initialize the swap parity counter.
    !
      iswap = 1
    !
    !  For each column JCOL...
    !
      do jcol = 1, min ( m, n )
    !
    !  Find the maximum element in rows JCOL through M.
    !
        amax = abs ( a(jcol,jcol) )
        imax = jcol
    
        do i = jcol + 1, m
          if ( amax < abs ( a(i,jcol) ) ) then
            amax = abs ( a(i,jcol) )
            imax = i
          end if
        end do
    !
    !  If the maximum entry is nonzero, then...
    !
        if ( amax /= 0 ) then
    !
    !  If the maximum entry does not occur in row JCOL, then swap rows.
    !
          if ( imax /= jcol ) then
            iswap = - iswap
            call i4vec_swap ( n, a(jcol,1:n), a(imax,1:n) )
          end if
    !
    !  Eliminate all nonzero entries in column JCOL, below the diagonal entry.
    !
          do i = jcol + 1, m
    
            if ( a(i,jcol) /= 0 ) then
    
              jmult = a(i,jcol)
              imult = a(jcol,jcol)
              ifact = i4_gcd ( imult, jmult )
              imult = imult / ifact
              jmult = jmult / ifact
    
              do j = jcol, n
                a(i,j) = jmult * a(jcol,j) - imult * a(i,j)
              end do
    
            end if
    
          end do
    !
    !  Remove any row or column factors.
    !
          call i4mat_red ( m, n, a, irow, icol )
    
        end if
    
      end do
    
      return
    end
    subroutine i4mat_flip_cols ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4MAT_FLIP_COLS swaps the columns of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is an integer matrix.
    !
    !    To "flip" the columns of an I4MAT is to start with something like
    !
    !      11 12 13 14 15
    !      21 22 23 24 25
    !      31 32 33 34 35
    !      41 42 43 44 45
    !      51 52 53 54 55
    !
    !    and return
    !
    !      15 14 13 12 11
    !      25 24 23 22 21
    !      35 34 33 32 31
    !      45 44 43 42 41
    !      55 54 53 52 51
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    21 June 2006
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N), the matrix whose columns
    !    are to be flipped.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) b(m)
      integer ( kind = 4 ) j
    
      do j = 1, n / 2
        b(1:m      ) = a(1:m,    j)
        a(1:m,    j) = a(1:m,n+1-j)
        a(1:m,n+1-j) = b(1:m)
      end do
    
      return
    end
    subroutine i4mat_flip_rows ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4MAT_FLIP_ROWS swaps the rows of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is an integer matrix.
    !
    !    To "flip" the rows of an I4MAT is to start with something like
    !
    !      11 12 13 14 15
    !      21 22 23 24 25
    !      31 32 33 34 35
    !      41 42 43 44 45
    !      51 52 53 54 55
    !
    !    and return
    !
    !      51 52 53 54 55
    !      41 42 43 44 45
    !      31 32 33 34 35
    !      21 22 23 24 25
    !      11 12 13 14 15
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    21 June 2006
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N), the matrix whose rows
    !    are to be flipped.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) b(n)
      integer ( kind = 4 ) i
    
      do i = 1, m / 2
        b(      1:n) = a(    i,1:n)
        a(    i,1:n) = a(m+1-i,1:n)
        a(m+1-i,1:n) = b(      1:n)
      end do
    
      return
    end
    subroutine i4mat_histogram ( m, n, a, histo_num, histo_gram )
    
    !*****************************************************************************80
    !
    !! I4MAT_HISTOGRAM computes a histogram of the elements of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is an array of I4's.
    !
    !    It is assumed that the entries in the vector A are nonnegative.
    !    Only values between 0 and HISTO_NUM will be histogrammed.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 June 2010
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the order of A.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the array to examine.
    !
    !    Input, integer ( kind = 4 ) HISTO_NUM, the maximum value for which a
    !    histogram entry will be computed.
    !
    !    Output, integer ( kind = 4 ) HISTO_GRAM(0:HISTO_NUM), contains the
    !    number of entries of A with the values of 0 through HISTO_NUM.
    !
      implicit none
    
      integer ( kind = 4 ) histo_num
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) histo_gram(0:histo_num)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
    
      histo_gram(0:histo_num) = 0
    
      do j = 1, n
        do i = 1, m
    
          if ( 0 <= a(i,j) .and. a(i,j) <= histo_num ) then
            histo_gram(a(i,j)) = histo_gram(a(i,j)) + 1
          end if
    
        end do
      end do
    
      return
    end
    subroutine i4mat_indicator ( m, n, table )
    
    !*****************************************************************************80
    !
    !! I4MAT_INDICATOR sets up an "indicator" I4MAT.
    !
    !  Discussion:
    !
    !    The value of each entry suggests its location, as in:
    !
    !      11  12  13  14
    !      21  22  23  24
    !      31  32  33  34
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    25 January 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows of the matrix.
    !    M must be positive.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns of the matrix.
    !    N must be positive.
    !
    !    Output, integer ( kind = 4 ) TABLE(M,N), the table.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) fac
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_log_10
      integer ( kind = 4 ) j
      integer ( kind = 4 ) table(m,n)
    
      fac = 10 ** ( i4_log_10 ( n ) + 1 )
    
      do i = 1, m
        do j = 1, n
          table(i,j) = fac * i + j
        end do
      end do
    
      return
    end
    subroutine i4mat_l1_inverse ( n, a, b )
    
    !*****************************************************************************80
    !
    !! I4MAT_L1_INVERSE inverts a unit lower triangular I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !    A unit lower triangular matrix is a matrix with only 1's on the main
    !    diagonal, and only 0's above the main diagonal.
    !
    !    The inverse of an integer unit lower triangular matrix is also
    !    an integer unit lower triangular matrix.
    !
    !    This routine can invert a matrix in place, that is, with no extra
    !    storage.  If the matrix is stored in A, then the call
    !
    !      call i4mat_l1_inverse ( n, a, a )
    !
    !    will result in A being overwritten by its inverse.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    18 April 2005
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
    !    Input, integer ( kind = 4 ) N, number of rows and columns in the matrix.
    !
    !    Input, integer ( kind = 4 ) A(N,N), the unit lower triangular matrix.
    !
    !    Output, integer ( kind = 4 ) B(N,N), the inverse matrix.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(n,n)
      integer ( kind = 4 ) b(n,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
    
      do i = 1, n
    
        do j = 1, i - 1
          b(i,j) = - dot_product ( a(i,1:i-1), b(1:i-1,j) )
        end do
    
        b(i,i) = 1
        b(i,i+1:n) = 0
    
      end do
    
      return
    end
    function i4mat_max ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4MAT_MAX returns the maximum of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    05 June 2010
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows in A.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns in A.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the M by N matrix.
    !
    !    Output, integer ( kind = 4 ) I4MAT_MAX, the maximum entry of A.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i4mat_max
    
      i4mat_max = maxval ( a )
    
      return
    end
    subroutine i4mat_max_index ( m, n, a, i_max, j_max )
    
    !*****************************************************************************80
    !
    !! I4MAT_MAX_INDEX returns the location of the maximum of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    21 September 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows in A.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns in A.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the M by N matrix.
    !
    !    Output, integer ( kind = 4 ) I_MAX, J_MAX, the indices of the
    !    maximum entry of A.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i_max
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j_max
    
      i_max = -1;
      j_max = -1;
    
      do j = 1, n
        do i = 1, m
          if ( i == 1 .and. j == 1 ) then
            i_max = i
            j_max = j
          else if ( a(i_max,j_max) < a(i,j) ) then
            i_max = i
            j_max = j
          end if
        end do
      end do
    
      return
    end
    function i4mat_min ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4MAT_MIN returns the minimum of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    05 June 2010
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows in A.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns in A.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the M by N matrix.
    !
    !    Output, integer ( kind = 4 ) I4MAT_MIN, the minimum entry of A.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i4mat_min
    
      i4mat_min = minval ( a )
    
      return
    end
    subroutine i4mat_min_index ( m, n, a, i_min, j_min )
    
    !*****************************************************************************80
    !
    !! I4MAT_MIN_INDEX returns the location of the minimum of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    20 September 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows in A.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns in A.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the M by N matrix.
    !
    !    Output, integer ( kind = 4 ) I_MIN, J_MIN, the indices of the
    !    minimum entry of A.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i_min
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j_min
    
      i_min = -1
      j_min = -1
    
      do j = 1, n
        do i = 1, m
          if ( i == 1 .and. j == 1 ) then
            i_min = i
            j_min = j
          else if ( a(i,j) < a(i_min,j_min) ) then
            i_min = i
            j_min = j
          end if
        end do
      end do
    
      return
    end
    subroutine i4mat_mm ( n1, n2, n3, a, b, c )
    
    !*****************************************************************************80
    !
    !! I4MAT_MM multiplies two I4MAT's.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !    In FORTRAN90, this operation is more efficiently done by the
    !    command:
    !
    !      C(1:N1,1:N3) = MATMUL ( A(1:N1,1;N2), B(1:N2,1:N3) )
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    19 October 2005
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N1, N2, N3, the order of the matrices.
    !
    !    Input, integer ( kind = 4 ) A(N1,N2), B(N2,N3), the matrices to multiply.
    !
    !    Output, integer ( kind = 4 ) C(N1,N3), the product matrix C = A * B.
    !
      implicit none
    
      integer ( kind = 4 ) n1
      integer ( kind = 4 ) n2
      integer ( kind = 4 ) n3
    
      integer ( kind = 4 ) a(n1,n2)
      integer ( kind = 4 ) b(n2,n3)
      integer ( kind = 4 ) c(n1,n3)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
    
      do i = 1, n1
        do j = 1, n3
          c(i,j) = 0
          do k = 1, n2
            c(i,j) = c(i,j) + a(i,k) * b(k,j)
          end do
        end do
      end do
    
      return
    end
    subroutine i4mat_perm ( n, a, p )
    
    !*****************************************************************************80
    !
    !! I4MAT_PERM permutes the rows and columns of a square I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !   30 September 2009
    !
    !  Author:
    !
    !    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf,
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
    !    Input, integer ( kind = 4 ) N, the order of the matrix.
    !
    !    Input/output, integer ( kind = 4 ) A(N,N).
    !    On input, the matrix to be permuted.
    !    On output, the permuted matrix.
    !
    !    Input, integer ( kind = 4 ) P(N), the permutation.  P(I) is the new
    !    number of row and column I.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(n,n)
      integer ( kind = 4 ), parameter :: base = 1
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i1
      integer ( kind = 4 ) ierror
      integer ( kind = 4 ) is
      integer ( kind = 4 ) it
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) k
      integer ( kind = 4 ) lc
      integer ( kind = 4 ) nc
      integer ( kind = 4 ) p(n)
      integer ( kind = 4 ) t
    
      call perm_check ( n, p, base, ierror )
    
      if ( ierror /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4MAT_PERM - Fatal error!'
        write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
        stop
      end if
    
      call perm_cycle ( n, p, is, nc, 1 )
    
      do i = 1, n
    
        i1 = - p(i)
    
        if ( 0 < i1 ) then
    
          lc = 0
    
          do
    
            i1 = p(i1)
            lc = lc + 1
    
            if ( i1 <= 0 ) then
              exit
            end if
    
          end do
    
          i1 = i
    
          do j = 1, n
    
            if ( p(j) <= 0 ) then
    
              j2 = j
              k = lc
    
              do
    
                j1 = j2
                it = a(i1,j1)
    
                do
    
                  i1 = abs ( p(i1) )
                  j1 = abs ( p(j1) )
    
                  t        = a(i1,j1)
                  a(i1,j1) = it
                  it       = t
    
                  if ( j1 /= j2 ) then
                    cycle
                  end if
    
                  k = k - 1
    
                  if ( i1 == i ) then
                    exit
                  end if
    
                end do
    
                j2 = abs ( p(j2) )
    
                if ( k == 0 ) then
                  exit
                end if
    
              end do
    
            end if
    
          end do
    
        end if
    
      end do
    !
    !  Restore the positive signs of the data.
    !
      p(1:n) = abs ( p(1:n) )
    
      return
    end
    subroutine i4mat_perm_uniform ( n, a, seed )
    
    !*****************************************************************************80
    !
    !! I4MAT_PERM_UNIFORM selects a random permutation of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !    The matrix is assumed to be square.  A single permutation is
    !    applied to both rows and columns.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    01 May 2005
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
    !    Input, integer ( kind = 4 ) N, the number of rows and columns
    !    in the array.
    !
    !    Input/output, integer ( kind = 4 ) A(N,N), the array to be permuted.
    !
    !    Input/output, integer ( kind = 4 ) SEED, a seed for the random
    !    number generator.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(n,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_uniform_ab
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) seed
    !
    !  Permute the rows and columns together.
    !
      do i = 1, n
    
        i2 = i4_uniform_ab ( i, n, seed )
    
        call i4vec_swap ( n, a(i2,1:n), a(i,1:n) )
        call i4vec_swap ( n, a(1:n,i2), a(1:n,i) )
    
      end do
    
      return
    end
    subroutine i4mat_perm2 ( m, n, a, p, q )
    
    !*****************************************************************************80
    !
    !! I4MAT_PERM2 permutes the rows and columns of a rectangular I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    23 April 2005
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
    !    Input, integer ( kind = 4 ) M, number of rows in the matrix.
    !
    !    Input, integer ( kind = 4 ) N, number of columns in the matrix.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N).
    !    On input, the matrix to be permuted.
    !    On output, the permuted matrix.
    !
    !    Input, integer ( kind = 4 ) P(M), the row permutation.  P(I) is the
    !    new number of row I.
    !
    !    Input, integer ( kind = 4 ) Q(N), the column permutation.  Q(I) is the
    !    new number of column I.  Note that this routine allows you to pass a
    !    single array as both P and Q.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ), parameter :: base = 1
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i1
      integer ( kind = 4 ) ierror
      integer ( kind = 4 ) is
      integer ( kind = 4 ) it
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) k
      integer ( kind = 4 ) lc
      integer ( kind = 4 ) nc
      integer ( kind = 4 ) p(m)
      integer ( kind = 4 ) q(n)
      integer ( kind = 4 ) t
    
      call perm_check ( m, p, base, ierror )
    
      if ( ierror /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4MAT_PERM2 - Fatal error!'
        write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
        stop
      end if
    
      call perm_cycle ( m, p, is, nc, 1 )
    
      call perm_check ( n, q, base, ierror )
    
      if ( ierror /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4MAT_PERM2 - Fatal error!'
        write ( *, '(a)' ) '  PERM_CHECK rejects this permutation.'
        stop
      end if
    
      if ( 0 < q(1) ) then
        call perm_cycle ( n, q, is, nc, 1 )
      end if
    
      do i = 1, m
    
        i1 = - p(i)
    
        if ( 0 < i1 ) then
    
          lc = 0
    
          do
    
            i1 = p(i1)
            lc = lc + 1
    
            if ( i1 <= 0 ) then
              exit
            end if
    
          end do
    
          i1 = i
    
          do j = 1, n
    
            if ( q(j) <= 0 ) then
    
              j2 = j
              k = lc
    
              do
    
                j1 = j2
                it = a(i1,j1)
    
                do
    
                  i1 = abs ( p(i1) )
                  j1 = abs ( q(j1) )
    
                  t        = a(i1,j1)
                  a(i1,j1) = it
                  it       = t
    
                  if ( j1 /= j2 ) then
                    cycle
                  end if
    
                  k = k - 1
    
                  if ( i1 == i ) then
                    cycle
                  end if
    
                end do
    
                j2 = abs ( q(j2) )
    
                if ( k == 0 ) then
                  exit
                end if
    
              end do
    
            end if
    
          end do
    
        end if
    
      end do
    !
    !  Restore the positive signs of the data.
    !
      p(1:m) = abs ( p(1:m) )
    
      if ( q(1) <= 0 ) then
        q(1:n) = abs ( q(1:n) )
      end if
    
      return
    end
    subroutine i4mat_perm2_uniform ( m, n, a, seed )
    
    !*****************************************************************************80
    !
    !! I4MAT_PERM2_UNIFORM selects a random permutation of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !    The matrix may be rectangular.  Separate permutations are
    !    applied to the rows and columns.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    01 May 2005
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
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N), the array to be permuted.
    !
    !    Input/output, integer ( kind = 4 ) SEED, a seed for the random
    !    number generator.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i4_uniform_ab
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) seed
    !
    !  Permute the rows.
    !
      do i = 1, m
        i2 = i4_uniform_ab ( i, m, seed )
        call i4vec_swap ( n, a(i2,1:n), a(i,1:n) )
      end do
    !
    !  Permute the columns.
    !
      do j = 1, n
        j2 = i4_uniform_ab ( j, n, seed )
        call i4vec_swap ( m, a(1:m,j2), a(1:m,j) )
      end do
    
      return
    end
    subroutine i4mat_print ( m, n, a, title )
    
    !*****************************************************************************80
    !
    !! I4MAT_PRINT prints an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    30 June 2003
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows in A.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns in A.
    !
    !    Input, integer ( kind = 4 ) A(M,N), the matrix to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) ihi
      integer ( kind = 4 ) ilo
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      character ( len = * ) title
    
      ilo = 1
      ihi = m
      jlo = 1
      jhi = n
    
      call i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )
    
      return
    end
    subroutine i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )
    
    !*****************************************************************************80
    !
    !! I4MAT_PRINT_SOME prints some of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    10 September 2009
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), an M by N matrix to be printed.
    !
    !    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
    !
    !    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: incx = 10
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      character ( len = 8 )  ctemp(incx)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2hi
      integer ( kind = 4 ) i2lo
      integer ( kind = 4 ) ihi
      integer ( kind = 4 ) ilo
      integer ( kind = 4 ) inc
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) j2hi
      integer ( kind = 4 ) j2lo
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
    
      if ( m <= 0 .or. n <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if
    
      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx
    
        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )
    
        inc = j2hi + 1 - j2lo
    
        write ( *, '(a)' ) ' '
    
        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i8)' ) j
        end do
    
        write ( *, '(''  Col '',10a8)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '
    
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )
    
        do i = i2lo, i2hi
    
          do j2 = 1, inc
    
            j = j2lo - 1 + j2
    
            write ( ctemp(j2), '(i8)' ) a(i,j)
    
          end do
    
          write ( *, '(i5,a,10a8)' ) i, ':', ( ctemp(j), j = 1, inc )
    
        end do
    
      end do
    
      return
    end
    subroutine i4mat_red ( m, n, a, row, col )
    
    !*****************************************************************************80
    !
    !! I4MAT_RED divides out common factors in a row or column of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    03 March 1999
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the number of rows in the matrix.
    !
    !    Input, integer ( kind = 4 ) N, the number of columns in the matrix.
    !
    !    Input/output, integer ( kind = 4 ) A(M,N), on input, the M by N matrix
    !    to be reduced.  On output, A has been reduced.  The greatest common
    !    factor in any row or column is 1.
    !
    !    Output, integer ( kind = 4 ) ROW(M), the row factors that were divided out.
    !
    !    Output, integer ( kind = 4 ) COL(N), the column factors that were divided
    !    out.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      integer ( kind = 4 ) col(n)
      integer ( kind = 4 ) factor
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) row(m)
    
      if ( m <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'IMAT_RED - Fatal error!'
        write ( *, '(a)' ) '  M must be greater than 0.'
        write ( *, '(a,i8)' ) '  Input M = ', m
        stop
      end if
    
      if ( n <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'IMAT_RED - Fatal error!'
        write ( *, '(a)' ) '  N must be greater than 0.'
        write ( *, '(a,i8)' ) '  Input N = ', n
        stop
      end if
    !
    !  Remove factors common to a column.
    !
      do j = 1, n
        call i4vec_red ( m, a(1:m,j), factor )
        col(j) = factor
      end do
    !
    !  Remove factors common to a row.
    !
      do i = 1, m
        call i4vec_red ( n, a(i,1:n), factor )
        row(i) = factor
      end do
    
      return
    end
    subroutine i4mat_transpose_print ( m, n, a, title )
    
    !*****************************************************************************80
    !
    !! I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    28 December 2004
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), an M by N matrix to be printed.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      character ( len = * ) title
    
      call i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )
    
      return
    end
    subroutine i4mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )
    
    !*****************************************************************************80
    !
    !! I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    10 September 2009
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
    !
    !    Input, integer ( kind = 4 ) A(M,N), an M by N matrix to be printed.
    !
    !    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
    !
    !    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
    !
    !    Input, character ( len = * ) TITLE, a title.
    !
      implicit none
    
      integer ( kind = 4 ), parameter :: incx = 10
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(m,n)
      character ( len = 8 ) ctemp(incx)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) i2hi
      integer ( kind = 4 ) i2lo
      integer ( kind = 4 ) ihi
      integer ( kind = 4 ) ilo
      integer ( kind = 4 ) inc
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j2hi
      integer ( kind = 4 ) j2lo
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
    
      if ( m <= 0 .or. n <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if
    
      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx
    
        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )
    
        inc = i2hi + 1 - i2lo
    
        write ( *, '(a)' ) ' '
    
        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8)' ) i
        end do
    
        write ( *, '(''  Row '',10a8)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Col'
        write ( *, '(a)' ) ' '
    
        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )
    
        do j = j2lo, j2hi
    
          do i2 = 1, inc
    
            i = i2lo - 1 + i2
    
            write ( ctemp(i2), '(i8)' ) a(i,j)
    
          end do
    
          write ( *, '(i5,a,10a8)' ) j, ':', ( ctemp(i), i = 1, inc )
    
        end do
    
      end do
    
      return
    end
    subroutine i4mat_u1_inverse ( n, a, b )
    
    !*****************************************************************************80
    !
    !! I4MAT_U1_INVERSE inverts a unit upper triangular I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a rectangular array of I4's.
    !
    !    A unit upper triangular matrix is a matrix with only 1's on the main
    !    diagonal, and only 0's below the main diagonal.
    !
    !    The inverse of an integer unit upper triangular matrix is also
    !    an integer unit upper triangular matrix.
    !
    !    This routine can invert a matrix in place, that is, with no extra
    !    storage.  If the matrix is stored in A, then the call
    !
    !      call i4mat_u1_inverse ( n, a, a )
    !
    !    will result in A being overwritten by its inverse.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    18 April 2005
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
    !    Input, integer ( kind = 4 ) N, number of rows and columns in the matrix.
    !
    !    Input, integer ( kind = 4 ) A(N,N), the unit upper triangular matrix.
    !
    !    Output, integer ( kind = 4 ) B(N,N), the inverse matrix.
    !
      implicit none
    
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a(n,n)
      integer ( kind = 4 ) b(n,n)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
    
      do j = n, 1, -1
    
        b(j+1:n,j) = 0
        b(j,j) = 1
    
        do i = j - 1, 1, -1
          b(i,j) = - dot_product ( a(i,i+1:j), b(i+1:j,j) )
        end do
    
      end do
    
      return
    end
    subroutine i4mat_uniform_ab ( m, n, a, b, seed, x )
    
    !*****************************************************************************80
    !
    !! I4MAT_UNIFORM_AB returns a scaled pseudorandom I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is a matrix of integer values.
    !
    !    The pseudorandom numbers will be scaled to be uniformly distributed
    !    between A and B.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    27 November 2006
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, N, the row and column dimensions
    !    of the matrix.
    !
    !    Input, integer ( kind = 4 ) A, B, the limits of the interval.
    !
    !    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
    !    NOT be 0.  On output, SEED has been updated.
    !
    !    Output, integer ( kind = 4 ) X(M,N), a matrix of values between A and B.
    !
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      integer ( kind = 4 ) a
      integer ( kind = 4 ) b
      integer ( kind = 4 ) i
      integer ( kind = 4 ), parameter :: i4_huge = 2147483647
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
      real ( kind = 4 ) r
      integer ( kind = 4 ) seed
      integer ( kind = 4 ) value
      integer ( kind = 4 ) x(m,n)
    
      if ( seed == 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4MAT_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if
    
      do j = 1, n
    
        do i = 1, m
    
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
            +             r   * ( real ( max ( a, b ), kind = 4 ) + 0.5E+00 )
    !
    !  Use rounding to convert R to an integer between A and B.
    !
          value = nint ( r, kind = 4 )
    
          value = max ( value, min ( a, b ) )
          value = min ( value, max ( a, b ) )
    
          x(i,j) = value
    
        end do
      end do
    
      return
    end
    subroutine i4mat_zero ( m, n, a )
    
    !*****************************************************************************80
    !
    !! I4MAT_ZERO zeroes out an I4MAT.
    !
    !  Discussion:
    !
    !    An I4MAT is an array of I4's.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    22 November 2010
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer M, N, the row and column dimensions of the matrix.
    !
    !    Output, integer A(M,N), a matrix of zeroes.
    !
      implicit none
    
      integer m
      integer n
    
      integer a(m,n)
    
      a(1:m,1:n) = 0
    
      return
    end

end module jburk_i4mat_
