subroutine r8mat_add ( m, n, alpha, a, beta, b, c )

!*****************************************************************************80
!
!! R8MAT_ADD computes C = alpha * A + beta * B for R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) ALPHA, the multiplier for A.
!
!    Input, real ( kind = 8 ) A(M,N), the first matrix.
!
!    Input, real ( kind = 8 ) BETA, the multiplier for A.
!
!    Input, real ( kind = 8 ) B(M,N), the second matrix.
!
!    Output, real ( kind = 8 ) C(M,N), the sum of alpha*A+beta*B.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(m,n)
  real ( kind = 8 ) beta
  real ( kind = 8 ) c(m,n)

  c(1:m,1:n) = alpha * a(1:m,1:n) + beta * b(1:m,1:n)

  return
end
function r8mat_amax ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_AMAX returns the maximum absolute value entry of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2012
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
!    Input, real ( kind = 8 ) A(M,N), the M by N matrix.
!
!    Output, real ( kind = 8 ) R8MAT_AMAX, the maximum absolute value 
!    entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_amax

  r8mat_amax = maxval ( abs ( a(1:m,1:n) ) )

  return
end
subroutine r8mat_border_add ( m, n, table, table2 )

!*****************************************************************************80
!
!! R8MAT_BORDER_ADD adds a "border" to an R8MAT.
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
!    Input, real ( kind = 8 ) TABLE(M,N), the table data.
!
!    Output, real ( kind = 8 ) TABLE2(M+2,N+2), the augmented table data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) table(m,n)
  real ( kind = 8 ) table2(m+2,n+2)

  table2(1,1:n+2) = 0.0D+00
  table2(m+2,1:n+2) = 0.0D+00
  table2(2:m+1,1) = 0.0D+00
  table2(2:m+1,n+2) = 0.0D+00

  table2(2:m+1,2:n+1) = table(1:m,1:n)

  return
end
subroutine r8mat_border_cut ( m, n, table, table2 )

!*****************************************************************************80
!
!! R8MAT_BORDER_CUT cuts the "border" of an R8MAT.
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
!    Input, real ( kind = 8 ) TABLE(M,N), the table data.
!
!    Output, real ( kind = 8 ) TABLE2(M-2,N-2), the new table data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) table(m,n)
  real ( kind = 8 ) table2(m-2,n-2)

  if ( m <= 2 .or. n <= 2 ) then
    return
  end if

  table2(1:m-2,1:n-2) = table(2:m-1,2:n-1)

  return
end
subroutine r8mat_cholesky_factor ( n, a, c, flag )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_FACTOR computes the Cholesky factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is a lower triangular matrix L such that:
!
!      A = L * L'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    Input, real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    Output, real ( kind = 8 ) C(N,N), the N by N lower triangular
!    Cholesky factor.
!
!    Output, integer ( kind = 4 ) FLAG:
!    0, no error occurred.
!    1, the matrix is not positive definite.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) flag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) sum2

  flag = 0

  c(1:n,1:n) = a(1:n,1:n)

  do j = 1, n

    c(1:j-1,j) = 0.0D+00

    do i = j, n

      sum2 = c(j,i) - dot_product ( c(j,1:j-1), c(i,1:j-1) )

      if ( i == j ) then
        if ( sum2 <= 0.0D+00 ) then
          flag = 1
          return
        else
          c(i,j) = sqrt ( sum2 )
        end if
      else
        if ( c(j,j) /= 0.0D+00 ) then
          c(i,j) = sum2 / c(j,j)
        else
          c(i,j) = 0.0D+00
        end if
      end if

    end do

  end do

  return
end
subroutine r8mat_cholesky_solve ( n, a, b, x )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_SOLVE solves a Cholesky factored linear system A * x = b.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    Input, real ( kind = 8 ) A(N,N), the N by N Cholesky factor of the
!    system matrix.
!
!    Input, real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Output, real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) x(n)
!
!  Solve L * y = b.
!
  call r8mat_l_solve ( n, a, b, x )
!
!  Solve L' * x = y.
!
  call r8mat_lt_solve ( n, a, x, x )

  return
end
subroutine r8mat_choresky_factor ( n, a, c, flag )

!*****************************************************************************80
!
!! R8MAT_CHORESKY_FACTOR computes the "Choresky" factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is an upper triangular matrix R such that:
!
!      A = R * R'
!
!    Note that the usual Cholesky factor is a LOWER triangular matrix L
!    such that
!
!      A = L * L'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    Input, real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    Output, real ( kind = 8 ) C(N,N), the N by N upper triangular
!    "Choresky" factor.
!
!    Output, integer ( kind = 4 ) FLAG:
!    0, no error occurred.
!    1, the matrix is not positive definite.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) flag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) sum2

  flag = 0

  c(n:1:-1,n:1:-1) = a(1:n,1:n)

  do j = 1, n

    c(1:j-1,j) = 0.0D+00

    do i = j, n

      sum2 = c(j,i) - dot_product ( c(j,1:j-1), c(i,1:j-1) )

      if ( i == j ) then
        if ( sum2 <= 0.0D+00 ) then
          flag = 1
          return
        else
          c(i,j) = sqrt ( sum2 )
        end if
      else
        if ( c(j,j) /= 0.0D+00 ) then
          c(i,j) = sum2 / c(j,j)
        else
          c(i,j) = 0.0D+00
        end if
      end if

    end do

  end do

  c(n:1:-1,n:1:-1) = c(1:n,1:n)

  return
end
subroutine r8mat_copy ( m, n, a, b )

!*****************************************************************************80
!
!! R8MAT_COPY copies an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix to be copied.
!
!    Output, real ( kind = 8 ) B(M,N), a copy of the matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m,n)

  b(1:m,1:n) = a(1:m,1:n)

  return
end
subroutine r8mat_det ( n, a, det )

!*****************************************************************************80
!
!! R8MAT_DET computes the determinant of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    Original FORTRAN77 version by Helmut Spaeth.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Helmut Spaeth,
!    Cluster Analysis Algorithms
!    for Data Reduction and Classification of Objects,
!    Ellis Horwood, 1980, page 125-127.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix whose determinant is desired.
!
!    Output, real ( kind = 8 ) DET, the determinant of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) piv(1)
  real ( kind = 8 ) t

  b(1:n,1:n) = a(1:n,1:n)

  det = 1.0D+00

  do k = 1, n

    piv = maxloc ( abs ( b(k:n,k) ) )

    m = piv(1) + k - 1

    if ( m /= k ) then
      det = - det
      t      = b(m,k)
      b(m,k) = b(k,k)
      b(k,k) = t
    end if

    det = det * b(k,k)

    if ( b(k,k) /= 0.0D+00 ) then

      b(k+1:n,k) = -b(k+1:n,k) / b(k,k)

      do j = k + 1, n
        if ( m /= k ) then
          t      = b(m,j)
          b(m,j) = b(k,j)
          b(k,j) = t
        end if
        b(k+1:n,j) = b(k+1:n,j) + b(k+1:n,k) * b(k,j)
      end do

    end if

  end do

  return
end
function r8mat_det_2d ( a )

!*****************************************************************************80
!
!! R8MAT_DET_2D computes the determinant of a 2 by 2 R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The formula for the determinant of a 2 by 2 matrix is
!
!      a11 * a22 - a12 * a21.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2,2), the matrix whose determinant is desired.
!
!    Output, real ( kind = 8 ) R8MAT_DET_2D, the determinant of the matrix.
!
  implicit none

  real ( kind = 8 ) a(2,2)
  real ( kind = 8 ) r8mat_det_2d

  r8mat_det_2d = a(1,1) * a(2,2) - a(1,2) * a(2,1)

  return
end
function r8mat_det_3d ( a )

!*****************************************************************************80
!
!! R8MAT_DET_3D computes the determinant of a 3 by 3 R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The formula for the determinant of a 3 by 3 matrix is
!
!        a11 * a22 * a33 - a11 * a23 * a32
!      + a12 * a23 * a31 - a12 * a21 * a33
!      + a13 * a21 * a32 - a13 * a22 * a31
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3,3), the matrix whose determinant is desired.
!
!    Output, real ( kind = 8 ) R8MAT_DET_3D, the determinant of the matrix.
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) r8mat_det_3d

  r8mat_det_3d = &
         a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
       + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) ) &
       + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) )

  return
end
function r8mat_det_4d ( a )

!*****************************************************************************80
!
!! R8MAT_DET_4D computes the determinant of a 4 by 4 R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(4,4), the matrix whose determinant is desired.
!
!    Output, real ( kind = 8 ) R8MAT_DET_4D, the determinant of the matrix.
!
  implicit none

  real ( kind = 8 ) a(4,4)
  real ( kind = 8 ) r8mat_det_4d

  r8mat_det_4d = &
         a(1,1) * ( &
             a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
           - a(2,3) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
           + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) ) &
       - a(1,2) * ( &
             a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
           - a(2,3) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) ) &
           + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) ) &
       + a(1,3) * ( &
             a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
           - a(2,2) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) ) &
           + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) ) &
       - a(1,4) * ( &
             a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
           - a(2,2) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) &
           + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )

  return
end
function r8mat_det_5d ( a )

!*****************************************************************************80
!
!! R8MAT_DET_5D computes the determinant of a 5 by 5 R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(5,5), the matrix whose determinant is desired.
!
!    Output, real ( kind = 8 ) R8MAT_DET_5D, the determinant of the matrix.
!
  implicit none

  real ( kind = 8 ) a(5,5)
  real ( kind = 8 ) b(4,4)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8mat_det_4d
  real ( kind = 8 ) r8mat_det_5d
!
!  Expand the determinant into the sum of the determinants of the
!  five 4 by 4 matrices created by dropping row 1, and column k.
!
  r8mat_det_5d = 0.0D+00

  do k = 1, 5

    do i = 1, 4
      do j = 1, 4

        if ( j < k ) then
          inc = 0
        else
          inc = 1
        end if

        b(i,j) = a(i+1,j+inc)

      end do
    end do

    r8mat_det_5d = r8mat_det_5d + (-1)**( k + 1 ) * a(1,k) * r8mat_det_4d ( b )

  end do

  return
end
subroutine r8mat_diag_add_scalar ( n, a, s )

!*****************************************************************************80
!
!! R8MAT_DIAG_ADD_SCALAR adds a scalar to the diagonal of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns.
!
!    Input/output, real ( kind = 8 ) A(N,N), the N by N matrix to be modified.
!
!    Input, real ( kind = 8 ) S, the value to be added to the diagonal
!    of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) s

  do i = 1, n
    a(i,i) = a(i,i) + s
  end do

  return
end
subroutine r8mat_diag_add_vector ( n, a, v )

!*****************************************************************************80
!
!! R8MAT_DIAG_ADD_VECTOR adds a vector to the diagonal of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix.
!
!    Input/output, real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    Input, real ( kind = 8 ) V(N), the vector to be added to the diagonal of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)

  do i = 1, n
    a(i,i) = a(i,i) + v(i)
  end do

  return
end
subroutine r8mat_diag_get_vector ( n, a, v )

!*****************************************************************************80
!
!! R8MAT_DIAG_GET_VECTOR gets the value of the diagonal of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    Output, real ( kind = 8 ) V(N), the diagonal entries
!    of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)

  do i = 1, n
    v(i) = a(i,i)
  end do

  return
end
subroutine r8mat_diag_set_scalar ( n, a, s )

!*****************************************************************************80
!
!! R8MAT_DIAG_SET_SCALAR sets the diagonal of an R8MAT to a scalar value.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns.
!
!    Input/output, real ( kind = 8 ) A(N,N), the N by N matrix to be modified.
!
!    Input, real ( kind = 8 ) S, the value to be assigned to the diagonal
!    of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) s

  do i = 1, n
    a(i,i) = s
  end do

  return
end
subroutine r8mat_diag_set_vector ( n, a, v )

!*****************************************************************************80
!
!! R8MAT_DIAG_SET_VECTOR sets the diagonal of an R8MAT to a vector.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns.
!
!    Input/output, real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    Input, real ( kind = 8 ) V(N), the vector to be assigned to the
!    diagonal of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)

  do i = 1, n
    a(i,i) = v(i)
  end do

  return
end
subroutine r8mat_expand_linear ( m, n, x, mfat, nfat, xfat )

!*****************************************************************************80
!
!! R8MAT_EXPAND_LINEAR linearly interpolates new data into an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In this routine, the expansion is specified by giving the number
!    of intermediate values to generate between each pair of original
!    data rows and columns.
!
!    The interpolation is not actually linear.  It uses the functions
!
!      1, x, y, and xy.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of
!    input data.
!
!    Input, real ( kind = 8 ) X(M,N), the original data.
!
!    Input, integer ( kind = 4 ) MFAT, NFAT, the number of data values
!    to interpolate between each row, and each column, of original data values.
!
!    Output, real ( kind = 8 ) XFAT(M2,N2), the fattened data, where
!    M2 = (M-1)*(MFAT+1)+1,
!    N2 = (N-1)*(NFAT+1)+1.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) mfat
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nfat

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) iii
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jj
  integer ( kind = 4 ) jjj
  integer ( kind = 4 ) jp1
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) x00
  real ( kind = 8 ) x01
  real ( kind = 8 ) x10
  real ( kind = 8 ) x11
  real ( kind = 8 ) xfat((m-1)*(mfat+1)+1,(n-1)*(nfat+1)+1)

  do i = 1, m

    if ( i < m ) then
      ihi = mfat
    else
      ihi = 0
    end if

    do j = 1, n

      if ( j < n ) then
        jhi = nfat
      else
        jhi = 0
      end if

      if ( i < m ) then
        ip1 = i + 1
      else
        ip1 = i
      end if

      if ( j < n ) then
        jp1 = j + 1
      else
        jp1 = j
      end if

      x00 = x(i,j)
      x10 = x(ip1,j)
      x01 = x(i,jp1)
      x11 = x(ip1,jp1)

      do ii = 0, ihi

        s = real ( ii, kind = 8 ) &
          / real ( ihi + 1, kind = 8 )

        do jj = 0, jhi

          t = real ( jj, kind = 8 ) &
            / real ( jhi + 1, kind = 8 )

          iii = 1 + ( i - 1 ) * ( mfat + 1 ) + ii
          jjj = 1 + ( j - 1 ) * ( nfat + 1 ) + jj

          xfat(iii,jjj) = &
                                            x00   &
              + s     * (       x10       - x00 ) &
              + t     * (             x01 - x00 ) &
              + s * t * ( x11 - x10 - x01 + x00 )

        end do

      end do

    end do

  end do

  return
end
subroutine r8mat_expand_linear2 ( m, n, a, m2, n2, a2 )

!*****************************************************************************80
!
!! R8MAT_EXPAND_LINEAR2 expands an R8MAT by linear interpolation.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In this version of the routine, the expansion is indicated
!    by specifying the dimensions of the expanded array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), a "small" M by N array.
!
!    Input, integer ( kind = 4 ) M2, N2, the number of rows and columns in A2.
!
!    Output, real ( kind = 8 ) A2(M2,N2), the expanded array, which
!    contains an interpolated version of the data in A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) a2(m2,n2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  real ( kind = 8 ) r
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) s
  real ( kind = 8 ) s1
  real ( kind = 8 ) s2

  do i = 1, m2

    if ( m2 == 1 ) then
      r = 0.5D+00
    else
      r = real ( i - 1, kind = 8 ) &
        / real ( m2 - 1, kind = 8 )
    end if

    i1 = 1 + int ( r * real ( m - 1, kind = 8 ) )
    i2 = i1 + 1

    if ( m < i2 ) then
      i1 = m - 1
      i2 = m
    end if

    r1 = real ( i1 - 1, kind = 8 ) &
       / real ( m - 1, kind = 8 )

    r2 = real ( i2 - 1, kind = 8 ) &
       / real ( m - 1, kind = 8 )

    do j = 1, n2

      if ( n2 == 1 ) then
        s = 0.5D+00
      else
        s = real ( j - 1, kind = 8 ) &
          / real ( n2 - 1, kind = 8 )
      end if

      j1 = 1 + int ( s * real ( n - 1, kind = 8 ) )
      j2 = j1 + 1

      if ( n < j2 ) then
        j1 = n - 1
        j2 = n
      end if

      s1 = real ( j1 - 1, kind = 8 ) &
         / real ( n - 1, kind = 8 )

      s2 = real ( j2 - 1, kind = 8 ) &
         / real ( n - 1, kind = 8 )

      a2(i,j) = &
        ( ( r2 - r ) * ( s2 - s ) * a(i1,j1) &
        + ( r - r1 ) * ( s2 - s ) * a(i2,j1) &
        + ( r2 - r ) * ( s - s1 ) * a(i1,j2) &
        + ( r - r1 ) * ( s - s1 ) * a(i2,j2) ) &
        / ( ( r2 - r1 ) * ( s2 - s1 ) )

    end do

  end do

  return
end
subroutine r8mat_flip_cols ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_FLIP_COLS swaps the columns of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    To "flip" the columns of an R8MAT is to start with something like
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
!    27 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input/output, real ( kind = 8 ) A(M,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  real ( kind = 8 ) t(m)

  jhi = n / 2

  do j = 1, jhi
    t(1:m)       = a(1:m,j)
    a(1:m,j)     = a(1:m,n+1-j)
    a(1:m,n+1-j) = t(1:m)
  end do

  return
end
subroutine r8mat_flip_rows ( m, n, a )

!****************************************************************************80
!
!! R8MAT_FLIP_ROWS swaps the rows of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    To "flip" the rows of an R8MAT is to start with something like
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
!    27 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input/output, real ( kind = 8 ) A(M,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) j
  real ( kind = 8 ) t(n)

  ihi = m / 2

  do i = 1, ihi
    t(1:n) = a(i,1:n)
    a(i,1:n) = a(m+1-i,1:n)
    a(m+1-i,1:n) = t(1:n)
  end do

  return
end
subroutine r8mat_fss ( n, a, nb, b, info )

!*****************************************************************************80
!
!! R8MAT_FSS factors and solves a system with multiple right hand sides.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    This routine does not save the LU factors of the matrix, and hence cannot
!    be used to efficiently solve multiple linear systems, or even to
!    factor A at one time, and solve a single linear system at a later time.
!
!    This routine uses partial pivoting, but no pivot vector is required.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input/output, real ( kind = 8 ) A(N,N).
!    On input, A is the coefficient matrix of the linear system.
!    On output, A is in unit upper triangular form, and
!    represents the U factor of an LU factorization of the
!    original coefficient matrix.
!
!    Input, integer ( kind = 4 ) NB, the number of right hand sides.
!
!    Input/output, real ( kind = 8 ) B(N,NB).
!    On input, the right hand sides of the linear system.
!    On output, the solutions of the linear systems.
!
!    Output, integer ( kind = 4 ) INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) nb

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,nb)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jcol
  real ( kind = 8 ) piv
  real ( kind = 8 ) row(n)
  real ( kind = 8 ) t(nb)
  real ( kind = 8 ) temp

  info = 0

  do jcol = 1, n
!
!  Find the maximum element in column I.
!
    piv = abs ( a(jcol,jcol) )
    ipiv = jcol
    do i = jcol + 1, n
      if ( piv < abs ( a(i,jcol) ) ) then
        piv = abs ( a(i,jcol) )
        ipiv = i
      end if
    end do

    if ( piv == 0.0D+00 ) then
      info = jcol
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_FSS - Fatal error!'
      write ( *, '(a,i8)' ) '  Zero pivot on step ', info
      stop
    end if
!
!  Switch rows JCOL and IPIV, and B.
!
    if ( jcol /= ipiv ) then

      row(1:n) = a(jcol,1:n)
      a(jcol,1:n) = a(ipiv,1:n)
      a(ipiv,1:n) = row(1:n)

      t(1:nb)      = b(jcol,1:nb)
      b(jcol,1:nb) = b(ipiv,1:nb)
      b(ipiv,1:nb) = t(1:nb)

    end if
!
!  Scale the pivot row.
!
    a(jcol,jcol+1:n) = a(jcol,jcol+1:n) / a(jcol,jcol)
    b(jcol,1:nb) = b(jcol,1:nb) / a(jcol,jcol)
    a(jcol,jcol) = 1.0D+00
!
!  Use the pivot row to eliminate lower entries in that column.
!
    do i = jcol + 1, n
      if ( a(i,jcol) /= 0.0D+00 ) then
        temp = - a(i,jcol)
        a(i,jcol) = 0.0D+00
        a(i,jcol+1:n) = a(i,jcol+1:n) + temp * a(jcol,jcol+1:n)
        b(i,1:nb) = b(i,1:nb) + temp * b(jcol,1:nb)
      end if
    end do

  end do
!
!  Back solve.
!
  do j = 1, nb
    do jcol = n, 2, -1
      b(1:jcol-1,j) = b(1:jcol-1,j) - a(1:jcol-1,jcol) * b(jcol,j)
    end do
  end do

  return
end
subroutine r8mat_givens_post ( n, a, row, col, g )

!*****************************************************************************80
!
!! R8MAT_GIVENS_POST computes the Givens postmultiplier rotation matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Givens post-multiplier matrix G(ROW,COL) has the property that
!    the (ROW,COL)-th entry of A*G is zero.
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
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrices A and G.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix to be operated upon.
!
!    Input, integer ( kind = 4 ) ROW, COL, the row and column of the
!    entry of A*G which is to be zeroed out.
!
!    Output, real ( kind = 8 ) G(N,N), the Givens rotation matrix.
!    G is an orthogonal matrix, that is, the inverse of
!    G is the transpose of G.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) col
  real ( kind = 8 ) g(n,n)
  integer ( kind = 4 ) row
  real ( kind = 8 ) theta

  call r8mat_identity ( n, g )

  theta = atan2 ( a(row,col), a(row,row) )

  g(row,row) =  cos ( theta )
  g(row,col) = -sin ( theta )
  g(col,row) =  sin ( theta )
  g(col,col) =  cos ( theta )

  return
end
subroutine r8mat_givens_pre ( n, a, row, col, g )

!*****************************************************************************80
!
!! R8MAT_GIVENS_PRE computes the Givens premultiplier rotation matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Givens premultiplier rotation matrix G(ROW,COL) has the
!    property that the (ROW,COL)-th entry of G*A is zero.
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
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrices A and G.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix to be operated upon.
!
!    Input, integer ( kind = 4 ) ROW, COL, the row and column of the
!    entry of the G*A which is to be zeroed out.
!
!    Output, real ( kind = 8 ) G(N,N), the Givens rotation matrix.
!    G is an orthogonal matrix, that is, the inverse of
!    G is the transpose of G.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) col
  real ( kind = 8 ) g(n,n)
  integer ( kind = 4 ) row
  real ( kind = 8 ) theta

  call r8mat_identity ( n, g )

  theta = atan2 ( a(row,col), a(col,col) )

  g(row,row) =  cos ( theta )
  g(row,col) = -sin ( theta )
  g(col,row) =  sin ( theta )
  g(col,col) =  cos ( theta )

  return
end
subroutine r8mat_hess ( fx, n, x, h )

!*****************************************************************************80
!
!! R8MAT_HESS approximates a Hessian matrix via finite differences.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    H(I,J) = d2 F / d X(I) d X(J)
!
!    The values returned by this routine will be only approximate.
!    In some cases, they will be so poor that they are useless.
!    However, one of the best applications of this routine is for
!    checking your own Hessian calculations, since as Heraclitus
!    said, you'll never get the same result twice when you differentiate
!    a complicated expression by hand.
!
!    The user function routine, here called "FX", should have the form:
!
!      subroutine fx ( n, x, f )
!      integer n
!      real ( kind = 8 ) f
!      real ( kind = 8 ) x(n)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, external FX, the name of the user function routine.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, real ( kind = 8 ) X(N), the values of the variables.
!
!    Output, real ( kind = 8 ) H(N,N), the approximated N by N Hessian matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) eps
  real ( kind = 8 ) f00
  real ( kind = 8 ) fmm
  real ( kind = 8 ) fmp
  real ( kind = 8 ) fpm
  real ( kind = 8 ) fpp
  external fx
  real ( kind = 8 ) h(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) s(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xi
  real ( kind = 8 ) xj
!
!  Choose the stepsizes.
!
  eps = ( epsilon ( eps ) )**0.33D+00

  do i = 1, n
    s(i) = eps * max ( abs ( x(i) ), 1.0D+00 )
  end do
!
!  Calculate the diagonal elements.
!
  do i = 1, n

    xi = x(i)

    call fx ( n, x, f00 )

    x(i) = xi + s(i)
    call fx ( n, x, fpp )

    x(i) = xi - s(i)
    call fx ( n, x, fmm )

    h(i,i) = ( ( fpp - f00 ) + ( fmm - f00 ) ) / s(i)**2

    x(i) = xi

  end do
!
!  Calculate the off diagonal elements.
!
  do i = 1, n

    xi = x(i)

    do j = i + 1, n

      xj = x(j)

      x(i) = xi + s(i)
      x(j) = xj + s(j)
      call fx ( n, x, fpp )

      x(i) = xi + s(i)
      x(j) = xj - s(j)
      call fx ( n, x, fpm )

      x(i) = xi - s(i)
      x(j) = xj + s(j)
      call fx ( n, x, fmp )

      x(i) = xi - s(i)
      x(j) = xj - s(j)
      call fx ( n, x, fmm )

      h(j,i) = ( ( fpp - fpm ) + ( fmm - fmp ) ) / ( 4.0D+00 * s(i) * s(j) )

      h(i,j) = h(j,i)

      x(j) = xj

    end do

    x(i) = xi

  end do

  return
end
subroutine r8mat_house_axh ( n, a, v, ah )

!*****************************************************************************80
!
!! R8MAT_HOUSE_AXH computes A*H where H is a compact Householder matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Householder matrix H(V) is defined by
!
!      H(V) = I - 2 * v * v' / ( v' * v )
!
!    This routine is not particularly efficient.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of A.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix to be postmultiplied.
!
!    Input, real ( kind = 8 ) V(N), a vector defining a Householder matrix.
!
!    Output, real ( kind = 8 ) AH(N,N), the product A*H.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) ah(n,n)
  real ( kind = 8 ) ah_temp(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) v_normsq

  v_normsq = sum ( v(1:n)**2 )
!
!  Compute A*H' = A*H
!
  do i = 1, n
    do j = 1, n
      ah_temp(i,j) = a(i,j)
      do k = 1, n
        ah_temp(i,j) = ah_temp(i,j) - 2.0D+00 * a(i,k) * v(k) * v(j) / v_normsq
      end do
    end do
  end do
!
!  Copy the temporary result into AH.
!  Doing it this way means the user can identify the input arguments A and AH.
!
  ah(1:n,1:n) = ah_temp(1:n,1:n)

  return
end
subroutine r8mat_house_form ( n, v, h )

!*****************************************************************************80
!
!! R8MAT_HOUSE_FORM constructs a Householder matrix from its compact form.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    H(v) = I - 2 * v * v' / ( v' * v )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) V(N), the vector defining the Householder matrix.
!
!    Output, real ( kind = 8 ) H(N,N), the Householder matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) beta
  real ( kind = 8 ) h(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) v(n)
!
!  Compute the L2 norm of V.
!
  beta = sum ( v(1:n)**2 )
!
!  Form the matrix H.
!
  call r8mat_identity ( n, h )

  do i = 1, n
    do j = 1, n
      h(i,j) = h(i,j) - 2.0D+00 * v(i) * v(j) / beta
    end do
  end do

  return
end
subroutine r8mat_house_hxa ( n, a, v, ha )

!*****************************************************************************80
!
!! R8MAT_HOUSE_HXA computes H*A where H is a compact Householder matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Householder matrix H(V) is defined by
!
!      H(V) = I - 2 * v * v' / ( v' * v )
!
!    This routine is not particularly efficient.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of A.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix to be premultiplied.
!
!    Input, real ( kind = 8 ) V(N), a vector defining a Householder matrix.
!
!    Output, real ( kind = 8 ) HA(N,N), the product H*A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) ha(n,n)
  real ( kind = 8 ) ha_temp(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) v_normsq

  v_normsq = sum ( v(1:n)**2 )
!
!  Compute A*H' = A*H
!
  do i = 1, n
    do j = 1, n
      ha_temp(i,j) = a(i,j)
      do k = 1, n
        ha_temp(i,j) = ha_temp(i,j) - 2.0D+00 * v(i) * v(k) * a(k,j) / v_normsq
      end do
    end do
  end do
!
!  Copy the temporary result into HA.
!  Doing it this way means the user can identify the input arguments A and HA.
!
  ha(1:n,1:n) = ha_temp(1:n,1:n)

  return
end



!*****************************************************************************80
!
!! R8MAT_HOUSE_POST computes a Householder post-multiplier matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    H(ROW,COL) has the property that the ROW-th column of
!    A*H(ROW,COL) is zero from entry COL+1 to the end.
!
!    In the most common case, where a QR factorization is being computed,
!    ROW = COL.
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
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrices.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix whose Householder matrix
!    is to be computed.
!
!    Input, integer ( kind = 4 ) ROW, COL, specify the location of the
!    entry of the matrix A which is to be preserved.  The entries in
!    the same row, but higher column, will be zeroed out if
!    A is postmultiplied by H.
!
!    Output, real ( kind = 8 ) H(N,N), the Householder matrix.
!
subroutine r8mat_house_post ( n, a, row, col, h )
use jburk_r8lib_r8vec_, only: r8vec_house_column
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) col
  real ( kind = 8 ) h(n,n)
  integer ( kind = 4 ) row
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
!
!  Set up the vector V.
!
  w(1:col-1) = 0.0D+00
  w(col:n) = a(row,col:n)

  call r8vec_house_column ( n, w, col, v )
!
!  Form the matrix H(V).
!
  call r8mat_house_form ( n, v, h )

  return
end



!*****************************************************************************80
!
!! R8MAT_HOUSE_PRE computes a Householder pre-multiplier matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    H(ROW,COL) has the property that the COL-th column of
!    H(ROW,COL)*A is zero from entry ROW+1 to the end.
!
!    In the most common case, where a QR factorization is being computed,
!    ROW = COL.
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
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrices.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix whose Householder matrix
!    is to be computed.
!
!    Input, integer ( kind = 4 ) ROW, COL, specify the location of the
!    entry of the matrix A which is to be preserved.  The entries in
!    the same column, but higher rows, will be zeroed out if A is
!    premultiplied by H.
!
!    Output, real ( kind = 8 ) H(N,N), the Householder matrix.
!
subroutine r8mat_house_pre ( n, a, row, col, h )
use jburk_r8lib_r8vec_, only: r8vec_house_column
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) col
  real ( kind = 8 ) h(n,n)
  integer ( kind = 4 ) row
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)
!
!  Set up the vector V.
!
  w(1:row-1) = 0.0D+00
  w(row:n) = a(row:n,col)

  call r8vec_house_column ( n, w, row, v )
!
!  Form the matrix H(V).
!
  call r8mat_house_form ( n, v, h )

  return
end
subroutine r8mat_identity ( n, a )

!*****************************************************************************80
!
!! R8MAT_IDENTITY stores the identity matrix in an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of A.
!
!    Output, real ( kind = 8 ) A(N,N), the N by N identity matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i

  a(1:n,1:n) = 0.0D+00

  do i = 1, n
    a(i,i) = 1.0D+00
  end do

  return
end
function r8mat_in_01 ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_IN_01 is TRUE if the entries of an R8MAT are in the range [0,1].
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 October 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Output, logical R8MAT_IN_01, is TRUE if every entry of A is
!    between 0 and 1.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  logical r8mat_in_01

  if ( any ( a(1:m,1:n) < 0.0D+00 .or. 1.0D+00 < a(1:m,1:n) ) ) then
    r8mat_in_01 = .false.
  else
    r8mat_in_01 = .true.
  end if

  return
end
subroutine r8mat_indicator ( m, n, table )

!*****************************************************************************80
!
!! R8MAT_INDICATOR sets up an "indicator" R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
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
!    28 May 2008
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
!    Output, real ( kind = 8 ) TABLE(M,N), the table.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) fac
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_log_10
  integer ( kind = 4 ) j
  real ( kind = 8 ) table(m,n)

  fac = 10 ** ( i4_log_10 ( n ) + 1 )

  do i = 1, m
    do j = 1, n
      table(i,j) = real ( fac * i + j, kind = 8 )
    end do
  end do

  return
end
function r8mat_insignificant ( m, n, r, s )

!*****************************************************************************80
!
!! R8MAT_INSIGNIFICANT determines if an R8MAT is insignificant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the dimension of the matrices.
!
!    Input, real ( kind = 8 ) R(M,N), the vector to be compared against.
!
!    Input, real ( kind = 8 ) S(M,N), the vector to be compared.
!
!    Output, logical R8MAT_INSIGNIFICANT, is TRUE if S is insignificant
!    compared to R.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r(m,n)
  logical r8mat_insignificant
  real ( kind = 8 ) s(m,n)
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  logical value

  value = .true.

  do j = 1, n
    do i = 1, m

      t = r(i,j) + s(i,j)
      tol = epsilon ( r(i,j) ) * abs ( r(i,j) )

      if ( tol < abs ( r(i,j) - t ) ) then 
        value = .false.
        exit
      end if

    end do
  end do
  
  r8mat_insignificant = value

  return
end
subroutine r8mat_inverse_2d ( a, b, det )

!*****************************************************************************80
!
!! R8MAT_INVERSE_2D inverts a 2 by 2 R8MAT using Cramer's rule.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the determinant is zero, then A is singular, and does not have an
!    inverse.  In that case, B is simply set to zero, and a
!    message is printed.
!
!    If the determinant is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2,2), the matrix to be inverted.
!
!    Output, real ( kind = 8 ) B(2,2), the inverse of the matrix A.
!
!    Output, real ( kind = 8 ) DET, the determinant of the matrix A.
!
  implicit none

  real ( kind = 8 ) a(2,2)
  real ( kind = 8 ) b(2,2)
  real ( kind = 8 ) det
  real ( kind = 8 ) r8mat_det_2d
!
!  Compute the determinant of A.
!
  det = r8mat_det_2d ( a )

  if ( det == 0.0D+00 ) then

    b(1:2,1:2) = 0.0D+00

  else

    b(1,1) =  a(2,2) / det
    b(1,2) = -a(1,2) / det
    b(2,1) = -a(2,1) / det
    b(2,2) =  a(1,1) / det

  end if

  return
end
subroutine r8mat_inverse_3d ( a, b, det )

!*****************************************************************************80
!
!! R8MAT_INVERSE_3D inverts a 3 by 3 R8MAT using Cramer's rule.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the determinant is zero, then A is singular, and does not have an
!    inverse.  In that case, B is simply set to zero, and a
!    message is printed.
!
!    If the determinant is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3,3), the matrix to be inverted.
!
!    Output, real ( kind = 8 ) B(3,3), the inverse of the matrix A.
!
!    Output, real ( kind = 8 ) DET, the determinant of the matrix A.
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) b(3,3)
  real ( kind = 8 ) det
  real ( kind = 8 ) r8mat_det_3d
!
!  Compute the determinant of A.
!
  det = r8mat_det_3d ( a )
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0D+00 ) then
    b(1:3,1:3) = 0.0D+00
    return
  end if
!
!  Compute the entries of the inverse matrix using an explicit
!  formula.
!
  b(1,1) =  ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) / det
  b(1,2) = -( a(1,2) * a(3,3) - a(1,3) * a(3,2) ) / det
  b(1,3) =  ( a(1,2) * a(2,3) - a(1,3) * a(2,2) ) / det

  b(2,1) = -( a(2,1) * a(3,3) - a(2,3) * a(3,1) ) / det
  b(2,2) =  ( a(1,1) * a(3,3) - a(1,3) * a(3,1) ) / det
  b(2,3) = -( a(1,1) * a(2,3) - a(1,3) * a(2,1) ) / det

  b(3,1) =  ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) / det
  b(3,2) = -( a(1,1) * a(3,2) - a(1,2) * a(3,1) ) / det
  b(3,3) =  ( a(1,1) * a(2,2) - a(1,2) * a(2,1) ) / det

  return
end
subroutine r8mat_inverse_4d ( a, b, det )

!*****************************************************************************80
!
!! R8MAT_INVERSE_4D inverts a 4 by 4 R8MAT using Cramer's rule.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the determinant is zero, then A is singular, and does not have an
!    inverse.  In that case, B is simply set to zero, and a
!    message is printed.
!
!    If the determinant is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(4,4), the matrix to be inverted.
!
!    Output, real ( kind = 8 ) B(4,4), the inverse of the matrix A.
!
!    Output, real ( kind = 8 ) DET, the determinant of the matrix A.
!
  implicit none

  real ( kind = 8 ) a(4,4)
  real ( kind = 8 ) b(4,4)
  real ( kind = 8 ) det
  real ( kind = 8 ) r8mat_det_4d
!
!  Compute the determinant of A.
!
  det = r8mat_det_4d ( a )
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0D+00 ) then

    b(1:4,1:4) = 0.0D+00

    return
  end if
!
!  Compute the entries of the inverse matrix using an explicit formula.
!
  b(1,1) = +( &
        + a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(2,3) * ( a(3,4) * a(4,2) - a(3,2) * a(4,4) ) &
        + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        ) / det

  b(2,1) = -( &
        + a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(2,3) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) &
        ) / det

  b(3,1) = +( &
        + a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
        + a(2,2) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(4,1) = -( &
        + a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        + a(2,2) * ( a(3,3) * a(4,1) - a(3,1) * a(4,3) ) &
        + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(1,2) = -( &
        + a(1,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(1,3) * ( a(3,4) * a(4,2) - a(3,2) * a(4,4) ) &
        + a(1,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        ) / det

  b(2,2) = +( &
        + a(1,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(1,3) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(1,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) &
        ) / det

  b(3,2) = -( &
        + a(1,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
        + a(1,2) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(1,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(4,2) = +( &
        + a(1,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        + a(1,2) * ( a(3,3) * a(4,1) - a(3,1) * a(4,3) ) &
        + a(1,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(1,3) = +( &
        + a(1,2) * ( a(2,3) * a(4,4) - a(2,4) * a(4,3) ) &
        + a(1,3) * ( a(2,4) * a(4,2) - a(2,2) * a(4,4) ) &
        + a(1,4) * ( a(2,2) * a(4,3) - a(2,3) * a(4,2) ) &
        ) / det

  b(2,3) = -( &
        + a(1,1) * ( a(2,3) * a(4,4) - a(2,4) * a(4,3) ) &
        + a(1,3) * ( a(2,4) * a(4,1) - a(2,1) * a(4,4) ) &
        + a(1,4) * ( a(2,1) * a(4,3) - a(2,3) * a(4,1) ) &
        ) / det

  b(3,3) = +( &
        + a(1,1) * ( a(2,2) * a(4,4) - a(2,4) * a(4,2) ) &
        + a(1,2) * ( a(2,4) * a(4,1) - a(2,1) * a(4,4) ) &
        + a(1,4) * ( a(2,1) * a(4,2) - a(2,2) * a(4,1) ) &
        ) / det

  b(4,3) = -( &
        + a(1,1) * ( a(2,2) * a(4,3) - a(2,3) * a(4,2) ) &
        + a(1,2) * ( a(2,3) * a(4,1) - a(2,1) * a(4,3) ) &
        + a(1,3) * ( a(2,1) * a(4,2) - a(2,2) * a(4,1) ) &
        ) / det

  b(1,4) = -( &
        + a(1,2) * ( a(2,3) * a(3,4) - a(2,4) * a(3,3) ) &
        + a(1,3) * ( a(2,4) * a(3,2) - a(2,2) * a(3,4) ) &
        + a(1,4) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
        ) / det

  b(2,4) = +( &
        + a(1,1) * ( a(2,3) * a(3,4) - a(2,4) * a(3,3) ) &
        + a(1,3) * ( a(2,4) * a(3,1) - a(2,1) * a(3,4) ) &
        + a(1,4) * ( a(2,1) * a(3,3) - a(2,3) * a(3,1) ) &
        ) / det

  b(3,4) = -( &
        + a(1,1) * ( a(2,2) * a(3,4) - a(2,4) * a(3,2) ) &
        + a(1,2) * ( a(2,4) * a(3,1) - a(2,1) * a(3,4) ) &
        + a(1,4) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) &
        ) / det

  b(4,4) = +( &
        + a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
        + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) ) &
        + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) &
        ) / det

  return
end
subroutine r8mat_is_identity ( n, a, error_frobenius )

!*****************************************************************************80
!
!! R8MAT_IS_IDENTITY determines if an R8MAT is the identity.
!
!  Discussion:
!
!    An R8MAT is a matrix of real ( kind = 8 ) values.
!
!    The routine returns the Frobenius norm of A - I.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix.
!
!    Output, real ( kind = 8 ) ERROR_FROBENIUS, the Frobenius norm
!    of the difference matrix A - I, which would be exactly zero
!    if A were the identity matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) error_frobenius
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) value

  error_frobenius = 0.0D+00

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        error_frobenius = error_frobenius + ( a(i,j) - 1.0D+00 )**2
      else
        error_frobenius = error_frobenius + a(i,j)**2
      end if
    end do 
  end do

  error_frobenius = sqrt ( error_frobenius )

  return
end
subroutine r8mat_is_nonnegative ( m, n, a, ival )

!*****************************************************************************80
!
!! R8MAT_IS_NONNEGATIVE checks whether an R8MAT is nonnegative.
!
!  Discussion:
!
!    An R8MAT is a matrix of real ( kind = 8 ) values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the row and column dimensions of 
!    the matrix.  M and N must be positive.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Output, logical IVAL:
!    TRUE, the matrix is nonnegative.
!    FALSE, at least one element of A is less than 0.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) ival

  ival = all ( 0.0D+00 <= a(1:m,1:n) )

  return
end
subroutine r8mat_is_symmetric ( m, n, a, error_frobenius )

!*****************************************************************************80
!
!! R8MAT_IS_SYMMETRIC checks an R8MAT for symmetry.
!
!  Discussion:
!
!    An R8MAT is a matrix of real ( kind = 8 ) values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Output, real ( kind = 8 ) ERROR_FROBENIUS, measures the 
!    Frobenius norm of ( A - A' ), which would be zero if the matrix
!    were exactly symmetric.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) error_frobenius
  real ( kind = 8 ) r8_huge

  if ( m /= n ) then
    error_frobenius = r8_huge ( )
    return
  end if

  error_frobenius = sqrt ( &
                      sum ( &
                        ( &
                          abs ( a(1:m,1:n) - transpose ( a(1:m,1:n) ) ) &
                         )**2 &
                       ) &
                     )

  return
end
subroutine r8mat_jac ( m, n, eps, fx, x, fprime )

!*****************************************************************************80
!
!! R8MAT_JAC estimates a dense jacobian matrix of the function FX.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    FPRIME(I,J) = d F(I) / d X(J).
!
!    The jacobian is assumed to be dense, and the LINPACK/LAPACK
!    double precision general matrix storage mode ("DGE") is used.
!
!    Forward differences are used, requiring N+1 function evaluations.
!
!    Values of EPS have typically been chosen between
!    sqrt ( EPSMCH ) and sqrt ( sqrt ( EPSMCH ) ) where EPSMCH is the
!    machine tolerance.
!
!    If EPS is too small, then F(X+EPS) will be the same as
!    F(X), and the jacobian will be full of zero entries.
!
!    If EPS is too large, the finite difference estimate will
!    be inaccurate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of functions.
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, real ( kind = 8 ) EPS, a tolerance to be used for shifting the
!    X values during the finite differencing.  No single value
!    of EPS will be reliable for all vectors X and functions FX.
!
!    Input, external FX, the name of the user written
!    routine which evaluates the function at a given point X, of the form:
!      subroutine fx ( m, n, x, f )
!      integer m
!      integer n
!      real ( kind = 8 ) f(m)
!      real ( kind = 8 ) x(n)
!      f(1:m) = ...
!      return
!      end
!
!    Input, real ( kind = 8 ) X(N), the point where the jacobian
!    is to be estimated.
!
!    Output, real ( kind = 8 ) FPRIME(M,N), the M by N estimated jacobian
!    matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) del
  real ( kind = 8 ) eps
  real ( kind = 8 ) fprime(m,n)
  external fx
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xsave
  real ( kind = 8 ) work1(m)
  real ( kind = 8 ) work2(m)
!
!  Evaluate the function at the base point, X.
!
  call fx ( m, n, x, work2 )
!
!  Now, one by one, vary each component J of the base point X, and
!  estimate DF(I)/DX(J) = ( F(X+) - F(X) )/ DEL.
!
  do j = 1, n

    xsave = x(j)
    del = eps * ( 1.0D+00 + abs ( x(j) ) )
    x(j) = x(j) + del
    call fx ( m, n, x, work1 )
    x(j) = xsave
    fprime(1:m,j) = ( work1(1:m) - work2(1:m) ) / del

  end do

  return
end
subroutine r8mat_l_inverse ( n, a, b )

!*****************************************************************************80
!
!! R8MAT_L_INVERSE inverts a lower triangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A lower triangular matrix is a matrix whose only nonzero entries
!    occur on or below the diagonal.
!
!    The inverse of a lower triangular matrix is a lower triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
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
!    Input, real ( kind = 8 ) A(N,N), the lower triangular matrix.
!
!    Output, real ( kind = 8 ) B(N,N), the inverse matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do j = 1, n

    do i = 1, n

      if ( i < j ) then
        b(i,j) = 0.0D+00
      else if ( j == i ) then
        b(i,j) = 1.0D+00 / a(i,j)
      else
        b(i,j) = - dot_product ( a(i,1:i-1), b(1:i-1,j) ) / a(i,i)
      end if

    end do
  end do

  return
end
subroutine r8mat_l_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_L_PRINT prints a lower triangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Example:
!
!    M = 5, N = 5
!    A = (/ 11, 21, 31, 41, 51, 22, 32, 42, 52, 33, 43, 53, 44, 54, 55 /)
!
!    11
!    21 22
!    31 32 33
!    41 42 43 44
!    51 52 53 54 55
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
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(*), the M by N matrix.  Only the lower
!    triangular elements are stored, in column major order.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(10)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) jmax
  integer ( kind = 4 ) nn
  integer ( kind = 4 ) size
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  jmax = min ( n, m )

  if ( m <= n ) then
    size = ( m * ( m + 1 ) ) / 2
  else if ( n < m ) then
    size = ( n * ( n + 1 ) ) / 2 + ( m - n ) * n
  end if

  if ( all ( a(1:size) == aint ( a(1:size) ) ) ) then

    nn = 10

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(a8,10i8)' ) '  Col   ', ( j, j = jlo, jhi )
      write ( *, '(a6)' ) '  Row '
      do i = jlo, m
        jhi = min ( jlo + nn - 1, i, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j - 1 ) ) / 2
        end do
        write ( *, '(i8,10i8)' ) i, int ( a(indx(1:jhi+1-jlo)) )
      end do
    end do

  else if ( maxval ( abs ( a(1:size) ) ) < 1000000.0D+00 ) then

    nn = 5

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(8x,5(i8,6x))' ) ( j, j = jlo, jhi )
      write ( *, '(a)' ) ' '
      do i = jlo, m
        jhi = min ( jlo + nn - 1, i, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j - 1 ) ) / 2
        end do
        write ( *, '(i8,5f14.6)' ) i, a(indx(1:jhi+1-jlo))
      end do
    end do

  else

    nn = 5

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(8x,5(i8,6x))' ) ( j, j = jlo, jhi )
      write ( *, '(a)' ) ' '
      do i = jlo, m
        jhi = min ( jlo + nn - 1, i, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j - 1 ) ) / 2
        end do
        write ( *, '(i8,5g14.6)' ) i, a(indx(1:jhi+1-jlo))
      end do
    end do

  end if

  return
end
subroutine r8mat_l_solve ( n, a, b, x )

!*****************************************************************************80
!
!! R8MAT_L_SOLVE solves a lower triangular linear system.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    Input, real ( kind = 8 ) A(N,N), the N by N lower triangular matrix.
!
!    Input, real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Output, real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
!
!  Solve L * x = b.
!
  do i = 1, n
    x(i) = ( b(i) - dot_product ( a(i,1:i-1), x(1:i-1) ) ) / a(i,i)
  end do

  return
end
subroutine r8mat_l1_inverse ( n, a, b )

!*****************************************************************************80
!
!! R8MAT_L1_INVERSE inverts a unit lower triangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A unit lower triangular matrix is a matrix with only 1's on the main
!    diagonal, and only 0's above the main diagonal.
!
!    The inverse of a unit lower triangular matrix is also
!    a unit lower triangular matrix.
!
!    This routine can invert a matrix in place, that is, with no extra
!    storage.  If the matrix is stored in A, then the call
!
!      call r8mat_l1_inverse ( n, a, a )
!
!    will result in A being overwritten by its inverse.
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
!    Input, real ( kind = 8 ) A(N,N), the unit lower triangular matrix.
!
!    Output, real ( kind = 8 ) B(N,N), the inverse matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do i = 1, n

    do j = 1, n

      if ( i < j ) then
        b(i,j) = 0.0D+00
      else if ( j == i ) then
        b(i,j) = 1.0D+00
      else
        b(i,j) = -dot_product ( a(i,1:i-1), b(1:i-1,j) )
      end if

    end do
  end do

  return
end
subroutine r8mat_lt_solve ( n, a, b, x )

!*****************************************************************************80
!
!! R8MAT_LT_SOLVE solves a transposed lower triangular linear system.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    Given the lower triangular matrix A, the linear system to be solved is:
!
!      A' * x = b
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns
!    of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the N by N lower triangular matrix.
!
!    Input, real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!    Output, real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
!
!  Solve L'*x = b.
!
  do i = n, 1, -1
    x(i) = ( b(i) - dot_product ( x(i+1:n), a(i+1:n,i) ) ) / a(i,i)
  end do

  return
end



!*****************************************************************************80
!
!! R8MAT_LU computes the LU factorization of a rectangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The routine is given an M by N matrix A, and produces
!
!      L, an M by M unit lower triangular matrix,
!      U, an M by N upper triangular matrix, and
!      P, an M by M permutation matrix P,
!
!    so that
!
!      A = P' * L * U.
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
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), the M by N matrix to be factored.
!
!    Output, real ( kind = 8 ) L(M,M), the M by M unit lower triangular factor.
!
!    Output, real ( kind = 8 ) P(M,M), the M by M permutation matrix.
!
!    Output, real ( kind = 8 ) U(M,N), the M by N upper triangular factor.
!
subroutine r8mat_lu ( m, n, a, l, p, u )
use jburk_r8lib_r8row_, only: r8row_swap
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ipiv
  integer ( kind = 4 ) j
  real ( kind = 8 ) l(m,m)
  real ( kind = 8 ) p(m,m)
  real ( kind = 8 ) pivot
  real ( kind = 8 ) u(m,n)

!  Initialize:
!
!    U:=A
!    L:=Identity
!    P:=Identity
!
  u(1:m,1:n) = a(1:m,1:n)

  call r8mat_identity ( m, l )

  p(1:m,1:m) = l(1:m,1:m)
!
!  On step J, find the pivot row, IPIV, and the pivot value PIVOT.
!
  do j = 1, min ( m - 1, n )

    pivot = 0.0D+00
    ipiv = 0

    do i = j, m

      if ( pivot < abs ( u(i,j) ) ) then
        pivot = abs ( u(i,j) )
        ipiv = i
      end if

    end do
!
!  Unless IPIV is zero, swap rows J and IPIV.
!
    if ( ipiv /= 0 ) then

      call r8row_swap ( m, n, u, j, ipiv )

      call r8row_swap ( m, m, l, j, ipiv )

      call r8row_swap ( m, m, p, j, ipiv )
!
!  Zero out the entries in column J, from row J+1 to M.
!
      do i = j + 1, m

        if ( u(i,j) /= 0.0D+00 ) then

          l(i,j) = u(i,j) / u(j,j)

          u(i,j) = 0.0D+00

          u(i,j+1:n) = u(i,j+1:n) - l(i,j) * u(j,j+1:n)

        end if

      end do

    end if

  end do

  return
end
function r8mat_max ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MAX returns the maximum entry of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the M by N matrix.
!
!    Output, real ( kind = 8 ) R8MAT_MAX, the maximum entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_max

  r8mat_max = maxval ( a(1:m,1:n) )

  return
end
subroutine r8mat_max_index ( m, n, a, i, j )

!*****************************************************************************80
!
!! R8MAT_MAX_INDEX returns the location of the maximum entry of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the M by N matrix.
!
!    Output, integer ( kind = 4 ) I, J, the indices of the maximum entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jj

  i = -1
  j = -1

  do jj = 1, n
    do ii = 1, m
      if ( ii == 1 .and. jj == 1 ) then
        i = ii
        j = jj
      else if ( a(i,j) < a(ii,jj) ) then
        i = ii
        j = jj
      end if
    end do
  end do

  return
end
function r8mat_maxcol_minrow ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MAXCOL_MINROW gets the maximum column minimum row of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    R8MAT_MAXCOL_MINROW = max ( 1 <= I <= N ) ( min ( 1 <= J <= M ) A(I,J) )
!
!    For a given matrix, R8MAT_MAXCOL_MINROW <= R8MAT_MINROW_MAXCOL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Output, real ( kind = 8 ) R8MAT_MAXCOL_MINROW, the maximum column
!    minimum row entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8mat_maxcol_minrow
  real ( kind = 8 ) r8mat_minrow

  r8mat_maxcol_minrow = 0.0D+00

  do i = 1, m

    r8mat_minrow = minval ( a(i,1:n) )

    if ( i == 1 ) then
      r8mat_maxcol_minrow = r8mat_minrow
    else
      r8mat_maxcol_minrow = max ( r8mat_maxcol_minrow, r8mat_minrow )
    end if

  end do

  return
end
function r8mat_maxrow_mincol ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MAXROW_MINCOL gets the maximum row minimum column of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    R8MAT_MAXROW_MINCOL = max ( 1 <= J <= N ) ( min ( 1 <= I <= M ) A(I,J) )
!
!    For a given matrix, R8MAT_MAXROW_MINCOL <= R8MAT_MINCOL_MAXROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Output, real ( kind = 8 ) R8MAT_MAXROW_MINCOL, the maximum row
!    minimum column entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_maxrow_mincol
  real ( kind = 8 ) r8mat_mincol

  r8mat_maxrow_mincol = 0.0D+00

  do j = 1, n

    r8mat_mincol = minval ( a(1:m,j) )

    if ( j == 1 ) then
      r8mat_maxrow_mincol = r8mat_mincol
    else
      r8mat_maxrow_mincol = max ( r8mat_maxrow_mincol, r8mat_mincol )
    end if

  end do

  return
end
function r8mat_min ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MIN returns the minimum entry of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Output, real ( kind = 8 ) R8MAT_MIN, the minimum entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_min

  r8mat_min = minval ( a(1:m,1:n) )

  return
end
subroutine r8mat_min_index ( m, n, a, i, j )

!*****************************************************************************80
!
!! R8MAT_MIN_INDEX returns the location of the minimum entry of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the M by N matrix.
!
!    Output, integer ( kind = 4 ) I, J, the indices of the minimum entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jj

  i = -1
  j = -1

  do jj = 1, n
    do ii = 1, m
      if ( ii == 1 .and. jj == 1 ) then
        i = ii
        j = jj
      else if ( a(ii,jj) < a(i,j) ) then
        i = ii
        j = jj
      end if
    end do
  end do

  return
end
function r8mat_mincol_maxrow ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MINCOL_MAXROW gets the minimum column maximum row of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    R8MAT_MINCOL_MAXROW = min ( 1 <= I <= N ) ( max ( 1 <= J <= M ) A(I,J) )
!
!    For a given matrix, R8MAT_MAXROW_MINCOL <= R8MAT_MINCOL_MAXROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Output, real ( kind = 8 ) R8MAT_MINCOL_MAXROW, the minimum column
!    maximum row entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8mat_mincol_maxrow
  real ( kind = 8 ) r8mat_maxrow

  r8mat_mincol_maxrow = 0.0D+00

  do i = 1, m

    r8mat_maxrow = maxval ( a(i,1:n) )

    if ( i == 1 ) then
      r8mat_mincol_maxrow = r8mat_maxrow
    else
      r8mat_mincol_maxrow = min ( r8mat_mincol_maxrow, r8mat_maxrow )
    end if

  end do

  return
end
function r8mat_minrow_maxcol ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MINROW_MAXCOL gets the minimum row maximum column of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    R8MAT_MINROW_MAXCOL = min ( 1 <= J <= N ) ( max ( 1 <= I <= M ) A(I,J) )
!
!    For a given matrix, R8MAT_MAXCOL_MINROW <= R8MAT_MINROW_MAXCOL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Output, real ( kind = 8 ) R8MAT_MINROW_MAXCOL, the minimum row
!    maximum column entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_minrow_maxcol
  real ( kind = 8 ) r8mat_maxcol

  r8mat_minrow_maxcol = 0.0D+00

  do j = 1, n

    r8mat_maxcol = maxval ( a(1:m,j) )

    if ( j == 1 ) then
      r8mat_minrow_maxcol = r8mat_maxcol
    else
      r8mat_minrow_maxcol = min ( r8mat_minrow_maxcol, r8mat_maxcol )
    end if

  end do

  return
end
subroutine r8mat_minvm ( n1, n2, a, b, c )

!*****************************************************************************80
!
!! R8MAT_MINVM computes inverse(A) * B for R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N1, N2, the order of the matrices.
!
!    Input, real ( kind = 8 ) A(N1,N1), B(N1,N2), the matrices.
!
!    Output, real ( kind = 8 ) C(N1,N2), the result, C = inverse(A) * B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  real ( kind = 8 ) a(n1,n1)
  real ( kind = 8 ) alu(n1,n1)
  real ( kind = 8 ) b(n1,n2)
  real ( kind = 8 ) c(n1,n2)
  integer ( kind = 4 ) info

  alu(1:n1,1:n1) = a(1:n1,1:n1)
  c(1:n1,1:n2) = b(1:n1,1:n2)

  call r8mat_fss ( n1, alu, n2, c, info )
 
  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_MINVM - Fatal error!'
    write ( *, '(a)' ) '  The matrix A was numerically singular.'
    stop
  end if

  return
end
subroutine r8mat_mm ( n1, n2, n3, a, b, c )

!*****************************************************************************80
!
!! R8MAT_MM multiplies two R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
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
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N1, N2, N3, the order of the matrices.
!
!    Input, real ( kind = 8 ) A(N1,N2), B(N2,N3), the matrices to multiply.
!
!    Output, real ( kind = 8 ) C(N1,N3), the product matrix C = A * B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3

  real ( kind = 8 ) a(n1,n2)
  real ( kind = 8 ) b(n2,n3)
  real ( kind = 8 ) c(n1,n3)

  c(1:n1,1:n3) = matmul ( a(1:n1,1:n2), b(1:n2,1:n3) )

  return
end
subroutine r8mat_mmt ( n1, n2, n3, a, b, c )

!*****************************************************************************80
!
!! R8MAT_MMT computes C = A * B' for two R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In FORTRAN90, this operation is more efficiently done by the
!    command:
!
!      C(1:N1,1:N3) = matmul ( A(1:N1,1;N2) ), transpose ( B(1:N3,1:N2) ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 November 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N1, N2, N3, the order of the matrices.
!
!    Input, real ( kind = 8 ) A(N1,N2), B(N3,N2), the matrices to multiply.
!
!    Output, real ( kind = 8 ) C(N1,N3), the product matrix C = A * B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3

  real ( kind = 8 ) a(n1,n2)
  real ( kind = 8 ) b(n3,n2)
  real ( kind = 8 ) c(n1,n3)

  c(1:n1,1:n3) = matmul ( a(1:n1,1:n2), transpose ( b(1:n3,1:n2) ))

  return
end
subroutine r8mat_mtm ( n1, n2, n3, a, b, c )

!*****************************************************************************80
!
!! R8MAT_MTM computes C = A' * B for two R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In FORTRAN90, this operation is more efficiently done by the
!    command:
!
!      C(1:N1,1:N3) = matmul ( transpose ( A(1:N2,1;N1) ), B(1:N2,1:N3) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N1, N2, N3, the order of the matrices.
!
!    Input, real ( kind = 8 ) A(N2,N1), B(N2,N3), the matrices to multiply.
!
!    Output, real ( kind = 8 ) C(N1,N3), the product matrix C = A * B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3

  real ( kind = 8 ) a(n2,n1)
  real ( kind = 8 ) b(n2,n3)
  real ( kind = 8 ) c(n1,n3)

  c(1:n1,1:n3) = matmul ( transpose ( a(1:n2,1:n1) ), b(1:n2,1:n3) )

  return
end
subroutine r8mat_mtv ( m, n, a, x, y )

!*****************************************************************************80
!
!! R8MAT_MTV multiplies a transposed matrix times a vector
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix.
!
!    Input, real ( kind = 8 ) A(M,N), the M by N matrix.
!
!    Input, real ( kind = 8 ) X(M), the vector to be multiplied by A.
!
!    Output, real ( kind = 8 ) Y(N), the product A'*X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) x(m)
  real ( kind = 8 ) y(n)

  y(1:n) = matmul ( transpose ( a(1:m,1:n) ), x(1:m) )

  return
end
subroutine r8mat_mv ( m, n, a, x, y )

!*****************************************************************************80
!
!! R8MAT_MV multiplies a matrix times a vector.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In FORTRAN90, this operation can be more efficiently carried
!    out by the command
!
!      Y(1:M) = MATMUL ( A(1:M,1:N), X(1:N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix.
!
!    Input, real ( kind = 8 ) A(M,N), the M by N matrix.
!
!    Input, real ( kind = 8 ) X(N), the vector to be multiplied by A.
!
!    Output, real ( kind = 8 ) Y(M), the product A*X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(m)

  do i = 1, m
    y(i) = 0.0D+00
    do j = 1, n
      y(i) = y(i) + a(i,j) * x(j)
    end do
  end do

  return
end
subroutine r8mat_nint ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NINT rounds the entries of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of A.
!
!    Input/output, real ( kind = 8 ) A(M,N), the matrix to be NINT'ed.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)

  a(1:m,1:n) = real ( nint ( a(1:m,1:n) ), kind = 8 )

  return
end
function r8mat_norm_eis ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_EIS returns the EISPACK norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The EISPACK norm is defined as:
!
!      R8MAT_NORM_EIS =
!        sum ( 1 <= I <= M ) sum ( 1 <= J <= N ) abs ( A(I,J) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix whose EISPACK norm is desired.
!
!    Output, real ( kind = 8 ) R8MAT_NORM_EIS, the EISPACK norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_norm_eis

  r8mat_norm_eis = sum ( abs ( a(1:m,1:n) ) )

  return
end
function r8mat_norm_fro ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_FRO returns the Frobenius norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Frobenius norm is defined as
!
!      R8MAT_NORM_FRO = sqrt (
!        sum ( 1 <= I <= M ) sum ( 1 <= j <= N ) A(I,J) * A(I,J) )
!
!    The matrix Frobenius norm is not derived from a vector norm, but
!    is compatible with the vector L2 norm, so that:
!
!      r8vec_norm_l2 ( A * x ) <= r8mat_norm_fro ( A ) * r8vec_norm_l2 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
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
!    Input, real ( kind = 8 ) A(M,N), the matrix whose Frobenius
!    norm is desired.
!
!    Output, real ( kind = 8 ) R8MAT_NORM_FRO, the Frobenius norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_norm_fro

  r8mat_norm_fro = sqrt ( sum ( a(1:m,1:n)**2 ) )

  return
end
function r8mat_norm_fro_affine ( m, n, a1, a2 )

!*****************************************************************************80
!
!! R8MAT_NORM_FRO_AFFINE returns the Frobenius norm of an R8MAT difference.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Frobenius norm is defined as
!
!      R8MAT_NORM_FRO = sqrt (
!        sum ( 1 <= I <= M ) sum ( 1 <= j <= N ) A(I,J) * A(I,J) )
!
!    The matrix Frobenius norm is not derived from a vector norm, but
!    is compatible with the vector L2 norm, so that:
!
!      r8vec_norm_l2 ( A * x ) <= r8mat_norm_fro ( A ) * r8vec_norm_l2 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows.
!
!    Input, integer ( kind = 4 ) N, the number of columns.
!
!    Input, real ( kind = 8 ) A1(M,N), A2(M,N), the matrices for whose 
!    difference the Frobenius norm is desired.
!
!    Output, real ( kind = 8 ) R8MAT_NORM_FRO_AFFINE, the Frobenius 
!    norm of A1 - A2.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(m,n)
  real ( kind = 8 ) a2(m,n)
  real ( kind = 8 ) r8mat_norm_fro_affine

  r8mat_norm_fro_affine = sqrt ( sum ( ( a1(1:m,1:n) - a2(1:m,1:n) )**2 ) )

  return
end
function r8mat_norm_l1 ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_L1 returns the matrix L1 norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The matrix L1 norm is defined as:
!
!      R8MAT_NORM_L1 = max ( 1 <= J <= N )
!        sum ( 1 <= I <= M ) abs ( A(I,J) ).
!
!    The matrix L1 norm is derived from the vector L1 norm, and
!    satisifies:
!
!      r8vec_norm_l1 ( A * x ) <= r8mat_norm_l1 ( A ) * r8vec_norm_l1 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix whose L1 norm is desired.
!
!    Output, real ( kind = 8 ) R8MAT_NORM_L1, the L1 norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_norm_l1

  r8mat_norm_l1 = 0.0D+00

  do j = 1, n
    r8mat_norm_l1 = max ( r8mat_norm_l1, sum ( abs ( a(1:m,j) ) ) )
  end do

  return
end
function r8mat_norm_l2 ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_L2 returns the matrix L2 norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The matrix L2 norm is defined as:
!
!      R8MAT_NORM_L2 = sqrt ( max ( 1 <= I <= M ) LAMBDA(I) )
!
!    where LAMBDA contains the eigenvalues of A * A'.
!
!    The matrix L2 norm is derived from the vector L2 norm, and
!    satisifies:
!
!      r8vec_norm_l2 ( A * x ) <= r8mat_norm_l2 ( A ) * r8vec_norm_l2 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2001
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
!    Input, real ( kind = 8 ) A(M,N), the matrix whose L2 norm is desired.
!
!    Output, real ( kind = 8 ) R8MAT_NORM_L2, the L2 norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m,m)
  real ( kind = 8 ) diag(m)
  real ( kind = 8 ) r8mat_norm_l2
!
!  Compute B = A * A'.
!
  b(1:m,1:m) = matmul ( a(1:m,1:n), transpose ( a(1:m,1:n) ) )
!
!  Diagonalize B.
!
  call r8mat_symm_jacobi ( m, b )
!
!  Find the maximum eigenvalue, and take its square root.
!
  call r8mat_diag_get_vector ( m, b, diag )

  r8mat_norm_l2 = sqrt ( maxval ( diag(1:m) ) )

  return
end
function r8mat_norm_li ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_LI returns the matrix L-oo norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The matrix L-oo norm is defined as:
!
!      R8MAT_NORM_LI =  max ( 1 <= I <= M ) sum ( 1 <= J <= N ) abs ( A(I,J) ).
!
!    The matrix L-oo norm is derived from the vector L-oo norm,
!    and satisifies:
!
!      r8vec_norm_li ( A * x ) <= r8mat_norm_li ( A ) * r8vec_norm_li ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix whose L-oo
!    norm is desired.
!
!    Output, real ( kind = 8 ) R8MAT_NORM_LI, the L-oo norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8mat_norm_li

  r8mat_norm_li = 0.0D+00

  do i = 1, m
    r8mat_norm_li = max ( r8mat_norm_li, sum ( abs ( a(i,1:n) ) ) )
  end do

  return
end




!*****************************************************************************80
!
!! R8MAT_NORMAL_01 returns a unit pseudonormal R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(M,N), the array of pseudonormal values.
!
subroutine r8mat_normal_01 ( m, n, seed, r )
use jburk_r8lib_r8vec_, only: r8vec_normal_01
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  call r8vec_normal_01 ( m * n, seed, r )

  return
end
subroutine r8mat_nullspace ( m, n, a, nullspace_size, nullspace )

!*****************************************************************************80
!
!! R8MAT_NULLSPACE computes the nullspace of a matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    Let A be an MxN matrix.
!
!    If X is an N-vector, and A*X = 0, then X is a null vector of A.
!
!    The set of all null vectors of A is called the nullspace of A.
!
!    The 0 vector is always in the null space.
!
!    If the 0 vector is the only vector in the nullspace of A, then A
!    is said to have maximum column rank.  (Because A*X=0 can be regarded
!    as a linear combination of the columns of A).  In particular, if A
!    is square, and has maximum column rank, it is nonsingular.
!
!    The dimension of the nullspace is the number of linearly independent
!    vectors that span the nullspace.  If A has maximum column rank,
!    its nullspace has dimension 0.
!
!    This routine uses the reduced row echelon form of A to determine
!    a set of NULLSPACE_SIZE independent null vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix to be analyzed.
!
!    Input, integer ( kind = 4 ) NULLSPACE_SIZE, the size of the nullspace.
!
!    Output, real ( kind = 8 ) NULLSPACE(N,NULLSPACE_SIZE), vectors that
!    span the nullspace.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nullspace_size

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) col(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  real ( kind = 8 ) nullspace(n,nullspace_size)
  integer ( kind = 4 ) row(m)
  real ( kind = 8 ) rref(m,n)
!
!  Make a copy of A.
!
  rref(1:m,1:n) = a(1:m,1:n)
!
!  Get the reduced row echelon form of A.
!
  call r8mat_rref ( m, n, rref )
!
!  Note in ROW the columns of the leading nonzeros.
!  COL(J) = +J if there is a leading 1 in that column, and -J otherwise.
!
  row(1:m) = 0

  do j = 1, n
    col(j) = - j
  end do

  do i = 1, m
    do j = 1, n
      if ( rref(i,j) == 1.0D+00 ) then
        row(i) = j
        col(j) = j
        exit
      end if
    end do
  end do

  nullspace(1:n,1:nullspace_size) = 0.0D+00

  j2 = 0
!
!  If column J does not contain a leading 1, then it contains
!  information about a null vector.
!
  do j = 1, n

    if ( col(j) < 0 ) then

      j2 = j2 + 1

      do i = 1, m
        if ( rref(i,j) /= 0.0D+00 ) then
          i2 = row(i)
          nullspace(i2,j2) = - rref(i,j)
        end if
      end do

      nullspace(j,j2) = 1.0D+00

    end if

  end do

  return
end
subroutine r8mat_nullspace_size ( m, n, a, nullspace_size )

!*****************************************************************************80
!
!! R8MAT_NULLSPACE_SIZE computes the size of the nullspace of a matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    Let A be an MxN matrix.
!
!    If X is an N-vector, and A*X = 0, then X is a null vector of A.
!
!    The set of all null vectors of A is called the nullspace of A.
!
!    The 0 vector is always in the null space.
!
!    If the 0 vector is the only vector in the nullspace of A, then A
!    is said to have maximum column rank.  (Because A*X=0 can be regarded
!    as a linear combination of the columns of A).  In particular, if A
!    is square, and has maximum column rank, it is nonsingular.
!
!    The dimension of the nullspace is the number of linearly independent
!    vectors that span the nullspace.  If A has maximum column rank,
!    its nullspace has dimension 0.
!
!    This routine ESTIMATES the dimension of the nullspace.  Cases of
!    singularity that depend on exact arithmetic will probably be missed.
!
!    The nullspace will be estimated by counting the leading 1's in the
!    reduced row echelon form of A, and subtracting this from N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix to be analyzed.
!
!    Output, integer ( kind = 4 ) NULLSPACE_SIZE, the estimated size
!    of the nullspace.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) leading
  integer ( kind = 4 ) nullspace_size
  real ( kind = 8 ) rref(m,n)
!
!  Get the reduced row echelon form of A.
!
  rref(1:m,1:n) = a(1:m,1:n)

  call r8mat_rref ( m, n, rref )
!
!  Count the leading 1's in A.
!
  leading = 0
  do i = 1, m
    do j = 1, n
      if ( rref(i,j) == 1.0D+00 ) then
        leading = leading + 1
        exit
      end if
    end do
  end do

  nullspace_size = n - leading

  return
end



!*****************************************************************************80
!
!! R8MAT_ORTH_UNIFORM returns a random orthogonal R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    Thanks to Eugene Petrov, B I Stepanov Institute of Physics,
!    National Academy of Sciences of Belarus, for convincingly
!    pointing out the severe deficiencies of an earlier version of
!    this routine.
!
!    Essentially, the computation involves saving the Q factor of the
!    QR factorization of a matrix whose entries are normally distributed.
!    However, it is only necessary to generate this matrix a column at
!    a time, since it can be shown that when it comes time to annihilate
!    the subdiagonal elements of column K, these (transformed) elements of
!    column K are still normally distributed random values.  Hence, there
!    is no need to generate them at the beginning of the process and
!    transform them K-1 times.
!
!    For computational efficiency, the individual Householder transformations
!    could be saved, as recommended in the reference, instead of being
!    accumulated into an explicit matrix format.
!
!  Properties:
!
!    The inverse of A is equal to A'.
!
!    A * A'  = A' * A = I.
!
!    Columns and rows of A have unit Euclidean norm.
!
!    Distinct pairs of columns of A are orthogonal.
!
!    Distinct pairs of rows of A are orthogonal.
!
!    The L2 vector norm of A*x = the L2 vector norm of x for any vector x.
!
!    The L2 matrix norm of A*B = the L2 matrix norm of B for any matrix B.
!
!    The determinant of A is +1 or -1.
!
!    All the eigenvalues of A have modulus 1.
!
!    All singular values of A are 1.
!
!    All entries of A are between -1 and 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 November 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Pete Stewart,
!    Efficient Generation of Random Orthogonal Matrices With an Application
!    to Condition Estimators,
!    SIAM Journal on Numerical Analysis,
!    Volume 17, Number 3, June 1980, pages 403-409.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) A(N,N), the orthogonal matrix.
!
subroutine r8mat_orth_uniform ( n, seed, a )
use jburk_r8lib_r8vec_, only: r8vec_house_column
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_normal_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
!
!  Start with A = the identity matrix.
!
  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        a(i,j) = 1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do
!
!  Now behave as though we were computing the QR factorization of
!  some other random matrix.  Generate the N elements of the first column,
!  compute the Householder matrix H1 that annihilates the subdiagonal elements,
!  and set A := A * H1' = A * H.
!
!  On the second step, generate the lower N-1 elements of the second column,
!  compute the Householder matrix H2 that annihilates them,
!  and set A := A * H2' = A * H2 = H1 * H2.
!
!  On the N-1 step, generate the lower 2 elements of column N-1,
!  compute the Householder matrix HN-1 that annihilates them, and
!  and set A := A * H(N-1)' = A * H(N-1) = H1 * H2 * ... * H(N-1).
!  This is our random orthogonal matrix.
!
  do j = 1, n - 1
!
!  Set the vector that represents the J-th column to be annihilated.
!
    x(1:j-1) = 0.0D+00

    do i = j, n
      x(i) = r8_normal_01 ( seed )
    end do
!
!  Compute the vector V that defines a Householder transformation matrix
!  H(V) that annihilates the subdiagonal elements of X.
!
    call r8vec_house_column ( n, x, j, v )
!
!  Postmultiply the matrix A by H'(V) = H(V).
!
    call r8mat_house_axh ( n, a, v, a )

  end do

  return
end
subroutine r8mat_plot ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PLOT "plots" an R8MAT, with an optional title.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character r8mat_plot_symbol
  character ( len = 70 ) string
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  do jlo = 1, n, 70
    jhi = min ( jlo + 70-1, n )
    write ( *, '(a)' ) ' '
    write ( *, '(8x,2x,70i1)' ) ( mod ( j, 10 ), j = jlo, jhi )
    write ( *, '(a)' ) ' '

    do i = 1, m
      do j = jlo, jhi
        string(j+1-jlo:j+1-jlo) = r8mat_plot_symbol ( a(i,j) )
      end do
      write ( *, '(i8,2x,a)' ) i, string(1:jhi+1-jlo)
    end do
  end do

  return
end
function r8mat_plot_symbol ( r )

!*****************************************************************************80
!
!! R8MAT_PLOT_SYMBOL returns a symbol for an element of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, a value whose symbol is desired.
!
!    Output, character R8MAT_PLOT_SYMBOL, is
!    '-' if R is negative,
!    '0' if R is zero,
!    '+' if R is positive.
!
  implicit none

  character r8mat_plot_symbol
  real ( kind = 8 ) r

  if ( r < 0.0D+00 ) then
    r8mat_plot_symbol = '-'
  else if ( r == 0.0D+00 ) then
    r8mat_plot_symbol = '0'
  else if ( 0.0D+00 < r ) then
    r8mat_plot_symbol = '+'
  end if

  return
end
subroutine r8mat_poly_char ( n, a, p )

!*****************************************************************************80
!
!! R8MAT_POLY_CHAR computes the characteristic polynomial of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    Output, real ( kind = 8 ) P(0:N), the coefficients of the characteristic
!    polynomial of A.  P(N) contains the coefficient of X^N
!    (which will be 1), P(I) contains the coefficient of X^I,
!    and P(0) contains the constant term.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
  real ( kind = 8 ) p(0:n)
  real ( kind = 8 ) r8mat_trace
  real ( kind = 8 ) trace
  real ( kind = 8 ) work1(n,n)
  real ( kind = 8 ) work2(n,n)
!
!  Initialize WORK1 to the identity matrix.
!
  call r8mat_identity ( n, work1 )

  p(n) = 1.0D+00

  do order = n - 1, 0, -1
!
!  Work2 = A * WORK1.
!
    work2(1:n,1:n) = matmul ( a(1:n,1:n), work1(1:n,1:n) )
!
!  Take the trace.
!
    trace = r8mat_trace ( n, work2 )
!
!  P(ORDER) = -Trace ( WORK2 ) / ( N - ORDER )
!
    p(order) = -trace / real ( n - order, kind = 8 )
!
!  WORK1 := WORK2 + P(ORDER) * Identity.
!
    work1(1:n,1:n) = work2(1:n,1:n)

    do i = 1, n
      work1(i,i) = work1(i,i) + p(order)
    end do

  end do

  return
end
subroutine r8mat_power ( n, a, npow, b )

!*****************************************************************************80
!
!! R8MAT_POWER computes a nonnegative power of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The algorithm is:
!
!      B = I
!      do NPOW times:
!        B = A * B
!      end
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
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of A.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix to be raised to a power.
!
!    Input, integer ( kind = 4 ) NPOW, the power to which A is to be raised.
!    NPOW must be nonnegative.
!
!    Output, real ( kind = 8 ) B(N,N), the value of A^NPOW.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) ipow
  integer ( kind = 4 ) npow

  if ( npow < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_POWER - Fatal error!'
    write ( *, '(a)' ) '  Input value of NPOW < 0.'
    write ( *, '(a,i8)' ) '  NPOW = ', npow
    stop
  end if

  call r8mat_identity ( n, b )

  do ipow = 1, npow
    b(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )
  end do

  return
end
subroutine r8mat_power_method ( n, a, r, v )

!*****************************************************************************80
!
!! R8MAT_POWER_METHOD applies the power method to an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the power method has not converged, then calling the routine
!    again immediately with the output from the previous call will
!    continue the iteration.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of A.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix.
!
!    Output, real ( kind = 8 ) R, the estimated eigenvalue.
!
!    Input/output, real ( kind = 8 ) V(N), on input, an estimate
!    for the eigenvector.  On output, an improved estimate for the
!    eigenvector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) av(n)
  real ( kind = 8 ) eps
  integer ( kind = 4 ) it
  real ( kind = 8 ), parameter :: it_eps = 0.0001D+00
  integer ( kind = 4 ), parameter :: it_max = 100
  integer ( kind = 4 ), parameter :: it_min = 10
  integer ( kind = 4 ) j
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  real ( kind = 8 ) r_old
  real ( kind = 8 ) v(n)

  eps = sqrt ( epsilon ( 1.0D+00 ) )

  r = sqrt ( sum ( v(1:n)**2 ) )

  if ( r == 0.0D+00 ) then
    v(1:n) = 1.0D+00
    r = sqrt ( real ( n, kind = 8 ) )
  end if

  v(1:n) = v(1:n) / r

  do it = 1, it_max

    av(1:n) = matmul ( a(1:n,1:n), v(1:n) )

    r_old = r
    r = sqrt ( sum ( av(1:n)**2 ) )

    if ( it_min < it ) then
      if ( abs ( r - r_old ) <= it_eps * ( 1.0D+00 + abs ( r ) ) ) then
        exit
      end if
    end if

    v(1:n) = av(1:n)

    if ( r /= 0.0D+00 ) then
      v(1:n) = v(1:n) / r
    end if
!
!  Perturb V a bit, to avoid cases where the initial guess is exactly
!  the eigenvector of a smaller eigenvalue.
!
    if ( it < it_max / 2 ) then
      j = 1 + mod ( it - 1, n )
      v(j) = v(j) + eps * ( 1.0D+00 + abs ( v(j) ) )
      r2 = sqrt ( sum ( v(1:n)**2 ) )
      v(1:n) = v(1:n) / r2
    end if

  end do

  return
end
subroutine r8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PRINT prints an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
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
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_PRINT_SOME prints some of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
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
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
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
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( a(i,j) == real ( int ( a(i,j) ), kind = 8 ) ) then
          write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
        else
          write ( ctemp(j2), '(g14.6)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine r8mat_print2 ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_PRINT2 prints an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2002
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
!    Input, real ( kind = 8 ) A(M,N), the M by N matrix to be printed.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amin
  integer ( kind = 4 ) i
  character ( len = 10 ) iform
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  logical integ
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) lmax
  integer ( kind = 4 ) npline
  real ( kind = 8 ) r8_log_10
!
!  Check if all entries are integral.
!
  integ = .true.

  do i = 1, m
    do j = 1, n

      if ( integ ) then
        if ( a(i,j) /= real ( int ( a(i,j) ), kind = 8 ) ) then
          integ = .false.
        end if
      end if

    end do
  end do
!
!  Find the maximum and minimum entries.
!
  amax = maxval ( a(1:m,1:n) )
  amin = minval ( a(1:m,1:n) )
!
!  Use the information about the maximum size of an entry to
!  compute an intelligent format for use with integer entries.
!
!  Later, we might also do this for real matrices.
!
  lmax = int ( r8_log_10 ( amax ) )

  if ( integ ) then
    npline = 79 / ( lmax + 3 )
    write ( iform, '(''('',i2,''I'',i2,'')'')' ) npline, lmax+3
  else
    npline = 5
    iform = ' '
  end if
!
!  Print a scalar quantity.
!
  if ( m == 1 .and. n == 1 ) then

    if ( integ ) then
      write ( *, iform ) int ( a(1,1) )
    else
      write ( *, '(2x,g14.6)' ) a(1,1)
    end if
!
!  Column vector of length M,
!
  else if ( n == 1 ) then

    do ilo = 1, m, npline

      ihi = min ( ilo+npline-1, m )

      if ( integ ) then
        write ( *, iform ) ( int ( a(i,1) ), i = ilo, ihi )
      else
        write ( *, '(2x,5g14.6)' ) a(ilo:ihi,1)
      end if

    end do
!
!  Row vector of length N,
!
  else if ( m == 1 ) then

    do jlo = 1, n, npline

      jhi = min ( jlo+npline-1, n )

      if ( integ ) then
        write ( *, iform ) int ( a(1,jlo:jhi) )
      else
        write ( *, '(2x,5g14.6)' ) a(1,jlo:jhi)
      end if

    end do
!
!  M by N Array
!
  else

    do jlo = 1, n, npline

      jhi = min ( jlo+npline-1, n )

      if ( npline < n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8,a,i8)' ) 'Matrix columns ', jlo, ' to ', jhi
        write ( *, '(a)' ) ' '
      end if

      do i = 1, m

        if ( integ ) then
          write ( *, iform ) int ( a(i,jlo:jhi) )
        else
          write ( *, '(2x,5g14.6)' ) a(i,jlo:jhi)
        end if

      end do
    end do

  end if

  return
end



!*****************************************************************************80
!
!! R8MAT_REF computes the row echelon form of a matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A matrix is in row echelon form if:
!
!    * The first nonzero entry in each row is 1.
!
!    * The leading 1 in a given row occurs in a column to
!      the right of the leading 1 in the previous row.
!
!    * Rows which are entirely zero must occur last.
!
!  Example:
!
!    Input matrix:
!
!     1.0  3.0  0.0  2.0  6.0  3.0  1.0
!    -2.0 -6.0  0.0 -2.0 -8.0  3.0  1.0
!     3.0  9.0  0.0  0.0  6.0  6.0  2.0
!    -1.0 -3.0  0.0  1.0  0.0  9.0  3.0
!
!    Output matrix:
!
!     1.0  3.0  0.0  2.0  6.0  3.0  1.0
!     0.0  0.0  0.0  1.0  2.0  4.5  1.5
!     0.0  0.0  0.0  0.0  0.0  1.0  0.3
!     0.0  0.0  0.0  0.0  0.0  0.0  0.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix A.
!
!    Input/output, real ( kind = 8 ) A(M,N).  On input, the matrix to be
!    analyzed.  On output, the REF form of the matrix.
!
subroutine r8mat_ref ( m, n, a )
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lead
  integer ( kind = 4 ) r
  real ( kind = 8 ) temp

  lead = 1

  do r = 1, m

    if ( n < lead ) then
      exit
    end if

    i = r

    do while ( a(i,lead) == 0.0D+00 )

      i = i + 1

      if ( m < i ) then
        i = r
        lead = lead + 1
        if ( n < lead ) then
          lead = -1
          exit
        end if
      end if

    end do

    if ( lead < 0 ) then
      exit
    end if

    do j = 1, n
      temp   = a(i,j)
      a(i,j) = a(r,j)
      a(r,j) = temp
    end do

    a(r,1:n) = a(r,1:n) / a(r,lead)

    do i = r + 1, m
      a(i,1:n) = a(i,1:n) - a(i,lead) * a(r,1:n)
    end do

    lead = lead + 1

  end do

  return
end
function r8mat_rms ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_RMS returns the RMS norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The matrix RMS norm is defined as:
!
!      R8MAT_RMS = sqrt ( 
!        sum ( 1 <= I <= M ) sum ( 1 <= J <= N ) A(I,J)^2 / M / N ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the dimensions of the matrix.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Output, real ( kind = 8 ) R8MAT_RMS, the RMS norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_rms

  r8mat_rms = sqrt ( sum ( a(1:m,1:n)**2 ) / m / n )

  return
end
subroutine r8mat_rref ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_RREF computes the reduced row echelon form of a matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A matrix is in row echelon form if:
!
!    * The first nonzero entry in each row is 1.
!
!    * The leading 1 in a given row occurs in a column to
!      the right of the leading 1 in the previous row.
!
!    * Rows which are entirely zero must occur last.
!
!    The matrix is in reduced row echelon form if, in addition to
!    the first three conditions, it also satisfies:
!
!    * Each column containing a leading 1 has no other nonzero entries.
!
!  Example:
!
!    Input matrix:
!
!     1.0  3.0  0.0  2.0  6.0  3.0  1.0
!    -2.0 -6.0  0.0 -2.0 -8.0  3.0  1.0
!     3.0  9.0  0.0  0.0  6.0  6.0  2.0
!    -1.0 -3.0  0.0  1.0  0.0  9.0  3.0
!
!    Output matrix:
!
!     1.0  3.0  0.0  0.0  2.0  0.0  0.0
!     0.0  0.0  0.0  1.0  2.0  0.0  0.0
!     0.0  0.0  0.0  0.0  0.0  1.0  0.3
!     0.0  0.0  0.0  0.0  0.0  0.0  0.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix A.
!
!    Input/output, real ( kind = 8 ) A(M,N).  On input, the matrix to be
!    analyzed.  On output, the RREF form of the matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lead
  integer ( kind = 4 ) r
  real ( kind = 8 ) temp

  lead = 1

  do r = 1, m

    if ( n < lead ) then
      exit
    end if

    i = r

    do while ( a(i,lead) == 0.0D+00 )

      i = i + 1

      if ( m < i ) then
        i = r
        lead = lead + 1
        if ( n < lead ) then
          lead = -1
          exit
        end if
      end if

    end do

    if ( lead < 0 ) then
      exit
    end if

    do j = 1, n
      temp   = a(i,j)
      a(i,j) = a(r,j)
      a(r,j) = temp
    end do

    a(r,1:n) = a(r,1:n) / a(r,lead)

    do i = 1, m
      if ( i /= r ) then
        a(i,1:n) = a(i,1:n) - a(i,lead) * a(r,1:n)
      end if
    end do

    lead = lead + 1

  end do

  return
end
subroutine r8mat_scale ( m, n, s, a )

!*****************************************************************************80
!
!! R8MAT_SCALE multiplies an R8MAT by a scalar.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) S, the scale factor.
!
!    Input/output, real ( kind = 8 ) A(M,N), the matrix to be scaled.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) s

  a(1:m,1:n) = a(1:m,1:n) * s

  return
end
subroutine r8mat_solve ( n, rhs_num, a, info )

!*****************************************************************************80
!
!! R8MAT_SOLVE uses Gauss-Jordan elimination to solve an N by N linear system.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, integer ( kind = 4 ) RHS_NUM, the number of right hand sides.
!    RHS_NUM must be at least 0.
!
!    Input/output, real ( kind = 8 ) A(N,N+RHS_NUM), contains in rows and
!    columns 1 to N the coefficient matrix, and in columns N+1 through
!    N+RHS_NUM, the right hand sides.  On output, the coefficient matrix
!    area has been destroyed, while the right hand sides have
!    been overwritten with the corresponding solutions.
!
!    Output, integer ( kind = 4 ) INFO, singularity flag.
!    0, the matrix was not singular, the solutions were computed;
!    J, factorization failed on step J, and the solutions could not
!    be computed.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rhs_num

  real ( kind = 8 ) a(n,n+rhs_num)
  real ( kind = 8 ) apivot
  real ( kind = 8 ) factor
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipivot
  integer ( kind = 4 ) j
  real ( kind = 8 ) t(n+rhs_num)

  info = 0

  do j = 1, n
!
!  Choose a pivot row.
!
    ipivot = j
    apivot = a(j,j)

    do i = j + 1, n
      if ( abs ( apivot ) < abs ( a(i,j) ) ) then
        apivot = a(i,j)
        ipivot = i
      end if
    end do

    if ( apivot == 0.0D+00 ) then
      info = j
      return
    end if
!
!  The pivot row moves into the J-th row.
!
    if ( ipivot /= j ) then
      t(       1:n+rhs_num) = a(ipivot,1:n+rhs_num)
      a(ipivot,1:n+rhs_num) = a(j,     1:n+rhs_num)
      a(j,     1:n+rhs_num) = t(       1:n+rhs_num)
    end if
!
!  A(J,J) becomes 1.
!
    a(j,j) = 1.0D+00
    a(j,j+1:n+rhs_num) = a(j,j+1:n+rhs_num) / apivot
!
!  A(I,J) becomes 0.
!
    do i = 1, n

      if ( i /= j ) then
        factor = a(i,j)
        a(i,j) = 0.0D+00
        a(i,j+1:n+rhs_num) = a(i,j+1:n+rhs_num) - factor * a(j,j+1:n+rhs_num)
      end if

    end do

  end do

  return
end
subroutine r8mat_solve_2d ( a, b, det, x )

!*****************************************************************************80
!
!! R8MAT_SOLVE_2D solves a 2 by 2 linear system using Cramer's rule.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the determinant DET is returned as zero, then the matrix A is
!    singular, and does not have an inverse.  In that case, X is
!    returned as the zero vector.
!
!    If DET is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2,2), the matrix.
!
!    Input, real ( kind = 8 ) B(2), the right hand side.
!
!    Output, real ( kind = 8 ) DET, the determinant of the matrix A.
!
!    Output, real ( kind = 8 ) X(2), the solution of the system,
!    if DET is nonzero.
!
  implicit none

  real ( kind = 8 ) a(2,2)
  real ( kind = 8 ) b(2)
  real ( kind = 8 ) det
  real ( kind = 8 ) x(2)
!
!  Compute the determinant.
!
  det = a(1,1) * a(2,2) - a(1,2) * a(2,1)
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0D+00 ) then
    x(1:2) = 0.0D+00
    return
  end if
!
!  Compute the solution.
!
  x(1) = (  a(2,2) * b(1) - a(1,2) * b(2) ) / det
  x(2) = ( -a(2,1) * b(1) + a(1,1) * b(2) ) / det

  return
end
subroutine r8mat_solve_3d ( a, b, det, x )

!*****************************************************************************80
!
!! R8MAT_SOLVE_3D solves a 3 by 3 linear system using Cramer's rule.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the determinant DET is returned as zero, then the matrix A is
!    singular, and does not have an inverse.  In that case, X is
!    returned as the zero vector.
!
!    If DET is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(3,3), the matrix.
!
!    Input, real ( kind = 8 ) B(3), the right hand side.
!
!    Output, real ( kind = 8 ) DET, the determinant of the matrix A.
!
!    Output, real ( kind = 8 ) X(3), the solution of the system,
!    if DET is nonzero.
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) b(3)
  real ( kind = 8 ) det
  real ( kind = 8 ) x(3)
!
!  Compute the determinant.
!
  det =  a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
       + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) ) &
       + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) )
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0D+00 ) then
    x(1:3) = 0.0D+00
    return
  end if
!
!  Compute the solution.
!
  x(1) = (   ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) * b(1) &
           - ( a(1,2) * a(3,3) - a(1,3) * a(3,2) ) * b(2) &
           + ( a(1,2) * a(2,3) - a(1,3) * a(2,2) ) * b(3) ) / det

  x(2) = ( - ( a(2,1) * a(3,3) - a(2,3) * a(3,1) ) * b(1) &
           + ( a(1,1) * a(3,3) - a(1,3) * a(3,1) ) * b(2) &
           - ( a(1,1) * a(2,3) - a(1,3) * a(2,1) ) * b(3) ) / det

  x(3) = (   ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) * b(1) &
           - ( a(1,1) * a(3,2) - a(1,2) * a(3,1) ) * b(2) &
           + ( a(1,1) * a(2,2) - a(1,2) * a(2,1) ) * b(3) ) / det

  return
end
subroutine r8mat_solve2 ( n, a, b, x, ierror )

!*****************************************************************************80
!
!! R8MAT_SOLVE2 computes the solution of an N by N linear system.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The linear system may be represented as
!
!      A*X = B
!
!    If the linear system is singular, but consistent, then the routine will
!    still produce a solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of equations.
!
!    Input/output, real ( kind = 8 ) A(N,N).
!    On input, A is the coefficient matrix to be inverted.
!    On output, A has been overwritten.
!
!    Input/output, real ( kind = 8 ) B(N).
!    On input, B is the right hand side of the system.
!    On output, B has been overwritten.
!
!    Output, real ( kind = 8 ) X(N), the solution of the linear system.
!
!    Output, integer ( kind = 4 ) IERROR.
!    0, no error detected.
!    1, consistent singularity.
!    2, inconsistent singularity.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) imax
  integer ( kind = 4 ) ipiv(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)

  ierror = 0

  ipiv(1:n) = 0
  x(1:n) = 0.0D+00
!
!  Process the matrix.
!
  do k = 1, n
!
!  In column K:
!    Seek the row IMAX with the properties that:
!      IMAX has not already been used as a pivot;
!      A(IMAX,K) is larger in magnitude than any other candidate.
!
    amax = 0.0D+00
    imax = 0
    do i = 1, n
      if ( ipiv(i) == 0 ) then
        if ( amax < abs ( a(i,k) ) ) then
          imax = i
          amax = abs ( a(i,k) )
        end if
      end if
    end do
!
!  If you found a pivot row IMAX, then,
!    eliminate the K-th entry in all rows that have not been used for pivoting.
!
    if ( imax /= 0 ) then

      ipiv(imax) = k
      a(imax,k+1:n) = a(imax,k+1:n) / a(imax,k)
      b(imax) = b(imax) / a(imax,k)
      a(imax,k) = 1.0D+00

      do i = 1, n

        if ( ipiv(i) == 0 ) then
          a(i,k+1:n) = a(i,k+1:n) - a(i,k) * a(imax,k+1:n)
          b(i) = b(i) - a(i,k) * b(imax)
          a(i,k) = 0.0D+00
        end if

      end do

    end if

  end do
!
!  Now, every row with nonzero IPIV begins with a 1, and
!  all other rows are all zero.  Begin solution.
!
  do j = n, 1, -1

    imax = 0
    do k = 1, n
      if ( ipiv(k) == j ) then
        imax = k
      end if
    end do

    if ( imax == 0 ) then

      x(j) = 0.0D+00

      if ( b(j) == 0.0D+00 ) then
        ierror = 1
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_SOLVE2 - Warning:'
        write ( *, '(a,i8)' ) '  Consistent singularity, equation = ', j
      else
        ierror = 2
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_SOLVE2 - Error:'
        write ( *, '(a,i8)' ) '  Inconsistent singularity, equation = ', j
      end if

    else

      x(j) = b(imax)

      do i = 1, n
        if ( i /= imax ) then
          b(i) = b(i) - a(i,j) * x(j)
        end if
      end do

    end if

  end do

  return
end
function r8mat_sum ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_SUM returns the sum of the entries of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In FORTRAN90, this facility is offered by the built in
!    SUM function:
!
!      R8MAT_SUM ( M, N, A ) = SUM ( A(1:M,1:N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) R8MAT_SUM, the sum of the entries.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_sum

  r8mat_sum = sum ( a(1:m,1:n) )

  return
end
subroutine r8mat_symm_eigen ( n, x, q, a )

!*****************************************************************************80
!
!! R8MAT_SYMM_EIGEN returns a symmetric matrix with given eigensystem.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The user must supply the desired eigenvalue vector, and the desired
!    eigenvector matrix.  The eigenvector matrix must be orthogonal.  A
!    suitable random orthogonal matrix can be generated by R8MAT_ORTH_UNIFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of A.
!
!    Input, real ( kind = 8 ) X(N), the desired eigenvalues for the matrix.
!
!    Input, real ( kind = 8 ) Q(N,N), the eigenvector matrix of A.
!
!    Output, real ( kind = 8 ) A(N,N), a symmetric matrix with
!    eigenvalues X and eigenvectors the columns of Q.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) q(n,n)
  real ( kind = 8 ) x(n)
!
!  Set A = Q * Lambda * Q'.
!
  a(1:n,1:n) = 0.0D+00

  do i = 1, n
    do j = 1, n
      do k = 1, n
        a(i,j) = a(i,j) + q(i,k) * x(k) * q(j,k)
      end do
    end do
  end do

  return
end
subroutine r8mat_symm_jacobi ( n, a )

!*****************************************************************************80
!
!! R8MAT_SYMM_JACOBI applies Jacobi eigenvalue iteration to a symmetric matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    This code was modified so that it treats as zero the off-diagonal
!    elements that are sufficiently close to, but not exactly, zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of A.
!
!    Input/output, real ( kind = 8 ) A(N,N), a symmetric N by N matrix.
!    On output, the matrix has been overwritten by an approximately
!    diagonal matrix, with the eigenvalues on the diagonal.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c
  real ( kind = 8 ) r8mat_norm_fro
  real ( kind = 8 ), parameter :: eps = 0.00001D+00
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it
  integer ( kind = 4 ), parameter :: it_max = 100
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) norm_fro
  real ( kind = 8 ) s
  real ( kind = 8 ) sum2
  real ( kind = 8 ) t
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) u

  norm_fro = r8mat_norm_fro ( n, n, a )

  it = 0

  do

    it = it + 1

    do i = 1, n
      do j = 1, i - 1

        if ( eps * norm_fro < abs ( a(i,j) ) + abs ( a(j,i) ) ) then

          u = ( a(j,j) - a(i,i) ) / ( a(i,j) + a(j,i) )

          t = sign ( 1.0D+00, u ) / ( abs ( u ) + sqrt ( u * u + 1.0D+00 ) )
          c = 1.0D+00 / sqrt ( t * t + 1.0D+00 )
          s = t * c
!
!  A -> A * Q.
!
          do k = 1, n
            t1 = a(i,k)
            t2 = a(j,k)
            a(i,k) = t1 * c - t2 * s
            a(j,k) = t1 * s + t2 * c
          end do
!
!  A -> QT * A
!
          do k = 1, n
            t1 = a(k,i)
            t2 = a(k,j)
            a(k,i) = c * t1 - s * t2
            a(k,j) = s * t1 + c * t2
          end do

        end if
      end do
    end do
!
!  Test the size of the off-diagonal elements.
!
    sum2 = 0.0D+00
    do i = 1, n
      do j = 1, i - 1
        sum2 = sum2 + abs ( a(i,j) )
      end do
    end do

    if ( sum2 <= eps * ( norm_fro + 1.0D+00 ) ) then
      exit
    end if

    if ( it_max <= it ) then
      exit
    end if

  end do

  return
end
subroutine r8mat_to_r8plu ( n, a, pivot, lu, info )

!*****************************************************************************80
!
!! R8MAT_TO_R8PLU factors a general R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    This routine is a simplified version of the LINPACK routine DGEFA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix to be factored.
!
!    Output, integer ( kind = 4 ) PIVOT(N), a vector of pivot indices.
!
!    Output, real ( kind = 8 ) LU(N,N), an upper triangular matrix U and
!    the multipliers L which were used to obtain it.  The factorization
!    can be written A = L * U, where L is a product of permutation and
!    unit lower triangular matrices and U is upper triangular.
!
!    Output, integer ( kind = 4 ) INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) lu(n,n)
  real ( kind = 8 ) temp

  lu(1:n,1:n) = a(1:n,1:n)

  info = 0

  do k = 1, n - 1
!
!  Find L, the index of the pivot row.
!
    l = k
    do i = k + 1, n
      if ( abs ( lu(l,k) ) < abs ( lu(i,k) ) ) then
        l = i
      end if
    end do

    pivot(k) = l
!
!  If the pivot index is zero, the algorithm has failed.
!
    if ( lu(l,k) == 0.0D+00 ) then
      info = k
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_TO_R8PLU - Fatal error!'
      write ( *, '(a,i8)' ) '  Zero pivot on step ', info
      return
    end if
!
!  Interchange rows L and K if necessary.
!
    if ( l /= k ) then
      temp    = lu(l,k)
      lu(l,k) = lu(k,k)
      lu(k,k) = temp
    end if
!
!  Normalize the values that lie below the pivot entry A(K,K).
!
    lu(k+1:n,k) = -lu(k+1:n,k) / lu(k,k)
!
!  Row elimination with column indexing.
!
    do j = k + 1, n

      if ( l /= k ) then
        temp    = lu(l,j)
        lu(l,j) = lu(k,j)
        lu(k,j) = temp
      end if

      lu(k+1:n,j) = lu(k+1:n,j) + lu(k+1:n,k) * lu(k,j)

    end do

  end do

  pivot(n) = n

  if ( lu(n,n) == 0.0D+00 ) then
    info = n
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_TO_R8PLU - Fatal error!'
    write ( *, '(a,i8)' ) '  Zero pivot on step ', info
  end if

  return
end
function r8mat_trace ( n, a )

!*****************************************************************************80
!
!! R8MAT_TRACE computes the trace of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The trace of a square matrix is the sum of the diagonal elements.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix A.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix whose trace is desired.
!
!    Output, real ( kind = 8 ) R8MAT_TRACE, the trace of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8mat_trace

  r8mat_trace = 0.0D+00
  do i = 1, n
    r8mat_trace = r8mat_trace + a(i,i)
  end do

  return
end
subroutine r8mat_transpose ( m, n, a, at )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE makes a transposed copy of a matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    FORTRAN90 provides the transpose ( ) function which should be preferred
!    over this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    of the matrix A.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix to be transposed.
!
!    Output, real ( kind = 8 ) AT(N,M), the matrix to be transposed.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) at(n,m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  at = transpose ( a )

  return
end
subroutine r8mat_transpose_in_place ( n, a )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_IN_PLACE transposes a square matrix in place.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns
!    of the matrix A.
!
!    Input/output, real ( kind = 8 ) A(N,N), the matrix to be transposed.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) t

  do j = 1, n
    do i = 1, j - 1
      t      = a(i,j)
      a(i,j) = a(j,i)
      a(j,i) = t
    end do
  end do

  return
end
subroutine r8mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
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
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
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
      write ( ctemp(i2), '(i8,6x)' ) i
    end do

    write ( *, '(''  Row   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Col'
    write ( *, '(a)' ) ' '

    j2lo = max ( jlo, 1 )
    j2hi = min ( jhi, n )

    do j = j2lo, j2hi

      do i2 = 1, inc
        i = i2lo - 1 + i2
        write ( ctemp(i2), '(g14.6)' ) a(i,j)
      end do

      write ( *, '(i5,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

    end do

  end do

  return
end
subroutine r8mat_u_inverse ( n, a, b )

!*****************************************************************************80
!
!! R8MAT_U_INVERSE inverts an upper triangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    An upper triangular matrix is a matrix whose only nonzero entries
!    occur on or above the diagonal.
!
!    The inverse of an upper triangular matrix is an upper triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
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
!    Input, real ( kind = 8 ) A(N,N), the upper triangular matrix.
!
!    Output, real ( kind = 8 ) B(N,N), the inverse matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do j = n, 1, -1

    do i = n, 1, -1

      if ( j < i ) then
        b(i,j) = 0.0D+00
      else if ( i == j ) then
        b(i,j) = 1.0D+00 / a(i,j)
      else
        b(i,j) = - dot_product ( a(i,i+1:j), b(i+1:j,j) ) / a(i,i)
      end if

    end do
  end do

  return
end
subroutine r8mat_u1_inverse ( n, a, b )

!*****************************************************************************80
!
!! R8MAT_U1_INVERSE inverts a unit upper triangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A unit upper triangular matrix is a matrix with only 1's on the main
!    diagonal, and only 0's below the main diagonal.
!
!    The inverse of a unit upper triangular matrix is also
!    a unit upper triangular matrix.
!
!    This routine can invert a matrix in place, that is, with no extra
!    storage.  If the matrix is stored in A, then the call
!
!      call r8mat_u1_inverse ( n, a, a )
!
!    will result in A being overwritten by its inverse.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt,
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
!    Input, real ( kind = 8 ) A(N,N), the unit upper triangular matrix.
!
!    Output, real ( kind = 8 ) B(N,N), the inverse matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do j = n, 1, -1

    do i = n, 1, -1

      if ( j < i ) then
        b(i,j) = 0.0D+00
      else if ( i == j ) then
        b(i,j) = 1.0D+00
      else
        b(i,j) = -dot_product ( a(i,i+1:j), b(i+1:j,j) )
      end if

    end do
  end do

  return
end
subroutine r8mat_uniform_01 ( m, n, seed, r )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_01 fills an R8MAT with unit pseudorandom numbers.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2004
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
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in
!    the array.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = real ( seed, kind = 8 ) * 4.656612875D-10

    end do
  end do

  return
end
subroutine r8mat_uniform_ab ( m, n, a, b, seed, r )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_AB returns a scaled pseudorandom R8MAT.
!
!  Discussion:
!
!    A <= R(I,J) <= B.
!
!    An R8MAT is an array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_UNIFORM_AB - Fatal error!'
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

      r(i,j) = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

    end do
  end do

  return
end
subroutine r8mat_uniform_abvec ( m, n, a, b, seed, r )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_ABVEC returns a scaled pseudorandom R8MAT.
!
!  Discussion:
!
!    A(I) <= R(I,J) <= B(I)
!
!    An R8MAT is an array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input, real ( kind = 8 ) A(M), B(M), the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m)
  real ( kind = 8 ) b(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_UNIFORM_ABVEC - Fatal error!'
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

      r(i,j) = a(i) + ( b(i) - a(i) ) * real ( seed, kind = 8 ) &
        * 4.656612875D-10

    end do
  end do

  return
end

subroutine r8mat_vand2 ( n, x, a )

!*****************************************************************************80
!
!! R8MAT_VAND2 returns the N by N row Vandermonde matrix A.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The row Vandermonde matrix returned by this routine reads "across"
!    rather than down.  In particular, each row begins with a 1, followed by
!    some value X, followed by successive powers of X.
!
!  Formula:
!
!    A(I,J) = X(I)^(J-1)
!
!  Properties:
!
!    A is nonsingular if, and only if, the X values are distinct.
!
!    The determinant of A is
!
!      det(A) = product ( 2 <= I <= N ) (
!        product ( 1 <= J <= I-1 ) ( ( X(I) - X(J) ) ) ).
!
!    The matrix A is generally ill-conditioned.
!
!  Example:
!
!    N = 5, X = (2, 3, 4, 5, 6)
!
!    1 2  4   8   16
!    1 3  9  27   81
!    1 4 16  64  256
!    1 5 25 125  625
!    1 6 36 216 1296
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix desired.
!
!    Input, real ( kind = 8 ) X(N), the values that define A.
!
!    Output, real ( kind = 8 ) A(N,N), the N by N row Vandermonde matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)

  do i = 1, n
    do j = 1, n

      if ( j == 1 .and. x(i) == 0.0D+00 ) then
        a(i,j) = 1.0D+00
      else
        a(i,j) = x(i)**(j-1)
      end if

    end do
  end do

  return
end
subroutine r8mat_zero ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_ZERO zeroes an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Output, real ( kind = 8 ) A(M,N), the matrix of zeroes.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)

  a(1:m,1:n) = 0.0D+00

  return
end
