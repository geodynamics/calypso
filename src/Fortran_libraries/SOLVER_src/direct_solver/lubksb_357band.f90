!>@file   lubksb_357band.f90
!!@brief  module lubksb_357band
!!
!!@author H. Matsui
!!@date Programmed on 2007
!
!>@brief  Solve 3, 5, or 7 band matriox after LU decomposition
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!      SUBROUTINE lubksb_3band(n, band_lu, i_pivot, x)
!!      SUBROUTINE lubksb_5band(n, band_lu, i_pivot, x)
!!      SUBROUTINE lubksb_7band(n, band_lu, i_pivot, x)
!!
!!c solve the set of n linear equations Ax=b. Here is a input,
!!c not at the matrix A but rather as its LU decompsition,
!!c determined by the routine ludcmp. i_pivot is input as the
!!c permutation vector returned by ludcmp. b(1:n) is input
!!c as the right-hand side vectror b, and returns with the
!!c solution vector x.
!!c a, n, np and i_pivot are not modified by this routine
!!c and be left in place for successive calls with different
!!c right hand sides b. This routine takes into account
!!c the possibility that b will begin with many zero elements,
!!c so it is efficient for use in matrix inversion.
!! 
!!
!!      Data formt of band matrix
!!
!!               | a(2,1)  a(1,2)  ........     0         0     |
!!               | a(3,1)  a(2,2)  ........     .         .     |
!!               |   0     a(3,2)  ........     .         .     |
!!    a(i,j)  =  |   .       0     ........     0         .     |
!!               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!!               |   .       .     ........  a(1,N-2)     0     |
!!               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!!               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!!
!!   Original band matrix
!!      band_a(i-j+iband+1,j) = a(i,j)
!!      band_a(k,j) = a(k+j-iband-1,j)
!!   3-band matrix
!!      band_a(i-j+2,j) = a(i,j)
!!      band_a(k,j) = a(k+j-2,j)
!!   5-band matrix
!!      band_lu(i-j+3,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-3,j)
!!   7-band matrix
!!      band_lu(i-j+4,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-4,j)
!!   9-band matrix
!!      band_lu(i-j+5,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-5,j)
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module lubksb_357band
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_3band(n, band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: n
      integer (kind = kint), intent(in) :: i_pivot(n)
      real(kind = kreal), intent(in) :: band_lu(5,n)
      real(kind = kreal), intent(inout) :: x(n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ll
!
!
      do i = 1, n
        ll = i_pivot(i)
        sum =   x(ll)
        x(ll) = x(i)
        x(i) =  sum
      end do
!
      x(2) = x(2) - band_lu(4,1) * x(1)
      do i = 3, n
        x(i) = x(i) - band_lu(5,i-2)*x(i-2) - band_lu(4,i-1)*x(i-1)
      end do
!
      x(n  ) = x(n) / band_lu(3,n)
      x(n-1) = (x(n-1) - band_lu(2,n) * x(n)) / band_lu(3,n-1)
      do i = n-2,1,-1
        x(i) = (x(i) - band_lu(2,i+1) * x(i+1)                          &
     &               - band_lu(1,i+2) * x(i+2) ) / band_lu(3,i)
      end do
!
      end subroutine lubksb_3band
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_5band(n, band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: n
      integer (kind = kint), intent(in) :: i_pivot(n)
      real(kind = kreal), intent(in) :: band_lu(9,n)
      real(kind = kreal), intent(inout) :: x(n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ll
!
!
      do i = 1, n
        ll = i_pivot(i)
        sum =   x(ll)
        x(ll) = x(i)
        x(i) =  sum
      end do
!
      x(2) = x(2) - band_lu(6,1)*x(1)
      x(3) = x(3) - band_lu(7,1)*x(1)                                   &
     &            - band_lu(6,2)*x(2)
      x(4) = x(4) - band_lu(8,1)*x(1)                                   &
     &            - band_lu(7,2)*x(2)                                   &
     &            - band_lu(6,3)*x(3)
      do i = 5, n
        x(i) = x(i) - band_lu(9,i-4)*x(i-4)                             &
     &              - band_lu(8,i-3)*x(i-3)                             &
     &              - band_lu(7,i-2)*x(i-2)                             &
     &              - band_lu(6,i-1)*x(i-1)
      end do
!
      x(n) =   x(n) / band_lu(5,n)
      x(n-1) = (x(n-1) - band_lu(4,n  ) * x(n  )) / band_lu(5,n-1)
      x(n-2) = (x(n-2) - band_lu(4,n-1) * x(n-1)                        &
     &                 - band_lu(3,n  ) * x(n  )) / band_lu(5,n-2)
      x(n-3) = (x(n-3) - band_lu(4,n-2) * x(n-2)                        &
     &                 - band_lu(3,n-1) * x(n-1)                        &
     &                 - band_lu(2,n  ) * x(n  )) / band_lu(5,n-3)
      do i = n-4, 1, -1
        x(i) = (x(i) - band_lu(4,i+1) * x(i+1)                          &
     &               - band_lu(3,i+2) * x(i+2)                          &
     &               - band_lu(2,i+3) * x(i+3)                          &
     &               - band_lu(1,i+4) * x(i+4) ) / band_lu(5,i)
      end do
!
      end subroutine lubksb_5band
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_7band(n, band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: n
      integer (kind = kint), intent(in) :: i_pivot(n)
      real(kind = kreal), intent(in) :: band_lu(13,n)
      real(kind = kreal), intent(inout) :: x(n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ll
!
!
      do i = 1, n
        ll = i_pivot(i)
        sum =   x(ll)
        x(ll) = x(i)
        x(i) =  sum
      end do
!
      x(2) = x(2) - band_lu(8, 1)*x(1)
      x(3) = x(3) - band_lu(9, 1)*x(1)                                  &
     &            - band_lu(8, 2)*x(2)
      x(4) = x(4) - band_lu(10,1)*x(1)                                  &
     &            - band_lu(9, 2)*x(2)                                  &
     &            - band_lu(8, 3)*x(3)
      x(5) = x(5) - band_lu(11,1)*x(1)                                  &
     &            - band_lu(10,2)*x(2)                                  &
     &            - band_lu(9, 3)*x(3)                                  &
     &            - band_lu(8, 4)*x(4)
      x(6) = x(6) - band_lu(12,1)*x(1)                                  &
     &            - band_lu(11,2)*x(2)                                  &
     &            - band_lu(10,3)*x(3)                                  &
     &            - band_lu(9, 4)*x(4)                                  &
     &            - band_lu(8, 5)*x(5)
      do i = 7, n
        x(i) = x(i) - band_lu(13,i-6)*x(i-6)                            &
     &              - band_lu(12,i-5)*x(i-5)                            &
     &              - band_lu(11,i-4)*x(i-4)                            &
     &              - band_lu(10,i-3)*x(i-3)                            &
     &              - band_lu(9, i-2)*x(i-2)                            &
     &              - band_lu(8, i-1)*x(i-1)
      end do
!
      x(n) =   x(n) / band_lu(7,n)
      x(n-1) = (x(n-1) - band_lu(6,n  ) * x(n  )) / band_lu(7,n-1)
      x(n-2) = (x(n-2) - band_lu(6,n-1) * x(n-1)                        &
     &                 - band_lu(5,n  ) * x(n  )) / band_lu(7,n-2)
      x(n-3) = (x(n-3) - band_lu(6,n-2) * x(n-2)                        &
     &                 - band_lu(5,n-1) * x(n-1)                        &
     &                 - band_lu(4,n  ) * x(n  )) / band_lu(7,n-3)
      x(n-4) = (x(n-4) - band_lu(6,n-3) * x(n-3)                        &
     &                 - band_lu(5,n-2) * x(n-2)                        &
     &                 - band_lu(4,n-1) * x(n-1)                        &
     &                 - band_lu(3,n  ) * x(n  )) / band_lu(7,n-4)
      x(n-5) = (x(n-5) - band_lu(6,n-4) * x(n-4)                        &
     &                 - band_lu(5,n-3) * x(n-3)                        &
     &                 - band_lu(4,n-2) * x(n-2)                        &
     &                 - band_lu(3,n-1) * x(n-1)                        &
     &                 - band_lu(2,n  ) * x(n  )) / band_lu(7,n-5)
      do i = n-6, 1, -1
        x(i) = (x(i) - band_lu(6,i+1) * x(i+1)                          &
     &               - band_lu(5,i+2) * x(i+2)                          &
     &               - band_lu(4,i+3) * x(i+3)                          &
     &               - band_lu(3,i+4) * x(i+4)                          &
     &               - band_lu(2,i+5) * x(i+5)                          &
     &               - band_lu(1,i+6) * x(i+6) ) / band_lu(7,i)
      end do
!
      end subroutine lubksb_7band
!
! ----------------------------------------------------------------------
!
      end module lubksb_357band
