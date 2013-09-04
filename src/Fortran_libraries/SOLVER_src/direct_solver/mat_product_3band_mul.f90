!>@file   mat_product_3band_mul.f90
!!@brief  module mat_product_3band_mul
!!
!!@author H. Matsui
!!@date Programmed on 2007
!!@n    Modified on May, 2013
!
!>@brief  Take product of band matrices
!!
!!@verbatim
!!---------------------------------------------------------------------
!!      subroutine cal_mat_product_3band_mul(n, mcomp, kr_st, kr_ed,    &
!!     &          a_left, a_right, a_prod)
!!      Evaluate product of 3-band matrices
!!                (a_prod) = (a_left)(a_right)
!!
!!      subroutine cal_mat_product_3band_mul(n, mcomp, kr_st, kr_ed,    &
!!     &          a3_left, a5_right, a7_prod)
!!       Evaluate product of 3-band and 5-band matrix
!!                (a7_prod) = (a3_left)(a5_right)
!!
!!   Format of 3-band matrix
!!               | a(2,1)  a(1,2)  ........     0         0     |
!!               | a(3,1)  a(2,2)  ........     .         .     |
!!               |   0     a(3,2)  ........     .         .     |
!!    a(i,j)  =  |   .       0     ........     0         .     |
!!               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!!               |   .       .     ........  a(1,N-2)     0     |
!!               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!!               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!!
!!   Arbitorary band matrix
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
!!---------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param n                       size of matrix
!!@n @param ncomp                   number of matrix
!!@n @param kr_st                   start address
!!@n @param kr_ed                   end address
!!
!!@n @param a_left(3,n,mcomp)       input matrix on left (3-band)
!!@n @param a_right(3,n,mcomp)      input matrix on right (3-band)
!!@n @param a3_left(3,n,mcomp)      input matrix on left (3-band)
!!@n @param a5_right(5,n,mcomp)     input matrix on left (5-band)
!!@n @param a_prod(5,n,mcomp)       produced matrix (5-band)
!!@n @param a7_prod(7,n,mcomp)      produced matrix (7-band)
!
!
      module mat_product_3band_mul
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_mat_product_3band_mul(n, mcomp, kr_st, kr_ed,      &
     &          a_left, a_right,  a_prod)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: n, mcomp
      real(kind = kreal), intent(in) :: a_left(3,n,mcomp)
      real(kind = kreal), intent(in) :: a_right(3,n,mcomp)
!
      real(kind = kreal), intent(inout) :: a_prod(5,n,mcomp)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do k = kr_st+2, kr_ed-2
        do j = 1, mcomp
          a_prod(5,k-2,j) =  a_left(3,k-1,j) * a_right(3,k-2,j)
          a_prod(4,k-1,j) =  a_left(3,k-1,j) * a_right(2,k-1,j)         &
     &                     + a_left(2,k  ,j) * a_right(3,k-1,j)
          a_prod(3,k,  j) =  a_left(3,k-1,j) * a_right(1,k  ,j)         &
     &                     + a_left(2,k  ,j) * a_right(2,k  ,j)         &
     &                     + a_left(1,k+1,j) * a_right(3,k  ,j)
          a_prod(2,k+1,j) =  a_left(2,k  ,j) * a_right(1,k+1,j)         &
     &                     + a_left(1,k+1,j) * a_right(2,k+1,j)
          a_prod(1,k+2,j) =  a_left(1,k+1,j) * a_right(1,k+2,j)
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private (k,j)
        do j = 1, mcomp
          k = kr_st
          a_prod(3,k,  j) = one
          a_prod(2,k+1,j) = zero
          a_prod(1,k+2,j) = zero
!
          k = kr_st + 1
!          a_prod(5,k-2,j) =  zero
          a_prod(4,k-1,j) =  a_left(3,k-1,j) * a_right(2,k-1,j)         &
     &                     + a_left(2,k  ,j) * a_right(3,k-1,j)
          a_prod(3,k,  j) =  a_left(3,k-1,j) * a_right(1,k  ,j)         &
     &                     + a_left(2,k  ,j) * a_right(2,k  ,j)         &
     &                     + a_left(1,k+1,j) * a_right(3,k  ,j)
          a_prod(2,k+1,j) =  a_left(2,k  ,j) * a_right(1,k+1,j)         &
     &                     + a_left(1,k+1,j) * a_right(2,k+1,j)
          a_prod(1,k+2,j)  = a_left(1,k+1,j) * a_right(1,k+2,j)
!
          k = kr_ed - 1
          a_prod(5,k-2,j) =  a_left(3,k-1,j) * a_right(3,k-2,j)
          a_prod(4,k-1,j) =  a_left(3,k-1,j) * a_right(2,k-1,j)         &
     &                     + a_left(2,k  ,j) * a_right(3,k-1,j)
          a_prod(3,k,  j) =  a_left(3,k-1,j) * a_right(1,k  ,j)         &
     &                     + a_left(2,k  ,j) * a_right(2,k  ,j)         &
     &                     + a_left(1,k+1,j) * a_right(3,k  ,j)
          a_prod(2,k+1,j) =  a_left(2,k  ,j) * a_right(1,k+1,j)         &
     &                     + a_left(1,k+1,j) * a_right(2,k+1,j)
!          a_prod(1,k+2,j) = zero
!
          k = kr_ed
          a_prod(5,k-2,j) = zero
          a_prod(4,k-1,j) = zero
          a_prod(3,k,  j) = one
        end do
!$omp end parallel do
!
      end subroutine cal_mat_product_3band_mul
!
! -----------------------------------------------------------------------
!
      subroutine cal_mat_prod_3b5b_mul(n, mcomp, kr_st, kr_ed,          &
     &          a3_left, a5_right, a7_prod)
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: n, mcomp
      real(kind = kreal), intent(in) :: a3_left(3,n,mcomp)
      real(kind = kreal), intent(in) :: a5_right(5,n,mcomp)
!
      real(kind = kreal), intent(inout) :: a7_prod(7,n,mcomp)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do k = kr_st+3, kr_ed-3
        do j = 1, mcomp
          a7_prod(7,k-3,j) =  a3_left(3,k-1,j) * a5_right(5,k-3,j)
          a7_prod(6,k-2,j) =  a3_left(3,k-1,j) * a5_right(4,k-2,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(5,k-2,j)
          a7_prod(5,k-1,j) =  a3_left(3,k-1,j) * a5_right(3,k-1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(4,k-1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(5,k-1,j)
          a7_prod(4,k,  j) =  a3_left(3,k-1,j) * a5_right(2,k  ,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(3,k  ,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(4,k  ,j)
          a7_prod(3,k+1,j) =  a3_left(3,k-1,j) * a5_right(1,k+1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(2,k+1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(3,k+1,j)
          a7_prod(2,k+2,j) =  a3_left(2,k  ,j) * a5_right(1,k+2,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(2,k+2,j)
          a7_prod(1,k+3,j) =  a3_left(1,k+1,j) * a5_right(1,k+3,j)
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private (k,j)
        do j = 1, mcomp
          k = kr_st
          a7_prod(4,k,  j) =  a3_left(2,k  ,j) * a5_right(3,k  ,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(4,k  ,j)
          a7_prod(3,k+1,j) =  a3_left(2,k  ,j) * a5_right(2,k+1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(3,k+1,j)
          a7_prod(2,k+2,j) =  a3_left(2,k  ,j) * a5_right(1,k+2,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(2,k+2,j)
          a7_prod(1,k+3,j) =  a3_left(1,k+1,j) * a5_right(1,k+3,j)
!
          k = kr_st+1
          a7_prod(5,k-1,j) =  a3_left(3,k-1,j) * a5_right(3,k-1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(4,k-1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(5,k-1,j)
          a7_prod(4,k,  j) =  a3_left(3,k-1,j) * a5_right(2,k  ,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(3,k  ,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(4,k  ,j)
          a7_prod(3,k+1,j) =  a3_left(3,k-1,j) * a5_right(1,k+1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(2,k+1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(3,k+1,j)
          a7_prod(2,k+2,j) =  a3_left(2,k  ,j) * a5_right(1,k+2,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(2,k+2,j)
          a7_prod(1,k+3,j) =  a3_left(1,k+1,j) * a5_right(1,k+3,j)
!
          k = kr_st+2
          a7_prod(6,k-2,j) =  a3_left(3,k-1,j) * a5_right(4,k-2,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(5,k-2,j)
          a7_prod(5,k-1,j) =  a3_left(3,k-1,j) * a5_right(3,k-1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(4,k-1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(5,k-1,j)
          a7_prod(4,k,  j) =  a3_left(3,k-1,j) * a5_right(2,k  ,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(3,k  ,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(4,k  ,j)
          a7_prod(3,k+1,j) =  a3_left(3,k-1,j) * a5_right(1,k+1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(2,k+1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(3,k+1,j)
          a7_prod(2,k+2,j) =  a3_left(2,k  ,j) * a5_right(1,k+2,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(2,k+2,j)
          a7_prod(1,k+3,j) =  a3_left(1,k+1,j) * a5_right(1,k+3,j)
!
!
          k = kr_ed - 2
          a7_prod(7,k-3,j) =  a3_left(3,k-1,j) * a5_right(5,k-3,j) 
          a7_prod(6,k-2,j) =  a3_left(3,k-1,j) * a5_right(4,k-2,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(5,k-2,j)
          a7_prod(5,k-1,j) =  a3_left(3,k-1,j) * a5_right(3,k-1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(4,k-1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(5,k-1,j)
          a7_prod(4,k,  j) =  a3_left(3,k-1,j) * a5_right(2,k  ,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(3,k  ,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(4,k  ,j)
          a7_prod(3,k+1,j) =  a3_left(3,k-1,j) * a5_right(1,k+1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(2,k+1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(3,k+1,j)
          a7_prod(2,k+2,j) =  a3_left(2,k  ,j) * a5_right(1,k+2,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(2,k+2,j)
!
          k = kr_ed - 1
          a7_prod(7,k-3,j) =  a3_left(3,k-1,j) * a5_right(5,k-3,j) 
          a7_prod(6,k-2,j) =  a3_left(3,k-1,j) * a5_right(4,k-2,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(5,k-2,j)
          a7_prod(5,k-1,j) =  a3_left(3,k-1,j) * a5_right(3,k-1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(4,k-1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(5,k-1,j)
          a7_prod(4,k,  j) =  a3_left(3,k-1,j) * a5_right(2,k  ,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(3,k  ,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(4,k  ,j)
          a7_prod(3,k+1,j) =  a3_left(3,k-1,j) * a5_right(1,k+1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(2,k+1,j)      &
     &                      + a3_left(1,k+1,j) * a5_right(3,k+1,j)
!
          k = kr_ed
          a7_prod(7,k-3,j) =  a3_left(3,k-1,j) * a5_right(5,k-3,j) 
          a7_prod(6,k-2,j) =  a3_left(3,k-1,j) * a5_right(4,k-2,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(5,k-2,j)
          a7_prod(5,k-1,j) =  a3_left(3,k-1,j) * a5_right(3,k-1,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(4,k-1,j)
          a7_prod(4,k,  j) =  a3_left(3,k-1,j) * a5_right(2,k  ,j)      &
     &                      + a3_left(2,k  ,j) * a5_right(3,k  ,j)
        end do
!$omp end parallel do
!
      end subroutine cal_mat_prod_3b5b_mul
!
! -----------------------------------------------------------------------
!
      end module mat_product_3band_mul
