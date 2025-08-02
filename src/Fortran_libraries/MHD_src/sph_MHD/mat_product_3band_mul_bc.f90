!>@file   mat_product_3band_mul_bc.f90
!!@brief  module mat_product_3band_mul_bc
!!
!!@author H. Matsui
!!@date Programmed on 2007
!!@n    Modified on May,  2013
!!@n    Modified on July, 2025
!
!>@brief  Take product of band matrices at boudaries
!!
!!@verbatim
!!---------------------------------------------------------------------
!!      subroutine cal_vp_evo_mat_product_bc(sph_bc_U, n, mcomp,        &
!!     &                                     a_left, a_right, a_prod)
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        integer(kind = kint), intent(in) :: n, mcomp
!!        real(kind = kreal), intent(in) :: a_left(3,n,mcomp)
!!        real(kind = kreal), intent(in) :: a_right(3,n,mcomp)
!!        real(kind = kreal), intent(inout) :: a_prod(5,n,mcomp)
!!      Evaluate product of 3-band matrices at boudaries
!!                (a_prod) = (a_left)(a_right)
!!
!!      subroutine cal_mat_prod_3b5b_mat_bc(n, mcomp, kr_st, kr_ed,     &
!!     &          a3_left, a5_right, a7_prod)
!!        integer(kind = kint), intent(in) :: kr_st, kr_ed
!!        integer(kind = kint), intent(in) :: n, mcomp
!!        real(kind = kreal), intent(in) :: a3_left(3,n,mcomp)
!!        real(kind = kreal), intent(in) :: a5_right(5,n,mcomp)
!!        real(kind = kreal), intent(inout) :: a7_prod(7,n,mcomp)
!!       Evaluate product of 3-band and 5-band matrix at boudaries
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
      module mat_product_3band_mul_bc
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
      subroutine cal_vp_evo_mat_product_bc(sph_bc_U, n, mcomp,          &
     &                                     a_left, a_right, a_prod)
!
      use t_boundary_params_sph_MHD
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: n, mcomp
      real(kind = kreal), intent(in) :: a_left(3,n,mcomp)
      real(kind = kreal), intent(in) :: a_right(3,n,mcomp)
!
      real(kind = kreal), intent(inout) :: a_prod(5,n,mcomp)
!
      integer(kind = kint) :: k, j
!
!$omp parallel
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        k = 1
!$omp do private(j)
        do j = 1, mcomp
!          a_prod(5,k-2,j) =  zero
!          a_prod(4,k-1,j) =  zero
          a_prod(3,k,  j) =  a_left(2,k  ,j) * a_right(2,k  ,j)         &
     &                     + a_left(1,k+1,j) * a_right(3,k  ,j)
          a_prod(2,k+1,j) =  a_left(2,k  ,j) * a_right(1,k+1,j)         &
     &                     + a_left(1,k+1,j) * a_right(2,k+1,j)
          a_prod(1,k+2,j)  = a_left(1,k+1,j) * a_right(1,k+2,j)
        end do
!$omp end do nowait
      else
        k = sph_bc_U%kr_in
!$omp do private (j)
        do j = 1, mcomp
          a_prod(3,k,  j) = one
          a_prod(2,k+1,j) = zero
          a_prod(1,k+2,j) = zero
        end do
!$omp end do nowait
      end if
!
!
      k = sph_bc_U%kr_out
!$omp do private(j)
      do j = 1, mcomp
        a_prod(5,k-2,j) = zero
        a_prod(4,k-1,j) = zero
        a_prod(3,k,  j) = one
      end do
!$omp end do
!$omp end parallel
!
      end subroutine cal_vp_evo_mat_product_bc
!
! -----------------------------------------------------------------------
!
      subroutine cal_mat_prod_3b5b_mat_bc(n, mcomp, kr_st, kr_ed,       &
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
!$omp parallel
      k = kr_st
!$omp do private(j)
      do j = 1, mcomp
        a7_prod(4,k,  j) =  a3_left(2,k  ,j) * a5_right(3,k  ,j)        &
     &                    + a3_left(1,k+1,j) * a5_right(4,k  ,j)
        a7_prod(3,k+1,j) =  a3_left(2,k  ,j) * a5_right(2,k+1,j)        &
     &                    + a3_left(1,k+1,j) * a5_right(3,k+1,j)
        a7_prod(2,k+2,j) =  a3_left(2,k  ,j) * a5_right(1,k+2,j)        &
     &                    + a3_left(1,k+1,j) * a5_right(2,k+2,j)
        a7_prod(1,k+3,j) =  a3_left(1,k+1,j) * a5_right(1,k+3,j)
      end do
!$omp end do nowait
!
      k = kr_ed
!$omp do private(j)
      do j = 1, mcomp
        a7_prod(7,k-3,j) =  a3_left(3,k-1,j) * a5_right(5,k-3,j) 
        a7_prod(6,k-2,j) =  a3_left(3,k-1,j) * a5_right(4,k-2,j)        &
     &                    + a3_left(2,k  ,j) * a5_right(5,k-2,j)
        a7_prod(5,k-1,j) =  a3_left(3,k-1,j) * a5_right(3,k-1,j)        &
     &                    + a3_left(2,k  ,j) * a5_right(4,k-1,j)
        a7_prod(4,k,  j) =  a3_left(3,k-1,j) * a5_right(2,k  ,j)        &
     &                    + a3_left(2,k  ,j) * a5_right(3,k  ,j)
      end do
!$omp end do
!$omp end parallel
!
      end subroutine cal_mat_prod_3b5b_mat_bc
!
! -----------------------------------------------------------------------
!
      end module mat_product_3band_mul_bc
