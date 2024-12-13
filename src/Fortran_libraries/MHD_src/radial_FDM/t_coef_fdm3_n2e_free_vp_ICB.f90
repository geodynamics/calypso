!>@file   t_coef_fdm3_n2e_free_vp_ICB.f90
!!@brief  module t_coef_fdm3_n2e_free_vp_ICB
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for non-slip at ICB
!!
!!@verbatim
!!      subroutine check_fdm3_n2e_ICB_free_vpol(id_file, fdm3e_free_ICB)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
!!
!!      subroutine cal_fdm3e_ICB_free_hdiv_vp                           &
!!     &         (dmat_vp_free_ICB, fdm3e_vp0_ICB, fdm3e_free_ICB)
!!        real(kind = kreal), intent(in) :: dmat_vp_free_ICB(-1:1,3)
!!        type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
!!        type(fdm3_n2e_ICB_free_vpol), intent(inout) :: fdm3e_free_ICB
!!      subroutine cal_fdm3_free_vp_ICB_ele(i_th, kr_in, nnod_rj, jmax, &
!!     &          fdm3e_free_ICB, d_rj, dfdr_rj, dele_bc)
!!        type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
!!        integer(kind = kint), intent(in) :: i_th, kr_in
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: d_rj(nnod_rj)
!!        real(kind = kreal), intent(in) :: dfdr_rj(nnod_rj)
!!        real(kind = kreal), intent(inout) :: dele_bc(nnod_rj)
!!
!!   Matrix for poloidal velocity with horizontal divergence at CMB
!!      dfdr_ICB = dmat_vp_free_ICB(0,2) * d_rj(ICB)
!!                + dmat_vp_free_ICB(1,2) * d_rj(ICB+1)
!!      dnfdr_e(0:3) =  fdm3e_ICB%dmat_vp0(-2,1:4) * dfdr_ICB           &
!!     &              + fdm3e_ICB%dmat_vp0(-1,1:4) * d_rj(ICB  )        &
!!     &              + fdm3e_ICB%dmat_vp0( 0,1:4) * d_rj(ICB+1)        &
!!     &              + fdm3e_ICB%dmat_vp0( 1,1:4) * d_rj(ICB+2)
!!                   =  fdm3e_ICB%dmat_vp0(-2,1:4)                      &
!!     &               * (dmat_vp_free_ICB(0,2) * d_rj(ICB)             &
!!                      + dmat_vp_free_ICB(1,2) * d_rj(ICB+1))          &
!!     &              + fdm3e_ICB%dmat_vp0(-1,1:4) * d_rj(ICB  )        &
!!     &              + fdm3e_ICB%dmat_vp0( 0,1:4) * d_rj(ICB+1)        &
!!     &              + fdm3e_ICB%dmat_vp0( 1,1:4) * d_rj(ICB+2)
!!                   =  (fdm3e_ICB%dmat_vp0(-1,1:4)                     &
!!     &                 + fdm3e_ICB%dmat_vp0(-2,1:4)                   &
!!     &                  * dmat_vp_free_ICB(0,2)) * d_rj(ICB  )        &
!!     &              + (fdm3e_ICB%dmat_vp0( 0,1:4)                     &
!!     &                + fdm3e_ICB%dmat_vp0(-2,1:4)                    &
!!     &                  * dmat_vp_free_ICB(1,2)) * d_rj(ICB+1)
!!     &              + fdm3e_ICB%dmat_vp0( 1,1:4) * d_rj(ICB+2)
!!@endverbatim
!!
!!@n @param r_from_ICB(0:3) radius to three next points of ICB
!!
      module t_coef_fdm3_n2e_free_vp_ICB
!
      use m_precision
!
      use m_constants
!
      implicit none
!
!
      type fdm3_n2e_ICB_free_vpol
!>        Matrix to evaluate radial derivative at Boundary
        real(kind = kreal) :: dmat_vp0(-2:1,4)
      end type fdm3_n2e_ICB_free_vpol
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_fdm3_n2e_ICB_free_vpol(id_file, fdm3e_free_ICB)
!
      integer(kind = kint), intent(in) :: id_file
      type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
!
!
      write(id_file,*) ' free slip at CMB'
      write(id_file,*) ' fdm3e_free_ICB%dmat_vp0'
      write(id_file,*) 'matrix for Interpolation to element'
      write(id_file,'(1p9E25.15e3)') fdm3e_free_ICB%dmat_vp0(-2:1,1)
      write(id_file,*) 'matrix for dfdr'
      write(id_file,'(1p9E25.15e3)') fdm3e_free_ICB%dmat_vp0(-2:1,2)
      write(id_file,*) 'matrix for d2fdr2'
      write(id_file,'(1p9E25.15e3)') fdm3e_free_ICB%dmat_vp0(-2:1,3)
      write(id_file,*) 'matrix for d3fdr3'
      write(id_file,'(1p9E25.15e3)') fdm3e_free_ICB%dmat_vp0(-2:1,4)
!
      end subroutine check_fdm3_n2e_ICB_free_vpol
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm3e_ICB_free_hdiv_vp                             &
     &         (dmat_vp_free_ICB, fdm3e_vp0_ICB, fdm3e_free_ICB)
!
      use t_coef_fdm3_n2e_zero_vp_ICB
!
      real(kind = kreal), intent(in) :: dmat_vp_free_ICB(-1:1,3)
      type(fdm3_n2e_ICB_zero_vpol), intent(in) :: fdm3e_vp0_ICB
!
      type(fdm3_n2e_ICB_free_vpol), intent(inout) :: fdm3e_free_ICB
!
!
      fdm3e_free_ICB%dmat_vp0(-2,1:4) = zero
      fdm3e_free_ICB%dmat_vp0(-1,1:4) = zero
!      fdm3e_free_ICB%dmat_vp0(-1,1:4) = fdm3e_vp0_ICB%dmat_vp0(-1,1:4) &
!     &   + fdm3e_vp0_ICB%dmat_vp0(-2,1:4) * dmat_vp_free_ICB(0,2)
      fdm3e_free_ICB%dmat_vp0( 0,1:4) = fdm3e_vp0_ICB%dmat_vp0( 0,1:4)  &
     &    + fdm3e_vp0_ICB%dmat_vp0(-2,1:4) * dmat_vp_free_ICB(1,2)
      fdm3e_free_ICB%dmat_vp0( 1,1:4) = fdm3e_vp0_ICB%dmat_vp0( 1,1:4)
!
      end subroutine cal_fdm3e_ICB_free_hdiv_vp
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm3_free_vp_ICB_ele(i_th, kr_in, nnod_rj, jmax,   &
     &          fdm3e_free_ICB, d_rj, dfdr_rj, dele_bc)
!
      type(fdm3_n2e_ICB_free_vpol), intent(in) :: fdm3e_free_ICB
      integer(kind = kint), intent(in) :: i_th, kr_in
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: d_rj(nnod_rj)
      real(kind = kreal), intent(in) :: dfdr_rj(nnod_rj)
!
      real(kind = kreal), intent(inout) :: dele_bc(nnod_rj)
!
      integer(kind = kint) :: inod, i_p1, i_p2, j
!
!
!$omp parallel do private(inod,i_p1,i_p2,j)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
        i_p1 = j + (kr_in  ) * jmax
        i_p2 = j + (kr_in+1) * jmax
!
        dele_bc(inod) = fdm3e_free_ICB%dmat_vp0( 1,i_th+1) * d_rj(i_p2) &
     &                + fdm3e_free_ICB%dmat_vp0( 0,i_th+1) * d_rj(i_p1) &
     &                + fdm3e_free_ICB%dmat_vp0(-1,i_th+1) * d_rj(inod) &
     &                + fdm3e_free_ICB%dmat_vp0(-2,i_th+1)              &
     &                                                 * dfdr_rj(inod)
      end do
!$omp end parallel do
!
      end subroutine cal_fdm3_free_vp_ICB_ele
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm3_n2e_free_vp_ICB
