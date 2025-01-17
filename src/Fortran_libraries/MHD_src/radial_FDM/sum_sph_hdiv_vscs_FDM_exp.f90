!>@file   sum_sph_hdiv_vscs_FDM_exp.f90
!!@brief  module sum_sph_hdiv_vscs_FDM_exp
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Substitute viscousity matrix in each layer
!!
!!@verbatim
!!      subroutine sub_exp_sph_hdiv_viscous(kr, nnod_rj, jmax, coef_p,  &
!!     &                                    press_e, hdiv_viscous_e)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!!      subroutine sum_exp_sph_hdiv_viscous(kr, nnod_rj, jmax,          &
!!     &          d_vpol, hdiv_visous_mat, hdiv_viscous_e)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!!        real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!!
!!      subroutine sum_exp_sph_hdiv_viscous_CTR(nnod_rj, jmax,          &
!!     &          d_vpol, mat_hdiv_vcs_CTR, e_hdiv_viscous)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: mat_hdiv_vcs_CTR(jmax,0:1)
!!        real(kind = kreal), intent(inout) :: e_hdiv_viscous(nnod_rj)
!!      subroutine sum_exp_sph_hdiv_viscous_ICB(k_ICB, nnod_rj, jmax,   &
!!     &          d_vpol, hdiv_visous_mat_ICB, hdiv_viscous_e)
!!        integer(kind = kint), intent(in) :: k_ICB
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_ICB(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!!      subroutine sum_exp_sph_hdiv_viscous_CMB(k_CMB, nnod_rj, jmax,   &
!!     &          d_vpol, hdiv_visous_mat_CMB, hdiv_viscous_e)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!!@endverbatim
!!
      module sum_sph_hdiv_vscs_FDM_exp
!
      use m_precision
      use m_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine sub_exp_sph_hdiv_viscous(kr, nnod_rj, jmax, coef_p,    &
     &                                    press_e, hdiv_viscous_e)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!
      integer(kind = kint) :: ist, ied
!
!
      ist = 1 + (kr-1) * jmax
      ied =      kr * jmax
      hdiv_viscous_e(ist:ied) = - coef_p * press_e(ist:ied)
!
      end subroutine sub_exp_sph_hdiv_viscous
!
!  -------------------------------------------------------------------
!
      subroutine sum_exp_sph_hdiv_viscous(kr, nnod_rj, jmax,            &
     &          d_vpol, hdiv_visous_mat, hdiv_viscous_e)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!
      real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, i_n2, inod, iele
!
!
      do j = 1, jmax
        iele = j + (kr-1) * jmax
        inod = iele
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
        i_p1 = inod + jmax
!
        hdiv_viscous_e(iele) =  hdiv_visous_mat(j,-2) * d_vpol(i_n2)    &
     &                        + hdiv_visous_mat(j,-1) * d_vpol(i_n1)    &
     &                        + hdiv_visous_mat(j, 0) * d_vpol(inod)    &
     &                        + hdiv_visous_mat(j, 1) * d_vpol(i_p1)
      end do
!
      end subroutine sum_exp_sph_hdiv_viscous
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine sum_exp_sph_hdiv_viscous_CTR(nnod_rj, jmax,            &
     &          d_vpol, mat_hdiv_vcs_CTR, e_hdiv_viscous)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: mat_hdiv_vcs_CTR(jmax,0:1)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(nnod_rj)
!
      integer(kind = kint) :: j, iele, i_p1, inod
!
!
!$omp parallel do private(j,iele,i_p1,inod)
      do j = 1, jmax
        iele = j
        i_p1 = iele + jmax
        inod = iele
!
        e_hdiv_viscous(iele) =  mat_hdiv_vcs_CTR(j, 0) * d_vpol(inod)   &
     &                        + mat_hdiv_vcs_CTR(j, 1) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
      end subroutine sum_exp_sph_hdiv_viscous_CTR
!
! -----------------------------------------------------------------------
!
      subroutine sum_exp_sph_hdiv_viscous_ICB(k_ICB, nnod_rj, jmax,     &
     &          d_vpol, hdiv_visous_mat_ICB, hdiv_viscous_e)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: hdiv_visous_mat_ICB(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, inod, iele
!
!
!$omp parallel do private(j,iele,inod,i_n1,i_p1)
      do j = 1, jmax
        iele = j + (k_ICB-1) * jmax
        inod = iele
        i_n1 = inod - jmax
        i_p1 = inod + jmax
!
        hdiv_viscous_e(iele)                                            &
     &                    =  hdiv_visous_mat_ICB(j,-1) * d_vpol(i_n1)   &
     &                     + hdiv_visous_mat_ICB(j, 0) * d_vpol(inod)   &
     &                     + hdiv_visous_mat_ICB(j, 1) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
!
      end subroutine sum_exp_sph_hdiv_viscous_ICB
!
! -----------------------------------------------------------------------
!
      subroutine sum_exp_sph_hdiv_viscous_CMB(k_CMB, nnod_rj, jmax,     &
     &          d_vpol, hdiv_visous_mat_CMB, hdiv_viscous_e)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!
      real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_n2, inod, iele
!
!
!$omp parallel do private(j,iele,inod,i_n2,i_n1)
      do j = 1, jmax
        iele = j + (k_CMB-1) * jmax
        inod = iele
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        hdiv_viscous_e(iele)                                            &
     &                    =  hdiv_visous_mat_CMB(j,-2) * d_vpol(i_n2)   &
     &                     + hdiv_visous_mat_CMB(j,-1) * d_vpol(i_n1)   &
     &                     + hdiv_visous_mat_CMB(j, 0) * d_vpol(inod)
      end do
!$omp end parallel do
!
      end subroutine sum_exp_sph_hdiv_viscous_CMB
!
! -----------------------------------------------------------------------
!
      end module sum_sph_hdiv_vscs_FDM_exp
