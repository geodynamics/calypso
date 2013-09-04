!>@file   set_non_slip_sph_mat_bc.f90
!!@brief  module set_non_slip_sph_mat_bc
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for non-slip boundaries
!!
!!@verbatim
!!      subroutine set_non_slip_icb_vt_sph_mat
!!      subroutine set_rgd_icb_vp_poisson3_mat
!!
!!      subroutine set_non_slip_cmb_vt_sph_mat
!!      subroutine set_rgd_cmb_vp_poisson3_mat
!!@endverbatim
!
      module set_non_slip_sph_mat_bc
!
      use m_precision
!
      use m_constants
      use m_t_int_parameter
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_radial_matrices_sph
      use m_fdm_coefs
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_non_slip_icb_vt_sph_mat
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
!       vt_evo_mat(3,nlayer_ICB-1,j) = zero
        vt_evo_mat(2,nlayer_ICB,  j) = one
        vt_evo_mat(1,nlayer_ICB+1,j) = zero
      end do
!
      end subroutine set_non_slip_icb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_rgd_icb_vp_poisson3_mat
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
!       vs_poisson_mat(3,nlayer_ICB-1,j) = -coef_fdm_fix_dr_ICB_2(-1,3)
        vs_poisson_mat(2,nlayer_ICB,  j) = -coef_fdm_fix_dr_ICB_2(0,3)  &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2)
        vs_poisson_mat(1,nlayer_ICB+1,j) = -coef_fdm_fix_dr_ICB_2(1,3)
      end do
!
      end subroutine set_rgd_icb_vp_poisson3_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_non_slip_cmb_vt_sph_mat
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        vt_evo_mat(3,nlayer_CMB-1,j) = zero
        vt_evo_mat(2,nlayer_CMB,  j) = one
!       vt_evo_mat(1,nlayer_CMB+1,j) = zero
      end do
!
      end subroutine set_non_slip_cmb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_rgd_cmb_vp_poisson3_mat
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        vs_poisson_mat(3,nlayer_CMB-1,j) = -coef_fdm_fix_dr_CMB_2(-1,3)
        vs_poisson_mat(2,nlayer_CMB,  j) = -coef_fdm_fix_dr_CMB_2(0,3)  &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)
!       vs_poisson_mat(1,nlayer_CMB+1,j) = -coef_fdm_fix_dr_CMB_2(1,3)
      end do
!
      end subroutine set_rgd_cmb_vp_poisson3_mat
!
! -----------------------------------------------------------------------
!
      end module set_non_slip_sph_mat_bc
