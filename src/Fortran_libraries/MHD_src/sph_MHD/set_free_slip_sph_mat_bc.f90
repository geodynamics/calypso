!>@file   set_free_slip_sph_mat_bc.f90
!!@brief  module set_free_slip_sph_mat_bc
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for free-slip boundaries
!!
!!@verbatim
!!      subroutine set_free_slip_icb_vt_sph_mat
!!      subroutine set_free_icb_vp_poisson3_mat
!!
!!      subroutine set_free_slip_cmb_vt_sph_mat
!!      subroutine set_free_cmb_vp_poisson3_mat
!!@endverbatim
!
      module set_free_slip_sph_mat_bc
!
      use m_precision
!
      use m_constants
      use m_t_int_parameter
      use m_physical_property
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
      subroutine set_free_slip_icb_vt_sph_mat
!
      use m_coef_fdm_free_ICB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
!       vt_evo_mat(3,nlayer_ICB-1,j) = zero
        vt_evo_mat(2,nlayer_ICB,  j) = one                              &
     &                            + coef_imp_v*dt*coef_d_velo           &
     &                             *(-coef_fdm_free_ICB_vt2(0,3)        &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2) )
        vt_evo_mat(1,nlayer_ICB+1,j) = -coef_fdm_free_ICB_vt2(1,3)      &
     &                            * coef_imp_v*dt*coef_d_velo
      end do
!
      end subroutine set_free_slip_icb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_free_icb_vp_poisson3_mat
!
      use m_coef_fdm_free_ICB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
!       vs_poisson_mat(3,nlayer_ICB-1,j) = -coef_fdm_free_ICB_vp2(-1,3)
        vs_poisson_mat(2,nlayer_ICB,  j) = -coef_fdm_free_ICB_vp2(0,3)  &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2)
        vs_poisson_mat(1,nlayer_ICB+1,j) = -coef_fdm_free_ICB_vp2(1,3)
      end do
!
      end subroutine set_free_icb_vp_poisson3_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_free_slip_cmb_vt_sph_mat
!
      use m_coef_fdm_free_CMB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        vt_evo_mat(3,nlayer_CMB-1,j) =-coef_imp_v*dt*coef_d_velo        &
     &                                * coef_fdm_free_CMB_vt2(-1,3)
        vt_evo_mat(2,nlayer_CMB,  j) = one                              &
     &                            + coef_imp_v*dt*coef_d_velo           &
     &                             *(-coef_fdm_free_CMB_vt2(0,3)        &
     &                          + g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2))
!       vt_evo_mat(1,nlayer_CMB+1,j) = -coef_fdm_free_CMB_vt2(1,3)
      end do
!
      end subroutine set_free_slip_cmb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_free_cmb_vp_poisson3_mat
!
      use m_coef_fdm_free_CMB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        vs_poisson_mat(3,nlayer_CMB-1,j) = -coef_fdm_free_CMB_vp2(-1,3)
        vs_poisson_mat(2,nlayer_CMB,  j) = -coef_fdm_free_CMB_vp2(0,3)  &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)
      end do
!
      end subroutine set_free_cmb_vp_poisson3_mat
!
! -----------------------------------------------------------------------
!
      end module set_free_slip_sph_mat_bc
