!>@file   const_sph_divergence.f90
!!@brief  module const_sph_divergence
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate divergence of forces
!!
!!@verbatim
!!      subroutine const_sph_scalar_advect                              &
!!     &         (sph_rj, r_2nd, sph_bc, fdm2_center, g_sph_rj,         &
!!     &          is_flux, is_advect, rj_fld)
!!      subroutine const_sph_div_force(sph_rj, r_2nd, sph_bc_U,         &
!!     &          g_sph_rj, is_fld, is_div, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!
!!@param is_flux    Spherical hermonics data address for input flux
!!@param is_advect  Spherical hermonics data address for advection
!!@param is_fld     Spherical hermonics data address for input vector
!!@param is_div     Spherical hermonics data address for divergence
!
      module const_sph_divergence
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_scalar_advect                                &
     &         (sph_rj, r_2nd, sph_bc, fdm2_center, g_sph_rj,           &
     &          is_flux, is_advect, rj_fld)
!
      use cal_sph_exp_rotation
      use select_exp_scalar_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      integer(kind = kint), intent(in) :: is_flux, is_advect
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_nod_vect_div2(sph_bc%kr_in, sph_bc%kr_out,           &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj, r_2nd%fdm(1)%dmat, &
     &    is_flux, is_advect, rj_fld%n_point, rj_fld%ntot_phys,         &
     &    rj_fld%d_fld)
      call sel_bc_sph_scalar_advect(sph_rj, sph_bc, fdm2_center,        &
     &    g_sph_rj, is_flux, is_advect, rj_fld)
!
      end subroutine const_sph_scalar_advect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_div_force(sph_rj, r_2nd, sph_bc_U,           &
     &          g_sph_rj, is_fld, is_div, rj_fld)
!
      use cal_sph_exp_rotation
      use cal_sph_exp_nod_none_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_fld, is_div
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_nod_vect_div2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj, r_2nd%fdm(1)%dmat, &
     &    is_fld, is_div, rj_fld%n_point, rj_fld%ntot_phys,             &
     &    rj_fld%d_fld)
!
      call cal_sph_nod_nobc_in_div2                                     &
     &   (sph_rj%nidx_rj(2), g_sph_rj, sph_bc_U%kr_in,                  &
     &    sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_fld_ICB, is_fld, is_div,    &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_sph_nod_nobc_out_div2                                    &
     &   (sph_rj%nidx_rj(2), g_sph_rj, sph_bc_U%kr_out,                 &
     &    sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_fld_CMB, is_fld, is_div,    &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine const_sph_div_force
!
! -----------------------------------------------------------------------
!
      end module const_sph_divergence
