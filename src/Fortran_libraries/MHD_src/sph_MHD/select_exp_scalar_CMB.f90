!> @file  select_exp_scalar_CMB.f90
!!      module select_exp_scalar_CMB
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives
!!
!!@verbatim
!!      subroutine set_CMB_scalar_sph_crank(sph_rj, sph_bc, CMB_Sspec,  &
!!     &          coef_f, coef_d, dt, coef_imp, is_field, rj_fld)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine sel_CMB_radial_grad_scalar                           &
!!     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj,                  &
!!     &          is_fld, is_grad, rj_fld)
!!          Input:    is_fld
!!          Solution: is_grad
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!      subroutine sel_CMB_sph_scalar_diffusion                         &
!!     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj, coef_diffuse,    &
!!     &          is_fld, is_diffuse, rj_fld)
!!          Input:    is_fld
!!          Solution: is_diffusee
!!
!!      subroutine sel_CMB_sph_scalar_advect                            &
!!     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj,                  &
!!     &          is_flux, is_advect, rj_fld)
!!          Input:    is_flux
!!          Solution: is_advect
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!
!!@param coef_diffuse   Diffusion coefficient
!!
!!@param is_fld       Spherical hermonics data address for input vector
!!@param is_grad      Spherical hermonics data address for gradient
!!@param is_diffuse   Input spectr diffusiton term address
!!@param is_flux    Spherical hermonics data address for input flux
!!@param is_advect  Spherical hermonics data address for advection
!
      module select_exp_scalar_CMB
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
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
      subroutine set_CMB_scalar_sph_crank(sph_rj, sph_bc, CMB_Sspec,    &
     &          coef_f, coef_d, dt, coef_imp, is_field, rj_fld)
!
      use set_scalar_boundary_sph
      use cal_sph_exp_center
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
      real(kind = kreal), intent(in) :: coef_imp, coef_f, coef_d
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: is_field
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!   Set RHS vector for CMB
      if (sph_bc%iflag_cmb .eq. iflag_fixed_field                       &
     &  .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
        call set_fixed_scalar_sph(sph_rj%nidx_rj(2),                    &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_bc%kr_out, sph_rj%nidx_rj(1), is_field,                 &
     &      CMB_Sspec%S_BC, CMB_Sspec%S_CTR,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(coef_f .ne. 0.0d0) then
        call adjust_out_fixed_flux_sph                                  &
     &     (sph_rj%nidx_rj(2), sph_bc%kr_out, sph_bc%r_CMB,             &
     &      sph_bc%fdm2_fix_dr_CMB, CMB_Sspec%S_BC, coef_d,             &
     &      coef_imp, dt, is_field, rj_fld%n_point, rj_fld%ntot_phys,   &
     &      rj_fld%d_fld)
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_flux                   &
!     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
      else
        call poisson_out_fixed_flux_sph                                 &
     &     (sph_rj%nidx_rj(2), sph_bc%kr_out, sph_bc%r_CMB,             &
     &      sph_bc%fdm2_fix_dr_CMB, CMB_Sspec%S_BC, is_field,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine set_CMB_scalar_sph_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_radial_grad_scalar                             &
     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj,                    &
     &          is_fld, is_grad, rj_fld)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_rotation
!
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!
      integer(kind = kint), intent(in) :: is_fld, is_grad
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux                        &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call dsdr_sph_out_fix_flux_2                                    &
     &     (sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_out,                 &
     &      sph_bc%r_CMB, CMB_Sspec%S_BC, is_fld, is_grad,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call dsdr_sph_lm0_out_fix_flux_2                                &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),               &
     &      sph_bc%kr_out, sph_bc%r_CMB, CMB_Sspec%S_BC, is_grad,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        call dsdr_sph_fix_scalar_out_2                                  &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      CMB_Sspec%S_BC, is_fld, is_grad,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call dsdr_sph_lm0_fix_scalar_out_2                              &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),               &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      CMB_Sspec%S_BC, is_fld, is_grad,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_CMB_radial_grad_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_scalar_diffusion                           &
     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj, coef_diffuse,      &
     &          is_fld, is_diffuse, rj_fld)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux                        &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call cal_sph_out_fix_flux_diffuse2                              &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      CMB_Sspec%S_BC, coef_diffuse, is_fld, is_diffuse,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        call cal_sph_out_fix_scalar_diffuse2                            &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      CMB_Sspec%S_BC, coef_diffuse, is_fld, is_diffuse,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_CMB_sph_scalar_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_scalar_advect                              &
     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj,                    &
     &          is_flux, is_advect, rj_fld)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!
      integer(kind = kint), intent(in) :: is_flux, is_advect
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux                        &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call cal_div_sph_out_fix_flux_2                                 &
     &     (sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_out,                 &
     &      sph_bc%r_CMB, CMB_Sspec%S_BC, is_flux, is_advect,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        call cal_sph_div_flux_4_fix_out                                 &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      CMB_Sspec%S_BC, is_flux, is_advect,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_CMB_sph_scalar_advect
!
! -----------------------------------------------------------------------
!
      end module select_exp_scalar_CMB
