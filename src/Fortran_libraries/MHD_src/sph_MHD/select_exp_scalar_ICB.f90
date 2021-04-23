!> @file  select_exp_scalar_ICB.f90
!!      module select_exp_scalar_ICB
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives
!!
!!@verbatim
!!      subroutine set_ICB_scalar_sph_crank(sph_rj, sph_bc, ICB_Sspec,  &
!!     &          coef_f, coef_d, diffuse_reduction, dt, coef_imp,      &
!!     &          is_field, rj_fld)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_BC_coef), intent(in) :: ICB_Sspec
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine sel_ICB_radial_grad_scalar                           &
!!     &         (sph_rj, sph_bc, ICB_Sspec, fdm2_center, g_sph_rj,     &
!!     &          is_fld, is_grad, rj_fld)
!!          Input:    is_fld
!!          Solution: is_grad
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!
!!      subroutine sel_ICB_sph_scalar_diffusion                         &
!!     &         (sph_rj, sph_bc, ICB_Sspec, fdm2_center,               &
!!     &          g_sph_rj, coef_diffuse, is_fld, is_diffuse, rj_fld)
!!          Input:    is_fld
!!          Solution: is_diffusee
!!
!!      subroutine sel_ICB_sph_scalar_advect                            &
!!     &         (sph_rj, sph_bc, ICB_Sspec, fdm2_center, g_sph_rj,     &
!!     &          is_flux, is_advect, rj_fld)
!!          Input:    is_flux
!!          Solution: is_advect
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
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
      module select_exp_scalar_ICB
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
      subroutine set_ICB_scalar_sph_crank(sph_rj, sph_bc, ICB_Sspec,    &
     &          coef_f, coef_d, diffuse_reduction, dt, coef_imp,        &
     &          is_field, rj_fld)
!
      use set_scalar_boundary_sph
      use cal_sph_exp_center
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: ICB_Sspec
      real(kind = kreal), intent(in) :: coef_imp, coef_f, coef_d
      real(kind = kreal), intent(in) :: diffuse_reduction
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: is_field
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!   Set RHS vector for ICB
      if(sph_bc%iflag_icb .eq. iflag_sph_fill_center) return
      if(sph_bc%iflag_icb .eq. iflag_fixed_field                        &
     &  .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
        call set_fixed_scalar_sph(sph_rj%nidx_rj(2),                    &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      ione, sph_bc%kr_in, is_field,                               &
     &      ICB_Sspec%S_BC, ICB_Sspec%S_CTR,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call cal_sph_fixed_center                                       &
     &     (sph_rj%inod_rj_center, sph_bc%CTR_fld, is_field,            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux                    &
     &     .and. coef_f .ne. 0.0d0) then
        call adjust_in_fixed_flux_sph                                   &
     &     (sph_rj%nidx_rj(2), sph_bc%kr_in, sph_bc%r_ICB,              &
     &      sph_bc%fdm2_fix_dr_ICB, ICB_Sspec%S_BC,                     &
     &      coef_d, diffuse_reduction, coef_imp, dt, is_field,          &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux                   &
!     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
      else
        call poisson_in_fixed_flux_sph                                  &
     &     (sph_rj%nidx_rj(2), sph_bc%kr_in, sph_bc%r_ICB,              &
     &      sph_bc%fdm2_fix_dr_ICB, ICB_Sspec%S_BC,                     &
     &      is_field, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine set_ICB_scalar_sph_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_radial_grad_scalar                             &
     &         (sph_rj, sph_bc, ICB_Sspec, fdm2_center, g_sph_rj,       &
     &          is_fld, is_grad, rj_fld)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_rotation
      use cal_sph_exp_center
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: ICB_Sspec
!
      integer(kind = kint), intent(in) :: is_fld, is_grad
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_center1_grad22                                     &
     &     (sph_rj%nidx_rj(2), sph_bc%r_ICB, g_sph_rj,                  &
     &      fdm2_center%dmat_fix_fld, is_fld, is_grad,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call sph0_scalar_fill_ctr_grad2                                 &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_rj%nidx_rj(2), fdm2_center%dmat_fix_fld,                &
     &      is_fld, is_grad, rj_fld%n_point, rj_fld%ntot_phys,          &
     &      rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call cal_sph_center1_grad22                                     &
     &     (sph_rj%nidx_rj(2), sph_bc%r_ICB, g_sph_rj,                  &
     &      fdm2_center%dmat_fix_fld, is_fld, is_grad,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call dsdr_sph_lm0_fixed_ctr_2                                   &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_rj%nidx_rj(2), sph_bc%r_ICB, g_sph_rj, sph_bc%CTR_fld,  &
     &      fdm2_center%dmat_fix_fld, fdm2_center%dmat_fixed,           &
     &      is_fld, is_grad, rj_fld%n_point, rj_fld%ntot_phys,          &
     &      rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux                    &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call dsdr_sph_in_fix_flux_2(sph_rj%nidx_rj(2), g_sph_rj,        &
     &      sph_bc%kr_in, sph_bc%r_ICB, ICB_Sspec%S_BC,                 &
     &      is_fld, is_grad, rj_fld%n_point, rj_fld%ntot_phys,          &
     &      rj_fld%d_fld)
        call dsdr_sph_lm0_in_fix_flux_2                                 &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),               &
     &      sph_bc%kr_in, sph_bc%r_ICB, ICB_Sspec%S_BC, is_grad,        &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!      else if(sph_bc%iflag_icb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
      else
        call dsdr_sph_fix_scalar_in_2                                   &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      ICB_Sspec%S_BC, is_fld, is_grad,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call dsdr_sph_lm0_fix_scalar_in_2                               &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),               &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      ICB_Sspec%S_BC, is_fld, is_grad,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_ICB_radial_grad_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_sph_scalar_diffusion                           &
     &         (sph_rj, sph_bc, ICB_Sspec, fdm2_center,                 &
     &          g_sph_rj, coef_diffuse, is_fld, is_diffuse, rj_fld)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_center
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: ICB_Sspec
!
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_filled_center_diffuse2                             &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_rj%nidx_rj(2), sph_bc%r_ICB, g_sph_rj,                  &
     &      fdm2_center%dmat_fix_fld, fdm2_center%dmat_fix_dr,          &
     &      coef_diffuse, is_fld, is_diffuse,                           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call cal_sph_fixed_center1_diffuse2                             &
     &     (sph_rj%nidx_rj(2), sph_bc%r_ICB, g_sph_rj,                  &
     &      fdm2_center%dmat_fix_fld, ICB_Sspec%S_BC, coef_diffuse,     &
     &      is_fld, is_diffuse, rj_fld%n_point, rj_fld%ntot_phys,       &
     &      rj_fld%d_fld)
        call cal_sph_fixed_center_diffuse2                              &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      is_diffuse, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux                    &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call cal_sph_in_fix_flux_diffuse2                               &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,         &
     &      ICB_Sspec%S_BC, coef_diffuse, is_fld, is_diffuse,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!      else if(sph_bc%iflag_icb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
      else
        call cal_sph_fix_scalar_in_diffuse2                             &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      ICB_Sspec%S_BC, coef_diffuse, is_fld, is_diffuse,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_ICB_sph_scalar_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_ICB_sph_scalar_advect                              &
     &         (sph_rj, sph_bc, ICB_Sspec, fdm2_center, g_sph_rj,       &
     &          is_flux, is_advect, rj_fld)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_center
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: ICB_Sspec
!
      integer(kind = kint), intent(in) :: is_flux, is_advect
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        call cal_sph_filled_center_diffuse2                             &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_rj%nidx_rj(2), sph_bc%r_ICB, g_sph_rj,                  &
     &      fdm2_center%dmat_fix_fld, fdm2_center%dmat_fix_dr,          &
     &      dminus, is_flux, is_advect,                                 &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call cal_sph_div_flux_4_fix_ctr(sph_rj%nidx_rj(2),              &
     &      sph_bc%r_ICB, g_sph_rj, ICB_Sspec%S_BC,                     &
     &      fdm2_center%dmat_fix_fld, is_flux, is_advect,               &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call cal_sph_fixed_center_diffuse2                              &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      is_advect, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux                    &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call cal_div_sph_in_fix_flux_2                                  &
     &     (sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_in,                  &
     &      sph_bc%r_ICB, ICB_Sspec%S_BC, is_flux, is_advect,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!      else if(sph_bc%iflag_icb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
      else
        call cal_sph_div_flux_4_fix_in                                  &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      ICB_Sspec%S_BC, is_flux, is_advect,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_ICB_sph_scalar_advect
!
! -----------------------------------------------------------------------
!
      end module select_exp_scalar_ICB
