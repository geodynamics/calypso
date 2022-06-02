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
!!     &          coef_f, coef_d, dt, coef_imp, is_field,               &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!
!!      subroutine sel_CMB_radial_grad_scalar                           &
!!     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj,                  &
!!     &          is_fld, is_grad, n_point, ntot_phys_rj, d_rj)
!!          Input:    is_fld
!!          Solution: is_grad
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!      subroutine sel_CMB_sph_scalar_diffusion                         &
!!     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj, coef_diffuse,    &
!!     &          is_fld, is_diffuse, n_point, ntot_phys_rj, d_rj)
!!          Input:    is_fld
!!          Solution: is_diffusee
!!
!!      subroutine sel_CMB_sph_scalar_advect                            &
!!     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj,                  &
!!     &          is_flux, is_advect, n_point, ntot_phys_rj, d_rj)
!!          Input:    is_flux
!!          Solution: is_advect
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!      subroutine set_CMB_scalar_boundary_1d                           &
!!     &         (sph_rj, sph_bc, CMB_Sspec, rhs)
!!      subroutine sel_CMB_radial_grad_1d_scalar                        &
!!     &         (sph_rj, sph_bc, CMB_Sspec, d_r, grad_r)
!!      subroutine fix_CMB_radial_grad_1d_scalar                        &
!!     &         (sph_rj, sph_bc, d_r, grad_r)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
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
     &          coef_f, coef_d, dt, coef_imp, is_field,                 &
     &          n_point, ntot_phys_rj, d_rj)
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
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_field
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
!   Set RHS vector for CMB
      if (sph_bc%iflag_cmb .eq. iflag_fixed_field                       &
     &  .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
        call set_fixed_scalar_sph(sph_rj%nidx_rj(2),                    &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_bc%kr_out, sph_rj%nidx_rj(1), is_field,                 &
     &      CMB_Sspec%S_BC, CMB_Sspec%S_CTR,                            &
     &      n_point, ntot_phys_rj, d_rj)
      else if(coef_f .ne. 0.0d0) then
        call adjust_out_fixed_flux_sph                                  &
     &     (sph_rj%nidx_rj(2), sph_bc%kr_out, sph_bc%r_CMB,             &
     &      sph_bc%fdm2_fix_dr_CMB, CMB_Sspec%S_BC, coef_d,             &
     &      coef_imp, dt, is_field, n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_flux                   &
!     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
      else
        call poisson_out_fixed_flux_sph                                 &
     &     (sph_rj%nidx_rj(2), sph_bc%kr_out, sph_bc%r_CMB,             &
     &      sph_bc%fdm2_fix_dr_CMB, CMB_Sspec%S_BC, is_field,           &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine set_CMB_scalar_sph_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_radial_grad_scalar                             &
     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj,                    &
     &          is_fld, is_grad, n_point, ntot_phys_rj, d_rj)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_rotation
!
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_fld, is_grad
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux                        &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call dsdr_sph_out_fix_flux_2                                    &
     &     (sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_out,                 &
     &      sph_bc%r_CMB, CMB_Sspec%S_BC, is_fld, is_grad,              &
     &      n_point, ntot_phys_rj, d_rj)
        call dsdr_sph_lm0_out_fix_flux_2                                &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),               &
     &      sph_bc%kr_out, sph_bc%r_CMB, CMB_Sspec%S_BC, is_grad,       &
     &      n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        call dsdr_sph_fix_scalar_out_2                                  &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      CMB_Sspec%S_BC, is_fld, is_grad,                            &
     &      n_point, ntot_phys_rj, d_rj)
        call dsdr_sph_lm0_fix_scalar_out_2                              &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),               &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      CMB_Sspec%S_BC, is_fld, is_grad,                            &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_CMB_radial_grad_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_scalar_diffusion                           &
     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj, coef_diffuse,      &
     &          is_fld, is_diffuse, n_point, ntot_phys_rj, d_rj)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux                        &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call cal_sph_out_fix_flux_diffuse2                              &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      CMB_Sspec%S_BC, coef_diffuse, is_fld, is_diffuse,           &
     &      n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        call cal_sph_out_fix_scalar_diffuse2                            &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      CMB_Sspec%S_BC, coef_diffuse, is_fld, is_diffuse,           &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_CMB_sph_scalar_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_sph_scalar_advect                              &
     &         (sph_rj, sph_bc, CMB_Sspec, g_sph_rj,                    &
     &          is_flux, is_advect, n_point, ntot_phys_rj, d_rj)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: is_flux, is_advect
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux                        &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call cal_div_sph_out_fix_flux_2                                 &
     &     (sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_out,                 &
     &      sph_bc%r_CMB, CMB_Sspec%S_BC, is_flux, is_advect,           &
     &      n_point, ntot_phys_rj, d_rj)
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        call cal_sph_div_flux_4_fix_out                                 &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      CMB_Sspec%S_BC, is_flux, is_advect,                         &
     &      n_point, ntot_phys_rj, d_rj)
      end if
!
      end subroutine sel_CMB_sph_scalar_advect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_CMB_scalar_boundary_1d                             &
     &         (sph_rj, sph_bc, CMB_Sspec, rhs)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
!
      real(kind = kreal), intent(inout) :: rhs(0:sph_rj%nidx_rj(1))
!
!
!   Set RHS vector for CMB
      if(     sph_bc%iflag_cmb .eq. iflag_fixed_flux                    &
     &   .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        if(      sph_bc%iflag_icb .eq. iflag_sph_fix_center             &
     &      .or. sph_bc%iflag_icb .eq. iflag_fixed_field                &
     &      .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
          rhs(sph_bc%kr_out)                                            &
     &       = (sph_bc%fdm2_fix_dr_CMB( 1,3) + two*sph_bc%r_CMB(1))     &
     &        * CMB_Sspec%S_BC(sph_rj%idx_rj_degree_zero)
        else
          rhs(sph_bc%kr_out) = 0.0d0
        end if
!      else if (sph_bc%iflag_cmb .eq. iflag_fixed_field                 &
!     &  .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        rhs(sph_bc%kr_out)                                              &
     &         = CMB_Sspec%S_BC(sph_rj%idx_rj_degree_zero)
      end if
!
      end subroutine set_CMB_scalar_boundary_1d
!
! -----------------------------------------------------------------------
!
      subroutine sel_CMB_radial_grad_1d_scalar                          &
     &         (sph_rj, sph_bc, CMB_Sspec, d_r, grad_r)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_scalar_BC_coef), intent(in) :: CMB_Sspec
      type(sph_boundary_type), intent(in) :: sph_bc
!
      real(kind = kreal), intent(in) :: d_r(0:sph_rj%nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: grad_r(0:sph_rj%nidx_rj(1))
!
      real(kind = kreal) :: BC0_CMB
      integer(kind = kint) :: kr
!
!
      kr = sph_bc%kr_out
      BC0_CMB = CMB_Sspec%S_BC(sph_rj%idx_rj_degree_zero)
      if(      sph_bc%iflag_cmb .eq. iflag_fixed_flux                   &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        if(      sph_bc%iflag_icb .eq. iflag_sph_fix_center             &
     &      .or. sph_bc%iflag_icb .eq. iflag_fixed_field                &
     &      .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
          grad_r(kr) = BC0_CMB
        else
          grad_r(kr) =  sph_bc%fdm2_fix_fld_CMB(2,2) * d_r(kr-2)        &
     &                + sph_bc%fdm2_fix_fld_CMB(1,2) * d_r(kr-1)        &
     &                + sph_bc%fdm2_fix_fld_CMB(0,2) * d_r(kr  )
          write(*,*) 'Given condition:   ',  BC0_CMB
          write(*,*) 'Numerical solution:',  grad_r(kr)
        end if
!
!      else if(sph_bc%iflag_cmb .eq. iflag_fixed_field                  &
!     &   .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        grad_r(kr) =  sph_bc%fdm2_fix_fld_CMB(2,2) * d_r(kr-2)          &
     &               + sph_bc%fdm2_fix_fld_CMB(1,2) * d_r(kr-1)         &
     &               + sph_bc%fdm2_fix_fld_CMB(0,2) * BC0_CMB
      end if
!
      end subroutine sel_CMB_radial_grad_1d_scalar
!
! -----------------------------------------------------------------------
!
      subroutine fix_CMB_radial_grad_1d_scalar                          &
     &         (sph_rj, sph_bc, d_r, grad_r)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
!
      real(kind = kreal), intent(in) :: d_r(0:sph_rj%nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: grad_r(0:sph_rj%nidx_rj(1))
!
      integer(kind = kint) :: kr
!
      kr = sph_bc%kr_out
      grad_r(kr) =  sph_bc%fdm2_fix_fld_CMB(2,2) * d_r(kr-2)            &
     &            + sph_bc%fdm2_fix_fld_CMB(1,2) * d_r(kr-1)            &
     &            + sph_bc%fdm2_fix_fld_CMB(0,2) * d_r(kr  )

      end subroutine fix_CMB_radial_grad_1d_scalar
!
! -----------------------------------------------------------------------
!
      end module select_exp_scalar_CMB
