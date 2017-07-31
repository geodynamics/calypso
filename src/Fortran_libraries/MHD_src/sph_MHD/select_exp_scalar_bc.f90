!> @file  select_exp_scalar_bc.f90
!!      module select_exp_scalar_bc
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives
!!
!!@verbatim
!!      subroutine sel_bc_radial_grad_scalar                            &
!!     &         (sph_rj, sph_bc, fdm2_center, g_sph_rj,                &
!!     &          is_fld, is_grad, rj_fld)
!!          Input:    is_fld
!!          Solution: is_grad
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!
!!      subroutine sel_bc_sph_scalar_diffusion                          &
!!     &         (sph_rj, sph_bc, fdm2_center, g_sph_rj, coef_diffuse,  &
!!     &          is_fld, is_diffuse, rj_fld)
!!          Input:    is_fld
!!          Solution: is_diffusee
!!
!!      subroutine sel_bc_sph_scalar_advect(sph_rj, sph_bc, fdm2_center,&
!!     &          g_sph_rj, is_flux, is_advect, rj_fld)
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
      module select_exp_scalar_bc
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
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
      subroutine sel_bc_radial_grad_scalar                              &
     &         (sph_rj, sph_bc, fdm2_center, g_sph_rj,                  &
     &          is_fld, is_grad, rj_fld)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_rotation
      use cal_sph_exp_center
!
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_center_mat), intent(in) :: fdm2_center
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
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call dsdr_sph_in_fix_flux_2(sph_rj%nidx_rj(2), g_sph_rj,        &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%ICB_flux,                &
     &      is_fld, is_grad, rj_fld%n_point, rj_fld%ntot_phys,          &
     &      rj_fld%d_fld)
        call dsdr_sph_lm0_in_fix_flux_2                                 &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),               &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%ICB_flux, is_grad,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call dsdr_sph_fix_scalar_in_2                                   &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      sph_bc%ICB_fld, is_fld, is_grad,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call dsdr_sph_lm0_fix_scalar_in_2                               &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),               &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      sph_bc%ICB_fld, is_fld, is_grad,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        call dsdr_sph_out_fix_flux_2                                    &
     &     (sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_out,                 &
     &      sph_bc%r_CMB, sph_bc%CMB_flux, is_fld, is_grad,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call dsdr_sph_lm0_out_fix_flux_2                                &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),               &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%CMB_flux, is_grad,      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call dsdr_sph_fix_scalar_out_2                                  &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      sph_bc%CMB_fld, is_fld, is_grad,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call dsdr_sph_lm0_fix_scalar_out_2                              &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),               &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      sph_bc%CMB_fld, is_fld, is_grad,                            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_bc_radial_grad_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_bc_sph_scalar_diffusion                            &
     &         (sph_rj, sph_bc, fdm2_center, g_sph_rj, coef_diffuse,    &
     &          is_fld, is_diffuse, rj_fld)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_center
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
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
     &      fdm2_center%dmat_fix_fld, sph_bc%ICB_fld, coef_diffuse,     &
     &      is_fld, is_diffuse, rj_fld%n_point, rj_fld%ntot_phys,       &
     &      rj_fld%d_fld)
        call cal_sph_fixed_center_diffuse2                              &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      is_diffuse, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call cal_sph_in_fix_flux_diffuse2                               &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,         &
     &      sph_bc%ICB_flux, coef_diffuse, is_fld, is_diffuse,          &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_sph_fix_scalar_in_diffuse2                             &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      sph_bc%ICB_fld, coef_diffuse, is_fld, is_diffuse,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_sph_out_fix_flux_diffuse2                              &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      sph_bc%CMB_flux, coef_diffuse, is_fld, is_diffuse,          &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_sph_out_fix_scalar_diffuse2                            &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      sph_bc%CMB_fld, coef_diffuse, is_fld, is_diffuse,           &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_bc_sph_scalar_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine sel_bc_sph_scalar_advect(sph_rj, sph_bc, fdm2_center,  &
     &          g_sph_rj, is_flux, is_advect, rj_fld)
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_center
!
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm2_center_mat), intent(in) :: fdm2_center
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
        call cal_sph_div_flux_4_fix_ctr                                 &
     &     (sph_rj%nidx_rj(2), sph_bc%r_ICB, g_sph_rj, sph_bc%ICB_fld,  &
     &      fdm2_center%dmat_fix_fld, is_flux, is_advect,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call cal_sph_fixed_center_diffuse2                              &
     &     (sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      is_advect, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call cal_div_sph_in_fix_flux_2                                  &
     &     (sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_in,                  &
     &      sph_bc%r_ICB, sph_bc%ICB_flux, is_flux, is_advect,          &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_sph_div_flux_4_fix_in                                  &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,        &
     &      sph_bc%ICB_fld, is_flux, is_advect,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux) then
        call cal_div_sph_out_fix_flux_2                                 &
     &     (sph_rj%nidx_rj(2), g_sph_rj, sph_bc%kr_out,                 &
     &      sph_bc%r_CMB, sph_bc%CMB_flux, is_flux, is_advect,          &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_sph_div_flux_4_fix_out                                 &
     &     (sph_rj%nidx_rj(2), g_sph_rj,                                &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,       &
     &      sph_bc%CMB_fld, is_flux, is_advect,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_bc_sph_scalar_advect
!
! -----------------------------------------------------------------------
!
      end module select_exp_scalar_bc
