!>@file   select_r_mat_scalar_bc_sph.f90
!!@brief  module select_r_mat_scalar_bc_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine sel_radial_mat_press_bc_sph(sph_rj, sph_bc_U,        &
!!     &          fdm2_center, g_sph_rj, r_coef, band_p_poisson)
!!      subroutine sel_radial_mat_scalar_bc_sph(sph_rj, sph_bc,         &
!!     &          fdm2_center, g_sph_rj, r_coef, band_s_evo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        real(kind = kreal), intent(in):: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        real(kind = kreal), intent(in) :: r_coef(sph_rj%nidx_rj(1))
!!        type(band_matrices_type), intent(inout) :: band_p_poisson
!!        type(band_matrices_type), intent(inout) :: band_s_evo
!!
!!      subroutine sel_radial_mat00_scalar_bc_sph                       &
!!     &         (sph_rj, sph_bc, fdm2_center, r_coef, band_s00_poisson)
!!      subroutine sel_r_mat_poisson_fixBC_sph                          &
!!     &         (sph_rj, sph_bc, fdm2_center, band_s00_poisson)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        real(kind = kreal), intent(in) :: r_coef(0:sph_rj%nidx_rj(1))
!!        type(band_matrix_type), intent(inout) :: band_s00_poisson
!!@endverbatim
!
      module select_r_mat_scalar_bc_sph
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_sph_matrices
      use t_sph_matrix
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
      subroutine sel_radial_mat_press_bc_sph(sph_rj, sph_bc_U,          &
     &          fdm2_center, g_sph_rj, r_coef, band_p_poisson)
!
      use set_sph_scalar_mat_bc
      use cal_inner_core_rotation
      use center_sph_matrices
      use set_radial_mat_sph
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: r_coef(sph_rj%nidx_rj(1))
!
      type(band_matrices_type), intent(inout) :: band_p_poisson
!
!
!   Boundary condition for ICB
      if(sph_bc_U%iflag_icb .eq. iflag_sph_fill_center) then
        call add_scalar_poisson_mat_ctr1                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%r_ICB, fdm2_center%dmat_fix_fld,                   &
     &      r_coef(1), band_p_poisson%mat)
!      else if(sph_bc_U%iflag_icb .eq. iflag_free_sph) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_evolve_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_field) then
!      else if(sph_bc_U%iflag_icb .eq. iflag_fixed_velo) then
      else
        call add_icb_scalar_poisson_mat                                 &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc_U%kr_in, sph_bc_U%r_ICB, sph_bc_U%fdm2_fix_dr_ICB,   &
     &      r_coef(sph_bc_U%kr_in), band_p_poisson%mat)
      end if
!
!   Boundary condition for CMB
      call add_cmb_scalar_poisson_mat                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,               &
     &    sph_bc_U%kr_out, sph_bc_U%r_CMB, sph_bc_U%fdm2_fix_dr_CMB,    &
     &    r_coef(sph_bc_U%kr_out), band_p_poisson%mat)
!
      end subroutine sel_radial_mat_press_bc_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_radial_mat_scalar_bc_sph(sph_rj, sph_bc,           &
     &          fdm2_center, g_sph_rj, r_coef, band_s_evo)
!
      use center_sph_matrices
      use set_sph_scalar_mat_bc
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: r_coef(sph_rj%nidx_rj(1))
!
      type(band_matrices_type), intent(inout) :: band_s_evo
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center               &
     &   .or. sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call add_scalar_poisson_mat_ctr1                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc%r_ICB, fdm2_center%dmat_fix_fld, r_coef(1),          &
     &      band_s_evo%mat)
      else if (sph_bc%iflag_icb .eq. iflag_fixed_flux                   &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call add_fix_flux_icb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_dr_ICB,         &
     &      r_coef(sph_bc%kr_in), band_s_evo%mat)
!      else if (sph_bc%iflag_icb .eq. iflag_fixed_field                 &
!     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
      else
        call set_fix_fld_icb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_in, band_s_evo%mat)
      end if
!
      if (sph_bc%iflag_cmb .eq. iflag_fixed_flux                        &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call add_fix_flux_cmb_poisson_mat                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), g_sph_rj,             &
     &      sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_dr_CMB,        &
     &      r_coef(sph_bc%kr_out), band_s_evo%mat)
!      else if (sph_bc%iflag_cmb .eq. iflag_fixed_field                 &
!     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      else
        call set_fix_fld_cmb_poisson_mat                                &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_out, band_s_evo%mat)
      end if
!
      end subroutine sel_radial_mat_scalar_bc_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_radial_mat00_scalar_bc_sph                         &
     &         (sph_rj, sph_bc, fdm2_center, r_coef, band_s00_poisson)
!
      use center_sph_matrices
      use set_sph_scalar_mat_bc
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
      real(kind = kreal), intent(in) :: r_coef(0:sph_rj%nidx_rj(1))
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson
!
      logical :: flag_undefined = .TRUE.
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        call add_scalar_poisson_mat_fill_ctr                            &
     &     (sph_rj%nidx_rj(1), sph_bc%r_ICB,                            &
     &      fdm2_center%dmat_fix_dr, fdm2_center%dmat_fix_fld,          &
     &      one, band_s00_poisson%mat)
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call add_scalar_poisson_mat_fix_ctr                             &
     &     (sph_rj%nidx_rj(1), sph_bc%r_ICB, fdm2_center%dmat_fix_fld,  &
     &      one, band_s00_poisson%mat)
      else if (sph_bc%iflag_icb .eq. iflag_fixed_flux                   &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call add_fix_flux_icb_poisson00_mat                             &
     &     (sph_rj%nidx_rj(1), sph_bc%kr_in, sph_bc%fdm2_fix_dr_ICB,    &
     &      r_coef(sph_bc%kr_in), band_s00_poisson%mat)
!      else if (sph_bc%iflag_icb .eq. iflag_fixed_field                 &
!     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
      else
        call set_fix_fld_icb_poisson00_mat                              &
     &     (sph_rj%nidx_rj(1), sph_bc%kr_in, band_s00_poisson%mat)
      end if
!
!
      flag_undefined = .TRUE.
      if(sph_bc%iflag_cmb .eq. iflag_fixed_flux                         &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        if(      sph_bc%iflag_icb .eq. iflag_sph_fix_center             &
     &      .or. sph_bc%iflag_icb .eq. iflag_fixed_field                &
     &      .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
          call add_fix_flux_cmb_poisson00_mat                           &
     &       (sph_rj%nidx_rj(1), sph_bc%kr_out, sph_bc%fdm2_fix_dr_CMB, &
     &        r_coef(sph_bc%kr_out), band_s00_poisson%mat)
          flag_undefined = .FALSE.
        end if
      end if
!
!      else if (sph_bc%iflag_cmb .eq. iflag_fixed_field                 &
!     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_field) then
      if(flag_undefined) then
        call set_fix_fld_cmb_poisson00_mat                              &
     &     (sph_rj%nidx_rj(1), sph_bc%kr_out, band_s00_poisson%mat)
      end if
!
      end subroutine sel_radial_mat00_scalar_bc_sph
!
! -----------------------------------------------------------------------
!
      subroutine sel_r_mat_poisson_fixBC_sph                            &
     &         (sph_rj, sph_bc, fdm2_center, band_s00_poisson)
!
      use center_sph_matrices
      use set_sph_scalar_mat_bc
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center               &
     &   .or. sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call add_scalar_poisson_mat_fix_ctr                             &
     &     (sph_rj%nidx_rj(1), sph_bc%r_ICB, fdm2_center%dmat_fix_fld,  &
     &      one,  band_s00_poisson%mat)
      else
        call set_fix_fld_icb_poisson00_mat                              &
     &     (sph_rj%nidx_rj(1), sph_bc%kr_in, band_s00_poisson%mat)
      end if
!
      call set_fix_fld_cmb_poisson00_mat                                &
     &   (sph_rj%nidx_rj(1), sph_bc%kr_out, band_s00_poisson%mat)
!
      end subroutine sel_r_mat_poisson_fixBC_sph
!
! -----------------------------------------------------------------------
!
      end module select_r_mat_scalar_bc_sph
