!>@file   sel_sph_r_mat_ref_scalar_bc.f90
!!@brief  module sel_sph_r_mat_ref_scalar_bc
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine sel_sph_r_mat_ref_scl_bc(flag_val_diffuse, sph_rj,   &
!!     &          sph_bc, fdm2_center, r_coef, k_ratio, dk_dr,          &
!!     &          band_s00_poisson)
!!      subroutine sel_sph_r_mat_poisson_fixBC(flag_val_diffuse, sph_rj,&
!!     &          sph_bc, fdm2_center, k_ratio, dk_dr, band_s00_poisson)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        real(kind = kreal), intent(in) :: r_coef
!!        real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
!!        real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
!!        logical, intent(in) :: flag_val_diffuse
!!        type(band_matrix_type), intent(inout) :: band_s00_poisson
!!@endverbatim
!
      module sel_sph_r_mat_ref_scalar_bc
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
      use t_boundary_sph_spectr
      use t_coef_fdm2_centre
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_sph_r_mat_ref_scl_bc(flag_val_diffuse, sph_rj,     &
     &          sph_bc, fdm2_center, r_coef, k_ratio, dk_dr,            &
     &          band_s00_poisson)
!
      use sph_zero_degree_matrices
      use set_sph_scalar_matrix_ICB
      use set_sph_scalar_matrix_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
      real(kind = kreal), intent(in) :: r_coef
      real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
      logical, intent(in) :: flag_val_diffuse
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson
!
      logical :: flag_undefined = .TRUE.
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center) then
        if(flag_val_diffuse) then
          call add_scl_val_dfse_mat_fill_ctr                            &
     &       (sph_rj%nidx_rj(1), sph_bc%r_ICB,                          &
     &        fdm2_center%dmat_fix_dr, fdm2_center%dmat_fix_fld,        &
     &        one, k_ratio(1), dk_dr(1), band_s00_poisson%mat)
        else
          call add_scalar_poisson_mat_fill_ctr                          &
     &       (sph_rj%nidx_rj(1), sph_bc%r_ICB,                          &
     &        fdm2_center%dmat_fix_dr, fdm2_center%dmat_fix_fld,        &
     &        one, band_s00_poisson%mat)
        end if
      else if(sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        if(flag_val_diffuse) then
          call add_scl_val_diffuse_mat_fix_ctr                          &
     &       (sph_rj%nidx_rj(1), sph_bc%r_ICB,                          &
     &        fdm2_center%dmat_fix_fld, one, k_ratio(1), dk_dr(1),      &
     &        band_s00_poisson%mat)
        else
          call add_scalar_poisson_mat_fix_ctr                           &
     &       (sph_rj%nidx_rj(1), sph_bc%r_ICB,                          &
     &        fdm2_center%dmat_fix_fld, one, band_s00_poisson%mat)
        end if
      else if (sph_bc%iflag_icb .eq. iflag_fixed_flux                   &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call add_fix_flux_icb_poisson00_mat                             &
     &     (sph_rj%nidx_rj(1), sph_bc%kr_in, sph_bc%fdm2_fix_dr_ICB,    &
     &      r_coef, band_s00_poisson%mat)
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
     &        r_coef, band_s00_poisson%mat)
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
      end subroutine sel_sph_r_mat_ref_scl_bc
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_sph_r_mat_poisson_fixBC(flag_val_diffuse, sph_rj,  &
     &          sph_bc, fdm2_center, k_ratio, dk_dr, band_s00_poisson)
!
      use sph_zero_degree_matrices
      use set_sph_scalar_matrix_ICB
      use set_sph_scalar_matrix_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
      real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
      logical, intent(in) :: flag_val_diffuse
!
      type(band_matrix_type), intent(inout) :: band_s00_poisson
!
!
      if     (sph_bc%iflag_icb .eq. iflag_sph_fill_center               &
     &   .or. sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        if(flag_val_diffuse) then
          call add_scl_val_diffuse_mat_fix_ctr                          &
     &       (sph_rj%nidx_rj(1), sph_bc%r_ICB,                          &
     &        fdm2_center%dmat_fix_fld, one, k_ratio(1), dk_dr(1),      &
     &        band_s00_poisson%mat)
        else
          call add_scalar_poisson_mat_fix_ctr                           &
     &       (sph_rj%nidx_rj(1), sph_bc%r_ICB,                          &
     &        fdm2_center%dmat_fix_fld, one, band_s00_poisson%mat)
        end if
      else
        call set_fix_fld_icb_poisson00_mat                              &
     &     (sph_rj%nidx_rj(1), sph_bc%kr_in, band_s00_poisson%mat)
      end if
!
      call set_fix_fld_cmb_poisson00_mat                                &
     &   (sph_rj%nidx_rj(1), sph_bc%kr_out, band_s00_poisson%mat)
!
      end subroutine sel_sph_r_mat_poisson_fixBC
!
! -----------------------------------------------------------------------
!
      end module sel_sph_r_mat_ref_scalar_bc
