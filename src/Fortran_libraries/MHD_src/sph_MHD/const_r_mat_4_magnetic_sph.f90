!>@file   const_r_mat_4_magnetic_sph.f90
!!@brief  module const_r_mat_4_magnetic_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of vector fields
!!
!!@verbatim
!!      subroutine const_radial_mat_4_magne_sph                         &
!!     &         (dt, sph_rj, r_2nd, cd_prop, sph_bc_B, bcs_B,          &
!!     &          fdm2_center, g_sph_rj, band_bp_evo, band_bt_evo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(sph_vector_boundary_data), intent(in) :: bcs_B
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(band_matrices_type), intent(inout) :: band_bp_evo
!!        type(band_matrices_type), intent(inout) :: band_bt_evo
!!@endverbatim
!
      module const_r_mat_4_magnetic_sph
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_ludcmp_3band
!
      use t_physical_property
      use t_spheric_rj_data
      use t_sph_matrices
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_fdm2_centre
!
      use set_radial_mat_sph
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &           :: bp_evo_name = 'poloidal_magne_evolution'
      character(len=kchara), parameter, private                         &
     &           :: bt_evo_name = 'toroidal_magne_evolution'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_4_magne_sph                           &
     &         (dt, sph_rj, r_2nd, cd_prop, sph_bc_B, bcs_B,            &
     &          fdm2_center, g_sph_rj, band_bp_evo, band_bt_evo)
!
      use select_sph_r_mat_magne_BC
      use set_sph_scalar_matrix_CMB
      use center_sph_matrices
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(conductive_property), intent(in) :: cd_prop
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_vector_boundary_data), intent(in) :: bcs_B
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: dt
!
      type(band_matrices_type), intent(inout) :: band_bp_evo
      type(band_matrices_type), intent(inout) :: band_bt_evo
!
      real(kind = kreal) :: coef_dbt
!
!
      band_bp_evo%mat_name = bp_evo_name
      band_bt_evo%mat_name = bt_evo_name
!
      call alloc_band_mat_sph(ithree, sph_rj, band_bp_evo)
      call alloc_band_mat_sph(ithree, sph_rj, band_bt_evo)
!
      call set_unit_on_diag(band_bp_evo)
      call set_unit_on_diag(band_bt_evo)
!
      if(cd_prop%coef_diffuse .eq. zero) then
        coef_dbt = one
        call set_unit_mat_4_poisson                                     &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc_B%kr_in, sph_bc_B%kr_out, band_bp_evo%mat)
        call set_unit_mat_4_poisson                                     &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc_B%kr_in, sph_bc_B%kr_out, band_bt_evo%mat)
      else
        coef_dbt = cd_prop%coef_imp * cd_prop%coef_diffuse * dt
        call set_unit_mat_4_time_evo                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), band_bp_evo%mat)
        call set_unit_mat_4_time_evo                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), band_bt_evo%mat)
      end if
!
      call add_vector_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    g_sph_rj, sph_bc_B%kr_in, sph_bc_B%kr_out,                    &
     &    coef_dbt, r_2nd%fdm(2)%dmat, band_bp_evo%mat)
      call add_vector_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    g_sph_rj, sph_bc_B%kr_in, sph_bc_B%kr_out,                    &
     &    coef_dbt, r_2nd%fdm(2)%dmat, band_bt_evo%mat)
!
!  Matrices at ICB or center
      call sel_sph_r_mat_pol_magnetic_ICB(sph_rj, sph_bc_B, bcs_B,      &
     &    fdm2_center, g_sph_rj, coef_dbt, band_bp_evo)
      call sel_sph_r_mat_tor_magnetic_ICB(sph_rj, sph_bc_B, bcs_B,      &
     &    fdm2_center, g_sph_rj, coef_dbt, band_bt_evo)
!
!  Matrices at CMB
      call sel_sph_r_mat_pol_magnetic_CMB(sph_rj, sph_bc_B, g_sph_rj,   &
     &                                    coef_dbt, band_bp_evo)
      call set_fix_fld_cmb_poisson_mat                                  &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    sph_bc_B%kr_out, band_bt_evo%mat)
!
!
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_bp_evo)
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_bt_evo)
!
      end subroutine const_radial_mat_4_magne_sph
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_4_magnetic_sph
