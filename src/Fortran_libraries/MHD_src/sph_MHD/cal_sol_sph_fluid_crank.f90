!>@file   cal_sol_sph_fluid_crank.f90
!!@brief  module cal_sol_sph_fluid_crank
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update each field for MHD dynamo model
!!
!!@verbatim
!!      subroutine cal_sol_velo_by_vort_sph_crank(sph_rj, sph_bc_U,     &
!!     &          bc_Uspectr, fdm2_free_ICB, fdm2_free_CMB,             &
!!     &          band_vp_evo, band_vt_evo, ipol, itor, rj_fld)
!!        Input address:    ipol%i_vort, itor%i_vort
!!        Solution address: ipol%i_velo, itor%i_velo
!!
!!      subroutine cal_sol_pressure_by_div_v                            &
!!     &         (sph_rj, sph_bc_U, band_p_poisson, ipol, rj_fld)
!!        Solution address: ipol%i_press
!!
!!
!!      subroutine cal_sol_magne_sph_crank                              &
!!     &         (sph_rj, sph_bc_B, band_bp_evo, band_bt_evo, g_sph_rj, &
!!     &          ipol, itor, rj_fld)
!!        Input address:    ipol%i_magne, itor%i_magne
!!        Solution address: ipol%i_magne, itor%i_magne
!!
!!      subroutine cal_sol_scalar_sph_crank                             &
!!     &        (dt, sph_rj, property, sph_bc, band_s_evo, band_s00_evo,&
!!     &         is_scalar, rj_fld, x00_w_center)
!!         type(sph_rj_grid), intent(in) :: sph_rj
!!         type(scalar_property), intent(in) :: cp_prop
!!         type(band_matrices_type), intent(in) :: band_comp_evo
!!         type(phys_address), intent(in) :: ipol
!!         type(phys_data), intent(inout) :: rj_fld
!!        Input address:    ipol%i_light
!!        Solution address: ipol%i_light
!!@endverbatim
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module cal_sol_sph_fluid_crank
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_sph_matrices
      use t_boundary_data_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      use set_reference_sph_mhd
      use lubksb_357band_mul
!
      implicit none
!
      private :: set_bc_velo_sph_crank
      private :: set_bc_magne_sph_crank, set_bc_scalar_sph_crank
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_by_vort_sph_crank(sph_rj, sph_bc_U,       &
     &          bc_Uspectr, fdm2_free_ICB, fdm2_free_CMB,               &
     &          band_vp_evo, band_vt_evo, ipol, itor, rj_fld)
!
      use copy_field_smp
      use solve_sph_fluid_crank
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_velocity_BC_spectr), intent(in) :: bc_Uspectr
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB, fdm2_free_CMB
      type(band_matrices_type), intent(in) :: band_vp_evo, band_vt_evo
      type(phys_address), intent(in) :: ipol, itor
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      call copy_nod_scalar_smp(rj_fld%n_point,                          &
     &    rj_fld%d_fld(1,itor%i_vort), rj_fld%d_fld(1,ipol%i_velo))
      call copy_nod_scalar_smp(rj_fld%n_point,                          &
     &    rj_fld%d_fld(1,ipol%i_vort), rj_fld%d_fld(1,itor%i_velo))
!$omp end parallel
!
      call set_bc_velo_sph_crank(ipol%i_velo, sph_rj,                   &
     &    sph_bc_U, bc_Uspectr, fdm2_free_ICB, fdm2_free_CMB, rj_fld)
!
      call solve_velo_by_vort_sph_crank                                 &
     &   (sph_rj, band_vp_evo, band_vt_evo, ipol%i_velo, itor%i_velo,   &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine cal_sol_velo_by_vort_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_pressure_by_div_v                              &
     &         (sph_rj, sph_bc_U, band_p_poisson, ipol, rj_fld)
!
      use set_reference_sph_mhd
      use solve_sph_fluid_crank
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(band_matrices_type), intent(in) :: band_p_poisson
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call solve_pressure_by_div_v                                      &
     &   (sph_rj, band_p_poisson, ipol%i_press,                         &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call adjust_by_ave_pressure_on_CMB                                &
     &   (sph_bc_U%kr_in, sph_bc_U%kr_out, sph_rj%idx_rj_degree_zero,   &
     &    sph_rj%nidx_rj, ipol%i_press,                                 &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine cal_sol_pressure_by_div_v
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_sph_crank                                &
     &         (sph_rj, sph_bc_B, band_bp_evo, band_bt_evo, g_sph_rj,   &
     &          ipol, itor, rj_fld)
!
      use solve_sph_fluid_crank
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(band_matrices_type), intent(in) :: band_bp_evo, band_bt_evo
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call set_bc_magne_sph_crank                                       &
     &   (sph_rj, g_sph_rj, sph_bc_B, ipol%i_magne, rj_fld)
!
      call solve_magne_sph_crank                                        &
     &   (sph_rj, band_bp_evo, band_bt_evo, ipol%i_magne, itor%i_magne, &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine cal_sol_magne_sph_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sol_scalar_sph_crank                               &
     &        (dt, sph_rj, property, sph_bc, band_s_evo, band_s00_evo,  &
     &         is_scalar, rj_fld, x00_w_center)
!
      use t_physical_property
      use t_sph_center_matrix
      use t_boundary_params_sph_MHD
      use solve_sph_fluid_crank
      use fill_scalar_field
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(band_matrices_type), intent(in) :: band_s_evo
      type(band_matrix_type), intent(in) :: band_s00_evo
      type(scalar_property), intent(in) :: property
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: is_scalar
!
      type(phys_data), intent(inout) :: rj_fld
      real(kind = kreal), intent(inout)                                 &
     &                   :: x00_w_center(0:sph_rj%nidx_rj(1))
!
!
      call set_bc_scalar_sph_crank(sph_rj, sph_bc,                      &
     &    property%coef_advect, property%coef_diffuse,                  &
     &    dt, property%coef_imp, is_scalar, rj_fld)
!
      call solve_scalar_sph_crank(sph_rj, band_s_evo, band_s00_evo,     &
     &    is_scalar, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,    &
     &    x00_w_center)
!
      call fill_scalar_at_external(sph_bc%kr_in, sph_bc%kr_out,         &
     &    sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,             &
     &    sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    is_scalar, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine cal_sol_scalar_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_velo_sph_crank                                  &
     &         (is_velo, sph_rj, sph_bc_U, bc_Uspectr,                  &
     &          fdm2_free_ICB, fdm2_free_CMB, rj_fld)
!
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
!
      integer(kind = kint), intent(in) :: is_velo
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_velocity_BC_spectr), intent(in) :: bc_Uspectr
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB, fdm2_free_CMB
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call delete_zero_degree_vect                                      &
     &   (is_velo, sph_rj%idx_rj_degree_zero, rj_fld%n_point,           &
     &    sph_rj%nidx_rj, rj_fld%ntot_phys, rj_fld%d_fld)
!
      if     (sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_vpol2                                 &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_in, fdm2_free_ICB%dmat_vp,   &
     &      is_velo, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2                               &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%idx_rj_degree_one,        &
     &      sph_rj%nidx_rj, sph_bc_U%kr_in, sph_bc_U%r_ICB,             &
     &      sph_rj%radius_1d_rj_r, bc_Uspectr%vt_ICB_bc, is_velo,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_sph_nod_icb_rigid_velo2                                &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_in,                          &
     &      sph_bc_U%r_ICB, bc_Uspectr%vt_ICB_bc, is_velo,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_vpol2                                 &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_out, fdm2_free_CMB%dmat_vp,  &
     &      is_velo, rj_fld%n_point,rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_sph_nod_cmb_rigid_velo2                                &
     &     (sph_rj%nidx_rj(2), sph_bc_U%kr_out,                         &
     &      sph_bc_U%r_CMB, bc_Uspectr%vt_CMB_bc, is_velo,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine set_bc_velo_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_magne_sph_crank                                 &
     &         (sph_rj, g_sph_rj, sph_bc_B, is_magne, rj_fld)
!
      use const_sph_radial_grad
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_cmb_qvac
      use cal_sph_exp_nod_icb_qvac
!
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call delete_zero_degree_vect(is_magne,                            &
     &    sph_rj%idx_rj_degree_zero, rj_fld%n_point, sph_rj%nidx_rj,    &
     &    rj_fld%ntot_phys, rj_fld%d_fld)
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call cal_sph_nod_icb_ins_mag2(sph_rj%nidx_rj(2), g_sph_rj,      &
     &      sph_bc_B%kr_in, sph_bc_B%r_ICB, is_magne,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_mag2                                   &
     &     (sph_rj%nidx_rj(2), sph_bc_B%kr_in, is_magne,                &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_mag2                                   &
     &     (sph_rj%nidx_rj(2), sph_bc_B%kr_out, is_magne,               &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call cal_sph_nod_cmb_ins_mag2(sph_rj%nidx_rj(2), g_sph_rj,      &
     &      sph_bc_B%kr_out, sph_bc_B%r_CMB, is_magne,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine set_bc_magne_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_scalar_sph_crank(sph_rj, sph_bc,                &
     &          coef_f, coef_d, dt, coef_imp, is_field, rj_fld)
!
      use set_scalar_boundary_sph
      use cal_sph_exp_center
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      real(kind = kreal), intent(in) :: coef_imp, coef_f, coef_d
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: is_field
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!   Set RHS vector for CMB
      if (sph_bc%iflag_cmb .eq. iflag_fixed_field) then
        call set_fixed_scalar_sph(sph_rj%nidx_rj(2),                    &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      sph_bc%kr_out, sph_rj%nidx_rj(1), is_field, sph_bc%CMB_fld, &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(coef_f .ne. 0.0d0) then
        call adjust_out_fixed_flux_sph                                  &
     &     (sph_rj%nidx_rj(2), sph_bc%kr_out, sph_bc%r_CMB,             &
     &      sph_bc%fdm2_fix_dr_CMB, sph_bc%CMB_flux, coef_d,            &
     &      coef_imp, dt, is_field, rj_fld%n_point, rj_fld%ntot_phys,   &
     &      rj_fld%d_fld)
      else
        call poisson_out_fixed_flux_sph                                 &
     &     (sph_rj%nidx_rj(2), sph_bc%kr_out, sph_bc%r_CMB,             &
     &      sph_bc%fdm2_fix_dr_CMB, sph_bc%CMB_flux, is_field,          &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
!   Set RHS vector for ICB
      if (sph_bc%iflag_icb .eq. iflag_fixed_field) then
        call set_fixed_scalar_sph(sph_rj%nidx_rj(2),                    &
     &      sph_rj%inod_rj_center, sph_rj%idx_rj_degree_zero,           &
     &      ione, sph_bc%kr_in, is_field, sph_bc%ICB_fld,               &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if (sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        call cal_sph_fixed_center                                       &
     &     (sph_rj%inod_rj_center, sph_bc%CTR_fld, is_field,            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(sph_bc%iflag_icb .eq. iflag_fixed_flux                    &
     &     .and. coef_f .ne. 0.0d0) then
        call adjust_in_fixed_flux_sph                                   &
     &     (sph_rj%nidx_rj(2), sph_bc%kr_in, sph_bc%r_ICB,              &
     &      sph_bc%fdm2_fix_dr_ICB, sph_bc%ICB_flux, coef_d,            &
     &      coef_imp, dt, is_field, rj_fld%n_point, rj_fld%ntot_phys,   &
     &      rj_fld%d_fld)
      else if (sph_bc%iflag_icb .eq. iflag_fixed_flux) then
        call poisson_in_fixed_flux_sph                                  &
     &     (sph_rj%nidx_rj(2), sph_bc%kr_in, sph_bc%r_ICB,              &
     &      sph_bc%fdm2_fix_dr_ICB, sph_bc%ICB_flux,                    &
     &      is_field, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine set_bc_scalar_sph_crank
!
! -----------------------------------------------------------------------
!
      end module cal_sol_sph_fluid_crank
