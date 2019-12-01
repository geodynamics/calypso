!>@file   cal_sol_sph_fluid_crank.f90
!!@brief  module cal_sol_sph_fluid_crank
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update each field for MHD dynamo model
!!
!!@verbatim
!!      subroutine cal_sol_velo_by_vort_sph_crank(sph_rj, r_2nd,        &
!!     &          sph_bc_U, bcs_U, fdm2_free_ICB, fdm2_free_CMB,        &
!!     &          band_vp_evo, band_vt_evo, ipol, itor, rj_fld)
!!        Input address:    ipol%i_vort, itor%i_vort
!!        Solution address: ipol%i_velo, itor%i_velo
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_U
!!
!!      subroutine cal_sol_pressure_by_div_v                            &
!!     &         (sph_rj, sph_bc_U, band_p_poisson, ipol, rj_fld)
!!        Solution address: ipol%i_press
!!
!!
!!      subroutine cal_sol_magne_sph_crank(sph_rj, r_2nd,               &
!!     &          sph_bc_B, bcs_B, band_bp_evo, band_bt_evo,            &
!!     &          g_sph_rj, ipol, itor, rj_fld)
!!        Input address:    ipol%i_magne, itor%i_magne
!!        Solution address: ipol%i_magne, itor%i_magne
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        type(sph_vector_boundary_data), intent(in) :: bcs_B
!!
!!      subroutine cal_sol_scalar_sph_crank(dt, sph_rj, property,       &
!!     &         sph_bc, bcs_S, band_s_evo, band_s00_evo,               &
!!     &         is_scalar, rj_fld, x00_w_center)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_S
!!        type(scalar_property), intent(in) :: cp_prop
!!        type(band_matrices_type), intent(in) :: band_comp_evo
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
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
      use t_fdm_coefs
      use t_sph_matrices
      use t_boundary_sph_spectr
      use t_boundary_data_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      use set_reference_sph_mhd
      use lubksb_357band_mul
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_by_vort_sph_crank(sph_rj, r_2nd,          &
     &          sph_bc_U, bcs_U, fdm2_free_ICB, fdm2_free_CMB,          &
     &          band_vp_evo, band_vt_evo, ipol, itor, rj_fld)
!
      use copy_field_smp
      use solve_sph_fluid_crank
      use set_reference_sph_mhd
      use set_evoluved_boundaries
      use select_exp_velocity_ICB
      use select_exp_velocity_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_vector_boundary_data), intent(in) :: bcs_U
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
      call delete_zero_degree_vect                                      &
     &   (ipol%i_velo, sph_rj%idx_rj_degree_zero, rj_fld%n_point,       &
     &    sph_rj%nidx_rj, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call sel_ICB_grad_poloidal_moment                                 &
     &   (sph_rj, r_2nd, sph_bc_U, bcs_U%ICB_Vspec, fdm2_free_ICB,      &
     &    ipol%i_velo, rj_fld)
      call sel_CMB_grad_poloidal_moment                                 &
     &   (sph_rj, sph_bc_U, bcs_U%CMB_Vspec, fdm2_free_CMB,             &
     &    ipol%i_velo, rj_fld)
!
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
      subroutine cal_sol_magne_sph_crank(sph_rj, r_2nd,                 &
     &          sph_bc_B, bcs_B, band_bp_evo, band_bt_evo,              &
     &          g_sph_rj, ipol, itor, rj_fld)
!
      use solve_sph_fluid_crank
      use set_reference_sph_mhd
      use set_evoluved_boundaries
      use select_exp_magne_ICB
      use select_exp_magne_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_vector_boundary_data), intent(in) :: bcs_B
      type(band_matrices_type), intent(in) :: band_bp_evo, band_bt_evo
      type(phys_address), intent(in) :: ipol, itor
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call delete_zero_degree_vect(ipol%i_magne,                        &
     &    sph_rj%idx_rj_degree_zero, rj_fld%n_point, sph_rj%nidx_rj,    &
     &    rj_fld%ntot_phys, rj_fld%d_fld)
!
      call sel_ICB_grad_poloidal_magne(sph_rj, r_2nd,                   &
     &    sph_bc_B, bcs_B%ICB_Vspec, g_sph_rj, ipol%i_magne, rj_fld)
      call sel_CMB_grad_poloidal_magne(sph_rj,                          &
     &    sph_bc_B, bcs_B%CMB_Vspec, g_sph_rj, ipol%i_magne, rj_fld)
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
      subroutine cal_sol_scalar_sph_crank(dt, sph_rj, property,         &
     &         sph_bc, bcs_S, band_s_evo, band_s00_evo,                 &
     &         is_scalar, rj_fld, x00_w_center)
!
      use t_physical_property
      use t_sph_center_matrix
      use t_boundary_params_sph_MHD
      use solve_sph_fluid_crank
      use fill_scalar_field
      use set_evoluved_boundaries
      use select_exp_scalar_ICB
      use select_exp_scalar_CMB
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_boundary_data), intent(in) :: bcs_S
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
      call set_CMB_scalar_sph_crank(sph_rj, sph_bc, bcs_S%CMB_Sspec,    &
     &    property%coef_advect, property%coef_diffuse, dt,              &
     &    property%coef_imp, is_scalar, rj_fld)
      call set_ICB_scalar_sph_crank(sph_rj, sph_bc, bcs_S%ICB_Sspec,    &
     &    property%coef_advect, property%coef_diffuse, dt,              &
     &    property%coef_imp, is_scalar, rj_fld)
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
      end module cal_sol_sph_fluid_crank
