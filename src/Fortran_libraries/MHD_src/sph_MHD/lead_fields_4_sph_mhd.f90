!>@file   lead_fields_4_sph_mhd.f90
!!@brief  module lead_fields_4_sph_mhd
!!
!!@author H. Matsui (UC Berkeley) and T. Kera (Tohoku University)
!!@date Programmed in Aug, 2007
!>        Modified by T. Kera in Aug., 2021
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine s_lead_fields_4_sph_mhd                              &
!!     &         (monitor, r_2nd, MHD_prop, sph_MHD_bc, trans_p,        &
!!     &          sph_MHD_mat, WK, SPH_MHD, SR_sig, SR_r)
!!        type(sph_mhd_monitor_data), intent(in) :: monitor
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!        type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine lead_fields_by_sph_trans(sph, comms_sph, MHD_prop,   &
!!     &          trans_p, trns_MHD, trns_snap, WK_leg, WK_FFTs, rj_fld,&
!!     &          SR_sig, SR_r)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine cal_sph_enegy_fluxes(ltr_crust, sph, comms_sph,      &
!!     &          r_2nd, MHD_prop, sph_MHD_bc, trans_p,                 &
!!     &          ipol, trns_MHD, trns_snap, trns_difv, trns_eflux,     &
!!     &          WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(address_4_sph_trans), intent(in) :: trns_snap
!!        type(address_4_sph_trans), intent(in) :: trns_difv
!!        type(address_4_sph_trans), intent(inout) :: trns_eflux
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine gradients_of_vectors_sph(sph, comms_sph, r_2nd,      &
!!     &          sph_MHD_bc, trans_p, ipol, trns_snap,                 &
!!     &          trns_difv, WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(address_4_sph_trans), intent(in) :: trns_snap
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_difv
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module lead_fields_4_sph_mhd
!
      use m_precision
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_SPH_mesh_field_data
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_sph_matrices
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_boundary_data_sph_MHD
      use t_radial_matrices_sph_MHD
      use t_sph_mhd_monitor_data_IO
      use t_solver_SR
!
      implicit none
!
      private :: pressure_4_sph_mhd
      private :: enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_lead_fields_4_sph_mhd                                &
     &         (monitor, r_2nd, MHD_prop, sph_MHD_bc, trans_p,          &
     &          sph_MHD_mat, WK, SPH_MHD, SR_sig, SR_r)
!
      use sph_transforms_4_MHD
      use cal_energy_flux_rtp
      use cal_self_buoyancies_sph
      use decomp_w_sym_rj_base_field
      use adjust_scalar_rj_fields
!
      type(sph_mhd_monitor_data), intent(in) :: monitor
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: ibuo_temp,  ibuo_comp
!
!
      call sel_field_address_for_buoyancies(SPH_MHD%ipol%base,          &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    ibuo_temp, ibuo_comp)
      call sel_buoyancies_sph_MHD                                       &
     &   (SPH_MHD%sph%sph_rj, trans_p%leg, SPH_MHD%ipol%forces,         &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,                        &
     &    ibuo_temp, ibuo_comp, SPH_MHD%fld)
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call pressure_4_sph_mhd                                         &
     &     (SPH_MHD%sph, MHD_prop, sph_MHD_bc, r_2nd, trans_p%leg,      &
     &      sph_MHD_mat%band_p_poisson, SPH_MHD%ipol, SPH_MHD%fld)
      end if
!
      call s_adjust_scalar_rj_fields(SPH_MHD%sph,                       &
     &    SPH_MHD%ipol%base, SPH_MHD%ipol%fld_cmp,                      &
     &    SPH_MHD%ipol%prod_fld, SPH_MHD%fld)
      call s_decomp_w_sym_rj_base_field(SPH_MHD%sph%sph_rj,             &
     &    SPH_MHD%ipol%base, SPH_MHD%ipol%sym_fld,                      &
     &    SPH_MHD%ipol%asym_fld, SPH_MHD%fld)
!
      call sel_field_address_for_buoyancies(SPH_MHD%ipol%sym_fld,       &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    ibuo_temp, ibuo_comp)
      call sel_buoyancies_sph_MHD(SPH_MHD%sph%sph_rj, trans_p%leg,      &
     &    SPH_MHD%ipol%forces_by_sym_asym, MHD_prop%fl_prop,            &
     &    sph_MHD_bc%sph_bc_U, ibuo_temp, ibuo_comp, SPH_MHD%fld)
!
      call sel_field_address_for_buoyancies(SPH_MHD%ipol%asym_fld,      &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    ibuo_temp, ibuo_comp)
      call sel_buoyancies_sph_MHD(SPH_MHD%sph%sph_rj, trans_p%leg,      &
     &    SPH_MHD%ipol%forces_by_sym_sym, MHD_prop%fl_prop,             &
     &    sph_MHD_bc%sph_bc_U, ibuo_temp, ibuo_comp, SPH_MHD%fld)
!
      call lead_fields_by_sph_trans(SPH_MHD%sph, SPH_MHD%comms,         &
     &    MHD_prop, trans_p, WK%trns_MHD, WK%trns_snap,                 &
     &    WK%WK_leg, WK%WK_FFTs, SPH_MHD%fld, SR_sig, SR_r)
      call gradients_of_vectors_sph                                     &
     &   (SPH_MHD%sph, SPH_MHD%comms, r_2nd, sph_MHD_bc, trans_p,       &
     &    SPH_MHD%ipol, WK%trns_snap, WK%trns_difv,                     &
     &    WK%WK_leg, WK%WK_FFTs, SPH_MHD%fld, SR_sig, SR_r)
!
      call enegy_fluxes_4_sph_mhd(monitor%ltr_crust, SPH_MHD%sph,       &
     &    SPH_MHD%comms, r_2nd, MHD_prop, sph_MHD_bc, trans_p,          &
     &    SPH_MHD%ipol, WK%trns_MHD, WK%trns_snap, WK%trns_difv,        &
     &    WK%trns_eflux, WK%WK_leg, WK%WK_FFTs, SPH_MHD%fld,            &
     &    SR_sig, SR_r)
!
      end subroutine s_lead_fields_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine lead_fields_by_sph_trans(sph, comms_sph, MHD_prop,     &
     &          trans_p, trns_MHD, trns_snap, WK_leg, WK_FFTs, rj_fld,  &
     &          SR_sig, SR_r)
!
      use sph_transforms_snapshot
      use cal_nonlinear_sph_MHD
      use get_components_from_field
      use cal_lorentz_by_dipole
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iflag_debug.gt.0) write(*,*) 'sph_back_trans_snapshot_MHD'
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p,         &
     &    rj_fld, trns_snap%backward, WK_leg, WK_FFTs, SR_sig, SR_r)
!
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center)  &
     &      then
        if (iflag_debug.gt.0) write(*,*) 'cal_nonlinear_pole_MHD'
        call cal_nonlinear_pole_MHD(sph%sph_rtp, MHD_prop,              &
     &      trns_snap%b_trns%base, trns_MHD%f_trns%forces,              &
     &      trns_snap%backward, trns_MHD%forward)
      end if
!
!
      call get_components_from_fld(sph%sph_rtp, trans_p%leg,            &
     &    trns_snap%b_trns%base, trns_snap%f_trns%fld_cmp,              &
     &    trns_snap%backward%ncomp, trns_snap%backward%fld_rtp,         &
     &    trns_snap%forward%ncomp, trns_snap%forward%fld_rtp)
!
!
      call nonlinear_terms_on_node_w_sym                                &
     &   (MHD_prop, trns_snap%b_trns%sym_fld, trns_snap%b_trns%sym_fld, &
     &    trns_snap%f_trns%forces_by_sym_sym, sph%sph_rtp%nnod_rtp,     &
     &    trns_snap%backward%ncomp, trns_snap%backward%fld_rtp,         &
     &    trns_snap%forward%ncomp,  trns_snap%forward%fld_rtp)
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center)  &
     &      then
      call nonlinear_terms_on_node_w_sym                                &
     &   (MHD_prop, trns_snap%b_trns%sym_fld, trns_snap%b_trns%sym_fld, &
     &    trns_snap%f_trns%forces_by_sym_sym, sph%sph_rtp%nnod_pole,    &
     &    trns_snap%backward%ncomp, trns_snap%backward%fld_pole,        &
     &    trns_snap%forward%ncomp,  trns_snap%forward%fld_pole)
      end if
!
      call nonlinear_terms_on_node_w_sym(MHD_prop,                      &
     &    trns_snap%b_trns%asym_fld, trns_snap%b_trns%asym_fld,         &
     &    trns_snap%f_trns%forces_by_asym_asym, sph%sph_rtp%nnod_rtp,   &
     &    trns_snap%backward%ncomp, trns_snap%backward%fld_rtp,         &
     &    trns_snap%forward%ncomp,  trns_snap%forward%fld_rtp)
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center)  &
     &      then
      call nonlinear_terms_on_node_w_sym(MHD_prop,                      &
     &    trns_snap%b_trns%asym_fld, trns_snap%b_trns%asym_fld,         &
     &    trns_snap%f_trns%forces_by_asym_asym, sph%sph_rtp%nnod_pole,  &
     &    trns_snap%backward%ncomp, trns_snap%backward%fld_pole,        &
     &    trns_snap%forward%ncomp,  trns_snap%forward%fld_pole)
      end if
!
      call nonlinear_terms_on_node_w_sym(MHD_prop,                      &
     &    trns_snap%b_trns%sym_fld, trns_snap%b_trns%asym_fld,          &
     &    trns_snap%f_trns%forces_by_sym_asym, sph%sph_rtp%nnod_rtp,    &
     &    trns_snap%backward%ncomp, trns_snap%backward%fld_rtp,         &
     &    trns_snap%forward%ncomp,  trns_snap%forward%fld_rtp)
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center)  &
     &      then
      call nonlinear_terms_on_node_w_sym(MHD_prop,                      &
     &    trns_snap%b_trns%sym_fld, trns_snap%b_trns%asym_fld,          &
     &    trns_snap%f_trns%forces_by_sym_asym, sph%sph_rtp%nnod_pole,   &
     &    trns_snap%backward%ncomp, trns_snap%backward%fld_pole,        &
     &    trns_snap%forward%ncomp,  trns_snap%forward%fld_pole)
      end if
!
      call nonlinear_terms_on_node_w_sym(MHD_prop,                      &
     &    trns_snap%b_trns%asym_fld, trns_snap%b_trns%sym_fld,          &
     &    trns_snap%f_trns%forces_by_asym_sym, sph%sph_rtp%nnod_rtp,    &
     &    trns_snap%backward%ncomp, trns_snap%backward%fld_rtp,         &
     &    trns_snap%forward%ncomp,  trns_snap%forward%fld_rtp)
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center)  &
     &      then
      call nonlinear_terms_on_node_w_sym(MHD_prop,                      &
     &    trns_snap%b_trns%asym_fld, trns_snap%b_trns%asym_fld,         &
     &    trns_snap%f_trns%forces_by_asym_sym, sph%sph_rtp%nnod_pole,   &
     &    trns_snap%backward%ncomp, trns_snap%backward%fld_pole,        &
     &    trns_snap%forward%ncomp,  trns_snap%forward%fld_pole)
      end if
!
      call s_cal_lorentz_by_dipole(sph%sph_rtp, MHD_prop%fl_prop,       &
     &    trns_snap%b_trns, trns_snap%f_trns, trns_snap%backward,       &
     &    trns_snap%forward)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &    'sph_forward_trans_snapshot_MHD for snapshot'
      call sph_forward_trans_snapshot_MHD                               &
     &   (sph, comms_sph, trans_p, trns_snap%forward,                   &
     &    WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!
      end subroutine lead_fields_by_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine pressure_4_sph_mhd(sph, MHD_prop, sph_MHD_bc,          &
     &          r_2nd, leg, band_p_poisson, ipol, rj_fld)
!
      use cal_sol_sph_fluid_crank
!
      use cal_sph_divergence_of_force
      use const_radial_forces_on_bc
      use cal_div_of_forces
      use sph_radial_grad_4_velocity
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_grids), intent(in) ::  sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(band_matrices_type), intent(in) :: band_p_poisson
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.gt.0) write(*,*) 'cal_div_of_forces_sph_2'
      call cal_div_of_forces_sph_2                                      &
     &   (sph%sph_rj, r_2nd, MHD_prop, sph_MHD_bc, leg%g_sph_rj,        &
     &    ipol%forces, ipol%div_forces, rj_fld)
!     &   ipol%base, ipol%grad_fld, ipol%forces, ipol%div_forces, rj_fld)
!
      call s_const_radial_forces_on_bc(sph%sph_rj, leg%g_sph_rj,        &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,                        &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    ipol%base, ipol%diffusion, ipol%forces, ipol%div_forces,      &
     &    rj_fld)
!
      call sum_div_of_forces                                            &
     &    (MHD_prop%fl_prop, ipol%base, ipol%div_forces, rj_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sol_pressure_by_div_v'
      call cal_sol_pressure_by_div_v(sph%sph_rj, sph_MHD_bc%sph_bc_U,   &
     &    band_p_poisson, ipol, rj_fld)
!
      if(ipol%forces%i_press_grad .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'const_pressure_gradient'
        call const_pressure_gradient                                    &
     &     (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_U,                     &
     &      leg%g_sph_rj, MHD_prop%fl_prop%coef_press,                  &
     &      ipol%base%i_press, ipol%forces%i_press_grad, rj_fld)
      end if
!
      end subroutine pressure_4_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine enegy_fluxes_4_sph_mhd(ltr_crust, sph, comms_sph,      &
     &          r_2nd, MHD_prop, sph_MHD_bc, trans_p,                   &
     &          ipol, trns_MHD, trns_snap, trns_difv, trns_eflux,       &
     &          WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!
      use sph_transforms_snapshot
      use cal_energy_flux_rtp
      use cal_energy_flux_rj
      use cal_geomagnetic_data
      use cal_helicities_rtp
!
      integer(kind = kint), intent(in) :: ltr_crust
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(address_4_sph_trans), intent(in) :: trns_difv
!
      type(address_4_sph_trans), intent(inout) :: trns_eflux
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_geomagnetic_rtp                                          &
     &   (sph%sph_rtp, sph%sph_rj, sph_MHD_bc%sph_bc_B,                 &
     &    trns_MHD%b_trns%base, trns_eflux%f_trns%prod_fld,             &
     &    trns_MHD%backward%ncomp, trns_MHD%backward%fld_rtp,           &
     &    trns_eflux%forward%ncomp, trns_eflux%forward%fld_rtp)
      call cal_sph_enegy_fluxes                                         &
     &   (ltr_crust, sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc,       &
     &    trans_p, ipol, trns_MHD, trns_snap, trns_difv, trns_eflux,    &
     &    WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &                          'sph_forward_trans_snapshot_MHD'
      call sph_forward_trans_snapshot_MHD                               &
     &   (sph, comms_sph, trans_p, trns_eflux%forward,                  &
     &    WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!
      end subroutine enegy_fluxes_4_sph_mhd
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_sph_enegy_fluxes(ltr_crust, sph, comms_sph,        &
     &          r_2nd, MHD_prop, sph_MHD_bc, trans_p,                   &
     &          ipol, trns_MHD, trns_snap, trns_difv, trns_eflux,       &
     &          WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!
      use sph_transforms_snapshot
      use cal_sph_field_by_rotation
      use cal_energy_flux_rj
      use cal_energy_flux_rtp
      use cal_ene_flux_by_sym_rtp
      use cal_helicities_rtp
!
      integer(kind = kint), intent(in) :: ltr_crust
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(address_4_sph_trans), intent(in) :: trns_difv
!
      type(address_4_sph_trans), intent(inout) :: trns_eflux
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
!      Evaluate magnetic induction with respect to equatorial symmetry
      call s_cal_mag_induct_by_sym_rj                                   &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc, trans_p%leg, ipol, rj_fld)
!
!      Evaluate fields for output in spectrum space
      if (iflag_debug.gt.0) write(*,*) 's_cal_energy_flux_rj'
      call s_cal_energy_flux_rj                                         &
     &   (ltr_crust, sph%sph_rj, r_2nd, sph_MHD_bc, ipol, rj_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'sph_back_trans_snapshot_MHD'
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p,         &
     &    rj_fld, trns_eflux%backward, WK_leg, WK_FFTs, SR_sig, SR_r)
!
!       Evaluate helicities
      if (iflag_debug.gt.0) write(*,*) 's_cal_helicities_rtp'
      call s_cal_helicities_rtp                                         &
     &   (sph%sph_rtp, MHD_prop%fl_prop, trans_p%leg,                   &
     &    trns_snap%b_trns, trns_eflux%b_trns, trns_eflux%f_trns,       &
     &    trns_snap%backward, trns_eflux%backward, trns_eflux%forward)
!       Evaluate energy fluxes
      if (iflag_debug.gt.0) write(*,*) 's_cal_energy_flux_rtp'
      call s_cal_energy_flux_rtp                                        &
     &   (sph%sph_rtp, MHD_prop%fl_prop, MHD_prop%cd_prop,              &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    trns_MHD%f_trns, trns_snap%b_trns, trns_eflux%b_trns,         &
     &    trns_difv%b_trns, trns_eflux%f_trns,                          &
     &    trns_MHD%forward, trns_snap%backward, trns_eflux%backward,    &
     &    trns_difv%backward, trns_eflux%forward)
!
      call s_cal_ene_flux_by_sym_rtp(sph%sph_rtp, MHD_prop%fl_prop,     &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    trns_snap%b_trns, trns_snap%f_trns,                           &
     &    trns_eflux%b_trns, trns_eflux%f_trns,                         &
     &    trns_snap%backward, trns_snap%forward,                        &
     &    trns_eflux%backward, trns_eflux%forward)
!
      end subroutine cal_sph_enegy_fluxes
!
! ----------------------------------------------------------------------
!
      subroutine gradients_of_vectors_sph(sph, comms_sph, r_2nd,        &
     &          sph_MHD_bc, trans_p, ipol, trns_snap,                   &
     &          trns_difv, WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!
      use sph_transforms_snapshot
      use copy_rtp_vectors_4_grad
      use cal_grad_of_sph_vectors
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(inout) :: trns_difv
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_vectors_rtp_4_grad'
      call copy_vectors_rtp_4_grad                                      &
     &   (sph, trns_snap%b_trns%base, trns_difv%f_trns%diff_vector,     &
     &    trns_snap%backward, trns_difv%forward)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &      'sph_forward_trans_snapshot_MHD for diff of vector'
      call sph_forward_trans_snapshot_MHD(sph, comms_sph, trans_p,      &
     &    trns_difv%forward, WK_leg, WK_FFTs, rj_fld, SR_sig, SR_r)
!
      if (iflag_debug.gt.0) write(*,*) 'overwrt_grad_of_vectors_sph'
      call overwrt_grad_of_vectors_sph(sph, r_2nd, sph_MHD_bc,          &
     &    trans_p%leg, ipol%diff_vector, rj_fld)
!
      if (iflag_debug.gt.0) write(*,*)                                  &
     &      'sph_back_trans_snapshot_MHD for diff of vector'
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p,         &
     &    rj_fld, trns_difv%backward, WK_leg, WK_FFTs, SR_sig, SR_r)
!
      end subroutine gradients_of_vectors_sph
!
! ----------------------------------------------------------------------
!
      end module lead_fields_4_sph_mhd
