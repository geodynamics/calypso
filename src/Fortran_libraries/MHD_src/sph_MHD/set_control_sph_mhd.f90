!>@file   set_control_sph_mhd.f90
!!@brief  module set_control_sph_mhd
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_control_SPH_MHD_w_viz                            &
!!     &         (Dmodel_ctl, psph_ctl, smonitor_ctl, zm_ctls,          &
!!     &          MHD_prop, sph, rj_fld, nod_fld, monitor)
!!      subroutine set_control_SPH_MHD_noviz(Dmodel_ctl, smonitor_ctl,  &
!!     &          MHD_prop, rj_fld, monitor)
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!
!!      subroutine set_control_4_SPH_MHD(plt, org_plt,                  &
!!     &          Dmodel_ctl, smctl_ctl, nmtr_ctl, psph_ctl,            &
!!     &          MHD_files, bc_IO, MHD_step, MHD_prop, MHD_BC,         &
!!     &          trans_p, WK, sph_maker)
!!        type(platform_data_control), intent(in) :: plt
!!        type(platform_data_control), intent(in) :: org_plt
!!        type(mhd_DNS_model_control), intent(in) :: Dmodel_ctl
!!        type(sph_mhd_control_control), intent(in) :: smctl_ctl
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(node_monitor_control), intent(in) :: nmtr_ctl
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!      subroutine set_control_SPH_MHD_bcs                              &
!!     &         (MHD_prop, nbc_ctl, sbc_ctl, MHD_BC)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(node_bc_control), intent(in) :: nbc_ctl
!!        type(surf_bc_control), intent(in) :: sbc_ctl
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!      subroutine set_control_SPH_MHD_monitors                         &
!!     &         (smonitor_ctl, rj_fld, monitor)
!!      subroutine set_crustal_filtering_control                        &
!!     &         (crust_truncation_c, monitor)
!!        type(phys_data), intent(in) :: crust_truncation_c
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!@endverbatim
!
      module set_control_sph_mhd
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_field_data_IO
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_control_data_dynamo_vizs
      use t_bc_data_list
      use t_flex_delta_t_data
      use t_SPH_mesh_field_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_SPH_MHD_w_viz                              &
     &         (Dmodel_ctl, psph_ctl, smonitor_ctl, zm_ctls,            &
     &          MHD_prop, sph, rj_fld, nod_fld, monitor)
!
      use t_phys_data
      use t_sph_mhd_monitor_data_IO
!
      use set_control_sph_data_MHD
      use set_control_field_data
      use set_controls_4_sph_shell
      use set_nodal_field_name
      use node_monitor_IO
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(mhd_DNS_model_control), intent(inout) :: Dmodel_ctl
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(sph_dynamo_viz_controls), intent(in) :: zm_ctls
      type(sph_grids), intent(inout) :: sph
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      integer(kind = kint) :: ierr
!
!
!       set nodal field list
      if (iflag_debug.gt.0) write(*,*) 's_set_control_field_data'
      call s_set_control_field_data                                     &
     &   (Dmodel_ctl%fld_ctl%field_ctl, nod_fld, ierr)
!
!       set spectr field list
      if (iflag_debug.gt.0) write(*,*) 'set_control_sph_mhd_fields'
      call set_control_sph_mhd_fields                                   &
     &   (MHD_prop, Dmodel_ctl%fld_ctl%field_ctl, rj_fld)
!
!   set_pickup modes
      call set_control_SPH_MHD_monitors(smonitor_ctl, rj_fld, monitor)
!
      call set_crustal_filtering_control                                &
     &   (zm_ctls%crust_filter_ctl, monitor)
!
      call set_FEM_mesh_mode_4_SPH(psph_ctl%spctl, sph%sph_params)
      call count_field_4_monitor                                        &
     &   (rj_fld, num_field_monitor, ntot_comp_monitor)
!
      end subroutine set_control_SPH_MHD_w_viz
!
! ----------------------------------------------------------------------
!
      subroutine set_control_SPH_MHD_noviz(Dmodel_ctl, smonitor_ctl,    &
     &          MHD_prop, rj_fld, monitor)
!
      use t_phys_data
      use t_sph_mhd_monitor_data_IO
!
      use set_control_sph_data_MHD
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(mhd_DNS_model_control), intent(inout) :: Dmodel_ctl
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
!       set spectr field list
      if (iflag_debug.gt.0) write(*,*) 'set_control_sph_mhd_fields'
      call set_control_sph_mhd_fields                                   &
     &   (MHD_prop, Dmodel_ctl%fld_ctl%field_ctl, rj_fld)
!
!   set_pickup modes
      call set_control_SPH_MHD_monitors(smonitor_ctl, rj_fld, monitor)
!
      end subroutine set_control_SPH_MHD_noviz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_MHD(plt, org_plt,                    &
     &          Dmodel_ctl, smctl_ctl, nmtr_ctl, psph_ctl,              &
     &          MHD_files, bc_IO, MHD_step, MHD_prop, MHD_BC,           &
     &          trans_p, WK, SPH_MHD)
!
      use t_spheric_parameter
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_SPH_mesh_field_data
      use t_sph_trans_arrays_MHD
      use t_const_spherical_grid
      use t_sph_boundary_input_data
      use t_ctl_params_gen_sph_shell
      use t_sph_trans_arrays_MHD
      use t_coef_parameters_list
!
      use gen_sph_grids_modes
      use set_control_platform_item
      use set_control_platform_data
      use set_ctl_parallel_platform
      use set_control_4_model
      use set_control_sph_data_MHD
      use set_control_4_force
      use set_ctl_4_shell_grids
!
      use set_control_4_pickup_sph
      use parallel_ucd_IO_select
!
      type(platform_data_control), intent(in) :: plt
      type(platform_data_control), intent(in) :: org_plt
!
      type(mhd_DNS_model_control), intent(in) :: Dmodel_ctl
      type(sph_mhd_control_control), intent(in) :: smctl_ctl
      type(node_monitor_control), intent(in) :: nmtr_ctl
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
      integer(kind = kint) :: ierr
!
!
!   set parameters for data files
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call check_control_num_domains(plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_sph_mesh(plt, psph_ctl%Fmesh_ctl,                &
     &    MHD_files%sph_file_param, MHD_files%mesh_file_IO,             &
     &    MHD_files%sph_file_IO, MHD_files%FEM_mesh_flags)
      call set_control_restart_file_def(plt, MHD_files%fst_file_IO)
      call set_merged_ucd_file_define(plt, MHD_files%ucd_file_IO)
      call set_control_org_sph_files(org_plt, MHD_files)
!
      call s_set_control_4_model                                        &
     &    (Dmodel_ctl%reft_ctl, Dmodel_ctl%refc_ctl,                    &
     &     smctl_ctl%mevo_ctl, Dmodel_ctl%evo_ctl, nmtr_ctl, MHD_prop)
!
!   set spherical shell parameters
!
      call set_ctl_4_sph_grid_maker(nprocs, psph_ctl,                   &
     &    plt%sph_file_prefix, MHD_files%sph_file_param,                &
     &    SPH_MHD%sph_maker, ierr)
!
!   set forces
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_force'
      call s_set_control_4_force(Dmodel_ctl%frc_ctl, Dmodel_ctl%g_ctl,  &
     &    Dmodel_ctl%cor_ctl, Dmodel_ctl%mcv_ctl, MHD_prop)
!
!   set parameters for general information
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data_MHD'
      call s_set_control_sph_data_MHD(plt, smctl_ctl%mevo_ctl,          &
     &    MHD_files%org_rj_file_IO, MHD_files%org_rst_file_IO,          &
     &    MHD_files%fst_file_IO, bc_IO, trans_p, WK%WK_leg)
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_4_normalize'
      call set_control_4_normalize                                      &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop, MHD_prop%ht_prop,         &
     &    MHD_prop%cp_prop, Dmodel_ctl%dless_ctl, Dmodel_ctl%eqs_ctl,   &
     &    MHD_prop%MHD_coef_list)
!
      call set_coefs_4_magnetic_scale                                   &
     &   (Dmodel_ctl%bscale_ctl, MHD_prop%MHD_coef_list)
!
!   set boundary conditions
!
      call set_control_SPH_MHD_bcs                                      &
     &   (MHD_prop, Dmodel_ctl%nbc_ctl, Dmodel_ctl%sbc_ctl, MHD_BC)
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_time_steps'
      call s_set_control_4_time_steps                                   &
     &   (smctl_ctl%mrst_ctl, smctl_ctl%tctl, MHD_step)
!
      call s_set_control_4_crank(smctl_ctl%mevo_ctl,                    &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop)
!
      end subroutine set_control_4_SPH_MHD
!
! ----------------------------------------------------------------------
!
      subroutine set_control_SPH_MHD_bcs                                &
     &         (MHD_prop, nbc_ctl, sbc_ctl, MHD_BC)
!
      use t_ctl_data_node_boundary
      use t_ctl_data_surf_boundary
!
      use set_control_4_velo
      use set_control_4_press
      use set_control_4_temp
      use set_control_4_magne
      use set_control_4_composition
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(node_bc_control), intent(in) :: nbc_ctl
      type(surf_bc_control), intent(in) :: sbc_ctl
!
      type(MHD_BC_lists), intent(inout) :: MHD_BC
!
!
!   set boundary conditions for temperature
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_temp'
      call s_set_control_4_temp(MHD_prop%ht_prop,                       &
     &    nbc_ctl%node_bc_T_ctl, sbc_ctl%surf_bc_HF_ctl,                &
     &    MHD_BC%temp_BC%nod_BC, MHD_BC%temp_BC%surf_BC)
!
!
!   set boundary conditions for velocity
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_velo'
      call s_set_control_4_velo(MHD_prop%fl_prop,                       &
     &    nbc_ctl%node_bc_U_ctl, sbc_ctl%surf_bc_ST_ctl,                &
     &    MHD_BC%velo_BC%nod_BC, MHD_BC%velo_BC%surf_BC)
!
!  set boundary conditions for pressure
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_press'
      call s_set_control_4_press(MHD_prop%fl_prop,                      &
     &    nbc_ctl%node_bc_P_ctl, sbc_ctl%surf_bc_PN_ctl,                &
     &    MHD_BC%press_BC%nod_BC, MHD_BC%press_BC%surf_BC)!
!   set boundary conditions for composition variation
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_composition'
      call s_set_control_4_composition(MHD_prop%cp_prop,                &
     &    nbc_ctl%node_bc_C_ctl, sbc_ctl%surf_bc_CF_ctl,                &
     &    MHD_BC%light_BC%nod_BC, MHD_BC%light_BC%surf_BC)
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_magne'
      call s_set_control_4_magne(MHD_prop%cd_prop,                      &
     &    nbc_ctl%node_bc_B_ctl, sbc_ctl%surf_bc_BN_ctl,                &
     &    MHD_BC%magne_BC%nod_BC, MHD_BC%magne_BC%surf_BC)
!
      end subroutine set_control_SPH_MHD_bcs
!
! ----------------------------------------------------------------------
!
      subroutine set_control_SPH_MHD_monitors                           &
     &         (smonitor_ctl, rj_fld, monitor)
!
      use t_phys_data
      use t_sph_mhd_monitor_data_IO
      use t_no_heat_Nusselt
      use t_CMB_dipolarity
      use t_sph_typical_scales
!
      use set_control_4_pickup_sph
!
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      type(phys_data), intent(in) :: rj_fld
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
!   set_pickup modes
!
      call set_ctl_params_layered_spectr                                &
     &   (smonitor_ctl%lp_ctl, monitor%pwr)
      call set_ctl_params_sph_spectr(smonitor_ctl, monitor%pwr)
!
      call set_ctl_params_pick_sph                                      &
     &   (smonitor_ctl%pspec_ctl, monitor%pick_list, monitor%pick_coef)
!
      call set_ctl_params_pick_gauss                                    &
     &   (smonitor_ctl%g_pwr, monitor%gauss_list, monitor%gauss_coef)
!
      call set_ctl_params_no_heat_Nu                                    &
     &   (smonitor_ctl%heat_nusselt_file_prefix,                        &
     &    rj_fld, monitor%heat_Nusselt)
      call set_ctl_params_no_heat_Nu                                    &
     &   (smonitor_ctl%comp_nusselt_file_prefix,                        &
     &    rj_fld, monitor%comp_Nusselt)
!
      call set_ctl_dipolarity_params                                    &
     &   (smonitor_ctl%fdip_ctl%fdip_file_prefix_ctl,                   &
     &    smonitor_ctl%fdip_ctl%fdip_truncation_ctl,                    &
     &    rj_fld, monitor%dip)
      call set_ctl_typical_scale_params                                 &
     &   (smonitor_ctl%typ_scale_file_prefix_ctl, rj_fld, monitor%tsl)
!
      end subroutine set_control_SPH_MHD_monitors
!
! ----------------------------------------------------------------------
!
      subroutine set_crustal_filtering_control(crust_c, monitor)
!
      use t_control_data_dynamo_vizs
      use t_sph_mhd_monitor_data_IO
!
      use set_control_4_pickup_sph
!
      type(clust_filtering_ctl), intent(in) :: crust_c
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
!
      monitor%ltr_crust = 1
      if(crust_c%crust_truncation_ctl%iflag .gt. 0) then
        monitor%ltr_crust = crust_c%crust_truncation_ctl%intvalue
      end if
!
      end subroutine set_crustal_filtering_control
!
! ----------------------------------------------------------------------
!
      end module set_control_sph_mhd
