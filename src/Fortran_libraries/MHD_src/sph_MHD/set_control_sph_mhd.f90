!>@file   set_control_sph_mhd.f90
!!@brief  module set_control_sph_mhd
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_control_4_SPH_MHD                                &
!!     &         (plt, org_plt, model_ctl, smctl_ctl, psph_ctl,         &
!!     &          MHD_files, bc_IO, refs, MHD_step, MHD_prop, MHD_BC,   &
!!     &          trans_p, WK, sph_maker)
!!        type(platform_data_control), intent(in) :: plt
!!        type(platform_data_control), intent(in) :: org_plt
!!        type(mhd_model_control), intent(in) :: model_ctl
!!        type(sph_mhd_control_control), intent(in) :: smctl_ctl
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!        type(clust_filtering_ctl), intent(in) :: crust_filter_ctl
!!        type(node_monitor_control), intent(in) :: nmtr_ctl
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!        type(radial_reference_field), intent(inout) :: refs
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
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
      use t_ctl_data_crust_filter
      use t_bc_data_list
      use t_flex_delta_t_data
      use t_sph_grid_maker_in_sim
      use t_radial_reference_field
      use t_field_on_circle
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_MHD(plt, org_plt,                    &
     &          model_ctl, smctl_ctl, psph_ctl, MHD_files,              &
     &          bc_IO, refs, MHD_step, MHD_prop, MHD_BC,                &
     &          trans_p, WK, sph_maker)
!
      use t_spheric_parameter
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_sph_trans_arrays_MHD
      use t_const_spherical_grid
      use t_sph_boundary_input_data
      use t_ctl_params_gen_sph_shell
      use t_sph_trans_arrays_MHD
      use t_coef_parameters_list
      use t_ctl_param_val_density
      use t_ctl_param_val_diffusion
!
      use gen_sph_grids_modes
      use set_control_platform_item
      use set_control_platform_data
      use set_ctl_parallel_platform
      use set_control_4_model
      use set_control_sph_data_MHD
      use set_control_SPH_MHD_bcs
      use set_control_4_force
      use set_ctl_4_shell_grids
!
      use set_control_4_pickup_sph
      use parallel_ucd_IO_select
      use m_initial_field_control
      use set_reference_scalar_param
!
      type(platform_data_control), intent(in) :: plt
      type(platform_data_control), intent(in) :: org_plt
!
      type(mhd_model_control), intent(in) :: model_ctl
      type(sph_mhd_control_control), intent(in) :: smctl_ctl
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(radial_reference_field), intent(inout) :: refs
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
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
     &    (model_ctl%reft_ctl, model_ctl%refc_ctl,                      &
     &     smctl_ctl%mevo_ctl, model_ctl%evo_ctl, MHD_prop)
!
!   set spherical shell parameters
!
      call set_ctl_4_sph_grid_maker(nprocs, psph_ctl,                   &
     &    plt%sph_file_prefix, MHD_files%sph_file_param,                &
     &    sph_maker, ierr)
!
!   set forces
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_force'
      call s_set_control_4_force(model_ctl, MHD_prop)
!
!   set parameters for general information
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data_MHD'
      call s_set_control_sph_data_MHD(plt, smctl_ctl%mevo_ctl,          &
     &    MHD_files%org_rj_file_IO, MHD_files%org_rst_file_IO,          &
     &    MHD_files%fst_file_IO, bc_IO, refs%ref_input_IO,              &
     &    trans_p, WK%WK_leg)
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_4_normalize'
      call set_control_4_normalize                                      &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop, MHD_prop%ht_prop,         &
     &    MHD_prop%cp_prop, model_ctl%dless_ctl, model_ctl%eqs_ctl,     &
     &    MHD_prop%MHD_coef_list)
!
!   Set external magnetic field scale
      call set_coefs_4_magnetic_scale(model_ctl%bscale_ctl,             &
     &                                MHD_prop%MHD_coef_list)
!
!   Set polytrope and valuable diffusivities
      call set_ctl_SPH_val_diffusions(model_ctl, MHD_prop)
!
!   set boundary conditions
!
      call s_set_control_SPH_MHD_bcs                                    &
     &   (MHD_prop, model_ctl%nbc_ctl, model_ctl%sbc_ctl, MHD_BC)
!
!   set time step parameters
!
      if (iflag_debug.gt.0) write(*,*) 'set_initial_field_id'
      call set_initial_field_id(smctl_ctl%mrst_ctl%restart_flag_ctl,    &
     &                          MHD_step%iflag_restart_mode)
      call check_set_initial_time(MHD_step%iflag_restart_mode,          &
     &                            smctl_ctl%tctl, MHD_step%init_d%time)
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_time_steps'
      call s_set_control_4_time_steps(smctl_ctl%tctl, MHD_step)
!
      call s_set_control_4_crank(smctl_ctl%mevo_ctl,                    &
     &                           MHD_prop%fl_prop, MHD_prop%cd_prop,    &
     &                           MHD_prop%ht_prop, MHD_prop%cp_prop)
!
      end subroutine set_control_4_SPH_MHD
!
! ----------------------------------------------------------------------
!
      end module set_control_sph_mhd
