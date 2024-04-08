!>@file   input_control_sph_MHD_vizs.f90
!!@brief  module input_control_sph_MHD_vizs
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine load_control_4_sph_MHD_w_vizs(file_name, MHD_ctl,    &
!!     &                                         add_SMHD_ctl)
!!      subroutine s_input_control_SPH_MHD_vizs                         &
!!     &         (ctl_file_name, MHD_files, MHD_ctl, add_VMHD_ctl,      &
!!     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!!        character(len=kchara), intent(in) :: ctl_file_name
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(add_vizs_sph_mhd_ctl), intent(inout) :: add_VMHD_ctl
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!!@endverbatim
!
!
      module input_control_sph_MHD_vizs
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_const_spherical_grid
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_FEM_mesh_field_data
      use t_control_data_dynamo_vizs
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_flex_delta_t_data
      use t_work_SPH_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine load_control_4_sph_MHD_w_vizs(file_name, MHD_ctl,      &
     &                                         add_VMHD_ctl)
!
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_vizs
      use t_read_control_elements
      use bcast_control_sph_MHD
      use bcast_ctl_data_viz3
      use bcast_dynamo_viz_control

!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(add_vizs_sph_mhd_ctl), intent(inout) :: add_VMHD_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        call read_control_4_sph_MHD_w_vizs(file_name, MHD_ctl,          &
     &                                     add_VMHD_ctl, c_buf1)
      end if
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(MHD_ctl%i_mhd_ctl, trim(file_name))
      end if
!
      call bcast_sph_mhd_control_data(MHD_ctl)
      call bcast_viz3_controls(add_VMHD_ctl%viz3_ctls)
      call s_bcast_dynamo_viz_control(add_VMHD_ctl%zm_ctls)
!
      end subroutine load_control_4_sph_MHD_w_vizs
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_input_control_SPH_MHD_vizs                           &
     &         (ctl_file_name, MHD_files, MHD_ctl, add_VMHD_ctl,        &
     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!
      use t_time_data
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_vizs
      use t_node_monitor_IO
      use m_error_IDs
!
      use set_control_sph_mhd
      use set_control_SPH_MHD_w_viz
      use sph_file_IO_select
      use set_control_4_SPH_to_FEM
      use parallel_load_data_4_sph
!
      character(len=kchara), intent(in) :: ctl_file_name
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(add_vizs_sph_mhd_ctl), intent(inout) :: add_VMHD_ctl
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!
!  Read control file
      if (iflag_debug.eq.1) write(*,*) 'load_control_4_sph_MHD_w_vizs'
      call load_control_4_sph_MHD_w_vizs(ctl_file_name, MHD_ctl,        &
     &                                   add_VMHD_ctl)
!
!  Set parameters from control
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD(MHD_ctl%plt, MHD_ctl%org_plt,          &
     &    MHD_ctl%model_ctl, MHD_ctl%smctl_ctl, MHD_ctl%psph_ctl,       &
     &    MHD_files, SPH_model%bc_IO, SPH_model%refs, MHD_step,         &
     &    SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_WK%trans_p,         &
     &    SPH_WK%trns_WK, SPH_MHD)
!
      call s_set_control_SPH_MHD_w_viz                                  &
     &   (MHD_ctl%model_ctl, MHD_ctl%psph_ctl, MHD_ctl%smonitor_ctl,    &
     &    add_VMHD_ctl%zm_ctls%crust_filter_ctl, MHD_ctl%nmtr_ctl,      &
     &    SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_MHD%sph,            &
     &    SPH_MHD%fld, FEM_dat%field, SPH_WK%monitor, FEM_dat%nod_mntr)
!
!  Load spherical shell table
      if (iflag_debug.eq.1) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (MHD_files%FEM_mesh_flags, MHD_files%sph_file_param,           &
     &    SPH_MHD, FEM_dat%geofem, MHD_files%mesh_file_IO)
!
      call dealloc_sph_mhd_ctl_data(MHD_ctl)
!
      call sph_boundary_IO_control                                      &
     &   (SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_model%bc_IO)
!
!   Set initial time into time data
      if (iflag_debug.eq.1) write(*,*) 'copy_delta_t'
      call copy_delta_t(MHD_step%init_d, MHD_step%time_d)
!
      end subroutine s_input_control_SPH_MHD_vizs
!
! ----------------------------------------------------------------------
!
      end module input_control_sph_MHD_vizs
