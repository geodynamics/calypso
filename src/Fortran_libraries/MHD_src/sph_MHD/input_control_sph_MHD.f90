!>@file   input_control_sph_MHD.f90
!!@brief  module input_control_sph_MHD
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine load_control_4_sph_MHD_w_psf(file_name, MHD_ctl,     &
!!     &                                        add_SMHD_ctl)
!!      subroutine load_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
!!
!!      subroutine input_control_SPH_MHD_psf                            &
!!     &         (ctl_file_name, MHD_files, MHD_ctl, add_SMHD_ctl,      &
!!     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!!        type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
!!      subroutine input_control_4_SPH_MHD_nosnap                       &
!!     &         (ctl_file_name, MHD_files, MHD_ctl, MHD_step,          &
!!     &          SPH_model, SPH_WK, SPH_MHD)
!!
!!      subroutine input_control_4_SPH_make_init                        &
!!     &         (ctl_file_name, MHD_files, MHD_ctl, add_SMHD_ctl,      &
!!     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!!        character(len=kchara), intent(in) :: ctl_file_name
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!!@endverbatim
!
!
      module input_control_sph_MHD
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
      use t_control_data_dynamo_sects
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
      subroutine load_control_4_sph_MHD_w_psf(file_name, MHD_ctl,       &
     &                                        add_SMHD_ctl)
!
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_psf
      use bcast_control_sph_MHD
      use bcast_ctl_data_surfacings
      use bcast_dynamo_sect_control
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_4_sph_MHD_w_psf(file_name, MHD_ctl,          &
     &                                    add_SMHD_ctl)
      end if
!
      call bcast_sph_mhd_control_data(MHD_ctl)
      call bcast_surfacing_controls(add_SMHD_ctl%surfacing_ctls)
      call s_bcast_dynamo_section_control(add_SMHD_ctl%zm_sects)
!
      if(MHD_ctl%i_mhd_ctl .ne. 1) then
        call calypso_MPI_abort(MHD_ctl%i_mhd_ctl, trim(file_name))
      end if
!
      end subroutine load_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine load_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!
      use t_ctl_data_MHD
      use bcast_control_sph_MHD
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
!
!
      if(my_rank .eq. 0) then
        call read_control_4_sph_MHD_noviz(file_name, MHD_ctl)
      end if
!
      call bcast_sph_mhd_control_data(MHD_ctl)
!
      if(MHD_ctl%i_mhd_ctl .le. 0) then
        call calypso_MPI_abort(MHD_ctl%i_mhd_ctl, trim(file_name))
      end if
!
      end subroutine load_control_4_sph_MHD_noviz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_MHD_psf                              &
     &         (ctl_file_name, MHD_files, MHD_ctl, add_SMHD_ctl,        &
     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!
      use t_time_data
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_psf
      use t_node_monitor_IO
      use m_error_IDs
!
      use set_control_sph_mhd
      use sph_file_IO_select
      use set_control_4_SPH_to_FEM
      use parallel_load_data_4_sph
!
      character(len=kchara), intent(in) :: ctl_file_name
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!
!  Read control file
      if (iflag_debug.eq.1) write(*,*) 'load_control_4_sph_MHD_w_psf'
      call load_control_4_sph_MHD_w_psf(ctl_file_name, MHD_ctl,         &
     &                                  add_SMHD_ctl)
!
!  Set parameters from control
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD(MHD_ctl%plt, MHD_ctl%org_plt,          &
     &    MHD_ctl%model_ctl, MHD_ctl%smctl_ctl, MHD_ctl%psph_ctl,       &
     &    MHD_files, SPH_model%bc_IO, SPH_model%refs, MHD_step,         &
     &    SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_WK%trans_p,         &
     &    SPH_WK%trns_WK, SPH_MHD)
!
      call set_control_SPH_MHD_w_viz                                    &
     &   (MHD_ctl%model_ctl, MHD_ctl%psph_ctl, MHD_ctl%smonitor_ctl,    &
     &    add_SMHD_ctl%zm_sects%crust_filter_ctl, MHD_ctl%nmtr_ctl,     &
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
      end subroutine input_control_SPH_MHD_psf
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_MHD_nosnap                         &
     &         (ctl_file_name, MHD_files, MHD_ctl, MHD_step,            &
     &          SPH_model, SPH_WK, SPH_MHD)
!
      use t_ctl_data_MHD
      use t_node_monitor_IO
      use set_control_sph_mhd
      use parallel_load_data_4_sph
      use set_control_4_SPH_to_FEM
!
      character(len=kchara), intent(in) :: ctl_file_name
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
!  Read control file
      call load_control_4_sph_MHD_noviz(ctl_file_name, MHD_ctl)
!
!  Set parameters from control
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD(MHD_ctl%plt, MHD_ctl%org_plt,          &
     &    MHD_ctl%model_ctl, MHD_ctl%smctl_ctl, MHD_ctl%psph_ctl,       &
     &    MHD_files, SPH_model%bc_IO, SPH_model%refs, MHD_step,         &
     &    SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_WK%trans_p,         &
     &    SPH_WK%trns_WK, SPH_MHD)
!
      call set_control_SPH_MHD_noviz                                    &
     &   (MHD_ctl%model_ctl, MHD_ctl%smonitor_ctl, SPH_model%MHD_prop,  &
     &    SPH_model%MHD_BC, SPH_MHD%fld, SPH_WK%monitor)
!
      if (iflag_debug.eq.1) write(*,*) 'check_and_make_SPH_mesh'
      call check_and_make_SPH_mesh(MHD_files%sph_file_param, SPH_MHD)
!
      call dealloc_sph_mhd_ctl_data(MHD_ctl)
!
      call sph_boundary_IO_control                                      &
     &    (SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_model%bc_IO)
!
      end subroutine input_control_4_SPH_MHD_nosnap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_make_init                          &
     &         (ctl_file_name, MHD_files, MHD_ctl, add_SMHD_ctl,        &
     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!
      use t_time_data
      use t_ctl_data_MHD
      use t_ctl_data_sph_MHD_w_psf
      use set_control_sph_mhd
      use parallel_load_data_4_sph
      use set_control_4_SPH_to_FEM
!
      character(len=kchara), intent(in) :: ctl_file_name
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!
!  Read control file
      if (iflag_debug.eq.1) write(*,*) 'load_control_4_sph_MHD_w_psf'
      call load_control_4_sph_MHD_w_psf(ctl_file_name, MHD_ctl,         &
     &                                  add_SMHD_ctl)
!
!  Set parameters from control
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD                                        &
     &   (MHD_ctl%plt, MHD_ctl%org_plt, MHD_ctl%model_ctl,              &
     &    MHD_ctl%smctl_ctl, MHD_ctl%psph_ctl,                          &
     &    MHD_files, SPH_model%bc_IO, SPH_model%refs, MHD_step,         &
     &    SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_WK%trans_p,         &
     &    SPH_WK%trns_WK, SPH_MHD)
!
      call set_control_SPH_MHD_w_viz                                    &
     &   (MHD_ctl%model_ctl, MHD_ctl%psph_ctl, MHD_ctl%smonitor_ctl,    &
     &    add_SMHD_ctl%zm_sects%crust_filter_ctl, MHD_ctl%nmtr_ctl,     &
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
      end subroutine input_control_4_SPH_make_init
!
! ----------------------------------------------------------------------
!
      end module input_control_sph_MHD
