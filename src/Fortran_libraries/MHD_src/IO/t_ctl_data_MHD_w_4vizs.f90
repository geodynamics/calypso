!>@file   t_ctl_data_MHD_w_4vizs.f90
!!@brief  module t_ctl_data_MHD_w_4vizs
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!@n        Modified by H. Matsui on Nov., 2021
!!
!!@verbatim
!!      subroutine input_control_sph_MHD_4vizs(MHD_files, MHD_v4_ctl,   &
!!     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(DNS_mhd_sim_w_viz4_control), intent(inout) :: MHD_v4_ctl
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!!
!!      subroutine read_sph_mhd_ctl_w_4vizs                             &
!!     &         (id_control, hd_block, MHD_v4_ctl, c_buf)
!!      subroutine bcast_sph_mhd_ctl_4vizs(MHD_v4_ctl)
!!      subroutine dealloc_sph_mhd_v4_ctl_data(MHD_v4_ctl)
!!        type(DNS_mhd_sim_w_viz4_control), intent(inout) :: MHD_v4_ctl
!!@endverbatim
!
      module t_ctl_data_MHD_w_4vizs
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
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_flex_delta_t_data
      use t_work_SPH_MHD
!
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_control_data_viz4
      use t_control_data_dynamo_vizs
!
      implicit none
!
      integer(kind=kint), parameter :: ctl_file_code = 11
      private :: ctl_file_code
!
!
      type DNS_mhd_sim_w_viz4_control
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Control structure for orginal file informations
        type(platform_data_control) :: org_plt
!>        Control structure for new file informations
        type(platform_data_control) :: new_plt
!
!>        Control structure for parallel spherical shell
        type(parallel_sph_shell_control) :: psph_ctl
!
!>        Control structure for MHD/model
        type(mhd_DNS_model_control) :: model_ctl
!>        Control structure for MHD/control
        type(sph_mhd_control_control) :: smctl_ctl
!
!>        Structure for spectr monitoring control
        type(sph_monitor_control) :: smonitor_ctl
!>        Structure for monitoring plave list
        type(node_monitor_control) :: nmtr_ctl
!
!>        Structures of four visualization controls
        type(vis4_controls) :: viz4_ctl
!
!>        Structures of zonal mean controls
        type(sph_dynamo_viz_controls) :: zm_ctls
!
        integer (kind=kint) :: i_mhd_ctl = 0
      end type DNS_mhd_sim_w_viz4_control
!
!   Top level
      character(len=kchara), parameter, private                         &
     &                    :: hd_mhd_ctl = 'MHD_control'
!
!   2nd level for MHD
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter                                  &
     &      :: hd_sph_shell = 'spherical_shell_ctl'
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
      character(len=kchara), parameter                                  &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_monitor_data = 'monitor_data_ctl'
!
      character(len=kchara), parameter :: hd_viz_ctl = 'visual_control'
      character(len=kchara), parameter                                  &
     &                    :: hd_dynamo_viz_ctl = 'dynamo_vizs_control'
!
!>      Here is the old label
      character(len=kchara), parameter                                  &
     &                    :: hd_zm_viz_ctl = 'zonal_mean_control'
!
      private :: hd_platform, hd_org_data, hd_new_data
      private :: hd_sph_shell, hd_model, hd_control
      private :: hd_pick_sph, hd_monitor_data
      private :: hd_viz_ctl, hd_dynamo_viz_ctl, hd_zm_viz_ctl
!
      private :: read_sph_mhd_ctl_w_4vizs
      private :: bcast_sph_mhd_ctl_4vizs, dealloc_sph_mhd_v4_ctl_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD_4vizs(file_name, MHD_v4_ctl)
!
      use viz4_step_ctls_to_time_ctl
!
      character(len=kchara), intent(in) :: file_name
      type(DNS_mhd_sim_w_viz4_control), intent(inout) :: MHD_v4_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(ctl_file_code, file = file_name, status='old' )
!
        do
          call load_one_line_from_control(ctl_file_code, c_buf1)
          call read_sph_mhd_ctl_w_4vizs                                 &
     &       (ctl_file_code, hd_mhd_ctl, MHD_v4_ctl, c_buf1)
          if(MHD_v4_ctl%i_mhd_ctl .gt. 0) exit
        end do
        close(ctl_file_code)
!
        call s_viz4_step_ctls_to_time_ctl(MHD_v4_ctl%viz4_ctl,          &
     &                                    MHD_v4_ctl%smctl_ctl%tctl)
        call add_fields_viz4_to_fld_ctl(MHD_v4_ctl%viz4_ctl,            &
     &      MHD_v4_ctl%model_ctl%fld_ctl%field_ctl)
      end if
!
      call bcast_sph_mhd_ctl_4vizs(MHD_v4_ctl)
!
      end subroutine read_control_4_sph_MHD_4vizs
!
! ----------------------------------------------------------------------
!
      subroutine input_control_sph_MHD_4vizs(MHD_files, MHD_v4_ctl,     &
     &          MHD_step, SPH_model, SPH_WK, SPH_MHD, FEM_dat)
!
      use m_error_IDs
!
      use set_control_sph_mhd
      use sph_file_IO_select
      use set_control_4_SPH_to_FEM
      use parallel_load_data_4_sph
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(DNS_mhd_sim_w_viz4_control), intent(inout) :: MHD_v4_ctl
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD(MHD_v4_ctl%plt, MHD_v4_ctl%org_plt,    &
     &    MHD_v4_ctl%model_ctl, MHD_v4_ctl%smctl_ctl,                   &
     &    MHD_v4_ctl%nmtr_ctl, MHD_v4_ctl%psph_ctl,                     &
     &    MHD_files, SPH_model%bc_IO, MHD_step, SPH_model%MHD_prop,     &
     &    SPH_model%MHD_BC, SPH_WK%trans_p, SPH_WK%trns_WK, SPH_MHD)
!
      call set_control_SPH_MHD_w_viz                                    &
     &   (MHD_v4_ctl%model_ctl, MHD_v4_ctl%psph_ctl,                    &
     &    MHD_v4_ctl%smonitor_ctl, MHD_v4_ctl%zm_ctls,                  &
     &    SPH_model%MHD_prop, SPH_MHD%sph, SPH_MHD%fld, FEM_dat%field,  &
     &    SPH_WK%monitor)
!
!  Load spherical shell table
      if (iflag_debug.eq.1) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (MHD_files%FEM_mesh_flags, MHD_files%sph_file_param,           &
     &    SPH_MHD, FEM_dat%geofem, MHD_files%mesh_file_IO)
!
      call dealloc_sph_mhd_v4_ctl_data(MHD_v4_ctl)
!
      call sph_boundary_IO_control                                      &
     &   (SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_model%bc_IO)
!
      end subroutine input_control_sph_MHD_4vizs
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_ctl_w_4vizs                               &
     &         (id_control, hd_block, MHD_v4_ctl, c_buf)
!
      use read_four_viz_controls
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(DNS_mhd_sim_w_viz4_control), intent(inout) :: MHD_v4_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(MHD_v4_ctl%i_mhd_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, MHD_v4_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, MHD_v4_ctl%org_plt, c_buf)
!
        call read_parallel_shell_in_MHD_ctl                             &
     &     (id_control, hd_sph_shell, MHD_v4_ctl%psph_ctl, c_buf)
!
        call read_sph_mhd_model                                         &
     &     (id_control, hd_model, MHD_v4_ctl%model_ctl, c_buf)
        call read_sph_mhd_control                                       &
     &     (id_control, hd_control, MHD_v4_ctl%smctl_ctl, c_buf)
!
        call read_monitor_data_ctl                                      &
     &     (id_control, hd_monitor_data, MHD_v4_ctl%nmtr_ctl, c_buf)
        call read_sph_monitoring_ctl                                    &
     &     (id_control, hd_pick_sph, MHD_v4_ctl%smonitor_ctl, c_buf)
!
        call s_read_viz4_controls                                       &
     &     (id_control, hd_viz_ctl, MHD_v4_ctl%viz4_ctl, c_buf)
!
        call read_dynamo_viz_control                                    &
     &     (id_control, hd_dynamo_viz_ctl, MHD_v4_ctl%zm_ctls, c_buf)
        call read_dynamo_viz_control                                    &
     &     (id_control, hd_zm_viz_ctl, MHD_v4_ctl%zm_ctls, c_buf)
      end do
      MHD_v4_ctl%i_mhd_ctl = 1
!
      end subroutine read_sph_mhd_ctl_w_4vizs
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_ctl_4vizs(MHD_v4_ctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
!
      type(DNS_mhd_sim_w_viz4_control), intent(inout) :: MHD_v4_ctl
!
!
      call bcast_ctl_data_4_platform(MHD_v4_ctl%plt)
      call bcast_ctl_data_4_platform(MHD_v4_ctl%org_plt)
!
      call bcast_sph_mhd_model(MHD_v4_ctl%model_ctl)
      call bcast_sph_mhd_control(MHD_v4_ctl%smctl_ctl)
!
      call bcast_parallel_shell_ctl(MHD_v4_ctl%psph_ctl)
!
      call bcast_monitor_data_ctl(MHD_v4_ctl%nmtr_ctl)
      call bcast_sph_monitoring_ctl(MHD_v4_ctl%smonitor_ctl)
!
      call calypso_mpi_bcast_one_int(MHD_v4_ctl%i_mhd_ctl, 0)
!
      call bcast_viz4_controls(MHD_v4_ctl%viz4_ctl)
      call bcast_dynamo_viz_control(MHD_v4_ctl%zm_ctls)
!
      end subroutine bcast_sph_mhd_ctl_4vizs
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_mhd_v4_ctl_data(MHD_v4_ctl)
!
      type(DNS_mhd_sim_w_viz4_control), intent(inout) :: MHD_v4_ctl
!
!
      call dealloc_monitor_data_ctl(MHD_v4_ctl%nmtr_ctl)
      call dealloc_parallel_shell_ctl(MHD_v4_ctl%psph_ctl)
      call dealloc_sph_monitoring_ctl(MHD_v4_ctl%smonitor_ctl)
      call dealloc_sph_mhd_model(MHD_v4_ctl%model_ctl)
!
      MHD_v4_ctl%i_mhd_ctl = 0
!
      end subroutine dealloc_sph_mhd_v4_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_MHD_w_4vizs
