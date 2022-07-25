!>@file   t_ctl_data_MHD.f90
!!@brief  module t_ctl_data_MHD
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_sph_mhd_ctl_w_psf                               &
!!     &         (id_control, hd_block, DMHD_ctl, c_buf)
!!      subroutine read_sph_mhd_ctl_noviz                               &
!!     &         (id_control, hd_block, DMHD_ctl, c_buf)
!!
!!      subroutine bcast_sph_mhd_ctl_w_psf(DMHD_ctl)
!!      subroutine bcast_sph_mhd_ctl_data(DMHD_ctl)
!!      subroutine dealloc_sph_mhd_ctl_data(DMHD_ctl)
!!        type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!!@endverbatim
!
      module t_ctl_data_MHD
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_control_data_surfacings
      use t_control_data_dynamo_vizs
!
      implicit none
!
!
      type DNS_mhd_simulation_control
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
!>        Structures of visualization controls
        type(surfacing_controls) :: surfacing_ctls
!
!>        Structures of zonal mean controls
        type(sph_dynamo_viz_controls) :: zm_ctls
!
        integer (kind=kint) :: i_mhd_ctl = 0
      end type DNS_mhd_simulation_control
!
!   2nd level for MHD
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_shell = 'spherical_shell_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_model =   'model'
      character(len=kchara), parameter, private                         &
     &                    :: hd_control = 'control'
      character(len=kchara), parameter, private                         &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_monitor_data = 'monitor_data_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_ctl = 'visual_control'
      character(len=kchara), parameter, private                         &
     &                    :: hd_dynamo_viz_ctl = 'dynamo_vizs_control'
!
!>      Here is the old label
      character(len=kchara), parameter                                  &
     &                    :: hd_zm_viz_ctl = 'zonal_mean_control'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_ctl_w_psf                                 &
     &         (id_control, hd_block, DMHD_ctl, c_buf)
!
      use read_surfacing_controls
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(DMHD_ctl%i_mhd_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, DMHD_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, DMHD_ctl%org_plt, c_buf)
!
        call read_parallel_shell_in_MHD_ctl                             &
     &     (id_control, hd_sph_shell, DMHD_ctl%psph_ctl, c_buf)
!
        call read_sph_mhd_model                                         &
     &     (id_control, hd_model, DMHD_ctl%model_ctl, c_buf)
        call read_sph_mhd_control                                       &
     &     (id_control, hd_control, DMHD_ctl%smctl_ctl, c_buf)
!
        call read_monitor_data_ctl                                      &
     &     (id_control, hd_monitor_data, DMHD_ctl%nmtr_ctl, c_buf)
        call read_sph_monitoring_ctl                                    &
     &     (id_control, hd_pick_sph, DMHD_ctl%smonitor_ctl, c_buf)
!
        call s_read_surfacing_controls                                  &
     &     (id_control, hd_viz_ctl, DMHD_ctl%surfacing_ctls, c_buf)
!
        call read_dynamo_viz_control                                    &
     &     (id_control, hd_dynamo_viz_ctl, DMHD_ctl%zm_ctls, c_buf)
        call read_dynamo_viz_control                                    &
     &     (id_control, hd_zm_viz_ctl, DMHD_ctl%zm_ctls, c_buf)
      end do
      DMHD_ctl%i_mhd_ctl = 1
!
      end subroutine read_sph_mhd_ctl_w_psf
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_mhd_ctl_noviz                                 &
     &         (id_control, hd_block, DMHD_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(DMHD_ctl%i_mhd_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, DMHD_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, DMHD_ctl%org_plt, c_buf)
!
        call read_parallel_shell_in_MHD_ctl                             &
     &     (id_control, hd_sph_shell, DMHD_ctl%psph_ctl, c_buf)
!
        call read_sph_mhd_model                                         &
     &     (id_control, hd_model, DMHD_ctl%model_ctl, c_buf)
        call read_sph_mhd_control                                       &
     &     (id_control, hd_control, DMHD_ctl%smctl_ctl, c_buf)
!
        call read_monitor_data_ctl                                      &
     &     (id_control, hd_monitor_data, DMHD_ctl%nmtr_ctl, c_buf)
        call read_sph_monitoring_ctl                                    &
     &     (id_control, hd_pick_sph, DMHD_ctl%smonitor_ctl, c_buf)
      end do
      DMHD_ctl%i_mhd_ctl = 1
!
      end subroutine read_sph_mhd_ctl_noviz
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_ctl_w_psf(DMHD_ctl)
!
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
!
      call bcast_sph_mhd_ctl_data(DMHD_ctl)
      call bcast_surfacing_controls(DMHD_ctl%surfacing_ctls)
      call bcast_dynamo_viz_control(DMHD_ctl%zm_ctls)
!
      end subroutine bcast_sph_mhd_ctl_w_psf
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_ctl_data(DMHD_ctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_field_ctl
      use bcast_4_sph_monitor_ctl
      use bcast_4_sphere_ctl
!
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
!
      call bcast_ctl_data_4_platform(DMHD_ctl%plt)
      call bcast_ctl_data_4_platform(DMHD_ctl%org_plt)
!
      call bcast_sph_mhd_model(DMHD_ctl%model_ctl)
      call bcast_sph_mhd_control(DMHD_ctl%smctl_ctl)
!
      call bcast_parallel_shell_ctl(DMHD_ctl%psph_ctl)
!
      call bcast_monitor_data_ctl(DMHD_ctl%nmtr_ctl)
      call bcast_sph_monitoring_ctl(DMHD_ctl%smonitor_ctl)
!
      call calypso_mpi_bcast_one_int(DMHD_ctl%i_mhd_ctl, 0)
!
      end subroutine bcast_sph_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_mhd_ctl_data(DMHD_ctl)
!
      type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!
!
      call dealloc_monitor_data_ctl(DMHD_ctl%nmtr_ctl)
      call dealloc_parallel_shell_ctl(DMHD_ctl%psph_ctl)
      call dealloc_sph_monitoring_ctl(DMHD_ctl%smonitor_ctl)
      call dealloc_sph_mhd_model(DMHD_ctl%model_ctl)
!
      DMHD_ctl%i_mhd_ctl = 0
!
      end subroutine dealloc_sph_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_MHD
