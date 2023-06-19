!>@file   t_ctl_data_sph_MHD_w_psf.f90
!!@brief  module t_ctl_data_sph_MHD_w_psf
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
!!      subroutine read_control_4_sph_MHD_w_psf(file_name, MHD_ctl,     &
!!     &                                        add_SMHD_ctl)
!!      subroutine read_sph_mhd_ctl_w_psf(id_control, hd_block,         &
!!     &                                  MHD_ctl, add_SMHD_ctl, c_buf)
!!        character(len=kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_4_sph_MHD_w_psf(file_name, MHD_ctl,    &
!!     &                                         add_SMHD_ctl)
!!      subroutine write_sph_mhd_ctl_w_psf(id_control, hd_block,        &
!!     &                                   MHD_ctl, add_SMHD_ctl, level)
!!        character(len=kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_simulation_control), intent(in) :: MHD_ctl
!!        type(add_psf_sph_mhd_ctl), intent(in) :: add_SMHD_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_sph_mhd_ctl_w_psf(add_SMHD_ctl)
!!        type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
!!@endverbatim
!
      module t_ctl_data_sph_MHD_w_psf
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_MHD
      use t_ctl_data_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
!
      use t_control_data_surfacings
      use t_control_data_dynamo_sects
!
      implicit none
!
!
      integer(kind = kint), parameter, private :: id_control_file = 11
!
!>      Additional structures for spherical MHD dynamo with viz module
      type add_psf_sph_mhd_ctl
!>        Structures of visualization controls
        type(surfacing_controls) :: surfacing_ctls
!>        Structures of zonal mean controls
        type(sph_dynamo_section_controls) :: zm_sects
      end type add_psf_sph_mhd_ctl
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
     &                    :: hd_pick_sph = 'sph_monitor_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_monitor_data = 'monitor_data_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_ctl = 'visual_control'
      character(len=kchara), parameter, private                         &
     &                    :: hd_dynamo_viz_ctl = 'dynamo_vizs_control'
!
!   Top level of label
      character(len=kchara), parameter, private                         &
     &                    :: hd_mhd_ctl = 'MHD_control'
!
!>      Here is the old label
      character(len=kchara), parameter, private                         &
     &                    :: hd_zm_viz_ctl = 'zonal_mean_control'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD_w_psf(file_name, MHD_ctl,       &
     &                                        add_SMHD_ctl)
!
      use t_control_data_surfacings
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      open(id_control_file, file = file_name, status='old' )
!
      do
        call load_one_line_from_control(id_control_file, hd_mhd_ctl,    &
     &                                  c_buf1)
        if(c_buf1%iend .gt. 0) exit
!
        call read_sph_mhd_ctl_w_psf(id_control_file, hd_mhd_ctl,        &
     &      MHD_ctl, add_SMHD_ctl, c_buf1)
        if(MHD_ctl%i_mhd_ctl .gt. 0) exit
      end do
      close(id_control_file)
!
      if(c_buf1%iend .gt. 0) then
        MHD_ctl%i_mhd_ctl = c_buf1%iend
        return
      end if
!
      call section_step_ctls_to_time_ctl(add_SMHD_ctl%surfacing_ctls,   &
     &                                   MHD_ctl%smctl_ctl%tctl)
      call add_fields_4_scts_to_fld_ctl(add_SMHD_ctl%surfacing_ctls,    &
     &    MHD_ctl%model_ctl%fld_ctl%field_ctl)
!
      end subroutine read_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
!
      subroutine write_control_4_sph_MHD_w_psf(file_name, MHD_ctl,      &
     &                                         add_SMHD_ctl)
!
      use t_control_data_surfacings
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(in) :: MHD_ctl
      type(add_psf_sph_mhd_ctl), intent(in) :: add_SMHD_ctl
!
      integer(kind = kint) :: level1
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      write(*,*) 'Write MHD control file: ', trim(file_name)
      open(id_control_file, file = file_name)
      level1 = 0
      call write_sph_mhd_ctl_w_psf(id_control_file, hd_mhd_ctl,         &
     &                             MHD_ctl, add_SMHD_ctl, level1)
      close(id_control_file)
!
      end subroutine write_control_4_sph_MHD_w_psf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_ctl_w_psf(id_control, hd_block,           &
     &                                  MHD_ctl, add_SMHD_ctl, c_buf)
!
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use ctl_data_MHD_model_IO
      use control_data_surfacing_IO
      use ctl_file_gen_sph_shell_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(MHD_ctl%i_mhd_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, MHD_ctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, MHD_ctl%org_plt, c_buf)
!
        call sel_read_ctl_gen_shell_grids(id_control, hd_sph_shell,     &
     &      MHD_ctl%fname_psph_ctl, MHD_ctl%psph_ctl, c_buf)
!
        call read_sph_mhd_model                                         &
     &     (id_control, hd_model, MHD_ctl%model_ctl, c_buf)
        call read_sph_mhd_control                                       &
     &     (id_control, hd_control, MHD_ctl%smctl_ctl, c_buf)
!
        call read_monitor_data_ctl                                      &
     &     (id_control, hd_monitor_data, MHD_ctl%nmtr_ctl, c_buf)
        call read_sph_monitoring_ctl                                    &
     &     (id_control, hd_pick_sph, MHD_ctl%smonitor_ctl, c_buf)
!
        call s_read_surfacing_controls                                  &
     &     (id_control, hd_viz_ctl, add_SMHD_ctl%surfacing_ctls, c_buf)
!
        call read_dynamo_sects_control                                  &
     &    (id_control, hd_dynamo_viz_ctl, add_SMHD_ctl%zm_sects, c_buf)
        call read_dynamo_sects_control                                  &
     &    (id_control, hd_zm_viz_ctl, add_SMHD_ctl%zm_sects, c_buf)
      end do
      MHD_ctl%i_mhd_ctl = 1
!
      end subroutine read_sph_mhd_ctl_w_psf
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_mhd_ctl_w_psf(id_control, hd_block,          &
     &                                   MHD_ctl, add_SMHD_ctl, level)
!
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use ctl_data_MHD_model_IO
      use control_data_surfacing_IO
      use ctl_file_gen_sph_shell_IO
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(mhd_simulation_control), intent(in) :: MHD_ctl
      type(add_psf_sph_mhd_ctl), intent(in) :: add_SMHD_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(MHD_ctl%i_mhd_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, MHD_ctl%plt, level)
      call write_control_platforms                                      &
     &   (id_control, hd_org_data, MHD_ctl%org_plt, level)
!
      call sel_write_ctl_gen_shell_grids(id_control, hd_sph_shell,      &
     &    MHD_ctl%fname_psph_ctl, MHD_ctl%psph_ctl, level)
!
      call write_sph_mhd_model                                          &
     &   (id_control, hd_model, MHD_ctl%model_ctl, level)
      call write_sph_mhd_control                                        &
     &   (id_control, hd_control, MHD_ctl%smctl_ctl, level)
!
      call write_monitor_data_ctl                                       &
     &   (id_control, hd_monitor_data, MHD_ctl%nmtr_ctl, level)
      call write_sph_monitoring_ctl                                     &
     &   (id_control, hd_pick_sph, MHD_ctl%smonitor_ctl, level)
!
      call write_surfacing_controls                                     &
     &   (id_control, hd_viz_ctl, add_SMHD_ctl%surfacing_ctls, level)
!
      call write_dynamo_sects_control                                   &
     &   (id_control, hd_dynamo_viz_ctl, add_SMHD_ctl%zm_sects, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_mhd_ctl_w_psf
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_mhd_ctl_w_psf(add_SMHD_ctl)
!
      type(add_psf_sph_mhd_ctl), intent(inout) :: add_SMHD_ctl
!
!
      call dealloc_surfacing_controls(add_SMHD_ctl%surfacing_ctls)
      call dealloc_dynamo_sects_control(add_SMHD_ctl%zm_sects)
!
      end subroutine dealloc_sph_mhd_ctl_w_psf
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_sph_MHD_w_psf
