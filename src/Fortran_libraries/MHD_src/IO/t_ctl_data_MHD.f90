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
!!      subroutine read_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!!      subroutine write_sph_mhd_ctl_noviz                              &
!!     &         (id_control, hd_block, MHD_ctl, level)
!!        character(len=kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_simulation_control), intent(in) :: MHD_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_sph_mhd_ctl_data(MHD_ctl)
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
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
!
      implicit none
!
      integer(kind=kint), parameter, private :: id_control_file = 11
!
      type mhd_simulation_control
!>        Block name
        character(len=kchara) :: block_name = 'MHD_control'
!
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Control structure for orginal file informations
        type(platform_data_control) :: org_plt
!>        Control structure for new file informations
        type(platform_data_control) :: new_plt
!
!>        file name for parallel spherical shell control
        character(len = kchara) :: fname_psph = 'NO_FILE'
!>        Control structure for parallel spherical shell
        type(parallel_sph_shell_control) :: psph_ctl
!
!>        Control structure for MHD/model
        type(mhd_model_control) :: model_ctl
!>        Control structure for MHD/control
        type(sph_mhd_control_control) :: smctl_ctl
!
!>        Structure for spectr monitoring control
        type(sph_monitor_control) :: smonitor_ctl
!>        Structure for monitoring node list
        type(node_monitor_control) :: nmtr_ctl
!
        integer (kind=kint) :: i_mhd_ctl = 0
      end type mhd_simulation_control
!
!
      character(len=kchara), parameter, private                         &
     &                                 :: hd_mhd_ctl = 'MHD_control'
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
      private :: read_sph_mhd_ctl_noviz, init_sph_mhd_ctl_noviz_label
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_sph_MHD_noviz                           &
     &         (file_name, MHD_ctl, c_buf)
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      call init_sph_mhd_ctl_noviz_label(hd_mhd_ctl, MHD_ctl)
      open(id_control_file, file = file_name, status='old' )
!
      do
        call load_one_line_from_control(id_control_file,                &
     &                                  hd_mhd_ctl, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_sph_mhd_ctl_noviz                                     &
     &     (id_control_file, hd_mhd_ctl, MHD_ctl, c_buf)
        if(MHD_ctl%i_mhd_ctl .gt. 0) exit
      end do
      close(id_control_file)
      c_buf%level = c_buf%level - 1
!
      end subroutine read_control_4_sph_MHD_noviz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_control_4_sph_MHD_noviz(file_name, MHD_ctl)
!
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(mhd_simulation_control), intent(in) :: MHD_ctl
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
      call write_sph_mhd_ctl_noviz                                      &
     &   (id_control_file, MHD_ctl%block_name, MHD_ctl, level1)
      close(id_control_file)
!
      end subroutine write_control_4_sph_MHD_noviz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_ctl_noviz                                 &
     &         (id_control, hd_block, MHD_ctl, c_buf)
!
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use ctl_data_MHD_model_IO
      use ctl_file_gen_sph_shell_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(MHD_ctl%i_mhd_ctl .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
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
     &      MHD_ctl%fname_psph, MHD_ctl%psph_ctl, c_buf)
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
      end do
      MHD_ctl%i_mhd_ctl = 1
!
      end subroutine read_sph_mhd_ctl_noviz
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_mhd_ctl_noviz                                &
     &         (id_control, hd_block, MHD_ctl, level)
!
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use ctl_data_MHD_model_IO
      use ctl_file_gen_sph_shell_IO
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(mhd_simulation_control), intent(in) :: MHD_ctl
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
      call sel_write_ctl_gen_shell_grids(id_control,                    &
     &    MHD_ctl%fname_psph, MHD_ctl%psph_ctl, level)
!
      call write_sph_mhd_model(id_control, MHD_ctl%model_ctl, level)
      call write_sph_mhd_control(id_control, MHD_ctl%smctl_ctl, level)
!
      call write_monitor_data_ctl(id_control, MHD_ctl%nmtr_ctl, level)
      call write_sph_monitoring_ctl                                     &
     &   (id_control, MHD_ctl%smonitor_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_mhd_ctl_noviz
!
!   --------------------------------------------------------------------
!
      subroutine init_sph_mhd_ctl_noviz_label(hd_block, MHD_ctl)
!
      use ctl_data_platforms_IO
      use ctl_data_sph_monitor_IO
      use ctl_data_MHD_model_IO
      use ctl_file_gen_sph_shell_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
!
!
      MHD_ctl%block_name = trim(hd_block)
      call init_platforms_labels(hd_platform, MHD_ctl%plt)
      call init_platforms_labels(hd_org_data, MHD_ctl%org_plt)
      call init_parallel_shell_ctl_label(hd_sph_shell,                  &
     &                                   MHD_ctl%psph_ctl)
      call init_sph_mhd_model_label(hd_model, MHD_ctl%model_ctl)
      call init_sph_mhd_control_label(hd_control, MHD_ctl%smctl_ctl)
      call init_sph_monitoring_labels(hd_pick_sph,                      &
     &                                MHD_ctl%smonitor_ctl)
      call init_monitor_data_ctl_label(hd_monitor_data,                 &
     &                                 MHD_ctl%nmtr_ctl)
!
      end subroutine init_sph_mhd_ctl_noviz_label
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_mhd_ctl_data(MHD_ctl)
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
!
!
      call reset_control_platforms(MHD_ctl%plt)
      call reset_control_platforms(MHD_ctl%org_plt)
!
      call dealloc_sph_mhd_model(MHD_ctl%model_ctl)
      call reset_sph_mhd_control(MHD_ctl%smctl_ctl)
!
      call dealloc_parallel_shell_ctl(MHD_ctl%psph_ctl)
!
      call dealloc_monitor_data_ctl(MHD_ctl%nmtr_ctl)
      call dealloc_sph_monitoring_ctl(MHD_ctl%smonitor_ctl)
!
      MHD_ctl%i_mhd_ctl = 0
!
      end subroutine dealloc_sph_mhd_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_MHD
