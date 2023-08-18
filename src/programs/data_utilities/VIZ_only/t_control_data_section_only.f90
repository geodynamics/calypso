!>@file   t_control_data_section_only.f90
!!@brief  module t_control_data_section_only
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control data of node monitoring
!!
!!@verbatim
!!      subroutine read_control_file_section_only(file_name,            &
!!     &                                          sec_viz_ctl, c_buf)
!!      subroutine write_control_file_section_only(file_name,           &
!!     &                                           sec_viz_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!      subroutine dealloc_section_control_data(sec_viz_ctl)
!!        type(control_data_section_only), intent(inout) :: sec_viz_ctl
!!
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin visualizer
!!    begin data_files_def
!!      ...
!!    end data_files_def
!!
!!    begin time_step_ctl
!!      ...
!!    end time_step_ctl
!!
!!    begin visual_control
!!      ...
!!    end  visual_control
!!  end  visualizer
!!
!!    -------------------------------------------------------------------
!!@endverbatim
!
      module t_control_data_section_only
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_control_data_surfacings
      use t_control_array_character3
!
      implicit  none
!
!
      integer(kind = kint), parameter :: viz_ctl_file_code = 11
!
!>      Structure of control data for sectioning only
      type control_data_section_only
!>        Control block name
        character(len = kchara) :: block_name = 'visualizer'
!
!>      Structure for file settings
        type(platform_data_control) :: sect_plt
!>      Structure for time stepping control
        type(time_data_control) :: t_sect_ctl
!
!>        Structures of visualization controls
        type(surfacing_controls) :: surfacing_ctls
!
!>        Structures of field used in visualization
        type(ctl_array_c3) :: viz_field_ctl
!
        integer (kind=kint) :: i_viz_only_file = 0
      end type control_data_section_only
!
!     top level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_only_file = 'visualizer'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_time_step = 'time_step_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_ctl = 'visual_control'
!
      private :: viz_ctl_file_code
!
      private :: read_section_control_data, write_section_control_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_file_section_only(file_name,              &
     &                                          sec_viz_ctl, c_buf)
!
      use skip_comment_f
      use t_control_data_surfacings
!
      character(len=kchara), intent(in) :: file_name
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      call init_section_control_label(hd_viz_only_file, sec_viz_ctl)
      open (viz_ctl_file_code, file=file_name, status='old' )
      do
        call load_one_line_from_control                                 &
     &     (viz_ctl_file_code, hd_viz_only_file, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_section_control_data                                  &
     &     (viz_ctl_file_code, hd_viz_only_file, sec_viz_ctl, c_buf)
        if(sec_viz_ctl%i_viz_only_file .gt. 0) exit
      end do
      close(viz_ctl_file_code)
!
      c_buf%level = c_buf%level - 1
      if(c_buf%iend .gt. 0) return
!
      call section_step_ctls_to_time_ctl                                &
     &   (sec_viz_ctl%surfacing_ctls, sec_viz_ctl%t_sect_ctl)
!
      sec_viz_ctl%viz_field_ctl%num =  0
      call alloc_control_array_c3(sec_viz_ctl%viz_field_ctl)
      call add_fields_4_scts_to_fld_ctl(sec_viz_ctl%surfacing_ctls,     &
     &                                  sec_viz_ctl%viz_field_ctl)
!
      end subroutine read_control_file_section_only
!
!   --------------------------------------------------------------------
!
      subroutine write_control_file_section_only(file_name,             &
     &                                           sec_viz_ctl)
!
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(control_data_section_only), intent(in) :: sec_viz_ctl
!
      integer(kind = kint) :: level1
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      write(*,*) 'Write surfacing control file: ', trim(file_name)
      level1 = 0
      open (viz_ctl_file_code, file=file_name)
      call write_section_control_data                                   &
     &   (viz_ctl_file_code, hd_viz_only_file, sec_viz_ctl, level1)
      close(viz_ctl_file_code)
!
      end subroutine write_control_file_section_only
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_section_control_data                              &
     &         (id_control, hd_block, sec_viz_ctl, c_buf)
!
      use skip_comment_f
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
      use control_data_surfacing_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(sec_viz_ctl%i_viz_only_file .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, sec_viz_ctl%sect_plt, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, sec_viz_ctl%t_sect_ctl, c_buf)
!
        call s_read_surfacing_controls                                  &
     &     (id_control, hd_viz_ctl, sec_viz_ctl%surfacing_ctls, c_buf)
      end do
      sec_viz_ctl%i_viz_only_file = 1
!
      end subroutine read_section_control_data
!
!   --------------------------------------------------------------------
!
      subroutine write_section_control_data                             &
     &         (id_control, hd_block, sec_viz_ctl, level)
!
      use skip_comment_f
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
      use control_data_surfacing_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(control_data_section_only), intent(in) :: sec_viz_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(sec_viz_ctl%i_viz_only_file .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, sec_viz_ctl%sect_plt, level)
      call write_control_time_step_data                                 &
     &   (id_control, sec_viz_ctl%t_sect_ctl, level)
!
      call write_surfacing_controls                                     &
     &   (id_control, hd_viz_ctl, sec_viz_ctl%surfacing_ctls, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_section_control_data
!
!   --------------------------------------------------------------------
!
      subroutine init_section_control_label(hd_block, sec_viz_ctl)
!
      use skip_comment_f
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
      use control_data_surfacing_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
!
!
      sec_viz_ctl%block_name = hd_block
      call init_platforms_labels(hd_platform, sec_viz_ctl%sect_plt)
      call init_ctl_time_step_label(hd_time_step,                       &
     &                              sec_viz_ctl%t_sect_ctl)
      call init_surfacing_ctl_label(hd_viz_ctl,                         &
     &                              sec_viz_ctl%surfacing_ctls)
!
      end subroutine init_section_control_label
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_section_control_data(sec_viz_ctl)
!
      type(control_data_section_only), intent(inout) :: sec_viz_ctl
!
!
      call dealloc_control_array_c3(sec_viz_ctl%viz_field_ctl)
      call reset_control_platforms(sec_viz_ctl%sect_plt)
      call reset_ctl_data_4_time_step(sec_viz_ctl%t_sect_ctl)
!
      sec_viz_ctl%i_viz_only_file = 0
!
      end subroutine dealloc_section_control_data
!
!   --------------------------------------------------------------------
!
      end module t_control_data_section_only
