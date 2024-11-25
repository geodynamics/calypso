!>@file   t_control_data_four_vizs.f90
!!@brief  module t_control_data_four_vizs
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!
!>@brief Control data for visualization without repartitioning
!!
!!@verbatim
!!      subroutine read_control_file_four_vizs(file_name, viz4_c, c_buf)
!!      subroutine write_control_file_four_vizs(file_name, viz4_c)
!!      subroutine dealloc_four_vizs_control_data(viz4_c)
!!        character(len = kchara), intent(in) :: file_name
!!        type(control_data_four_vizs), intent(inout) :: viz4_c
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
!!    -------------------------------------------------------------------
!!@endverbatim
!
      module t_control_data_four_vizs
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_control_data_viz4
      use t_control_array_character3
!
      implicit  none
!
!
      integer(kind = kint), parameter :: viz_ctl_file_code = 11
!
!>      Structure for visulization program
      type control_data_four_vizs
!>        Block name
        character(len=kchara) :: block_name = 'visualizer'
!>        Structure for file settings
        type(platform_data_control) :: viz_plt
!>        Structure for time stepping control
        type(time_data_control) :: t_viz_ctl
!
!>        Structures of visualization controls
        type(vis4_controls) :: viz4_ctl
!
!>        Structures of field used in visualization
        type(ctl_array_c3) :: viz_field_ctl
!
        integer(kind=kint) :: i_viz_only_file = 0
      end type control_data_four_vizs
!
!     Top level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_only_file = 'visualizer'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_time_step = 'time_step_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_control = 'visual_control'
!
      private :: viz_ctl_file_code
      private :: read_four_vizs_control_data
      private :: write_four_vizs_control_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_file_four_vizs(file_name, viz4_c, c_buf)
!
      use skip_comment_f
      use viz4_step_ctls_to_time_ctl
!
      character(len = kchara), intent(in) :: file_name
      type(control_data_four_vizs), intent(inout) :: viz4_c
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open (viz_ctl_file_code, file=file_name, status='old')
      do
        call load_one_line_from_control(viz_ctl_file_code,              &
     &                                  hd_viz_only_file, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_four_vizs_control_data                                &
     &     (viz_ctl_file_code, hd_viz_only_file, viz4_c, c_buf)
        if(viz4_c%i_viz_only_file .gt. 0) exit
      end do
      close(viz_ctl_file_code)
!
      c_buf%level = c_buf%level - 1
      if(c_buf%iend .gt. 0) return
!
      call s_viz4_step_ctls_to_time_ctl                                 &
     &   (viz4_c%viz4_ctl, viz4_c%t_viz_ctl)
!
      viz4_c%viz_field_ctl%num =  0
      call alloc_control_array_c3(viz4_c%viz_field_ctl)
      call add_fields_viz4_to_fld_ctl(viz4_c%viz4_ctl,                  &
     &                                viz4_c%viz_field_ctl)
!
      end subroutine read_control_file_four_vizs
!
!   --------------------------------------------------------------------
!
      subroutine write_control_file_four_vizs(file_name, viz4_c)
!
      use delete_data_files
!
      character(len = kchara), intent(in) :: file_name
      type(control_data_four_vizs), intent(inout) :: viz4_c
      integer(kind = kint) :: level1
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      open (viz_ctl_file_code, file=file_name)
      level1 = 0
      call write_four_vizs_control_data                                 &
     &   (viz_ctl_file_code, hd_viz_only_file, viz4_c, level1)
      close(viz_ctl_file_code)
!
      end subroutine write_control_file_four_vizs
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_four_vizs_control_data                            &
     &         (id_control, hd_block, viz4_c, c_buf)
!
      use skip_comment_f
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
      use ctl_data_four_vizs_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_four_vizs), intent(inout) :: viz4_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(viz4_c%i_viz_only_file .gt. 0) return
      call init_platforms_labels(hd_platform, viz4_c%viz_plt)
      call init_ctl_time_step_label(hd_time_step, viz4_c%t_viz_ctl)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, viz4_c%viz_plt, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, viz4_c%t_viz_ctl, c_buf)
!
        call s_read_viz4_controls(id_control, hd_viz_control,           &
     &                            viz4_c%viz4_ctl, c_buf)
      end do
      viz4_c%i_viz_only_file = 1
!
      end subroutine read_four_vizs_control_data
!
!   --------------------------------------------------------------------
!
      subroutine write_four_vizs_control_data                           &
     &         (id_control, hd_block, viz4_c, level)
!
      use skip_comment_f
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
      use ctl_data_four_vizs_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(control_data_four_vizs), intent(in) :: viz4_c
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(viz4_c%i_viz_only_file .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, viz4_c%viz_plt, level)
      call write_control_time_step_data                                 &
     &   (id_control, viz4_c%t_viz_ctl, level)
!
      call write_viz4_controls(id_control, viz4_c%viz4_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_four_vizs_control_data
!
!   --------------------------------------------------------------------
!
      subroutine init_four_vizs_control_label(hd_block, viz4_c)
!
      use ctl_data_platforms_IO
      use ctl_data_4_time_steps_IO
      use ctl_data_four_vizs_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(control_data_four_vizs), intent(inout) :: viz4_c
!
!
      viz4_c%block_name = hd_block
      call init_platforms_labels(hd_platform, viz4_c%viz_plt)
      call init_ctl_time_step_label(hd_time_step, viz4_c%t_viz_ctl)
      call init_viz4_ctl_label(hd_viz_control, viz4_c%viz4_ctl)
!
      end subroutine init_four_vizs_control_label
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_four_vizs_control_data(viz4_c)
!
      type(control_data_four_vizs), intent(inout) :: viz4_c
!
      call dealloc_control_array_c3(viz4_c%viz_field_ctl)
      call reset_control_platforms(viz4_c%viz_plt)
      call reset_ctl_data_4_time_step(viz4_c%t_viz_ctl)
!
      viz4_c%t_viz_ctl%i_tstep = 0
      viz4_c%i_viz_only_file =   0
!
      end subroutine dealloc_four_vizs_control_data
!
!   --------------------------------------------------------------------
!
      end module t_control_data_four_vizs
