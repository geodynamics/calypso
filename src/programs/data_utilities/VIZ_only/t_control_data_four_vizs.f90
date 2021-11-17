!>@file   t_control_data_four_vizs.f90
!!@brief  module t_control_data_four_vizs
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!
!>@brief Control data for visualization without repartitioning
!!
!!@verbatim
!!      subroutine read_control_file_four_vizs(viz4_c)
!!      subroutine dealloc_four_vizs_control_data(viz4_c)
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
      character(len = kchara), parameter :: fname_viz_ctl = "ctl_viz"
!
!>      Structure for visulization program
      type control_data_four_vizs
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
      private :: viz_ctl_file_code, fname_viz_ctl
      private :: read_four_vizs_control_data
      private :: bcast_four_vizs_control_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_file_four_vizs(viz4_c)
!
      use skip_comment_f
      use viz4_step_ctls_to_time_ctl
!
      type(control_data_four_vizs), intent(inout) :: viz4_c
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open (viz_ctl_file_code, file=fname_viz_ctl, status='old' )
        do
          call load_one_line_from_control(viz_ctl_file_code, c_buf1)
          call read_four_vizs_control_data                              &
     &       (viz_ctl_file_code, hd_viz_only_file, viz4_c, c_buf1)
          if(viz4_c%i_viz_only_file .gt. 0) exit
        end do
        close(viz_ctl_file_code)
!
        call s_viz4_step_ctls_to_time_ctl                               &
     &     (viz4_c%viz4_ctl, viz4_c%t_viz_ctl)
!
        viz4_c%viz_field_ctl%num =  0
        call alloc_control_array_c3(viz4_c%viz_field_ctl)
        call add_fields_viz4_to_fld_ctl(viz4_c%viz4_ctl,                &
     &                                  viz4_c%viz_field_ctl)
      end if
!
      call bcast_four_vizs_control_data(viz4_c)
!
      end subroutine read_control_file_four_vizs
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_four_vizs_control_data                            &
     &         (id_control, hd_block, viz4_c, c_buf)
!
      use skip_comment_f
      use read_four_viz_controls
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_four_vizs), intent(inout) :: viz4_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(viz4_c%i_viz_only_file .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
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
      subroutine bcast_four_vizs_control_data(viz4_c)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_control_arrays
!
      type(control_data_four_vizs), intent(inout) :: viz4_c
!
!
      call bcast_ctl_array_c3(viz4_c%viz_field_ctl)
      call bcast_ctl_data_4_platform(viz4_c%viz_plt)
      call bcast_ctl_data_4_time_step(viz4_c%t_viz_ctl)
!
      call bcast_viz4_controls(viz4_c%viz4_ctl)
!
      call calypso_mpi_bcast_one_int(viz4_c%i_viz_only_file, 0)
!
      end subroutine bcast_four_vizs_control_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_four_vizs_control_data(viz4_c)
!
      use bcast_4_time_step_ctl
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
!   --------------------------------------------------------------------
!
      end module t_control_data_four_vizs
