!>@file   t_ctl_data_mhd_restart.f90
!!@brief  module t_ctl_data_mhd_restart
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief data structure for restart data control block
!!
!!@verbatim
!!      subroutine init_restart_ctl_label(hd_block, mr_ctl)
!!      subroutine read_restart_ctl(id_control, hd_block, mr_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_restart_control), intent(inout) :: mr_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_restart_ctl(id_control, mr_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(mhd_restart_control), intent(in) :: mr_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine bcast_restart_ctl(mr_ctl)
!!      subroutine reset_restart_ctl(mr_ctl)
!!        type(mhd_restart_control), intent(inout) :: mr_ctl
!! !!!!  control for initial and restart data  !!!!!!!!!!!!!!!!!!!!!!!!!!
!!   no_data:             No initial values
!!   start_from_rst_file: Read restart data as initial values
!!
!!   dynamo_benchmark_0: Initial values for dynamo benchmark Case 0
!!   dynamo_benchmark_1: Initial values for dynamo benchmark Case 1
!!   dynamo_benchmark_2: Initial values for dynamo benchmark Case 1
!!
!!   pseudo_vacuum_benchmark: Initial values for pseudo vacuum benchmark
!!
!!   rotate_x: rotate around x-axis
!!   rotate_y: rotate around y-axis
!!   rotate_z: rotate around z-axis
!!
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin restart_file_ctl
!!     rst_ctl                start_from_rst_file
!!    end restart_file_ctl
!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_mhd_restart
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
!
      implicit  none
!
!
!>   control flage for restart data
      type mhd_restart_control
!>        Block name
        character(len=kchara) :: block_name = 'restart_file_ctl'
!>        Initial data type control
        type(read_character_item) :: restart_flag_ctl
!
        integer (kind=kint) :: i_restart_file =   0
      end type mhd_restart_control
!
!    4th level for restart
!
      character(len=kchara), parameter, private                         &
     &                     :: hd_rst_flag = 'rst_ctl'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_restart_ctl(id_control, hd_block, mr_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_restart_control), intent(inout) :: mr_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(mr_ctl%i_restart_file .gt. 0) return
      mr_ctl%block_name = hd_block
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_rst_flag,                    &
     &                           mr_ctl%restart_flag_ctl)
      end do
      mr_ctl%i_restart_file = 1
!
      end subroutine read_restart_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_restart_ctl(id_control, mr_ctl, level)
!
      use t_read_control_elements
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(mhd_restart_control), intent(in) :: mr_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(mr_ctl%i_restart_file .le. 0) return
      maxlen = len_trim(hd_rst_flag)
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 mr_ctl%block_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          mr_ctl%restart_flag_ctl)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                mr_ctl%block_name)
!
      end subroutine write_restart_ctl
!
!   --------------------------------------------------------------------
!
      subroutine init_restart_ctl_label(hd_block, mr_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(mhd_restart_control), intent(inout) :: mr_ctl
!
!
      mr_ctl%block_name = hd_block
        call init_chara_ctl_item_label(hd_rst_flag,                     &
     &                           mr_ctl%restart_flag_ctl)
!
      end subroutine init_restart_ctl_label
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine reset_restart_ctl(mr_ctl)
!
      type(mhd_restart_control), intent(inout) :: mr_ctl
!
      mr_ctl%restart_flag_ctl%iflag = 0
      mr_ctl%i_restart_file = 0
!
      end subroutine reset_restart_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_mhd_restart
