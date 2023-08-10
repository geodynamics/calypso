!>@file   t_ctl_data_node_monitor.f90
!!@brief  module t_ctl_data_node_monitor
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control data of node monitoring
!!
!!@verbatim
!!      subroutine init_monitor_data_ctl_label(hd_block, nmtr_ctl)
!!      subroutine read_monitor_data_ctl                                &
!!     &         (id_control, hd_block, nmtr_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(node_monitor_control), intent(inout) :: nmtr_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_monitor_data_ctl(id_control, nmtr_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(node_monitor_control), intent(in) :: nmtr_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_monitor_data_ctl(nmtr_ctl)
!!        type(node_monitor_control), intent(inout) :: nmtr_ctl
!!
!!   --------------------------------------------------------------------
!!
!!   control for monitor nodal data
!!
!!  begin monitor_data_ctl
!!    array monitor_position_list   1
!!      monitor_position_list     1.038e0  0.0e0   0.0e0
!!    end array monitor_position_list
!!
!!    array monitor_node_list   1
!!      monitor_node_list          2   132
!!    end array monitor_node_list
!!
!!    array monitor_grp_ctl   1
!!      monitor_grp_ctl     mid_equator
!!    end array monitor_grp_ctl
!!  end  monitor_data_ctl
!!
!!   --------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_node_monitor
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_integer2
      use t_control_array_real3
!
      implicit  none
!
!
      type node_monitor_control
!>        Block name
        character(len=kchara) :: block_name = 'monitor_data_ctl'
!>        Structure for monitoring plave list
!!@n       xx_4_monitor_ctl%vec1: X position
!!@n       xx_4_monitor_ctl%vec2: Y position
!!@n       xx_4_monitor_ctl%vec3: Z position
        type(ctl_array_r3) :: xx_4_monitor_ctl
!>        Structure for monitoring plave list
!!@n       node_4_monitor_ctl%int1: domain ID for monitor
!!@n       node_4_monitor_ctl%int2: local node ID for monitor
        type(ctl_array_i2) :: node_4_monitor_ctl
!>        Structure for monitoring plave list
!!@n       group_4_monitor_ctl%c_tbl: Name of node group to monitor field
        type(ctl_array_chara) :: group_4_monitor_ctl
!
        integer (kind=kint) :: i_monitor_data = 0
      end type node_monitor_control
!
!   3rd level for monitor data
!
      character(len=kchara) :: hd_monitor_position                      &
     &                                     = 'monitor_position_list'
      character(len=kchara) :: hd_monitor_node = 'monitor_node_list'
      character(len=kchara) :: hd_monitor_grp =  'monitor_grp_ctl'
!
      private :: hd_monitor_grp, hd_monitor_position, hd_monitor_node
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_monitor_data_ctl                                  &
     &         (id_control, hd_block, nmtr_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(node_monitor_control), intent(inout) :: nmtr_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(nmtr_ctl%i_monitor_data .gt. 0) return
      nmtr_ctl%block_name = hd_block
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control,                          &
     &      hd_monitor_grp, nmtr_ctl%group_4_monitor_ctl, c_buf)
        call read_control_array_r3(id_control,                          &
     &      hd_monitor_position, nmtr_ctl%xx_4_monitor_ctl, c_buf)
        call read_control_array_i2(id_control,                          &
     &      hd_monitor_node, nmtr_ctl%node_4_monitor_ctl, c_buf)
      end do
      nmtr_ctl%i_monitor_data = 1
!
      end subroutine read_monitor_data_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_monitor_data_ctl(id_control, nmtr_ctl, level)
!
      use t_read_control_elements
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(node_monitor_control), intent(in) :: nmtr_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(nmtr_ctl%i_monitor_data .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 nmtr_ctl%block_name)
      call write_control_array_c1(id_control, level,                    &
     &    nmtr_ctl%group_4_monitor_ctl)
      call write_control_array_r3(id_control, level,                    &
     &    nmtr_ctl%xx_4_monitor_ctl)
      call write_control_array_i2(id_control, level,                    &
     &    nmtr_ctl%node_4_monitor_ctl)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                 nmtr_ctl%block_name)
!
      end subroutine write_monitor_data_ctl
!
!   --------------------------------------------------------------------
!
      subroutine init_monitor_data_ctl_label(hd_block, nmtr_ctl)
      character(len=kchara), intent(in) :: hd_block
      type(node_monitor_control), intent(inout) :: nmtr_ctl
!
      nmtr_ctl%block_name = hd_block
!
        call init_chara_ctl_array_label                                 &
     &     (hd_monitor_grp, nmtr_ctl%group_4_monitor_ctl)
        call init_r3_ctl_array_label                                    &
     &     (hd_monitor_position, nmtr_ctl%xx_4_monitor_ctl)
        call init_int2_ctl_array_label                                  &
     &     (hd_monitor_node, nmtr_ctl%node_4_monitor_ctl)
!
      end subroutine init_monitor_data_ctl_label
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_monitor_data_ctl(nmtr_ctl)
!
      type(node_monitor_control), intent(inout) :: nmtr_ctl
!
!
      call dealloc_control_array_chara(nmtr_ctl%group_4_monitor_ctl)
      call dealloc_control_array_r3(nmtr_ctl%xx_4_monitor_ctl)
      call dealloc_control_array_i2(nmtr_ctl%node_4_monitor_ctl)
      nmtr_ctl%i_monitor_data = 0
!
      end subroutine dealloc_monitor_data_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_node_monitor
