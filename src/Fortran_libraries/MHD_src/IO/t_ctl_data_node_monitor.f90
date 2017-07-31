!t_ctl_data_node_monitor.f90
!      module t_ctl_data_node_monitor
!
!        programmed by H.Matsui on March. 2006
!
!!      subroutine read_monitor_data_ctl
!!
!!   --------------------------------------------------------------------
!!
!!   control for monitor nodal data
!!
!!  begin monitor_data_ctl
!!    array monitor_grp_ctl   1
!!      monitor_grp_ctl     mid_equator   end
!!    end array
!!  end  monitor_data_ctl
!!
!!   --------------------------------------------------------------------
!
      module t_ctl_data_node_monitor
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
!
      type node_monitor_control
!>        Structure for monitoring plave list
!!@n       group_4_monitor_ctl%c_tbl: Name of node group to monitor field
        type(ctl_array_chara) :: group_4_monitor_ctl
      end type node_monitor_control
!
!   3rd level for monitor data
!
      character(len=kchara) :: hd_monitor_grp = 'monitor_grp_ctl'
!
      private :: hd_monitor_grp
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_monitor_data_ctl(hd_block, iflag, nmtr_ctl)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(node_monitor_control), intent(inout) :: nmtr_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_block, iflag)
        if(iflag .gt. 0) exit
!
        call read_control_array_c1                                      &
     &     (hd_monitor_grp, nmtr_ctl%group_4_monitor_ctl)
      end do
!
      end subroutine read_monitor_data_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_monitor_data_ctl(nmtr_ctl)
!
      use bcast_control_arrays
!
      type(node_monitor_control), intent(inout) :: nmtr_ctl
!
!
      call bcast_ctl_array_c1(nmtr_ctl%group_4_monitor_ctl)
!
      end subroutine bcast_monitor_data_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_node_monitor
