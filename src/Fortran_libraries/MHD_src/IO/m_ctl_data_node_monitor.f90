!m_ctl_data_node_monitor.f90
!      module m_ctl_data_node_monitor
!
!        programmed by H.Matsui on March. 2006
!
!!      subroutine dealloc_monitor_grp_ctl
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
      module m_ctl_data_node_monitor
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
!
!>      Structure for monitoring plave list
!!@n      group_4_monitor_ctl%c_tbl: Name of node group to monitor field
      type(ctl_array_chara), save :: group_4_monitor_ctl
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_monitor_data = 'monitor_data_ctl'
      integer (kind=kint) :: i_monitor_data = 0
!
!   3rd level for monitor data
!
      character(len=kchara) :: hd_monitor_grp = 'monitor_grp_ctl'
!
      private :: hd_monitor_data, i_monitor_data, hd_monitor_grp
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_monitor_grp_ctl
!
      call dealloc_control_array_chara(group_4_monitor_ctl)
!
      end subroutine dealloc_monitor_grp_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_monitor_data_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_monitor_data) .eq. 0) return
      if (i_monitor_data .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_monitor_data, i_monitor_data)
        if(i_monitor_data .gt. 0) exit
!
        call read_control_array_c1                                      &
     &     (hd_monitor_grp, group_4_monitor_ctl)
      end do
!
      end subroutine read_monitor_data_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_node_monitor
