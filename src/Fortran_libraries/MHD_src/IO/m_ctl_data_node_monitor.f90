!m_ctl_data_node_monitor.f90
!      module m_ctl_data_node_monitor
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine dealloc_monitor_grp_ctl
!      subroutine read_monitor_data_ctl
!
!   --------------------------------------------------------------------
!
!!!!!   control for monitor nodal data
!  begin monitor_data_ctl
!    array monitor_grp_ctl   1
!      monitor_grp_ctl     mid_equator   end
!    end array
!  end  monitor_data_ctl
!
!   --------------------------------------------------------------------
!
      module m_ctl_data_node_monitor
!
      use m_precision
!
      implicit  none
!
!
      integer(kind=kint) :: num_monitor_ctl
      character (len=kchara), allocatable :: monitor_grp_ctl(:)
!
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
      integer (kind=kint) :: i_monitor_grp = 0
!
      private :: hd_monitor_data, i_monitor_data, hd_monitor_grp
      private :: alloc_monitor_grp_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_monitor_grp_ctl
!
      allocate(monitor_grp_ctl(num_monitor_ctl))
!
      end subroutine alloc_monitor_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_monitor_grp_ctl
!
      deallocate(monitor_grp_ctl)
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
        call find_control_array_flag(hd_monitor_grp, num_monitor_ctl)
        if (num_monitor_ctl.gt.0 .and. i_monitor_grp.eq.0) then
          call alloc_monitor_grp_ctl
          call read_control_array_chara_list(hd_monitor_grp,            &
     &        num_monitor_ctl, i_monitor_grp, monitor_grp_ctl)
        end if
      end do
!
      end subroutine read_monitor_data_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_node_monitor
