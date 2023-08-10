!>@file   bcast_monitor_data_ctl.f90
!!@brief  module bcast_monitor_data_ctl
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control data of node monitoring
!!
!!@verbatim
!!      subroutine bcast_node_monitor_data_ctl(nmtr_ctl)
!!        type(node_monitor_control), intent(inout) :: nmtr_ctl
!!@endverbatim
!
      module bcast_monitor_data_ctl
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_node_monitor
!
      use calypso_mpi
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine bcast_node_monitor_data_ctl(nmtr_ctl)
!
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(node_monitor_control), intent(inout) :: nmtr_ctl
!
!
      call bcast_ctl_array_c1(nmtr_ctl%group_4_monitor_ctl)
      call bcast_ctl_array_r3(nmtr_ctl%xx_4_monitor_ctl)
      call bcast_ctl_array_i2(nmtr_ctl%node_4_monitor_ctl)
!
      call calypso_mpi_bcast_character(nmtr_ctl%block_name,             &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(nmtr_ctl%i_monitor_data, 0)
!
      end subroutine bcast_node_monitor_data_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_monitor_data_ctl
