!>@file   m_solver_SR.f90
!!@brief  module m_solver_SR
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!
!>@brief  Work area for data communications
!!
!!@verbatim
!!@endverbatim
!!
!!@n @param  NB           Number of components
!!@n @param  NTOT_SEND    Total number of data points for export
!!@n @param  NTOT_RECV    Total number of data points for import
!!@n @param  NPE_SEND      Number of processses to receive
!!@n @param  NPE_RECV      Number of processses to send
!!
!!@n @param  N_SHIFT      number of shifting of the reversed import table
!!@n @param  ITEM_IMPORT  import table
!!@n @param  REV_IMPORT   reversed import table
!
      module m_solver_SR
!
      use m_precision
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
!
      implicit none
!
!>      Structure of communication flags
      type(send_recv_status), save :: SR_sig1
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), save :: SR_r1
!
!>      Structure of communication buffer for 4-byte integer
      type(send_recv_int_buffer), save :: SR_i1
!>      Structure of communication buffer for 8-byte integer
      type(send_recv_int8_buffer), save :: SR_il1
!
!
      end module m_solver_SR
