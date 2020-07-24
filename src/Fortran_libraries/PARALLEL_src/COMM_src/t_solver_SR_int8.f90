!>@file   t_solver_SR_int8.f90
!!@brief  module t_solver_SR_int8
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!!@n     Modified in July, 2020
!
!>@brief  Work area for 8-byte integer data communications
!!
!!@verbatim
!!      subroutine resize_i8work_SR(NPE_SEND, NPE_RECV,                 &
!!     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_il)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!
!!      subroutine resize_i8work_itp_SR_t                               &
!!     &         (NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_il)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
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
      module t_solver_SR_int8
!
      use m_precision
      use calypso_mpi
      use t_solver_SR
!
      implicit none
!
!
!>      Structure of communication buffer for 8-byte integer
      type send_recv_int8_buffer
!>        size of send buffer
        integer(kind = kint) :: n_i8WS = 0
!>        size of recieve buffer
        integer(kind = kint) :: n_i8WR = 0
!
!>        work array for 8 byte integer send buffer
        integer(kind = kint_gl), allocatable :: i8WS(:)
!>        work array for 8 byte integer recieve buffer
        integer(kind = kint_gl), allocatable :: i8WR(:)
      end type send_recv_int8_buffer
!
!
      private :: resize_i8send_SR, resize_i8recv_SR
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine resize_i8work_SR(NPE_SEND, NPE_RECV,                   &
     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_il)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NTOT_SEND, NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      call resize_SR_flag(NPE_SEND, NPE_RECV, SR_sig)
      call resize_i8send_SR(NTOT_SEND+1, SR_il)
      call resize_i8recv_SR(NTOT_RECV+1, SR_il)
!
      end subroutine resize_i8work_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_i8work_itp_SR_t                                 &
     &         (NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_il)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      call resize_SR_flag(NPE_SEND, NPE_RECV, SR_sig)
      call resize_i8recv_SR(NTOT_RECV, SR_il)
!
      end subroutine resize_i8work_itp_SR_t
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_i8send_SR(NTOT_SEND, SR_il)
!
      integer(kind=kint), intent(in) :: NTOT_SEND
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
      if(allocated(SR_il%i8WS)                                          &
     &    .and. (size(SR_il%i8WS) .lt. (NTOT_SEND)) ) then
        deallocate (SR_il%i8WS)
        SR_il%n_i8WS = -1
      end if
      if(allocated(SR_il%i8WS) .eqv. .false.) then
        allocate (SR_il%i8WS(NTOT_SEND))
        SR_il%n_i8WS = size(SR_il%i8WS)
     end if
!
      end subroutine resize_i8send_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_i8recv_SR(NTOT_RECV, SR_il)
!
      integer(kind=kint), intent(in) :: NTOT_RECV
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      if(allocated(SR_il%i8WR)                                          &
     &   .and. (size(SR_il%i8WR).lt.(NTOT_RECV))) then
        deallocate (SR_il%i8WR)
        SR_il%n_i8WR = -1
      end if
      if(allocated(SR_il%i8WR) .eqv. .false.) then
        allocate (SR_il%i8WR(NTOT_RECV))
        SR_il%n_i8WR = size(SR_il%i8WR)
      end if
!
      end subroutine resize_i8recv_SR
!
! ----------------------------------------------------------------------
!
      end module t_solver_SR_int8
