!>@file   t_solver_SR_int.f90
!!@brief  module t_solver_SR_int
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!!@n     Modified in July, 2020
!
!>@brief  Work area for data communications
!!
!!@verbatim
!!      subroutine resize_iwork_SR_t(NPE_SEND, NPE_RECV,                &
!!     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_i)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!
!!      subroutine resize_iwork_itp_SR                                  &
!!     &         (NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_i)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
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
      module t_solver_SR_int
!
      use m_precision
      use calypso_mpi
      use t_solver_SR
!
      implicit none
!
!>      Structure of communication buffer for integer
      type send_recv_int_buffer
!>        size of send buffer
        integer(kind = kint) :: n_iWS = 0
!>        size of recieve buffer
        integer(kind = kint) :: n_iWR = 0
!
!>        work array for integer send buffer
        integer(kind = kint), allocatable :: iWS(:)
!>        work array for integer recieve buffer
        integer(kind = kint), allocatable :: iWR(:)
      end type send_recv_int_buffer
!
!
      private :: resize_isend_SR, resize_irecv_SR
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine resize_iwork_SR_t(NPE_SEND, NPE_RECV,                  &
     &          NTOT_SEND, NTOT_RECV, SR_sig, SR_i)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NTOT_SEND, NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call resize_SR_flag(NPE_SEND, NPE_RECV, SR_sig)
      call resize_isend_SR(NTOT_SEND+1, SR_i)
      call resize_irecv_SR(NTOT_RECV+1, SR_i)
!
      end subroutine resize_iwork_SR_t
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_iwork_itp_SR                                    &
     &         (NPE_SEND, NPE_RECV, NTOT_RECV, SR_sig, SR_i)
!
      integer(kind = kint), intent(in) ::  NPE_SEND, NPE_RECV
      integer(kind = kint), intent(in) ::  NTOT_RECV
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      call resize_SR_flag(NPE_SEND, NPE_RECV, SR_sig)
      call resize_irecv_SR(NTOT_RECV, SR_i)
!
      end subroutine resize_iwork_itp_SR
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_isend_SR(NTOT_SEND, SR_i)
!
      integer(kind=kint), intent(in) :: NTOT_SEND
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      if(allocated(SR_i%iWS)                                            &
     &      .and. (size(SR_i%iWS) .lt. (NTOT_SEND)) ) then
        deallocate (SR_i%iWS)
        SR_i%n_iWS = -1
      end if
      if(allocated(SR_i%iWS) .eqv. .false.) then
        allocate (SR_i%iWS(NTOT_SEND))
        SR_i%n_iWS = size(SR_i%iWS)
      end if
!
      end subroutine resize_isend_SR
!
! ----------------------------------------------------------------------
!
      subroutine resize_irecv_SR(NTOT_RECV, SR_i)
!
      integer(kind=kint), intent(in) :: NTOT_RECV
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!
      if(allocated(SR_i%iWR)                                            &
     &     .and. (size(SR_i%iWR) .lt. (NTOT_RECV)) ) then
        deallocate(SR_i%iWR)
        SR_i%n_iWR = -1
      end if
      if(allocated(SR_i%iWR) .eqv. .false.) then
        allocate (SR_i%iWR(NTOT_RECV))
        SR_i%n_iWR = size(SR_i%iWR)
      end if
!
      end subroutine resize_irecv_SR
!
! ----------------------------------------------------------------------
!
      end module t_solver_SR_int
