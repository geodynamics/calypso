!>@file   calypso_SR_int.f90
!!@brief  module calypso_SR_int
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Integer data communication
!!
!!@verbatim
!!      subroutine calypso_send_recv_int(iflag_SR, nnod_org, nnod_new,  &
!!     &                       npe_send, isend_self,                    &
!!     &                       id_pe_send, istack_send, inod_export,    &
!!     &                       npe_recv, irecv_self,                    &
!!     &                       id_pe_recv, istack_recv, inod_import,    &
!!     &                       irev_import, iX_org, iX_new)
!!@endverbatim
!!
!!@n @param  iflag_SR    import table mode
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  id_pe_send(npe_send)      Process ID to send
!!@n @param  istack_send(0:npe_send)
!!                      End points of send buffer for each process
!!@n @param  inod_export(istack_send(npe_send))
!!                      local node ID to copy in send buffer
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  id_pe_recv(npe_send)      Process ID to receive
!!@n @param  istack_recv(0:npe_send)
!!                      End points of receive buffer for each process
!!@n @param  inod_import(istack_recv(npe_recv))
!!                      local node ID to copy from receive buffer
!!
!!@n @param  iX_org(nnod_org)   Send data
!!@n @param  iX_new(nnod_new)   Received data
!
      module calypso_SR_int
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine calypso_send_recv_int(iflag_SR, nnod_org, nnod_new,    &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       irev_import, iX_org, iX_new)
!
      use calypso_mpi
      use m_solver_SR
      use calypso_SR_core
      use set_to_send_buffer
      use select_copy_from_recv
!
      integer(kind = kint), intent(in) :: iflag_SR
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
!
      integer (kind=kint), intent(in):: iX_org(nnod_org)
!
      integer (kind=kint), intent(inout):: iX_new(nnod_new)
!
!
      call resize_iwork_sph_SR(npe_send, npe_recv,                      &
     &    istack_send(npe_send), istack_recv(npe_recv))
!
!C-- SEND
!
      call set_to_send_buf_int(nnod_org,                                &
     &    istack_send(npe_send), inod_export, iX_org, iWS)
!C
!C-- COMM
      call calypso_send_recv_intcore                                    &
     &             (npe_send, isend_self, id_pe_send, istack_send,      &
     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
!
!C-- RECV
      call sel_cppy_from_recv_buf_int(iflag_SR, nnod_new,               &
     &    istack_recv(npe_recv), inod_import, irev_import,              &
     &    iWR(1), iX_new)
!
!C-- WAIT
      call calypso_send_recv_fin(npe_send, isend_self)
!
      end subroutine calypso_send_recv_int
!
! ----------------------------------------------------------------------
!
      end module calypso_SR_int
