!>@file   calypso_SR_2.f90
!!@brief  module calypso_SR_2
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  solenoidal components data communication
!!
!!@verbatim
!!      subroutine calypso_send_recv_2(iflag_SR, nnod_org, nnod_new,    &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new)
!!      subroutine calypso_send_recv_3x2(iflag_SR, nnod_org, nnod_new,  &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X1_org, X2_org, X3_org,   &
!!     &                                      X1_new, X2_new, X3_new)
!!@endverbatim
!!
!!@n @param  iflag_SR    import table mode
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  id_pe_send(npe_send)      Process ID to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(istack_send(npe_send))
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  id_pe_recv(npe_send)      Process ID to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  inod_import(istack_recv(npe_recv))
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!                    local node ID to copy from receive buffer
!!@n
!!@n @param  X_org(2*nnod_org)   Send data
!!@n @param  X_new(2*nnod_new)   Received data
!
      module calypso_SR_2
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine calypso_send_recv_2(iflag_SR, nnod_org, nnod_new,      &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new)
!
      use calypso_mpi
      use m_solver_SR
      use calypso_SR_core
      use set_to_send_buffer
      use select_copy_from_recv
!
      integer(kind = kint), intent(in) :: iflag_SR
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(2*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(2*nnod_new)
!
!
      call resize_work_sph_SR(ithree, npe_send, npe_recv,               &
     &    istack_send(npe_send), istack_recv(npe_recv))
!
!C-- SEND
      call set_to_send_buf_2(nnod_org,                                  &
     &    istack_send(npe_send), inod_export, X_org, WS)
!
!C-- COMM
      call calypso_send_recv_core                                       &
     &       (itwo, npe_send, isend_self, id_pe_send, istack_send,      &
     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
!
!C-- RECV
      call sel_cppy_from_recv_buf_2(iflag_SR, nnod_new,                 &
     &    istack_recv(npe_recv), inod_import, irev_import,              &
     &    WR(1), X_new)
!
!C-- WAIT
      call calypso_send_recv_fin(npe_send, isend_self)
!
      end subroutine calypso_send_recv_2
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_3x2(iflag_SR, nnod_org, nnod_new,    &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X1_org, X2_org, X3_org,     &
     &                                      X1_new, X2_new, X3_new)
!
      use calypso_mpi
      use m_solver_SR
      use calypso_SR_core
      use set_to_send_buf_tri
      use select_copy_from_recv
!
      integer(kind = kint), intent(in) :: iflag_SR
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X1_org(2*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(2*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(2*nnod_org)
!
      real (kind=kreal), intent(inout):: X1_new(2*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(2*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(2*nnod_new)
!
!
      call resize_work_sph_SR((itwo*ithree), npe_send, npe_recv,        &
     &    istack_send(npe_send), istack_recv(npe_recv))
!
!C-- SEND
      call set_to_send_buf_3x2(nnod_org, istack_send(npe_send),         &
     &    inod_export, X1_org, X2_org, X3_org, WS)
!C
!C-- COMM
      call calypso_send_recv_core                                       &
     &       (isix, npe_send, isend_self, id_pe_send, istack_send,      &
     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
!
!C-- RECV
      call sel_cppy_from_recv_buf_3x2(iflag_SR, nnod_new,               &
     &    istack_recv(npe_recv), inod_import, irev_import,              &
     &    WR(1), X1_new, X2_new, X3_new)
!
!C-- WAIT
      call calypso_send_recv_fin(npe_send, isend_self)
!
      end subroutine calypso_send_recv_3x2
!
! ----------------------------------------------------------------------
!
      end module calypso_SR_2
