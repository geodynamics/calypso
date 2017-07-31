!>@file   calypso_SR_rev_N.f90
!!@brief  module calypso_SR_rev_N
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!@n      using reverse import table
!!
!!@verbatim
!!      subroutine calypso_send_recv_rev_N(NB, nnod_org, nnod_new,      &
!!     &                           npe_send, isend_self,                &
!!     &                           id_pe_send, istack_send, inod_export,&
!!     &                           npe_recv, irecv_self,                &
!!     &                           id_pe_recv, istack_recv, irev_import,&
!!     &                           X_org, X_new)
!!      subroutine calypso_send_recv_rev_3xN(NB, nnod_org, nnod_new,    &
!!     &                           npe_send, isend_self,                &
!!     &                           id_pe_send, istack_send, inod_export,&
!!     &                           npe_recv, irecv_self,                &
!!     &                           id_pe_recv, istack_recv, irev_import,&
!!     &                           X1_org, X2_org, X3_org,              &
!!     &                           X1_new, X2_new, X3_new)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
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
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_org(NB*nnod_org)   Send data
!!@n @param  X_new(NB*nnod_new)   Received data
!
      module calypso_SR_rev_N
!
      use m_precision
      use m_work_time
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine calypso_send_recv_rev_N(NB, nnod_org, nnod_new,        &
     &                           npe_send, isend_self,                  &
     &                           id_pe_send, istack_send, inod_export,  &
     &                           npe_recv, irecv_self,                  &
     &                           id_pe_recv, istack_recv, irev_import,  &
     &                           X_org, X_new)
!
      use calypso_mpi
      use m_solver_SR
      use calypso_SR_core
      use set_to_send_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
!
!C-- SEND
!
      call start_elapsed_time(36)
      call set_to_send_buf_N(NB, nnod_org, istack_send(npe_send),       &
     &    inod_export, X_org, WS)
      call end_elapsed_time(36)
!C
      call start_elapsed_time(37)
      call calypso_send_recv_core                                       &
     &         (NB, npe_send, isend_self, id_pe_send, istack_send,      &
     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
      call end_elapsed_time(37)
!
      call start_elapsed_time(38)
      call set_from_recv_buf_rev_N(NB, nnod_new,                        &
     &    istack_recv(npe_recv), irev_import, WR, X_new)
      call end_elapsed_time(38)
!
      end subroutine calypso_send_recv_rev_N
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_rev_3xN(NB, nnod_org, nnod_new,      &
     &                           npe_send, isend_self,                  &
     &                           id_pe_send, istack_send, inod_export,  &
     &                           npe_recv, irecv_self,                  &
     &                           id_pe_recv, istack_recv, irev_import,  &
     &                           X1_org, X2_org, X3_org,                &
     &                           X1_new, X2_new, X3_new)
!
      use calypso_mpi
      use m_solver_SR
      use calypso_SR_core
      use set_to_send_buf_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
!
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_export( istack_send(npe_send) )
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X1_org(NB*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(NB*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X1_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(NB*nnod_new)
!
!
!C-- SEND
!
      call set_to_send_buf_3xN(NB, nnod_org, istack_send(npe_send),     &
     &    inod_export, X1_org, X2_org, X3_org, WS)
!C
      call calypso_send_recv_core                                       &
     &        ((3*NB), npe_send, isend_self, id_pe_send, istack_send,   &
     &                 npe_recv, irecv_self, id_pe_recv, istack_recv)
!
      call set_from_recv_buf_rev_3xN(NB, nnod_new,                      &
     &    istack_recv(npe_recv), irev_import,                           &
     &    WR, X1_new, X2_new, X3_new)
!
      end subroutine calypso_send_recv_rev_3xN
!
! ----------------------------------------------------------------------
!
      end module calypso_SR_rev_N
