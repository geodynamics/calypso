!>@file   calypso_SR_N.f90
!!@brief  module calypso_SR_N
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!
!!@verbatim
!!      subroutine calypso_send_recv_N(NB, nnod_org, nnod_new,          &
!!     &                           npe_send, isend_self,                &
!!     &                           id_pe_send, istack_send, inod_export,&
!!     &                           npe_recv, irecv_self,                &
!!     &                           id_pe_recv, istack_recv, inod_import,&
!!     &                           X_org, X_new)
!!      subroutine calypso_send_recv_3xN(NB, nnod_org, nnod_new,        &
!!     &                           npe_send, isend_self,                &
!!     &                           id_pe_send, istack_send, inod_export,&
!!     &                           npe_recv, irecv_self,                &
!!     &                           id_pe_recv, istack_recv, inod_import,&
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
!!@n @param  inod_import(istack_recv(npe_recv))
!!                    local node ID to copy from receive buffer
!!@n
!!@n @param  X_org(NB*nnod_org)   Send data
!!@n @param  X_new(NB*nnod_new)   Received data
!
      module calypso_SR_N
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
      subroutine calypso_send_recv_N(NB, nnod_org, nnod_new,            &
     &                           npe_send, isend_self,                  &
     &                           id_pe_send, istack_send, inod_export,  &
     &                           npe_recv, irecv_self,                  &
     &                           id_pe_recv, istack_recv, inod_import,  &
     &                           X_org, X_new)
!
      use calypso_mpi
      use m_solver_SR
      use calypso_SR_core
      use set_to_send_buffer
      use set_from_recv_buffer
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
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
!
!C-- SEND
      call set_to_send_buf_N_mod(NB, nnod_org, npe_send,                &
     &    istack_send(npe_send), istack_send, inod_export, X_org, WS)
!C
      call calypso_send_recv_core                                       &
     &         (NB, npe_send, isend_self, id_pe_send, istack_send,      &
     &              npe_recv, irecv_self, id_pe_recv, istack_recv)
!
      call set_from_recv_buf_N_mod(NB, nnod_new, npe_recv,              &
     &    istack_recv(npe_recv), istack_recv, inod_import, WR, X_new)
!
      end subroutine calypso_send_recv_N
!
! ----------------------------------------------------------------------
!
      subroutine calypso_send_recv_3xN(NB, nnod_org, nnod_new,          &
     &                           npe_send, isend_self,                  &
     &                           id_pe_send, istack_send, inod_export,  &
     &                           npe_recv, irecv_self,                  &
     &                           id_pe_recv, istack_recv, inod_import,  &
     &                           X1_org, X2_org, X3_org,                &
     &                           X1_new, X2_new, X3_new)
!
      use calypso_mpi
      use m_solver_SR
      use calypso_SR_core
      use set_to_send_buf_tri
      use set_from_recv_buff_tri
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
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_import( istack_recv(npe_recv) )
!
      real (kind=kreal), intent(in)::    X1_org(NB*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(NB*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X1_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(NB*nnod_new)
!
      real (kind=kreal) :: elaps3(3)
!
      real(kind = kreal) :: s1time, s2time
!
!
!C-- SEND
!
      s1time = MPI_WTIME()
      call set_to_send_buf_3xN_mod(NB, nnod_org,                        &
     &    npe_send, istack_send(npe_send), istack_send, inod_export,    &
     &    X1_org, X2_org, X3_org, WS)
      elaps3(1) = elaps3(1) + MPI_WTIME() - s1time
!C
      call calypso_send_recv_core                                       &
     &        ((3*NB), npe_send, isend_self, id_pe_send, istack_send,   &
     &                 npe_recv, irecv_self, id_pe_recv, istack_recv)
!
!      s2time = MPI_WTIME()
      call set_from_recv_buf_3xN(NB, nnod_new,                          &
     &    npe_recv, istack_recv(npe_recv), istack_recv, inod_import,    &
     &    WR, X1_new, X2_new, X3_new)
!      elaps3(2) = elaps3(2) + MPI_WTIME() - s2time
!      elaps3(3) = elaps3(3) + MPI_WTIME() - s1time
!
      end subroutine calypso_send_recv_3xN
!
! ----------------------------------------------------------------------
!
      end module calypso_SR_N
