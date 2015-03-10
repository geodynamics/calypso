!>@file   select_calypso_SR.f90
!!@brief  module select_calypso_SR
!!
!!@author H. Matsui
!!@date Programmed in March, 2013
!
!>@brief  Select communication routines for spherical harmonics transform
!!
!!@verbatim
!!      subroutine sel_calypso_send_recv_N(iflag_SR,                    &
!!     &                         NB, nnod_org, nnod_new,                &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new)
!!      subroutine sel_calypso_send_recv_3xN(iflag_SR,                  &
!!     &                         NB, nnod_org, nnod_new,                &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X1_org, X2_org, X3_org,   &
!!     &                                      X1_new, X2_new, X3_new)
!!
!!      subroutine finish_calypso_send_recv(npe_send, isend_self)
!!
!!      subroutine check_calypso_send_recv_N                            &
!!     &         (NB, npe_send, isend_self, istack_send,                &
!!     &              npe_recv, irecv_self, istack_recv)
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
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_org(NB*nnod_org)   Arbitrary components of send data
!!@n @param  X_new(NB*nnod_new)   Arbitrary components of received data
!!@n
!!@n @param  X_org(6*nnod_org)   Six components of send data
!!@n @param  X_new(6*nnod_new)   Six components of received data
!!@n
!!@n @param  X_org(3*nnod_org)   Three components of send data
!!@n @param  X_new(3*nnod_new)   Three components of received data
!!@n
!!@n @param  X_org(2*nnod_org)   Two components of send data
!!@n @param  X_new(2*nnod_new)   Two components of received data
!!@n
!!@n @param  X_org(nnod_org)   Scalar send data
!!@n @param  X_new(nnod_new)   Scalar received data
!!@n
!!@n @param  iX_org(nnod_org)   Integer send data
!!@n @param  iX_new(nnod_new)   Integer received data
!
      module select_calypso_SR
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
      subroutine sel_calypso_send_recv_N(iflag_SR,                      &
     &                         NB, nnod_org, nnod_new,                  &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new)
!
      use m_solver_SR
      use select_copy_from_recv
      use calypso_SR_N
      use calypso_SR_rev_N
!
      integer(kind = kint), intent(in) :: iflag_SR
      integer(kind = kint), intent(in) :: NB
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
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
!
      call resize_work_sph_SR(NB, npe_send, npe_recv,                   &
     &    istack_send(npe_send), istack_recv(npe_recv))
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev_N(NB, nnod_org, nnod_new,            &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new)
      else
        call calypso_send_recv_N(NB, nnod_org, nnod_new,                &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new)
      end if
!
      end subroutine sel_calypso_send_recv_N
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_send_recv_3xN(iflag_SR,                    &
     &                         NB, nnod_org, nnod_new,                  &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X1_org, X2_org, X3_org,     &
     &                                      X1_new, X2_new, X3_new)
!
      use m_solver_SR
      use select_copy_from_recv
      use calypso_SR_N
      use calypso_SR_rev_N
!
      integer(kind = kint), intent(in) :: iflag_SR
      integer(kind = kint), intent(in) :: NB
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
      real (kind=kreal), intent(in)::    X1_org(NB*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(NB*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X1_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(NB*nnod_new)
!
!
      call resize_work_sph_SR( (3*NB), npe_send, npe_recv,              &
     &    istack_send(npe_send), istack_recv(npe_recv))
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev_3xN(NB, nnod_org, nnod_new,          &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X1_org, X2_org, X3_org,                    &
     &                       X1_new, X2_new, X3_new)
      else
        call calypso_send_recv_3xN(NB, nnod_org, nnod_new,              &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X1_org, X2_org, X3_org,                    &
     &                       X1_new, X2_new, X3_new)
      end if
!
      call finish_calypso_send_recv(npe_send, isend_self)
!
      end subroutine sel_calypso_send_recv_3xN
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine finish_calypso_send_recv(npe_send, isend_self)
!
      use m_solver_SR
      use calypso_mpi
      use calypso_SR_core
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
!
      call calypso_send_recv_fin(npe_send, isend_self)
!
      end subroutine finish_calypso_send_recv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_calypso_send_recv_N                              &
     &         (NB, npe_send, isend_self, istack_send,                  &
     &              npe_recv, irecv_self, istack_recv)
!
      use m_solver_SR
      use calypso_SR_core
!
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!
!
      call resize_work_sph_SR(NB, npe_send, npe_recv,                   &
     &    istack_send(npe_send), istack_recv(npe_recv))
!
      call calypso_send_recv_check                                      &
     &         (NB, npe_send, isend_self, istack_send,                  &
     &              npe_recv, irecv_self, istack_recv)
!
      end subroutine check_calypso_send_recv_N
!
!-----------------------------------------------------------------------
!
      end module select_calypso_SR
