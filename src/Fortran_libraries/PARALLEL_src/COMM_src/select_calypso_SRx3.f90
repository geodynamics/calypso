!>@file   select_calypso_SRx3.f90
!!@brief  module select_calypso_SRx3
!!
!!@author H. Matsui
!!@date Programmed in March, 2013
!
!>@brief  Select communication routines for spherical harmonics transform
!!
!!@verbatim
!!      subroutine sel_calypso_send_recv_3xN(iflag_SR,                  &
!!     &                         NB, nnod_org, nnod_new,                &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X1_org, X2_org, X3_org,   &
!!     &                                      X1_new, X2_new, X3_new)
!!
!!      subroutine sel_calypso_send_recv_3x6(iflag_SR,                  &
!!     &                         nnod_org, nnod_new,                    &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X1_org, X2_org, X3_org,   &
!!     &                                      X1_new, X2_new, X3_new)
!!
!!      subroutine sel_calypso_send_recv_3x3(iflag_SR,                  &
!!     &                         nnod_org, nnod_new,                    &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X1_org, X2_org, X3_org,   &
!!     &                                      X1_new, X2_new, X3_new)
!!
!!      subroutine sel_calypso_send_recv_3x2(iflag_SR,                  &
!!     &                         nnod_org, nnod_new,                    &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X1_org, X2_org, X3_org,   &
!!     &                                      X1_new, X2_new, X3_new)
!!
!!      subroutine sel_calypso_send_recv_3x1(iflag_SR,                  &
!!     &                         nnod_org, nnod_new,                    &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X1_org, X2_org, X3_org,   &
!!     &                                      X1_new, X2_new, X3_new)
!!
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
!
      module select_calypso_SRx3
!
      use m_precision
      use m_constants
      use select_calypso_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
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
      end subroutine sel_calypso_send_recv_3xN
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_send_recv_3x6(iflag_SR,                    &
     &                         nnod_org, nnod_new,                      &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X1_org, X2_org, X3_org,     &
     &                                      X1_new, X2_new, X3_new)
!
      use calypso_SR_6
      use calypso_SR_rev_6
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
      real (kind=kreal), intent(in)::    X1_org(isix*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(isix*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(isix*nnod_org)
!
      real (kind=kreal), intent(inout):: X1_new(isix*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(isix*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(isix*nnod_new)
!
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev_3x6(nnod_org, nnod_new,              &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X1_org, X2_org, X3_org,                    &
     &                       X1_new, X2_new, X3_new)
      else
        call calypso_send_recv_3x6(nnod_org, nnod_new,                  &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X1_org, X2_org, X3_org,                    &
     &                       X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_calypso_send_recv_3x6
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_send_recv_3x3(iflag_SR,                    &
     &                         nnod_org, nnod_new,                      &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X1_org, X2_org, X3_org,     &
     &                                      X1_new, X2_new, X3_new)
!
      use calypso_SR_3
      use calypso_SR_rev_3
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
      real (kind=kreal), intent(in)::    X1_org(ithree*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(ithree*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(ithree*nnod_org)
!
      real (kind=kreal), intent(inout):: X1_new(ithree*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(ithree*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(ithree*nnod_new)
!
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev_3x3(nnod_org, nnod_new,              &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X1_org, X2_org, X3_org,                    &
     &                       X1_new, X2_new, X3_new)
      else
        call calypso_send_recv_3x3(nnod_org, nnod_new,                  &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X1_org, X2_org, X3_org,                    &
     &                       X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_calypso_send_recv_3x3
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_send_recv_3x2(iflag_SR,                    &
     &                         nnod_org, nnod_new,                      &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X1_org, X2_org, X3_org,     &
     &                                      X1_new, X2_new, X3_new)
!
      use calypso_SR_2
      use calypso_SR_rev_2
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
      real (kind=kreal), intent(in)::    X1_org(itwo*nnod_org)
      real (kind=kreal), intent(in)::    X2_org(itwo*nnod_org)
      real (kind=kreal), intent(in)::    X3_org(itwo*nnod_org)
!
      real (kind=kreal), intent(inout):: X1_new(itwo*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(itwo*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(itwo*nnod_new)
!
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev_3x2(nnod_org, nnod_new,              &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X1_org, X2_org, X3_org,                    &
     &                       X1_new, X2_new, X3_new)
      else
        call calypso_send_recv_3x2(nnod_org, nnod_new,                  &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X1_org, X2_org, X3_org,                    &
     &                       X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_calypso_send_recv_3x2
!
! ----------------------------------------------------------------------
!
      subroutine sel_calypso_send_recv_3x1(iflag_SR,                    &
     &                         nnod_org, nnod_new,                      &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X1_org, X2_org, X3_org,     &
     &                                      X1_new, X2_new, X3_new)
!
      use calypso_SR
      use calypso_SR_rev
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
      real (kind=kreal), intent(in)::    X1_org(nnod_org)
      real (kind=kreal), intent(in)::    X2_org(nnod_org)
      real (kind=kreal), intent(in)::    X3_org(nnod_org)
!
      real (kind=kreal), intent(inout):: X1_new(nnod_new)
      real (kind=kreal), intent(inout):: X2_new(nnod_new)
      real (kind=kreal), intent(inout):: X3_new(nnod_new)
!
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev_3x1(nnod_org, nnod_new,              &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X1_org, X2_org, X3_org,                    &
     &                       X1_new, X2_new, X3_new)
      else
        call calypso_send_recv_3x1(nnod_org, nnod_new,                  &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X1_org, X2_org, X3_org,                    &
     &                       X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_calypso_send_recv_3x1
!
! ----------------------------------------------------------------------
!
      end module select_calypso_SRx3
