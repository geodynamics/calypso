!>@file   select_calypso_SR.f90
!!@brief  module select_calypso_SR
!!
!!@author H. Matsui
!!@date Programmed in March, 2013
!
!>@brief  Select communication routines for spherical harmonics transform
!!
!!@verbatim
!!      subroutine set_reverse_import_table(nnod_new, nnod_recv,        &
!!     &          inod_import, irev_import)
!!
!!      subroutine sel_calypso_send_recv_N(iflag_SR,                    &
!!     &                         NB, nnod_org, nnod_new,                &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new)
!!
!!      subroutine sel_calypso_send_recv_6(iflag_SR,                    &
!!     &                         nnod_org, nnod_new,                    &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new)
!!
!!      subroutine sel_calypso_send_recv_3(iflag_SR,                    &
!!     &                         nnod_org, nnod_new,                    &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new)
!!
!!      subroutine sel_calypso_send_recv_2(iflag_SR,                    &
!!     &                         nnod_org, nnod_new,                    &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new)
!!
!!      subroutine sel_calypso_send_recv(iflag_SR,                      &
!!     &                         nnod_org, nnod_new,                    &
!!     &                         npe_send, isend_self,                  &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self,                  &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new)
!!
!!
!!      subroutine sel_calypso_send_recv_int(iflag_SR,                  &
!!     &                       nnod_org, nnod_new,                      &
!!     &                       npe_send, isend_self,                    &
!!     &                       id_pe_send, istack_send, inod_export,    &
!!     &                       npe_recv, irecv_self,                    &
!!     &                       id_pe_recv, istack_recv, inod_import,    &
!!     &                       irev_import, iX_org, iX_new)
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
!>      Integer flag to use import table
      integer(kind = kint), parameter :: iflag_import_item = 0
!>      Integer flag to use reversed import table for data points
      integer(kind = kint), parameter :: iflag_import_rev =  1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_reverse_import_table(nnod_new, nnod_recv,          &
     &          inod_import, irev_import)
!
      integer(kind = kint), intent(in) :: nnod_new
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      integer(kind = kint), intent(inout) :: irev_import(nnod_new)
!
      integer(kind = kint) :: i, k
!
!
!$omp parallel do
      do i = 1, nnod_new
        irev_import(i) = nnod_recv + 1
      end do
!$omp end parallel do
!
!$omp parallel do private(i)
      do k = 1, nnod_recv
        i = inod_import(k)
        irev_import(i) = k
      end do
!$omp end parallel do
!
      end subroutine set_reverse_import_table
!
!-----------------------------------------------------------------------
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
      subroutine sel_calypso_send_recv_6(iflag_SR,                      &
     &                         nnod_org, nnod_new,                      &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new)
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
      real (kind=kreal), intent(in)::    X_org(6*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(6*nnod_new)
!
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev_6(nnod_org, nnod_new,                &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new)
      else
        call calypso_send_recv_6(nnod_org, nnod_new,                    &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new)
      end if
!
      end subroutine sel_calypso_send_recv_6
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_send_recv_3(iflag_SR,                      &
     &                         nnod_org, nnod_new,                      &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new)
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
      real (kind=kreal), intent(in)::    X_org(3*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(3*nnod_new)
!
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev_3(nnod_org, nnod_new,                &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new)
      else
        call calypso_send_recv_3(nnod_org, nnod_new,                    &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new)
      end if
!
      end subroutine sel_calypso_send_recv_3
!
!-----------------------------------------------------------------------
!
      subroutine sel_calypso_send_recv_2(iflag_SR,                      &
     &                         nnod_org, nnod_new,                      &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new)
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
      real (kind=kreal), intent(in)::    X_org(2*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(2*nnod_new)
!
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev_2(nnod_org, nnod_new,                &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new)
      else
        call calypso_send_recv_2(nnod_org, nnod_new,                    &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new)
      end if
!
      end subroutine sel_calypso_send_recv_2
!
! ----------------------------------------------------------------------
!
      subroutine sel_calypso_send_recv(iflag_SR,                        &
     &                         nnod_org, nnod_new,                      &
     &                         npe_send, isend_self,                    &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self,                    &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new)
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
      real (kind=kreal), intent(in)::    X_org(nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(nnod_new)
!
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev(nnod_org, nnod_new,                  &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new)
      else
        call calypso_send_recv(nnod_org, nnod_new,                      &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new)
      end if
!
      end subroutine sel_calypso_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_calypso_send_recv_int(iflag_SR,                    &
     &                       nnod_org, nnod_new,                        &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       irev_import, iX_org, iX_new)
!
      use calypso_SR_int
      use calypso_SR_rev_int
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
      integer (kind=kint), intent(in):: iX_org(nnod_org)
!
      integer (kind=kint), intent(inout):: iX_new(nnod_new)
!
!
      if(iflag_SR .eq. iflag_import_rev) then
        call calypso_send_recv_rev_int(nnod_org, nnod_new,              &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       iX_org, iX_new)
      else
        call calypso_send_recv_int(nnod_org, nnod_new,                  &
     &                       npe_send, isend_self,                      &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self,                      &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       iX_org, iX_new)
      end if
!
      end subroutine sel_calypso_send_recv_int
!
! ----------------------------------------------------------------------
!
      end module select_calypso_SR
