!
!     module select_spherical_SR
!
!     Written by H. Matsui on Aug., 2007
!
!>@file   select_spherical_SR.f90
!!@brief  module select_spherical_SR
!!
!!@author H. Matsui
!!@date Programmed in March, 2013
!
!>@brief  Select communication routines for spherical harmonics transform
!!
!!@verbatim
!!      subroutine sel_sph_send_recv_N(NB, nnod_org, nnod_new,          &
!!     &                         npe_send, isend_self, nnod_send,       &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self, nnod_recv,       &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new, SOLVER_COMM)
!!
!!      subroutine sel_sph_send_recv_6(nnod_org, nnod_new,              &
!!     &                         npe_send, isend_self, nnod_send,       &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self, nnod_recv,       &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new, SOLVER_COMM)
!!
!!      subroutine sel_sph_send_recv_3(nnod_org, nnod_new,              &
!!     &                         npe_send, isend_self, nnod_send,       &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self, nnod_recv,       &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new, SOLVER_COMM)
!!
!!      subroutine sel_sph_send_recv_2(nnod_org, nnod_new,              &
!!     &                         npe_send, isend_self, nnod_send,       &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self, nnod_recv,       &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new, SOLVER_COMM)
!!
!!      subroutine sel_sph_send_recv(nnod_org, nnod_new,                &
!!     &                         npe_send, isend_self, nnod_send,       &
!!     &                         id_pe_send, istack_send, inod_export,  &
!!     &                         npe_recv, irecv_self, nnod_recv,       &
!!     &                         id_pe_recv, istack_recv, inod_import,  &
!!     &                         irev_import, X_org, X_new, SOLVER_COMM)
!!
!!
!!      subroutine sel_sph_send_recv_int(nnod_org, nnod_new,            &
!!     &                       npe_send, isend_self, nnod_send,         &
!!     &                       id_pe_send, istack_send, inod_export,    &
!!     &                       npe_recv, irecv_self, nnod_recv,         &
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
!!@n @param  nnod_send   Number of data points to send
!!@n @param  id_pe_send(npe_send)      Process ID to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_export(nnod_send)
!!                    local node ID to copy in send buffer
!!@n
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  nnod_recv   Number of data points to receive
!!@n @param  id_pe_recv(npe_send)      Process ID to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  inod_import(nnod_recv)
!!                    local node ID to copy from receive buffer
!!@n @param  irev_import(nnod_recv)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_org(NB*nnod_org)   Arbitrary components of send data
!!@n @param  X_new(NB*nnod_recv)  Arbitrary components of received data
!!@n
!!@n @param  X_org(6*nnod_org)   Six components of send data
!!@n @param  X_new(6*nnod_recv)  Six components of received data
!!@n
!!@n @param  X_org(3*nnod_org)   Three components of send data
!!@n @param  X_new(3*nnod_recv)  Three components of received data
!!@n
!!@n @param  X_org(2*nnod_org)   Two components of send data
!!@n @param  X_new(2*nnod_recv)  Two components of received data
!!@n
!!@n @param  X_org(nnod_org)   Scalar send data
!!@n @param  X_new(nnod_recv)  Scalar received data
!!@n
!!@n @param  iX_org(nnod_org)   Integer send data
!!@n @param  iX_new(nnod_recv)  Integer received data
!!@n
!!@n @param  SOLVER_COMM          MPI communicator
!
      module select_spherical_SR
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
!>      Data communication mode for arbitrary size data
      integer(kind = kint) :: iflag_sph_SRN = iflag_import_item
!>      Data communication mode for six components data
      integer(kind = kint) :: iflag_sph_SR6 = iflag_import_item
!>      Data communication mode for vector
      integer(kind = kint) :: iflag_sph_SR3 = iflag_import_item
!>      Data communication mode for soleinoidal vection
      integer(kind = kint) :: iflag_sph_SR2 = iflag_import_item
!>      Data communication mode for scalar
      integer(kind = kint) :: iflag_sph_SR =  iflag_import_item
!
!>      Data communication mode for integer
      integer(kind = kint) :: iflag_sph_SR_int = iflag_import_item
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv_N(NB, nnod_org, nnod_new,            &
     &                         npe_send, isend_self, nnod_send,         &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self, nnod_recv,         &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
      use spherical_SR_N
      use spherical_SR_rev_N
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: NB
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(isix*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(isix*nnod_new)
!
!
      if(iflag_sph_SRN .eq. iflag_import_rev) then
        call sph_send_recv_by_rev_N(NB, nnod_org, nnod_new,             &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      else
        call sph_send_recv_N(NB, nnod_org, nnod_new,                    &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv_N
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv_6(nnod_org, nnod_new,                &
     &                         npe_send, isend_self, nnod_send,         &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self, nnod_recv,         &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
      use spherical_SR_6
      use spherical_SR_rev_6
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(isix*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(isix*nnod_new)
!
!
      if(iflag_sph_SR6 .eq. iflag_import_rev) then
        call sph_send_recv_by_rev_6(nnod_org, nnod_new,                 &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      else
        call sph_send_recv_6(nnod_org, nnod_new,                        &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv_6
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv_3(nnod_org, nnod_new,                &
     &                         npe_send, isend_self, nnod_send,         &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self, nnod_recv,         &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
      use spherical_SR_3
      use spherical_SR_rev_3
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(ithree*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(ithree*nnod_new)
!
!
      if(iflag_sph_SR3 .eq. iflag_import_rev) then
        call sph_send_recv_by_rev_3(nnod_org, nnod_new,                 &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      else
        call sph_send_recv_3(nnod_org, nnod_new,                        &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv_3
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv_2(nnod_org, nnod_new,                &
     &                         npe_send, isend_self, nnod_send,         &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self, nnod_recv,         &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
      use spherical_SR_2
      use spherical_SR_rev_2
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(itwo*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(itwo*nnod_new)
!
!
      if(iflag_sph_SR2 .eq. iflag_import_rev) then
        call sph_send_recv_by_rev_2(nnod_org, nnod_new,                 &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      else
        call sph_send_recv_2(nnod_org, nnod_new,                        &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv_2
!
! ----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv(nnod_org, nnod_new,                  &
     &                         npe_send, isend_self, nnod_send,         &
     &                         id_pe_send, istack_send, inod_export,    &
     &                         npe_recv, irecv_self, nnod_recv,         &
     &                         id_pe_recv, istack_recv, inod_import,    &
     &                         irev_import, X_org, X_new, SOLVER_COMM)
!
      use spherical_SR
      use spherical_SR_rev
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      real (kind=kreal), intent(in)::    X_org(nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(nnod_new)
!
!
      if(iflag_sph_SR .eq. iflag_import_rev) then
        call sph_send_recv_by_rev(nnod_org, nnod_new,                   &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      else
        call sph_send_recv(nnod_org, nnod_new,                          &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       X_org, X_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_sph_send_recv_int(nnod_org, nnod_new,              &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       irev_import, iX_org, iX_new, SOLVER_COMM)
!
      use spherical_SR_int
      use spherical_SR_rev_int
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      integer (kind=kint), intent(in):: iX_org(nnod_org)
!
      integer (kind=kint), intent(inout):: iX_new(nnod_new)
!
!
      if(iflag_sph_SR_int .eq. iflag_import_rev) then
        call sph_send_recv_by_rev_int(nnod_org, nnod_new,               &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, irev_import,      &
     &                       iX_org, iX_new, SOLVER_COMM)
      else
        call sph_send_recv_int(nnod_org, nnod_new,                      &
     &                       npe_send, isend_self, nnod_send,           &
     &                       id_pe_send, istack_send, inod_export,      &
     &                       npe_recv, irecv_self, nnod_recv,           &
     &                       id_pe_recv, istack_recv, inod_import,      &
     &                       iX_org, iX_new, SOLVER_COMM)
      end if
!
      end subroutine sel_sph_send_recv_int
!
! ----------------------------------------------------------------------
!
      end module select_spherical_SR
