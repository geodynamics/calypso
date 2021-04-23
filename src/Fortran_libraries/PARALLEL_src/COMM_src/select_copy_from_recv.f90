!>@file   select_copy_from_recv.f90
!!@brief  module select_copy_from_recv
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  scalar components data communication
!!
!!@verbatim
!!      subroutine sel_copy_from_recv_buf_1(iflag_recv, nnod_new,       &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X_new)
!!      subroutine sel_copy_from_recv_buf_2(iflag_recv, nnod_new,       &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X_new)
!!      subroutine sel_copy_from_recv_buf_3(iflag_recv, nnod_new,       &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X_new)
!!      subroutine sel_copy_from_recv_buf_6(iflag_recv, nnod_new,       &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X_new)
!!      subroutine sel_copy_from_recv_buf_N(iflag_recv, NB, nnod_new,   &
!!     &                         npe_recv, ntot_import, istack_recv,    &
!!     &                         inod_import, irev_import,              &
!!     &                         WR, X_new)
!!
!!      subroutine sel_copy_to_send_buf_N(iflag_recv, NB, nnod_org,     &
!!     &                         npe_send, ntot_export, istack_send,    &
!!     &                         inod_export, X_org, WS)
!!
!!      subroutine sel_cppy_from_recv_buf_int(iflag_recv, nnod_new,     &
!!     &                       ntot_import, inod_import, irev_import,   &
!!     &                       iWR, iX_new)
!!      subroutine sel_cppy_from_recv_buf_i8(iflag_recv, nnod_new,      &
!!     &                       ntot_import, inod_import, irev_import,   &
!!     &                       i8WR, i8X_new)
!!@endverbatim
!!
!!@n @param  iflag_recv  import table mode
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  ntot_import Number of import table
!!@n @param  inod_import(ntot_import)
!!                    local node ID to copy from receive buffer
!!@n @param  irev_import(nnod_new)
!!                    import buffer ID for each data point
!!@n
!!@n @param  X_new(nnod_new)   Received data
!
      module select_copy_from_recv
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Undefined flag
      integer(kind = kint), parameter :: iflag_import_UNDEFINED = -1
!>      Integer flag to use import table
      integer(kind = kint), parameter :: iflag_import_item = 0
!>      Integer flag to use reversed import table for data points
      integer(kind = kint), parameter :: iflag_import_rev =  1
!>      Integer flag to use testroutine to import
      integer(kind = kint), parameter :: iflag_import_mod = 2
!>      Integer flag to use testroutine to import
      integer(kind = kint), parameter :: iflag_import_test = -10
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_1(iflag_recv, nnod_new,         &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(ntot_import+1)
!
      real (kind=kreal), intent(inout):: X_new(nnod_new)
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_1(nnod_new,                          &
     &      ntot_import, irev_import, WR(1), X_new)
      else
!$omp parallel workshare
        X_new(1:nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_1(nnod_new,                              &
     &      ntot_import, inod_import, WR(1), X_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_1
!
!-----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_2(iflag_recv, nnod_new,         &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(2*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X_new(2*nnod_new)
!
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_2(nnod_new,                          &
     &      ntot_import, irev_import, WR(1), X_new)
      else
!$omp parallel workshare
        X_new(1:2*nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_2(nnod_new,                              &
     &      ntot_import, inod_import, WR(1), X_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_2
!
! ----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_3(iflag_recv, nnod_new,         &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(3*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X_new(3*nnod_new)
!
!C
!C-- RECEIVE
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3(nnod_new,                          &
     &      ntot_import, irev_import, WR(1), X_new)
      else
!$omp parallel workshare
        X_new(1:3*nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_3(nnod_new,                              &
     &    ntot_import, inod_import, WR(1), X_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_3
!
!-----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_6(iflag_recv, nnod_new,         &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(6*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X_new(6*nnod_new)
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_6(nnod_new,                          &
     &      ntot_import, irev_import, WR(1), X_new)
      else
!$omp parallel workshare
        X_new(1:6*nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_6(nnod_new,                              &
     &      ntot_import, inod_import, WR(1), X_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_6
!
! ----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_N(iflag_recv, NB, nnod_new,     &
     &                         npe_recv, ntot_import, istack_recv,      &
     &                         inod_import, irev_import,                &
     &                         WR, X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(NB*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_N(NB, nnod_new,                      &
     &      ntot_import, irev_import, WR(1), X_new)
      else if(iflag_recv .eq. iflag_import_mod) then
!$omp parallel workshare
        X_new(1:NB*nnod_new) = 0.0d0
!$omp end parallel workshare

        call set_from_recv_buf_N_mod(NB, nnod_new,                      &
     &      npe_recv, ntot_import, istack_recv, inod_import,            &
     &      WR(1), X_new)
      else
!$omp parallel workshare
        X_new(1:NB*nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_N(NB, nnod_new,                          &
     &      ntot_import, inod_import, WR(1), X_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_N
!
! ----------------------------------------------------------------------
!
      subroutine sel_copy_to_send_buf_N(iflag_recv, NB, nnod_org,       &
     &                         npe_send, ntot_export, istack_send,      &
     &                         inod_export, X_org, WS)
!
      use set_to_send_buffer
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_org
!
      integer(kind = kint), intent(in) :: ntot_export
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(ntot_export)
      real (kind=kreal), intent(in):: X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: WS(NB*(ntot_export+1))
!
!C-- SEND
      if(iflag_recv .eq. iflag_import_mod) then
        call set_to_send_buf_N_mod(NB, nnod_org, npe_send,              &
     &      ntot_export, istack_send, inod_export, X_org, WS)
      else
        call set_to_send_buf_N                                          &
     &     (NB, nnod_org, ntot_export, inod_export, X_org, WS)
      end if
!
      end subroutine sel_copy_to_send_buf_N
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_int(iflag_recv, nnod_new,       &
     &                       ntot_import, inod_import, irev_import,     &
     &                       iWR, iX_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      integer (kind=kint), intent(inout):: iWR(ntot_import+1)
!
      integer (kind=kint), intent(inout):: iX_new(nnod_new)
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_int(nnod_new,                        &
     &      ntot_import, irev_import, iWR(1), iX_new)
      else
!$omp parallel workshare
         iX_new(1:nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_int(nnod_new,                            &
     &      ntot_import, inod_import, iWR(1), iX_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_int
!
! ----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_i8(iflag_recv, nnod_new,        &
     &                       ntot_import, inod_import, irev_import,     &
     &                       i8WR, i8X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      integer(kind = kint_gl), intent(inout):: i8WR(ntot_import+1)
!
      integer(kind = kint_gl), intent(inout):: i8X_new(nnod_new)
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_i8(nnod_new,                         &
     &      ntot_import, irev_import, i8WR(1), i8X_new)
      else
!$omp parallel workshare
         i8X_new(1:nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_i8(nnod_new,                             &
     &      ntot_import, inod_import, i8WR(1), i8X_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_i8
!
! ----------------------------------------------------------------------
!
      end module select_copy_from_recv
