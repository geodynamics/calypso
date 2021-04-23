!>@file   select_copy_from_recv_tri.f90
!!@brief  module select_copy_from_recv_tri
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief select copy from triple recv buffer
!!
!!@verbatim
!!      subroutine sel_copy_from_recv_buf_3x1(iflag_recv, nnod_new,     &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X1_new, X2_new, X3_new)
!!      subroutine sel_copy_from_recv_buf_3x2(iflag_recv, nnod_new,     &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X1_new, X2_new, X3_new)
!!      subroutine sel_copy_from_recv_buf_3x3(iflag_recv, nnod_new,     &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X1_new, X2_new, X3_new)
!!      subroutine sel_copy_from_recv_buf_3x4(iflag_recv, nnod_new,     &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X1_new, X2_new, X3_new)
!!      subroutine sel_copy_from_recv_buf_3x6(iflag_recv, nnod_new,     &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X1_new, X2_new, X3_new)
!!      subroutine sel_copy_from_recv_buf_3x8(iflag_recv, nnod_new,     &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X1_new, X2_new, X3_new)
!!      subroutine sel_copy_from_recv_buf_3xN(iflag_recv, NB, nnod_new, &
!!     &                         npe_recv, ntot_import, istack_recv,    &
!!     &                         inod_import, irev_import,              &
!!     &                         WR, X1_new, X2_new, X3_new)
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
      module select_copy_from_recv_tri
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
      subroutine sel_copy_from_recv_buf_3x1(iflag_recv, nnod_new,       &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X1_new, X2_new, X3_new)
!
      use select_copy_from_recv
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(3*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X1_new(nnod_new)
      real (kind=kreal), intent(inout):: X2_new(nnod_new)
      real (kind=kreal), intent(inout):: X3_new(nnod_new)
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3x1(nnod_new, ntot_import,           &
     &      irev_import, WR(1), X1_new, X2_new, X3_new)
      else
!$omp parallel workshare
        X1_new(1:nnod_new) = 0.0d0
        X2_new(1:nnod_new) = 0.0d0
        X3_new(1:nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_3x1(nnod_new, ntot_import,               &
     &      inod_import, WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_3x1
!
! ----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_3x2(iflag_recv, nnod_new,       &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X1_new, X2_new, X3_new)
!
      use select_copy_from_recv
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(6*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X1_new(2*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(2*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(2*nnod_new)
!
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3x2(nnod_new, ntot_import,           &
     &    irev_import, WR(1), X1_new, X2_new, X3_new)
      else
!$omp parallel workshare
        X1_new(1:2*nnod_new) = 0.0d0
        X2_new(1:2*nnod_new) = 0.0d0
        X3_new(1:2*nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_3x2(nnod_new, ntot_import,               &
     &      inod_import, WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_3x2
!
!-----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_3x3(iflag_recv, nnod_new,       &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X1_new, X2_new, X3_new)
!
      use select_copy_from_recv
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(9*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X1_new(3*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(3*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(3*nnod_new)
!
!C
!C-- RECEIVE
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3x3(nnod_new, ntot_import,           &
     &    irev_import, WR(1), X1_new, X2_new, X3_new)
      else
!$omp parallel workshare
        X1_new(1:3*nnod_new) = 0.0d0
        X2_new(1:3*nnod_new) = 0.0d0
        X3_new(1:3*nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_3x3(nnod_new, ntot_import,               &
     &     inod_import, WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_3x3
!
! ----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_3x4(iflag_recv, nnod_new,       &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X1_new, X2_new, X3_new)
!
      use select_copy_from_recv
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(12*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X1_new(4*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(4*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(4*nnod_new)
!
!C
!C-- RECEIVE
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3x4(nnod_new, ntot_import,           &
     &    irev_import, WR(1), X1_new, X2_new, X3_new)
      else
!$omp parallel workshare
        X1_new(1:4*nnod_new) = 0.0d0
        X2_new(1:4*nnod_new) = 0.0d0
        X3_new(1:4*nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_3x4(nnod_new, ntot_import,               &
     &      inod_import, WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_3x4
!
! ----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_3x6(iflag_recv, nnod_new,       &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X1_new, X2_new, X3_new)
!
      use select_copy_from_recv
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(18*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X1_new(isix*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(isix*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(isix*nnod_new)
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3x6(nnod_new, ntot_import,           &
     &      irev_import, WR(1), X1_new, X2_new, X3_new)
      else
!$omp parallel workshare
        X1_new(1:6*nnod_new) = 0.0d0
        X2_new(1:6*nnod_new) = 0.0d0
        X3_new(1:6*nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_3x6(nnod_new, ntot_import,               &
     &      inod_import, WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_3x6
!
!-----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_3x8(iflag_recv, nnod_new,       &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X1_new, X2_new, X3_new)
!
      use select_copy_from_recv
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: inod_import(ntot_import)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
      real (kind=kreal), intent(inout):: WR(24*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X1_new(8*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(8*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(8*nnod_new)
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3x8(nnod_new, ntot_import,           &
     &      irev_import, WR(1), X1_new, X2_new, X3_new)
      else
!$omp parallel workshare
        X1_new(1:8*nnod_new) = 0.0d0
        X2_new(1:8*nnod_new) = 0.0d0
        X3_new(1:8*nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_3x8(nnod_new, ntot_import,               &
     &      inod_import, WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_3x8
!
!-----------------------------------------------------------------------
!
      subroutine sel_copy_from_recv_buf_3xN(iflag_recv, NB, nnod_new,   &
     &                         npe_recv, ntot_import, istack_recv,      &
     &                         inod_import, irev_import,                &
     &                         WR, X1_new, X2_new, X3_new)
!
      use select_copy_from_recv
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
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
      real (kind=kreal), intent(inout):: WR(3*NB*(ntot_import+1))
!
      real (kind=kreal), intent(inout):: X1_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X2_new(NB*nnod_new)
      real (kind=kreal), intent(inout):: X3_new(NB*nnod_new)
!
!C-- RECV
      if(iflag_recv .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3xN(NB, nnod_new, ntot_import,       &
     &      irev_import, WR(1), X1_new, X2_new, X3_new)
      else
!$omp parallel workshare
        X1_new(1:NB*nnod_new) = 0.0d0
        X2_new(1:NB*nnod_new) = 0.0d0
        X3_new(1:NB*nnod_new) = 0.0d0
!$omp end parallel workshare
!
        call set_from_recv_buf_3xN(NB, nnod_new,                        &
     &      npe_recv, ntot_import, istack_recv, inod_import,            &
     &      WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_copy_from_recv_buf_3xN
!
!-----------------------------------------------------------------------
!
      end module select_copy_from_recv_tri
