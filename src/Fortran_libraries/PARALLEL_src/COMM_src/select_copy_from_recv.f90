!>@file   select_copy_from_recv.f90
!!@brief  module select_copy_from_recv
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  scalar components data communication
!!
!!@verbatim
!!      subroutine sel_cppy_from_recv_buf_1(iflag_SR, nnod_new,         &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X_new)
!!      subroutine sel_cppy_from_recv_buf_2(iflag_SR, nnod_new,         &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X_new)
!!      subroutine sel_cppy_from_recv_buf_3(iflag_SR, nnod_new,         &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X_new)
!!      subroutine sel_cppy_from_recv_buf_6(iflag_SR, nnod_new,         &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X_new)
!!
!!      subroutine sel_cppy_from_recv_buf_3x1(iflag_SR, nnod_new,       &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X1_new, X2_new, X3_new)
!!      subroutine sel_cppy_from_recv_buf_3x2(iflag_SR, nnod_new,       &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X1_new, X2_new, X3_new)
!!      subroutine sel_cppy_from_recv_buf_3x3(iflag_SR, nnod_new,       &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X1_new, X2_new, X3_new)
!!      subroutine sel_cppy_from_recv_buf_3x6(iflag_SR, nnod_new,       &
!!     &                         ntot_import, inod_import, irev_import, &
!!     &                         WR, X1_new, X2_new, X3_new)
!!
!!      subroutine sel_cppy_from_recv_buf_int(iflag_SR, nnod_new,       &
!!     &                       ntot_import, inod_import, irev_import,   &
!!     &                       iWR, iX_new)
!!      subroutine sel_cppy_from_recv_buf_i8(iflag_SR, nnod_new,        &
!!     &                       ntot_import, inod_import, irev_import,   &
!!     &                       i8WR, i8X_new)
!!@endverbatim
!!
!!@n @param  iflag_SR    import table mode
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
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_1(iflag_SR, nnod_new,           &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_SR
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
      if(iflag_SR .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_1(nnod_new,                          &
     &      ntot_import, irev_import, WR(1), X_new)
      else
        call set_from_recv_buf_1(nnod_new,                              &
     &      ntot_import, inod_import, WR(1), X_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_1
!
!-----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_2(iflag_SR, nnod_new,           &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_SR
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
      if(iflag_SR .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_2(nnod_new,                          &
     &      ntot_import, irev_import, WR(1), X_new)
      else
        call set_from_recv_buf_2(nnod_new,                              &
     &      ntot_import, inod_import, WR(1), X_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_2
!
! ----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_3(iflag_SR, nnod_new,           &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_SR
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
      if(iflag_SR .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3(nnod_new,                          &
     &      ntot_import, irev_import, WR(1), X_new)
      else
        call set_from_recv_buf_3(nnod_new,                              &
     &    ntot_import, inod_import, WR(1), X_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_3
!
!-----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_6(iflag_SR, nnod_new,           &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_SR
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
      if(iflag_SR .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_6(nnod_new,                          &
     &      ntot_import, irev_import, WR(1), X_new)
      else
        call set_from_recv_buf_6(nnod_new,                              &
     &      ntot_import, inod_import, WR(1), X_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_6
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_3x1(iflag_SR, nnod_new,         &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X1_new, X2_new, X3_new)
!
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: iflag_SR
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
      if(iflag_SR .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3x1(nnod_new, ntot_import,           &
     &      irev_import, WR(1), X1_new, X2_new, X3_new)
      else
        call set_from_recv_buf_3x1(nnod_new, ntot_import,               &
     &      inod_import, WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_3x1
!
! ----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_3x2(iflag_SR, nnod_new,         &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X1_new, X2_new, X3_new)
!
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: iflag_SR
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
      if(iflag_SR .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3x2(nnod_new, ntot_import,           &
     &    irev_import, WR(1), X1_new, X2_new, X3_new)
      else
        call set_from_recv_buf_3x2(nnod_new, ntot_import,               &
     &      inod_import, WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_3x2
!
!-----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_3x3(iflag_SR, nnod_new,         &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X1_new, X2_new, X3_new)
!
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: iflag_SR
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
      if(iflag_SR .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3x3(nnod_new, ntot_import,           &
     &    irev_import, WR(1), X1_new, X2_new, X3_new)
      else
        call set_from_recv_buf_3x3(nnod_new, ntot_import,               &
     &     inod_import, WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_3x3
!
! ----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_3x6(iflag_SR, nnod_new,         &
     &                         ntot_import, inod_import, irev_import,   &
     &                         WR, X1_new, X2_new, X3_new)
!
      use set_from_recv_buff_tri
      use set_from_recv_buf_rev_tri
!
      integer(kind = kint), intent(in) :: iflag_SR
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
      if(iflag_SR .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_3x6(nnod_new, ntot_import,           &
     &      irev_import, WR(1), X1_new, X2_new, X3_new)
      else
        call set_from_recv_buf_3x6(nnod_new, ntot_import,               &
     &      inod_import, WR(1), X1_new, X2_new, X3_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_3x6
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_int(iflag_SR, nnod_new,         &
     &                       ntot_import, inod_import, irev_import,     &
     &                       iWR, iX_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_SR
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
      if(iflag_SR .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_int(nnod_new,                        &
     &      ntot_import, irev_import, iWR(1), iX_new)
      else
        call set_from_recv_buf_int(nnod_new,                            &
     &      ntot_import, inod_import, iWR(1), iX_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_int
!
! ----------------------------------------------------------------------
!
      subroutine sel_cppy_from_recv_buf_i8(iflag_SR, nnod_new,          &
     &                       ntot_import, inod_import, irev_import,     &
     &                       i8WR, i8X_new)
!
      use set_from_recv_buffer
      use set_from_recv_buf_rev
!
      integer(kind = kint), intent(in) :: iflag_SR
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
      if(iflag_SR .eq. iflag_import_rev) then
        call set_from_recv_buf_rev_i8(nnod_new,                         &
     &      ntot_import, irev_import, i8WR(1), i8X_new)
      else
        call set_from_recv_buf_i8(nnod_new,                             &
     &      ntot_import, inod_import, i8WR(1), i8X_new)
      end if
!
      end subroutine sel_cppy_from_recv_buf_i8
!
! ----------------------------------------------------------------------
!
      end module select_copy_from_recv
