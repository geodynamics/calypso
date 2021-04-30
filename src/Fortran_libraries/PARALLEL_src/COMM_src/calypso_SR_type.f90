!>@file   calypso_SR_type.f90
!!@brief  module calypso_SR_type
!!
!!@author H. Matsui
!!@date Programmed in March, 2013
!
!>@brief  Select communication routines for spherical harmonics transform
!!
!!@verbatim
!!      subroutine calypso_SR_type_1(iflag_recv, cps_tbl,               &
!!     &          nnod_org, nnod_new, X_org, X_new)
!!      subroutine calypso_SR_type_3(iflag_recv, cps_tbl,               &
!!     &          nnod_org, nnod_new, X_org, X_new)
!!      subroutine calypso_SR_type_N(iflag_recv, NB, cps_tbl,           &
!!     &          nnod_org, nnod_new, X_org, X_new)
!!      subroutine calypso_SR_type_3xN(iflag_recv, NB, cps_tbl,         &
!!     &          nnod_org, nnod_new, X1_org, X2_org, X3_org,           &
!!     &          X1_new, X2_new, X3_new)
!!
!!      subroutine calypso_SR_type_int(iflag_recv, cps_tbl,             &
!!     &          nnod_org, nnod_new, iX_org, iX_new)
!!      subroutine calypso_SR_type_int8(iflag_recv, cps_tbl,            &
!!     &          nnod_org, nnod_new, i8X_org, i8X_new)
!!
!!      subroutine check_calypso_SR_N(NB, cps_tbl)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!@n
!!@n @param  X_org(NB*nnod_org)   Arbitrary components of send data
!!@n @param  X_new(NB*nnod_new)   Arbitrary components of received data
!!@n
!!@n @param  X_org(nnod_org)   Scalar send data
!!@n @param  X_new(nnod_new)   Scalar received data
!!@n
!!@n @param  iX_org(nnod_org)   Integer send data
!!@n @param  iX_new(nnod_new)   Integer received data
!
      module calypso_SR_type
!
      use m_precision
      use m_constants
      use t_calypso_comm_table
      use t_solver_SR
      use m_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine calypso_SR_type_1(iflag_recv, cps_tbl,                 &
     &          nnod_org, nnod_new, X_org, X_new)
!
      use calypso_SR
!
      type(calypso_comm_table), intent(in) :: cps_tbl
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      real (kind=kreal), intent(in)::    X_org(nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(nnod_new)
!
!
      call calypso_send_recv(iflag_recv, nnod_org, nnod_new,            &
     &    cps_tbl%nrank_export, cps_tbl%irank_export,                   &
     &    cps_tbl%istack_export, cps_tbl%item_export,                   &
     &    cps_tbl%nrank_import, cps_tbl%irank_import,                   &
     &    cps_tbl%istack_import, cps_tbl%item_import,                   &
     &    cps_tbl%irev_import, cps_tbl%iflag_self_copy,                 &
     &    SR_sig1, SR_r1, X_org, X_new)
!
      end subroutine calypso_SR_type_1
!
!-----------------------------------------------------------------------
!
      subroutine calypso_SR_type_3(iflag_recv, cps_tbl,                 &
     &          nnod_org, nnod_new, X_org, X_new)
!
      use calypso_SR_3
!
      type(calypso_comm_table), intent(in) :: cps_tbl
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      real (kind=kreal), intent(in)::    X_org(3*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(3*nnod_new)
!
!
      call calypso_send_recv_3(iflag_recv, nnod_org, nnod_new,          &
     &    cps_tbl%nrank_export, cps_tbl%irank_export,                   &
     &    cps_tbl%istack_export, cps_tbl%item_export,                   &
     &    cps_tbl%nrank_import, cps_tbl%irank_import,                   &
     &    cps_tbl%istack_import, cps_tbl%item_import,                   &
     &    cps_tbl%irev_import, cps_tbl%iflag_self_copy,                 &
     &    SR_sig1, SR_r1, X_org, X_new)
!
      end subroutine calypso_SR_type_3
!
!-----------------------------------------------------------------------
!
      subroutine calypso_SR_type_N(iflag_recv, NB, cps_tbl,             &
     &          nnod_org, nnod_new, X_org, X_new)
!
      use calypso_SR_N
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
!
      type(calypso_comm_table), intent(in) :: cps_tbl
!
      real (kind=kreal), intent(in)::    X_org(NB*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
!
      call calypso_send_recv_N(iflag_recv, NB, nnod_org, nnod_new,      &
     &    cps_tbl%nrank_export, cps_tbl%irank_export,                   &
     &    cps_tbl%istack_export, cps_tbl%item_export,                   &
     &    cps_tbl%nrank_import, cps_tbl%irank_import,                   &
     &    cps_tbl%istack_import, cps_tbl%item_import,                   &
     &    cps_tbl%irev_import, cps_tbl%iflag_self_copy,                 &
     &    SR_sig1, SR_r1, X_org, X_new)
!
      end subroutine calypso_SR_type_N
!
!-----------------------------------------------------------------------
!
      subroutine calypso_SR_type_3xN(iflag_recv, NB, cps_tbl,           &
     &          nnod_org, nnod_new, X1_org, X2_org, X3_org,             &
     &          X1_new, X2_new, X3_new)
!
      use calypso_SR_N
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
!
      type(calypso_comm_table), intent(in) :: cps_tbl
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
      call calypso_send_recv_3xN(iflag_recv, NB, nnod_org, nnod_new,    &
     &    cps_tbl%nrank_export, cps_tbl%irank_export,                   &
     &    cps_tbl%istack_export, cps_tbl%item_export,                   &
     &    cps_tbl%nrank_import, cps_tbl%irank_import,                   &
     &    cps_tbl%istack_import, cps_tbl%item_import,                   &
     &    cps_tbl%irev_import, cps_tbl%iflag_self_copy,                 &
     &    SR_sig1, SR_r1, X1_org, X2_org, X3_org,                       &
     &                    X1_new, X2_new, X3_new)
!
      end subroutine calypso_SR_type_3xN
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine calypso_SR_type_int(iflag_recv, cps_tbl,               &
     &          nnod_org, nnod_new, iX_org, iX_new)
!
      use calypso_SR_int
!
      type(calypso_comm_table), intent(in) :: cps_tbl
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      integer (kind=kint), intent(in):: iX_org(nnod_org)
!
      integer (kind=kint), intent(inout):: iX_new(nnod_new)
!
!
      call calypso_send_recv_int(iflag_recv, nnod_org, nnod_new,        &
     &    cps_tbl%nrank_export, cps_tbl%irank_export,                   &
     &    cps_tbl%istack_export, cps_tbl%item_export,                   &
     &    cps_tbl%iflag_self_copy, cps_tbl%nrank_import,                &
     &    cps_tbl%irank_import, cps_tbl%istack_import,                  &
     &    cps_tbl%item_import, cps_tbl%irev_import,                     &
     &    SR_sig1, SR_i1, iX_org, iX_new)
!
      end subroutine calypso_SR_type_int
!
! ----------------------------------------------------------------------
!
      subroutine calypso_SR_type_int8(iflag_recv, cps_tbl,              &
     &          nnod_org, nnod_new, i8X_org, i8X_new)
!
      use calypso_SR_int
!
      type(calypso_comm_table), intent(in) :: cps_tbl
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      integer(kind = kint_gl), intent(in):: i8X_org(nnod_org)
!
      integer(kind = kint_gl), intent(inout):: i8X_new(nnod_new)
!
!
      call calypso_send_recv_int8(iflag_recv, nnod_org, nnod_new,       &
     &    cps_tbl%nrank_export, cps_tbl%irank_export,                   &
     &    cps_tbl%istack_export, cps_tbl%item_export,                   &
     &    cps_tbl%nrank_import, cps_tbl%iflag_self_copy,                &
     &    cps_tbl%irank_import, cps_tbl%istack_import,                  &
     &    cps_tbl%item_import, cps_tbl%irev_import,                     &
     &    SR_sig1, SR_il1, i8X_org, i8X_new)
!
      end subroutine calypso_SR_type_int8
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_calypso_SR_N(NB, cps_tbl)
!
      use calypso_SR_core
!
      integer(kind = kint), intent(in) :: NB
      type(calypso_comm_table), intent(in) :: cps_tbl
!
!
      call check_calypso_SR_stack                                       &
     &   (NB, cps_tbl%nrank_export, cps_tbl%iflag_self_copy,            &
     &    cps_tbl%istack_export, cps_tbl%nrank_import,                  &
     &    cps_tbl%iflag_self_copy, cps_tbl%istack_import,               &
     &    SR_sig1, SR_r1)
!
      end subroutine check_calypso_SR_N
!
!-----------------------------------------------------------------------
!
      end module calypso_SR_type
