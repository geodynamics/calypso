!>@file  cal_1edge_jacobians.f90
!!       module cal_1edge_jacobians
!!
!!@author H. Matsui
!!@date   Programmed on Dec., 2008
!!@n      Modified by H. Matsui on Aug., 2015
!
!> @brief  obtain jacobian at one gauss point for linear element
!!
!!@verbatim
!!      subroutine cal_jacobian_1d_2(numnod, numedge, nnod_4_edge,      &
!!     &           ie_edge, xx, np_smp, iedge_smp_stack,                &
!!     &           max_int_point, int_start1, ntot_int_1d,              &
!!     &           xjac, axjac, xeg, dnxi)
!!      subroutine cal_jacobian_1d_3(numnod, numedge, nnod_4_edge,      &
!!     &           ie_edge, xx, np_smp, iedge_smp_stack,                &
!!     &           max_int_point, int_start1, ntot_int_1d,              &
!!     &           xjac, axjac, xeg, dnxi)
!!
!!      subroutine cal_jacobian_1d_2_3(numnod, numedge, nnod_4_edge,    &
!!     &           ie_edge, xx, np_smp, iedge_smp_stack,                &
!!     &           max_int_point, int_start1, ntot_int_1d,              &
!!     &           xjac, axjac, xeg, dnxi)
!!@end verbatim
!
      module cal_1edge_jacobians
!
      use m_precision
      use m_geometry_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_1d_2(numnod, numedge, nnod_4_edge,        &
     &           ie_edge, xx, np_smp, iedge_smp_stack,                  &
     &           max_int_point, int_start1, ntot_int_1d,                &
     &           xjac, axjac, xeg, dnxi)
!
      use cal_jacobian_1d
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in)                                  &
     &                     :: ie_edge(numedge, nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
!
      integer(kind = kint), intent(in) :: ntot_int_1d
      real(kind = kreal), intent(in)                                    &
     &              :: dnxi(num_linear_edge,ntot_int_1d)
!
      real(kind = kreal), intent(inout) :: xjac(numedge,ntot_int_1d)
      real(kind = kreal), intent(inout) :: axjac(numedge,ntot_int_1d)
      real(kind = kreal), intent(inout) :: xeg(numedge,ntot_int_1d,3)
!
      integer (kind = kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0
          ix = int_start1(i0) + ii
!
          call s_cal_jacobian_1d_2(numnod, numedge, ie_edge, xx,        &
     &        np_smp, iedge_smp_stack, xjac(1,ix), axjac(1,ix),         &
     &        xeg(1,ix,1), xeg(1,ix,2), xeg(1,ix,3), dnxi(1,ix))
        end do
      end do
!
      end subroutine cal_jacobian_1d_2
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_1d_3(numnod, numedge, nnod_4_edge,        &
     &           ie_edge, xx, np_smp, iedge_smp_stack,                  &
     &           max_int_point, int_start1, ntot_int_1d,                &
     &           xjac, axjac, xeg, dnxi)
!
      use cal_jacobian_1d
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in)                                  &
     &                     :: ie_edge(numedge, nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
!
      integer(kind = kint), intent(in) :: ntot_int_1d
      real(kind = kreal), intent(in)                                    &
     &              :: dnxi(num_quad_edge,ntot_int_1d)
!
      real(kind = kreal), intent(inout) :: xjac(numedge,ntot_int_1d)
      real(kind = kreal), intent(inout) :: axjac(numedge,ntot_int_1d)
      real(kind = kreal), intent(inout) :: xeg(numedge,ntot_int_1d,3)
!
      integer (kind = kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0
          ix = int_start1(i0) + ii
!
          call s_cal_jacobian_1d_3(numnod, numedge, ie_edge, xx,        &
     &        np_smp, iedge_smp_stack, xjac(1,ix), axjac(1,ix),         &
     &        xeg(1,ix,1), xeg(1,ix,2), xeg(1,ix,3), dnxi(1,ix))
        end do
      end do
!
      end subroutine cal_jacobian_1d_3
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_1d_2_3(numnod, numedge, nnod_4_edge,      &
     &           ie_edge, xx, np_smp, iedge_smp_stack,                  &
     &           max_int_point, int_start1, ntot_int_1d,                &
     &           xjac, axjac, xeg, dnxi)
!
      use cal_jacobian_1d
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in)                                  &
     &                     :: ie_edge(numedge, nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start1(max_int_point)
!
      integer(kind = kint), intent(in) :: ntot_int_1d
      real(kind = kreal), intent(in)                                    &
     &              :: dnxi(num_quad_edge,ntot_int_1d)
!
      real(kind = kreal), intent(inout) :: xjac(numedge,ntot_int_1d)
      real(kind = kreal), intent(inout) :: axjac(numedge,ntot_int_1d)
      real(kind = kreal), intent(inout) :: xeg(numedge,ntot_int_1d,3)
!
      integer (kind = kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0
          ix = int_start1(i0) + ii
!
          call s_cal_jacobian_1d_2_3(numnod, numedge, ie_edge, xx,      &
     &        np_smp, iedge_smp_stack, xjac(1,ix), axjac(1,ix),         &
     &        xeg(1,ix,1), xeg(1,ix,2), xeg(1,ix,3), dnxi(1,ix))
        end do
      end do
!
      end subroutine cal_jacobian_1d_2_3
!
!-----------------------------------------------------------------------
!
      end module cal_1edge_jacobians
