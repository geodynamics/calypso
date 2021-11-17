!>@file  cal_1surf_grp_jacobians.f90
!!       module cal_1surf_grp_jacobians
!!
!!@author H. Matsui
!!@date   Programmed on Dec., 2008
!!@n      Modified by H. Matsui on Aug., 2015
!
!> @brief  obtain jacobian at one gauss point for linear element
!!
!!@verbatim
!!      subroutine cal_jacobian_sf_grp_4(numnod, numele, nnod_4_ele,    &
!!     &          ie, xx, num_surf, num_surf_bc, surf_item,             &
!!     &          np_smp, num_surf_smp, isurf_grp_smp_stack,            &
!!     &          max_int_point, int_start2, ntot_int_2d,               &
!!     &          xjac, axjac, xsf, dnxi, dnei)
!!      subroutine cal_jacobian_sf_grp_8(numnod, numele, nnod_4_ele,    &
!!     &          ie, xx, num_surf, num_surf_bc, surf_item,             &
!!     &          np_smp, num_surf_smp, isurf_grp_smp_stack,            &
!!     &          max_int_point, int_start2, ntot_int_2d,               &
!!     &          xjac, axjac, xsf, dnxi, dnei)
!!      subroutine cal_jacobian_sf_grp_9(numnod, numele, nnod_4_ele,    &
!!     &          ie, xx, num_surf, num_surf_bc, surf_item,             &
!!     &          np_smp, num_surf_smp, isurf_grp_smp_stack,            &
!!     &          max_int_point, int_start2, ntot_int_2d,               &
!!     &          xjac, axjac, xsf, dnxi, dnei)
!!
!!      subroutine cal_jacobian_sf_grp_4_8(numnod, numele, nnod_4_ele,  &
!!     &          ie, xx, num_surf, num_surf_bc, surf_item,             &
!!     &          np_smp, num_surf_smp, isurf_grp_smp_stack,            &
!!     &          max_int_point, int_start2, ntot_int_2d,               &
!!     &          xjac, axjac, xsf, dnxi, dnei)
!!@end verbatim
!
      module cal_1surf_grp_jacobians
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
      subroutine cal_jacobian_sf_grp_4(numnod, numele, nnod_4_ele,      &
     &          ie, xx, num_surf, num_surf_bc, surf_item,               &
     &          np_smp, num_surf_smp, isurf_grp_smp_stack,              &
     &          max_int_point, int_start2, ntot_int_2d,                 &
     &          xjac, axjac, xsf, dnxi, dnei)
!
      use cal_jacobian_sf_grp_linear
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind=kint), intent(in) :: np_smp
      integer (kind=kint), intent(in) :: num_surf_smp
      integer (kind=kint), intent(in)                                   &
     &                 :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind=kint), intent(in) :: num_surf
      integer (kind=kint), intent(in) :: num_surf_bc
      integer (kind=kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
!
      integer(kind = kint), intent(in) :: ntot_int_2d
      real(kind = kreal), intent(in) :: dnxi(num_linear_sf,ntot_int_2d)
      real(kind = kreal), intent(in) :: dnei(num_linear_sf,ntot_int_2d)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: xjac(num_surf_bc,ntot_int_2d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: axjac(num_surf_bc,ntot_int_2d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: xsf(num_surf_bc,ntot_int_2d,3)
!
      integer (kind = kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_4(numnod, numele, ie, xx,          &
     &        num_surf, num_surf_bc, surf_item,                         &
     &        np_smp, num_surf_smp, isurf_grp_smp_stack,                &
     &        xjac(1,ix), axjac(1,ix),                                  &
     &        xsf(1,ix,1), xsf(1,ix,2),  xsf(1,ix,3),                   &
     &        dnxi(1,ix), dnei(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_sf_grp_4
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_sf_grp_8(numnod, numele, nnod_4_ele,      &
     &          ie, xx, num_surf, num_surf_bc, surf_item,               &
     &          np_smp, num_surf_smp, isurf_grp_smp_stack,              &
     &          max_int_point, int_start2, ntot_int_2d,                 &
     &          xjac, axjac, xsf, dnxi, dnei)
!
      use cal_jacobian_sf_grp_quad
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind=kint), intent(in) :: np_smp
      integer (kind=kint), intent(in) :: num_surf_smp
      integer (kind=kint), intent(in)                                   &
     &                 :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind=kint), intent(in) :: num_surf
      integer (kind=kint), intent(in) :: num_surf_bc
      integer (kind=kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
!
      integer(kind = kint), intent(in) :: ntot_int_2d
      real(kind = kreal), intent(in) :: dnxi(num_t_quad,ntot_int_2d)
      real(kind = kreal), intent(in) :: dnei(num_t_quad,ntot_int_2d)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: xjac(num_surf_bc,ntot_int_2d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: axjac(num_surf_bc,ntot_int_2d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: xsf(num_surf_bc,ntot_int_2d,3)
!
      integer (kind = kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_8(numnod, numele, ie, xx,          &
     &        num_surf, num_surf_bc, surf_item,                         &
     &        np_smp, num_surf_smp, isurf_grp_smp_stack,                &
     &        xjac(1,ix), axjac(1,ix),                                  &
     &        xsf(1,ix,1), xsf(1,ix,2),  xsf(1,ix,3),                   &
     &        dnxi(1,ix), dnei(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_sf_grp_8
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_sf_grp_9(numnod, numele, nnod_4_ele,      &
     &          ie, xx, num_surf, num_surf_bc, surf_item,               &
     &          np_smp, num_surf_smp, isurf_grp_smp_stack,              &
     &          max_int_point, int_start2, ntot_int_2d,                 &
     &          xjac, axjac, xsf, dnxi, dnei)
!
      use cal_jacobian_sf_grp_lag
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind=kint), intent(in) :: np_smp
      integer (kind=kint), intent(in) :: num_surf_smp
      integer (kind=kint), intent(in)                                   &
     &                 :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind=kint), intent(in) :: num_surf
      integer (kind=kint), intent(in) :: num_surf_bc
      integer (kind=kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
!
      integer(kind = kint), intent(in) :: ntot_int_2d
      real(kind = kreal), intent(in) :: dnxi(num_lag_sf,ntot_int_2d)
      real(kind = kreal), intent(in) :: dnei(num_lag_sf,ntot_int_2d)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: xjac(num_surf_bc,ntot_int_2d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: axjac(num_surf_bc,ntot_int_2d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: xsf(num_surf_bc,ntot_int_2d,3)
!
      integer (kind = kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_9(numnod, numele, ie, xx,          &
     &        num_surf, num_surf_bc, surf_item,                         &
     &        np_smp, num_surf_smp, isurf_grp_smp_stack,                &
     &        xjac(1,ix), axjac(1,ix),                                  &
     &        xsf(1,ix,1), xsf(1,ix,2),  xsf(1,ix,3),                   &
     &        dnxi(1,ix), dnei(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_sf_grp_9
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_sf_grp_4_8(numnod, numele, nnod_4_ele,    &
     &          ie, xx, num_surf, num_surf_bc, surf_item,               &
     &          np_smp, num_surf_smp, isurf_grp_smp_stack,              &
     &          max_int_point, int_start2, ntot_int_2d,                 &
     &          xjac, axjac, xsf, dnxi, dnei)
!
      use cal_jacobian_sf_grp_l_quad
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind=kint), intent(in) :: np_smp
      integer (kind=kint), intent(in) :: num_surf_smp
      integer (kind=kint), intent(in)                                   &
     &                 :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind=kint), intent(in) :: num_surf
      integer (kind=kint), intent(in) :: num_surf_bc
      integer (kind=kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start2(max_int_point)
!
      integer(kind = kint), intent(in) :: ntot_int_2d
      real(kind = kreal), intent(in) :: dnxi(num_quad_sf,ntot_int_2d)
      real(kind = kreal), intent(in) :: dnei(num_quad_sf,ntot_int_2d)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: xjac(num_surf_bc,ntot_int_2d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: axjac(num_surf_bc,ntot_int_2d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: xsf(num_surf_bc,ntot_int_2d,3)
!
      integer (kind = kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0
          ix = int_start2(i0) + ii
!
          call s_cal_jacobian_sf_grp_4_8(numnod, numele, ie, xx,        &
     &        num_surf, num_surf_bc, surf_item,                         &
     &        np_smp, num_surf_smp, isurf_grp_smp_stack,                &
     &        xjac(1,ix), axjac(1,ix),                                  &
     &        xsf(1,ix,1), xsf(1,ix,2),  xsf(1,ix,3),                   &
     &        dnxi(1,ix), dnei(1,ix) )
        end do
      end do
!
      end subroutine cal_jacobian_sf_grp_4_8
!
!-----------------------------------------------------------------------
!
      end module cal_1surf_grp_jacobians
