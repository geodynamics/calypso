!>@file  cal_1ele_jacobians_infinte.f90
!!       module cal_1ele_jacobians_infinte
!!
!!@author H. Matsui
!!@date   Programmed on Dec., 2008
!!@n      Modified by H. Matsui on Aug., 2015
!
!> @brief  obtain jacobian at one gauss point for infity element
!!
!!@verbatim
!!      subroutine cal_jacobian_3d_inf_8(numnod, numele, nnod_4_ele,    &
!!     &          np_smp, ie, xx, num_surf_bc, surf_item, ngrp_sf_infty,&
!!     &          id_grp_sf_infty, num_surf_smp, isurf_grp_smp_stack,   &
!!     &          max_int_point, int_start3, ntot_int_3d,               &
!!     &          xjac, axjac, dnx, dxidx, dnxi, dnei, dnzi,            &
!!     &          dnxi_inf, dnei_inf, dnzi_inf)
!!      subroutine cal_jacobian_3d_inf_20(numnod, numele, nnod_4_ele,   &
!!     &          np_smp, ie, xx, num_surf_bc, surf_item, ngrp_sf_infty,&
!!     &          id_grp_sf_infty, num_surf_smp, isurf_grp_smp_stack,   &
!!     &          max_int_point, int_start3, ntot_int_3d,               &
!!     &          xjac, axjac, dnx, dxidx, dnxi, dnei, dnzi,            &
!!     &          dnxi_inf, dnei_inf, dnzi_inf)
!!      subroutine cal_jacobian_3d_inf_27(numnod, numele, nnod_4_ele,   &
!!     &          np_smp, ie, xx, num_surf_bc, surf_item, ngrp_sf_infty,&
!!     &          id_grp_sf_infty, num_surf_smp, isurf_grp_smp_stack,   &
!!     &          max_int_point, int_start3, ntot_int_3d,               &
!!     &          xjac, axjac, dnx, dxidx, dnxi, dnei, dnzi,            &
!!     &          dnxi_inf, dnei_inf, dnzi_inf)
!!
!!      subroutine cal_jacobian_3d_inf_8_20(numnod, numele, nnod_4_ele, &
!!     &          np_smp, ie, xx, num_surf_bc, surf_item, ngrp_sf_infty,&
!!     &          id_grp_sf_infty, num_surf_smp, isurf_grp_smp_stack,   &
!!     &          max_int_point, int_start3, ntot_int_3d,               &
!!     &          xjac, axjac, dnx, dxidx, dnxi, dnei, dnzi,            &
!!     &          dnxi_inf, dnei_inf, dnzi_inf)
!!@end verbatim
!
      module cal_1ele_jacobians_infinte
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use cal_shape_func_infty_3d
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_3d_inf_8(numnod, numele, nnod_4_ele,      &
     &          np_smp, ie, xx, num_surf_bc, surf_item, ngrp_sf_infty,  &
     &          id_grp_sf_infty, num_surf_smp, isurf_grp_smp_stack,     &
     &          max_int_point, int_start3, ntot_int_3d,                 &
     &          xjac, axjac, dnx, dxidx, dnxi, dnei, dnzi,              &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_linear
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind=kint), intent(in) :: num_surf_bc
      integer (kind=kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer (kind=kint), intent(in) :: ngrp_sf_infty
      integer (kind=kint), intent(in) :: id_grp_sf_infty(ngrp_sf_infty)
!
      integer(kind = kint), intent(in) :: np_smp, num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &              :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
!
      integer (kind=kint), intent(in) :: ntot_int_3d
      real(kind = kreal), intent(in) :: dnxi(num_t_linear,ntot_int_3d)
      real(kind = kreal), intent(in) :: dnei(num_t_linear,ntot_int_3d)
      real(kind = kreal), intent(in) :: dnzi(num_t_linear,ntot_int_3d)
!
      real(kind = kreal), intent(in)                                    &
     &              :: dnxi_inf(num_t_linear,nsurf_4_ele,ntot_int_3d)
      real(kind = kreal), intent(in)                                    &
     &              :: dnei_inf(num_t_linear,nsurf_4_ele,ntot_int_3d)
      real(kind = kreal), intent(in)                                    &
     &              :: dnzi_inf(num_t_linear,nsurf_4_ele,ntot_int_3d)
!
      real(kind = kreal), intent(inout) :: dxidx(numele,ntot_int_3d,3,3)
!
      real(kind = kreal), intent(inout) :: xjac(numele,ntot_int_3d)
      real(kind = kreal), intent(inout) :: axjac(numele,ntot_int_3d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: dnx(numele,num_t_linear,ntot_int_3d,3)
!
      integer (kind=kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_8                                  &
     &       (numnod, numele, np_smp, ie, xx, num_surf_bc, surf_item,   &
     &        ngrp_sf_infty, id_grp_sf_infty, num_surf_smp,             &
     &        isurf_grp_smp_stack, xjac(1,ix), axjac(1,ix),             &
     &        dnx(1,1,ix,1), dnx(1,1,ix,2), dnx(1,1,ix,3),              &
     &        dxidx(1,ix,1,1), dxidx(1,ix,2,1), dxidx(1,ix,3,1),        &
     &        dxidx(1,ix,1,2), dxidx(1,ix,2,2), dxidx(1,ix,3,2),        &
     &        dxidx(1,ix,1,3), dxidx(1,ix,2,3), dxidx(1,ix,3,3),        &
     &        dnxi(1,ix), dnei(1,ix), dnzi(1,ix),                       &
     &        dnxi_inf(1,1,ix), dnei_inf(1,1,ix), dnzi_inf(1,1,ix))
        end do
      end do
!
!
      end subroutine cal_jacobian_3d_inf_8
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_3d_inf_20(numnod, numele, nnod_4_ele,     &
     &          np_smp, ie, xx, num_surf_bc, surf_item, ngrp_sf_infty,  &
     &          id_grp_sf_infty, num_surf_smp, isurf_grp_smp_stack,     &
     &          max_int_point, int_start3, ntot_int_3d,                 &
     &          xjac, axjac, dnx, dxidx, dnxi, dnei, dnzi,              &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_quad
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind=kint), intent(in) :: num_surf_bc
      integer (kind=kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer (kind=kint), intent(in) :: ngrp_sf_infty
      integer (kind=kint), intent(in) :: id_grp_sf_infty(ngrp_sf_infty)
!
      integer(kind = kint), intent(in) :: np_smp, num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &              :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
!
      integer (kind=kint), intent(in) :: ntot_int_3d
      real(kind = kreal), intent(in) :: dnxi(num_t_quad,ntot_int_3d)
      real(kind = kreal), intent(in) :: dnei(num_t_quad,ntot_int_3d)
      real(kind = kreal), intent(in) :: dnzi(num_t_quad,ntot_int_3d)
!
      real(kind = kreal), intent(in)                                    &
     &              :: dnxi_inf(num_t_quad,nsurf_4_ele,ntot_int_3d)
      real(kind = kreal), intent(in)                                    &
     &              :: dnei_inf(num_t_quad,nsurf_4_ele,ntot_int_3d)
      real(kind = kreal), intent(in)                                    &
     &              :: dnzi_inf(num_t_quad,nsurf_4_ele,ntot_int_3d)
!
      real(kind = kreal), intent(inout) :: dxidx(numele,ntot_int_3d,3,3)
!
      real(kind = kreal), intent(inout) :: xjac(numele,ntot_int_3d)
      real(kind = kreal), intent(inout) :: axjac(numele,ntot_int_3d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: dnx(numele,num_t_quad,ntot_int_3d,3)
!
      integer (kind=kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_20                                 &
     &       (numnod, numele, np_smp, ie, xx, num_surf_bc, surf_item,   &
     &        ngrp_sf_infty, id_grp_sf_infty, num_surf_smp,             &
     &        isurf_grp_smp_stack, xjac(1,ix), axjac(1,ix),             &
     &        dnx(1,1,ix,1), dnx(1,1,ix,2), dnx(1,1,ix,3),              &
     &        dxidx(1,ix,1,1), dxidx(1,ix,2,1), dxidx(1,ix,3,1),        &
     &        dxidx(1,ix,1,2), dxidx(1,ix,2,2), dxidx(1,ix,3,2),        &
     &        dxidx(1,ix,1,3), dxidx(1,ix,2,3), dxidx(1,ix,3,3),        &
     &        dnxi(1,ix), dnei(1,ix), dnzi(1,ix),                       &
     &        dnxi_inf(1,1,ix), dnei_inf(1,1,ix), dnzi_inf(1,1,ix))
        end do
      end do
!
!
      end subroutine cal_jacobian_3d_inf_20
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_3d_inf_27(numnod, numele, nnod_4_ele,     &
     &          np_smp, ie, xx, num_surf_bc, surf_item, ngrp_sf_infty,  &
     &          id_grp_sf_infty, num_surf_smp, isurf_grp_smp_stack,     &
     &          max_int_point, int_start3, ntot_int_3d,                 &
     &          xjac, axjac, dnx, dxidx, dnxi, dnei, dnzi,              &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_lag
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind=kint), intent(in) :: num_surf_bc
      integer (kind=kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer (kind=kint), intent(in) :: ngrp_sf_infty
      integer (kind=kint), intent(in) :: id_grp_sf_infty(ngrp_sf_infty)
!
      integer(kind = kint), intent(in) :: np_smp, num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &              :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
!
      integer (kind=kint), intent(in) :: ntot_int_3d
      real(kind = kreal), intent(in) :: dnxi(num_t_lag,ntot_int_3d)
      real(kind = kreal), intent(in) :: dnei(num_t_lag,ntot_int_3d)
      real(kind = kreal), intent(in) :: dnzi(num_t_lag,ntot_int_3d)
!
      real(kind = kreal), intent(in)                                    &
     &              :: dnxi_inf(num_t_lag,nsurf_4_ele,ntot_int_3d)
      real(kind = kreal), intent(in)                                    &
     &              :: dnei_inf(num_t_lag,nsurf_4_ele,ntot_int_3d)
      real(kind = kreal), intent(in)                                    &
     &              :: dnzi_inf(num_t_lag,nsurf_4_ele,ntot_int_3d)
!
      real(kind = kreal), intent(inout) :: dxidx(numele,ntot_int_3d,3,3)
!
      real(kind = kreal), intent(inout) :: xjac(numele,ntot_int_3d)
      real(kind = kreal), intent(inout) :: axjac(numele,ntot_int_3d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: dnx(numele,num_t_lag,ntot_int_3d,3)
!
      integer (kind=kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_27                                 &
     &       (numnod, numele, np_smp, ie, xx, num_surf_bc, surf_item,   &
     &        ngrp_sf_infty, id_grp_sf_infty, num_surf_smp,             &
     &        isurf_grp_smp_stack, xjac(1,ix), axjac(1,ix),             &
     &        dnx(1,1,ix,1), dnx(1,1,ix,2), dnx(1,1,ix,3),              &
     &        dxidx(1,ix,1,1), dxidx(1,ix,2,1), dxidx(1,ix,3,1),        &
     &        dxidx(1,ix,1,2), dxidx(1,ix,2,2), dxidx(1,ix,3,2),        &
     &        dxidx(1,ix,1,3), dxidx(1,ix,2,3), dxidx(1,ix,3,3),        &
     &        dnxi(1,ix), dnei(1,ix), dnzi(1,ix),                       &
     &        dnxi_inf(1,1,ix), dnei_inf(1,1,ix), dnzi_inf(1,1,ix))
        end do
      end do
!
!
      end subroutine cal_jacobian_3d_inf_27
!
!-----------------------------------------------------------------------
!
      subroutine cal_jacobian_3d_inf_8_20(numnod, numele, nnod_4_ele,   &
     &          np_smp, ie, xx, num_surf_bc, surf_item, ngrp_sf_infty,  &
     &          id_grp_sf_infty, num_surf_smp, isurf_grp_smp_stack,     &
     &          max_int_point, int_start3, ntot_int_3d,                 &
     &          xjac, axjac, dnx, dxidx, dnxi, dnei, dnzi,              &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      use cal_jacobian_3d_inf_l_quad
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind=kint), intent(in) :: num_surf_bc
      integer (kind=kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer (kind=kint), intent(in) :: ngrp_sf_infty
      integer (kind=kint), intent(in) :: id_grp_sf_infty(ngrp_sf_infty)
!
      integer(kind = kint), intent(in) :: np_smp, num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &              :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
!
      integer (kind=kint), intent(in) :: ntot_int_3d
      real(kind = kreal), intent(in) :: dnxi(num_t_quad,ntot_int_3d)
      real(kind = kreal), intent(in) :: dnei(num_t_quad,ntot_int_3d)
      real(kind = kreal), intent(in) :: dnzi(num_t_quad,ntot_int_3d)
!
      real(kind = kreal), intent(in)                                    &
     &              :: dnxi_inf(num_t_quad,nsurf_4_ele,ntot_int_3d)
      real(kind = kreal), intent(in)                                    &
     &              :: dnei_inf(num_t_quad,nsurf_4_ele,ntot_int_3d)
      real(kind = kreal), intent(in)                                    &
     &              :: dnzi_inf(num_t_quad,nsurf_4_ele,ntot_int_3d)
!
      real(kind = kreal), intent(inout) :: dxidx(numele,ntot_int_3d,3,3)
!
      real(kind = kreal), intent(inout) :: xjac(numele,ntot_int_3d)
      real(kind = kreal), intent(inout) :: axjac(numele,ntot_int_3d)
      real(kind = kreal), intent(inout)                                 &
     &                   :: dnx(numele,num_t_quad,ntot_int_3d,3)
!
      integer (kind=kint) :: ii, ix, i0
!
!
      do i0 = 1, max_int_point
        do ii = 1, i0*i0*i0
          ix = int_start3(i0) + ii
!
          call s_cal_jacobian_3d_inf_8_20                               &
     &       (numnod, numele, np_smp, ie, xx, num_surf_bc, surf_item,   &
     &        ngrp_sf_infty, id_grp_sf_infty, num_surf_smp,             &
     &        isurf_grp_smp_stack, xjac(1,ix), axjac(1,ix),             &
     &        dnx(1,1,ix,1), dnx(1,1,ix,2), dnx(1,1,ix,3),              &
     &        dxidx(1,ix,1,1), dxidx(1,ix,2,1), dxidx(1,ix,3,1),        &
     &        dxidx(1,ix,1,2), dxidx(1,ix,2,2), dxidx(1,ix,3,2),        &
     &        dxidx(1,ix,1,3), dxidx(1,ix,2,3), dxidx(1,ix,3,3),        &
     &        dnxi(1,ix), dnei(1,ix), dnzi(1,ix),                       &
     &        dnxi_inf(1,1,ix), dnei_inf(1,1,ix), dnzi_inf(1,1,ix))
        end do
      end do
!
!
      end subroutine cal_jacobian_3d_inf_8_20
!
!-----------------------------------------------------------------------
!
      end module cal_1ele_jacobians_infinte
