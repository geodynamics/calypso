!>@file   cal_position_and_grad.f90
!!@brief  module cal_position_and_grad
!!
!!@date  Programmed by H.Matsui in Aug. 2011
!
!>@brief find point in one element and return local coordinate
!!
!!@verbatim
!!      subroutine cal_position_and_gradient(nnod_ele, xx_z,            &
!!     &          dnxi, dnei, dnzi, x_local_ele, xi)
!!        integer (kind = kint), intent(in) :: nnod_ele
!!        real(kind = kreal), intent(in) :: x_local_ele(nnod_ele,3)
!!        real(kind = kreal), intent(in) :: xi(3)
!!        real(kind=kreal), intent(inout) :: xx_z(3)
!!        real(kind=kreal), intent(inout) :: dnxi(3), dnei(3), dnzi(3)
!!      subroutine cal_position_and_grad_surf(nnod_sf, xx_z, dnxi, dnei,&
!!     &                                      x_local_ele, xi)
!!        integer (kind = kint), intent(in) :: nnod_sf
!!        real(kind = kreal), intent(in) :: x_local_ele(nnod_sf,3)
!!        real(kind = kreal), intent(in) :: xi(2)
!!        real(kind=kreal), intent(inout) :: xx_z(3)
!!        real(kind=kreal), intent(inout) :: dnxi(3), dnei(3)
!!      subroutine cal_position_and_grad_edge(nnod_ed, xx_z,  dnxi,     &
!!     &                                      x_local_ele, xi)
!!        integer (kind = kint), intent(in) :: nnod_ed
!!        real(kind = kreal), intent(in) :: x_local_ele(nnod_ed,3)
!!        real(kind = kreal), intent(in) :: xi(1)
!!        real(kind=kreal), intent(inout) :: xx_z(3), dnxi(3)
!!@endverbatim
!
      module cal_position_and_grad
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
!
      implicit none
!
      private :: cal_position_and_grad_8, cal_position_and_grad_20
      private :: cal_position_and_grad_27
      private :: cal_position_and_grad_4, cal_position_and_grad_sf8
      private :: cal_position_and_grad_9
      private :: cal_position_and_grad_2, cal_position_and_grad_3
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_gradient(nnod_ele, xx_z,              &
     &          dnxi, dnei, dnzi, x_local_ele, xi)
!
      integer (kind = kint), intent(in) :: nnod_ele
      real(kind = kreal), intent(in) :: x_local_ele(nnod_ele,3)
      real(kind = kreal), intent(in) :: xi(3)
!
      real(kind=kreal), intent(inout) :: xx_z(3)
      real(kind=kreal), intent(inout) :: dnxi(3), dnei(3), dnzi(3)
!
!
        if (nnod_ele .eq. num_t_linear) then
          call cal_position_and_grad_8(xx_z, dnxi, dnei, dnzi,          &
     &                                 x_local_ele, xi)
        else if (nnod_ele .eq. num_t_quad) then
          call cal_position_and_grad_20(xx_z, dnxi, dnei, dnzi,         &
     &                                  x_local_ele, xi)
        else if (nnod_ele .eq. num_t_lag) then
          call cal_position_and_grad_27(xx_z, dnxi, dnei, dnzi,         &
     &                                  x_local_ele, xi)
        end if
!
      end subroutine cal_position_and_gradient
!
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_grad_surf(nnod_sf, xx_z, dnxi, dnei,  &
     &                                      x_local_ele, xi)
!
      integer (kind = kint), intent(in) :: nnod_sf
      real(kind = kreal), intent(in) :: x_local_ele(nnod_sf,3)
      real(kind = kreal), intent(in) :: xi(2)
!
      real(kind=kreal), intent(inout) :: xx_z(3)
      real(kind=kreal), intent(inout) :: dnxi(3), dnei(3)
!
!
      if (nnod_sf .eq. num_linear_sf) then
        call cal_position_and_grad_4(xx_z, dnxi, dnei,                  &
     &                               x_local_ele, xi)
      else if (nnod_sf .eq. num_quad_sf) then
        call cal_position_and_grad_sf8(xx_z, dnxi, dnei,                &
     &                                 x_local_ele, xi)
      else if (nnod_sf .eq. num_lag_sf) then
        call cal_position_and_grad_9(xx_z, dnxi, dnei,                  &
     &                               x_local_ele, xi)
      end if
!
      end subroutine cal_position_and_grad_surf
!
!
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_grad_edge(nnod_ed, xx_z,  dnxi,       &
     &                                      x_local_ele, xi)
!
      integer (kind = kint), intent(in) :: nnod_ed
      real(kind = kreal), intent(in) :: x_local_ele(nnod_ed,3)
      real(kind = kreal), intent(in) :: xi(1)
!
      real(kind=kreal), intent(inout) :: xx_z(3), dnxi(3)
!
!
        if (nnod_ed .eq. num_linear_edge) then
          call cal_position_and_grad_2(xx_z, dnxi, x_local_ele, xi)
        else if (nnod_ed .eq. num_quad_edge) then
          call cal_position_and_grad_3(xx_z, dnxi, x_local_ele, xi)
        end if
!
      end subroutine cal_position_and_grad_edge
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_grad_8(xx_z, dnxi, dnei, dnzi,        &
     &          x_local_ele, xi)
!
      use cal_shape_function_3d
      use interpolate_position_in_ele
!
      real(kind = kreal), intent(in) :: x_local_ele(8,3)
      real(kind = kreal), intent(in) :: xi(3)
!
      real(kind=kreal), intent(inout) :: xx_z(3)
      real(kind=kreal), intent(inout) :: dnxi(3), dnei(3), dnzi(3)
!
      real(kind=kreal) :: an_l(8,1)
      real(kind=kreal) :: dnxi_l(8,1), dnei_l(8,1),  dnzi_l(8,1)
!
!
          call s_cal_shape_function_linear(ione, an_l, dnxi_l,         &
     &        dnei_l, dnzi_l, xi(1), xi(2), xi(3) )
          call interporate_one_position_linear(xx_z, x_local_ele,      &
     &        an_l(1,1) )
          call interporate_one_position_linear(dnxi, x_local_ele,      &
     &        dnxi_l(1,1) )
          call interporate_one_position_linear(dnei, x_local_ele,      &
     &        dnei_l(1,1) )
          call interporate_one_position_linear(dnzi, x_local_ele,      &
     &        dnzi_l(1,1) )
!
      end subroutine cal_position_and_grad_8
!
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_grad_20(xx_z, dnxi, dnei, dnzi,       &
     &          x_local_ele, xi)
!
      use cal_shape_function_3d
      use interpolate_position_in_ele
!
      real(kind = kreal), intent(in) :: x_local_ele(20,3)
      real(kind = kreal), intent(in) :: xi(3)
!
      real(kind=kreal), intent(inout) :: xx_z(3)
      real(kind=kreal), intent(inout) :: dnxi(3), dnei(3), dnzi(3)
!
      real(kind=kreal) :: an_l(20,1)
      real(kind=kreal) :: dnxi_l(20,1), dnei_l(20,1),  dnzi_l(20,1)
!
!
          call s_cal_shape_function_quad(ione, an_l, dnxi_l,           &
     &           dnei_l, dnzi_l, xi(1), xi(2), xi(3) )
          call interporate_one_position_quad(xx_z, x_local_ele,        &
     &        an_l(1,1) )
          call interporate_one_position_quad(dnxi, x_local_ele,        &
     &        dnxi_l(1,1) )
          call interporate_one_position_quad(dnei, x_local_ele,        &
     &        dnei_l(1,1) )
          call interporate_one_position_quad(dnzi, x_local_ele,        &
     &        dnzi_l(1,1) )
!
      end subroutine cal_position_and_grad_20
!
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_grad_27(xx_z, dnxi, dnei, dnzi,       &
     &          x_local_ele, xi)
!
      use cal_shape_function_3d
      use interpolate_position_in_ele
!
      real(kind = kreal), intent(in) :: x_local_ele(27,3)
      real(kind = kreal), intent(in) :: xi(3)
!
      real(kind=kreal), intent(inout) :: xx_z(3)
      real(kind=kreal), intent(inout) :: dnxi(3), dnei(3), dnzi(3)
!
      real(kind=kreal) :: an_l(27,1)
      real(kind=kreal) :: dnxi_l(27,1), dnei_l(27,1),  dnzi_l(27,1)
!
!
          call s_cal_shape_function_lag(ione, an_l, dnxi_l,            &
     &           dnei_l, dnzi_l, xi(1), xi(2), xi(3) )
          call interporate_one_position_lag(xx_z, x_local_ele,         &
     &        an_l(1,1) )
          call interporate_one_position_lag(dnxi, x_local_ele,         &
     &        dnxi_l(1,1) )
          call interporate_one_position_lag(dnei, x_local_ele,         &
     &        dnei_l(1,1) )
          call interporate_one_position_lag(dnzi, x_local_ele,         &
     &        dnzi_l(1,1) )
!
      end subroutine cal_position_and_grad_27
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_grad_4(xx_z, dnxi, dnei,              &
     &          x_local_ele, xi)
!
      use cal_shape_function_2d
      use interpolate_position_in_ele
!
      real(kind = kreal), intent(in) :: x_local_ele(num_linear_sf,3)
      real(kind = kreal), intent(in) :: xi(2)
!
      real(kind=kreal), intent(inout) :: xx_z(3)
      real(kind=kreal), intent(inout) :: dnxi(3), dnei(3)
!
      real(kind=kreal) :: an_sf(num_linear_sf,1)
      real(kind=kreal) :: dnxi_sf1(num_linear_sf,1)
      real(kind=kreal) :: dnei_sf1(num_linear_sf,1)
!
!
          call s_cal_shape_function_2d_linear(ione, an_sf, dnxi_sf1,    &
     &        dnei_sf1, xi(1), xi(2) )
          call interporate_one_position_by_4(xx_z, x_local_ele,         &
     &        an_sf(1,1) )
          call interporate_one_position_by_4(dnxi, x_local_ele,         &
     &        dnxi_sf1(1,1) )
          call interporate_one_position_by_4(dnei, x_local_ele,         &
     &        dnei_sf1(1,1) )
!
      end subroutine cal_position_and_grad_4
!
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_grad_sf8(xx_z, dnxi, dnei,            &
     &          x_local_ele, xi)
!
      use cal_shape_function_2d
      use interpolate_position_in_ele
!
      real(kind = kreal), intent(in) :: x_local_ele(num_quad_sf,3)
      real(kind = kreal), intent(in) :: xi(2)
!
      real(kind=kreal), intent(inout) :: xx_z(3)
      real(kind=kreal), intent(inout) :: dnxi(3), dnei(3)
!
      real(kind=kreal) :: an_sf(num_quad_sf,1)
      real(kind=kreal) :: dnxi_sf8(num_quad_sf,1)
      real(kind=kreal) :: dnei_sf8(num_quad_sf,1)
!
!
          call s_cal_shape_function_2d_quad(ione, an_sf, dnxi_sf8,      &
     &        dnei_sf8, xi(1), xi(2) )
          call interporate_one_position_linear(xx_z, x_local_ele,       &
     &        an_sf(1,1) )
          call interporate_one_position_linear(dnxi, x_local_ele,       &
     &        dnxi_sf8(1,1) )
          call interporate_one_position_linear(dnei, x_local_ele,       &
     &        dnei_sf8(1,1) )
!
      end subroutine cal_position_and_grad_sf8
!
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_grad_9(xx_z, dnxi, dnei,              &
     &          x_local_ele, xi)
!
      use cal_shape_function_2d
      use interpolate_position_in_ele
!
      real(kind = kreal), intent(in) :: x_local_ele(num_lag_sf,3)
      real(kind = kreal), intent(in) :: xi(2)
!
      real(kind=kreal), intent(inout) :: xx_z(3)
      real(kind=kreal), intent(inout) :: dnxi(3), dnei(3)
!
      real(kind=kreal) :: an_sf(num_lag_sf,1)
      real(kind=kreal) :: dnxi_sf9(num_lag_sf,1)
      real(kind=kreal) :: dnei_sf9(num_lag_sf,1)
!
!
          call s_cal_shape_function_2d_lag(ione, an_sf, dnxi_sf9,       &
     &        dnei_sf9, xi(1), xi(2) )
          call interporate_one_position_by_9(xx_z, x_local_ele,         &
     &        an_sf(1,1) )
          call interporate_one_position_by_9(dnxi, x_local_ele,         &
     &        dnxi_sf9(1,1) )
          call interporate_one_position_by_9(dnei, x_local_ele,         &
     &        dnei_sf9(1,1) )
!
      end subroutine cal_position_and_grad_9
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_grad_2(xx_z, dnxi, x_local_ele, xi)
!
      use cal_shape_function_1d
      use interpolate_position_in_ele
!
      real(kind = kreal), intent(in) :: x_local_ele(num_linear_edge,3)
      real(kind = kreal), intent(in) :: xi(1)
!
      real(kind=kreal), intent(inout) :: xx_z(3), dnxi(3)
!
      real(kind=kreal) :: an_ed(num_linear_edge,1)
      real(kind=kreal) :: dnxi_ed1(num_linear_edge,1)
!
!
      call s_cal_shape_function_1d_linear(ione, an_ed, dnxi_ed1, xi)
      call interporate_one_position_by_2(xx_z, x_local_ele, an_ed(1,1))
      call interporate_one_position_by_2(dnxi, x_local_ele,             &
     &    dnxi_ed1(1,1) )
!
      end subroutine cal_position_and_grad_2
!
!-----------------------------------------------------------------------
!
      subroutine cal_position_and_grad_3(xx_z, dnxi, x_local_ele, xi)
!
      use cal_shape_function_1d
      use interpolate_position_in_ele
!
      real(kind = kreal), intent(in) :: x_local_ele(num_quad_edge,3)
      real(kind = kreal), intent(in) :: xi(1)
!
      real(kind=kreal), intent(inout) :: xx_z(3), dnxi(3)
!
      real(kind=kreal) :: an_ed(num_quad_edge,1)
      real(kind=kreal) :: dnxi_ed3(num_quad_edge,1)
!
!
          call s_cal_shape_function_1d_quad(ione, an_ed, dnxi_ed3, xi)
          call interporate_one_position_by_3(xx_z, x_local_ele,         &
     &        an_ed(1,1) )
          call interporate_one_position_by_3(dnxi, x_local_ele,         &
     &        dnxi_ed3(1,1) )
!
      end subroutine cal_position_and_grad_3
!
!-----------------------------------------------------------------------
!
      end module cal_position_and_grad
