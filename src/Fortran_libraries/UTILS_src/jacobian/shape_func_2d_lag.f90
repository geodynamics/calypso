!
!      module shape_func_2d_lag
!
!     Written by H. Matsui on June. 2006
!
!      subroutine shape_function_an_sf27(an_27, xi, ei,                 &
!     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre)
!      subroutine shape_function_dnxi_sf27(dnxi, xi, ei,                &
!     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,  &
!     &          dxi)
!      subroutine shape_function_dnei_sf27(dnei, xi, ei,                &
!     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,  &
!     &          dei)
!
!     xi, ei:  \xi, \eta
!     xi_nega:   1 - \xi
!     xi_posi:   1 + \xi
!     xi_sqre:   1 - \xi
!
      module shape_func_2d_lag
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
      subroutine shape_function_an_sf27(an_27, xi, ei,                  &
     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre)
!
      real (kind=kreal), intent(inout) :: an_27(9)
!
      real (kind=kreal), intent(in) :: xi, ei
      real (kind=kreal), intent(in) :: xi_nega, ei_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre
!
!
      an_27(1) = quad * xi_nega * ei_nega * xi * ei
      an_27(2) = quad * xi_posi * ei_nega * xi * ei
      an_27(3) = quad * xi_posi * ei_posi * xi * ei
      an_27(4) = quad * xi_nega * ei_posi * xi * ei
!
      an_27(5) = half * xi_sqre * ei_nega * ei
      an_27(6) = half * xi_posi * ei_sqre * xi
      an_27(7) = half * xi_sqre * ei_posi * ei
      an_27(8) = half * xi_nega * ei_sqre * xi
!
      an_27(9) =       xi_sqre * ei_sqre
!
      end subroutine shape_function_an_sf27
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnxi_sf27(dnxi, xi, ei,                 &
     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,   &
     &          dxi)
!
      real (kind=kreal), intent(inout) :: dnxi(9)
!
      real (kind=kreal), intent(in) :: xi, ei
      real (kind=kreal), intent(in) :: xi_nega, ei_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre
      real (kind=kreal), intent(in) :: dxi
!
!
      dnxi(1) = quad * (xi_nega -xi) * ei_nega * ei * dxi
      dnxi(2) = quad * (xi_posi +xi) * ei_nega * ei * dxi
      dnxi(3) = quad * (xi_posi +xi) * ei_posi * ei * dxi
      dnxi(4) = quad * (xi_nega -xi) * ei_posi * ei * dxi
!
      dnxi(5) =                - xi * ei_nega * ei * dxi
      dnxi(6) = half * (xi_posi +xi) * ei_sqre *      dxi
      dnxi(7) =                - xi * ei_posi * ei * dxi
      dnxi(8) = half * (xi_nega -xi) * ei_sqre *      dxi
!
      dnxi(9) =          - two * xi * ei_sqre *      dxi
!
      end subroutine shape_function_dnxi_sf27
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnei_sf27(dnei, xi, ei,                 &
     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,   &
     &          dei)
!
      real (kind=kreal), intent(inout) :: dnei(9)
!
      real (kind=kreal), intent(in) :: xi, ei
      real (kind=kreal), intent(in) :: xi_nega, ei_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre
      real (kind=kreal), intent(in) :: dei
!
!
      dnei(1) = quad * xi_nega * (ei_nega -ei) * xi * dei
      dnei(2) = quad * xi_posi * (ei_nega -ei) * xi * dei
      dnei(3) = quad * xi_posi * (ei_posi +ei) * xi * dei
      dnei(4) = quad * xi_nega * (ei_posi +ei) * xi * dei
!
      dnei(5) = half * xi_sqre * (ei_nega -ei) *      dei
      dnei(6) =     - xi_posi *            ei * xi * dei
      dnei(7) = half * xi_sqre * (ei_posi +ei) *      dei
      dnei(8) =     - xi_nega *            ei * xi * dei
!
      dnei(9) = -two *xi_sqre *            ei *      dei
!
      end subroutine shape_function_dnei_sf27
!
!-----------------------------------------------------------------------
!
      end module shape_func_2d_lag
