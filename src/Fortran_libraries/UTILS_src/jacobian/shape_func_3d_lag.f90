!
!      module shape_func_3d_lag
!
!     Written by H. Matsui on June. 2006
!
!      subroutine shape_function_an_27(an_27, xi, ei, zi,               &
!     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,      &
!     &      xi_sqre, ei_sqre, zi_sqre)
!      subroutine shape_function_dnxi_27(dnxi, xi, ei, zi,              &
!     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,      &
!     &      xi_sqre, ei_sqre, zi_sqre, dxi)
!      subroutine shape_function_dnei_27(dnei, xi, ei, zi,              &
!     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,      &
!     &      xi_sqre, ei_sqre, zi_sqre, dei)
!      subroutine shape_function_dnzi_27(dnzi, xi, ei, zi,              &
!     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,      &
!     &      xi_sqre, ei_sqre, zi_sqre, dzi)
!
!     xi, ei, zi:  \xi, \eta, \zeta
!     xi_nega:   1 - \xi
!     xi_posi:   1 + \xi
!     xi_sqre:   1 - \xi
!
      module shape_func_3d_lag
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
      subroutine shape_function_an_27(an_27, xi, ei, zi,                &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre)
!
      real (kind=kreal), intent(inout) :: an_27(27)
!
      real (kind=kreal), intent(in)    :: xi, ei, zi
      real (kind=kreal), intent(in) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre, zi_sqre
!
!
      an_27(1)  = r125 * xi_nega * ei_nega * zi_nega * xi * ei * zi
      an_27(2)  = r125 * xi_posi * ei_nega * zi_nega * xi * ei * zi
      an_27(3)  = r125 * xi_posi * ei_posi * zi_nega * xi * ei * zi
      an_27(4)  = r125 * xi_nega * ei_posi * zi_nega * xi * ei * zi
      an_27(5)  = r125 * xi_nega * ei_nega * zi_posi * xi * ei * zi
      an_27(6)  = r125 * xi_posi * ei_nega * zi_posi * xi * ei * zi
      an_27(7)  = r125 * xi_posi * ei_posi * zi_posi * xi * ei * zi
      an_27(8)  = r125 * xi_nega * ei_posi * zi_posi * xi * ei * zi
!
      an_27(9)  = quad * xi_sqre * ei_nega * zi_nega * ei * zi
      an_27(10) = quad * xi_posi * ei_sqre * zi_nega * xi * zi
      an_27(11) = quad * xi_sqre * ei_posi * zi_nega * ei * zi
      an_27(12) = quad * xi_nega * ei_sqre * zi_nega * xi * zi
!
      an_27(13) = quad * xi_sqre * ei_nega * zi_posi * ei * zi
      an_27(14) = quad * xi_posi * ei_sqre * zi_posi * xi * zi
      an_27(15) = quad * xi_sqre * ei_posi * zi_posi * ei * zi
      an_27(16) = quad * xi_nega * ei_sqre * zi_posi * xi * zi
!
      an_27(17) = quad * xi_nega * ei_nega * zi_sqre * xi * ei
      an_27(18) = quad * xi_posi * ei_nega * zi_sqre * xi * ei
      an_27(19) = quad * xi_posi * ei_posi * zi_sqre * xi * ei
      an_27(20) = quad * xi_nega * ei_posi * zi_sqre * xi * ei
!
      an_27(21) = half * xi_nega * ei_sqre * zi_sqre * xi
      an_27(22) = half * xi_posi * ei_sqre * zi_sqre * xi
      an_27(23) = half * xi_sqre * ei_nega * zi_sqre * ei
      an_27(24) = half * xi_sqre * ei_posi * zi_sqre * ei
      an_27(25) = half * xi_sqre * ei_sqre * zi_nega * zi
      an_27(26) = half * xi_sqre * ei_sqre * zi_posi * zi
!
      an_27(27) =        xi_sqre * ei_sqre * zi_sqre
!
      end subroutine shape_function_an_27
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnxi_27(dnxi, xi, ei, zi,               &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, dxi)
!
      real (kind=kreal), dimension(27) :: dnxi
!
      real (kind=kreal), intent(in)    :: xi, ei, zi
      real (kind=kreal), intent(in)    :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(in)    :: xi_posi, ei_posi, zi_posi
      real (kind=kreal), intent(in)    :: xi_sqre, ei_sqre, zi_sqre
      real (kind=kreal), intent(in)    :: dxi
!
!
      dnxi(1)  = r125 * (xi_nega -xi) * ei_nega * zi_nega * ei*zi * dxi
      dnxi(2)  = r125 * (xi_posi +xi) * ei_nega * zi_nega * ei*zi * dxi
      dnxi(3)  = r125 * (xi_posi +xi) * ei_posi * zi_nega * ei*zi * dxi
      dnxi(4)  = r125 * (xi_nega -xi) * ei_posi * zi_nega * ei*zi * dxi
      dnxi(5)  = r125 * (xi_nega -xi) * ei_nega * zi_posi * ei*zi * dxi
      dnxi(6)  = r125 * (xi_posi +xi) * ei_nega * zi_posi * ei*zi * dxi
      dnxi(7)  = r125 * (xi_posi +xi) * ei_posi * zi_posi * ei*zi * dxi
      dnxi(8)  = r125 * (xi_nega -xi) * ei_posi * zi_posi * ei*zi * dxi
!
      dnxi(9)  =-half *            xi * ei_nega * zi_nega * ei*zi * dxi
      dnxi(10) = quad * (xi_posi +xi) * ei_sqre * zi_nega * zi *    dxi
      dnxi(11) =-half *            xi * ei_posi * zi_nega * ei*zi * dxi
      dnxi(12) = quad * (xi_nega -xi) * ei_sqre * zi_nega * zi *    dxi
!
      dnxi(13) =-half *            xi * ei_nega * zi_posi * ei*zi * dxi
      dnxi(14) = quad * (xi_posi +xi) * ei_sqre * zi_posi *    zi * dxi
      dnxi(15) =-half *            xi * ei_posi * zi_posi * ei*zi * dxi
      dnxi(16) = quad * (xi_nega -xi) * ei_sqre * zi_posi *    zi * dxi
!
      dnxi(17) = quad * (xi_nega -xi) * ei_nega * zi_sqre * ei *    dxi
      dnxi(18) = quad * (xi_posi +xi) * ei_nega * zi_sqre * ei *    dxi
      dnxi(19) = quad * (xi_posi +xi) * ei_posi * zi_sqre * ei *    dxi
      dnxi(20) = quad * (xi_nega -xi) * ei_posi * zi_sqre * ei *    dxi
!
      dnxi(21) = half * (xi_nega -xi) * ei_sqre * zi_sqre *         dxi
      dnxi(22) = half * (xi_posi +xi) * ei_sqre * zi_sqre *         dxi
      dnxi(23) =                 - xi * ei_nega * zi_sqre * ei *    dxi
      dnxi(24) =                 - xi * ei_posi * zi_sqre * ei *    dxi
      dnxi(25) =                 - xi * ei_sqre * zi_nega * zi *    dxi
      dnxi(26) =                 - xi * ei_sqre * zi_posi * zi *    dxi
!
      dnxi(27) =            -two * xi * ei_sqre * zi_sqre *         dxi
!
      end subroutine shape_function_dnxi_27
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnei_27(dnei, xi, ei, zi,               &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, dei)
!
      real (kind=kreal), intent(inout) :: dnei(27)
!
      real (kind=kreal), intent(in)    :: xi, ei, zi
      real (kind=kreal), intent(in)    :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(in)    :: xi_posi, ei_posi, zi_posi
      real (kind=kreal), intent(in)    :: xi_sqre, ei_sqre, zi_sqre
      real (kind=kreal), intent(in)    :: dei
!
!
      dnei(1)  = r125 * xi_nega * (ei_nega -ei) * zi_nega * xi*zi * dei
      dnei(2)  = r125 * xi_posi * (ei_nega -ei) * zi_nega * xi*zi * dei
      dnei(3)  = r125 * xi_posi * (ei_posi +ei) * zi_nega * xi*zi * dei
      dnei(4)  = r125 * xi_nega * (ei_posi +ei) * zi_nega * xi*zi * dei
      dnei(5)  = r125 * xi_nega * (ei_nega -ei) * zi_posi * xi*zi * dei
      dnei(6)  = r125 * xi_posi * (ei_nega -ei) * zi_posi * xi*zi * dei
      dnei(7)  = r125 * xi_posi * (ei_posi +ei) * zi_posi * xi*zi * dei
      dnei(8)  = r125 * xi_nega * (ei_posi +ei) * zi_posi * xi*zi * dei
!
      dnei(9)  = quad * xi_sqre * (ei_nega -ei) * zi_nega *    zi * dei
      dnei(10) =-half * xi_posi *            ei * zi_nega * xi*zi * dei
      dnei(11) = quad * xi_sqre * (ei_posi +ei) * zi_nega * zi *    dei
      dnei(12) =-half * xi_nega *            ei * zi_nega * xi*zi * dei
!
      dnei(13) = quad * xi_sqre * (ei_nega -ei) * zi_posi *    zi * dei
      dnei(14) =-half * xi_posi *            ei * zi_posi * xi*zi * dei
      dnei(15) = quad * xi_sqre * (ei_posi +ei) * zi_posi *    zi * dei
      dnei(16) =-half * xi_nega *            ei * zi_posi * xi*zi * dei
!
      dnei(17) = quad * xi_nega * (ei_nega -ei) * zi_sqre * xi *    dei
      dnei(18) = quad * xi_posi * (ei_nega -ei) * zi_sqre * xi *    dei
      dnei(19) = quad * xi_posi * (ei_posi +ei) * zi_sqre * xi *    dei
      dnei(20) = quad * xi_nega * (ei_posi +ei) * zi_sqre * xi *    dei
!
      dnei(21) =      - xi_nega *            ei * zi_sqre * xi *    dei
      dnei(22) =      - xi_posi *            ei * zi_sqre * xi *    dei
      dnei(23) = half * xi_sqre * (ei_nega -ei) * zi_sqre *         dei
      dnei(24) = half * xi_sqre * (ei_posi +ei) * zi_sqre *         dei
      dnei(25) =      - xi_sqre *            ei * zi_nega *    zi * dei
      dnei(26) =      - xi_sqre *            ei * zi_posi *    zi * dei
!
      dnei(27) = -two * xi_sqre *            ei * zi_sqre *         dei
!
      end subroutine shape_function_dnei_27
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnzi_27(dnzi, xi, ei, zi,               &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, dzi)
!
      real (kind=kreal), intent(inout) :: dnzi(27)
!
      real (kind=kreal), intent(in)    :: xi, ei, zi
      real (kind=kreal), intent(in)    :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(in)    :: xi_posi, ei_posi, zi_posi
      real (kind=kreal), intent(in)    :: xi_sqre, ei_sqre, zi_sqre
      real (kind=kreal), intent(in)    :: dzi
!
!
      dnzi(1)  = r125 * xi_nega * ei_nega * (zi_nega -zi) * xi*ei * dzi
      dnzi(2)  = r125 * xi_posi * ei_nega * (zi_nega -zi) * xi*ei * dzi
      dnzi(3)  = r125 * xi_posi * ei_posi * (zi_nega -zi) * xi*ei * dzi
      dnzi(4)  = r125 * xi_nega * ei_posi * (zi_nega -zi) * xi*ei * dzi
      dnzi(5)  = r125 * xi_nega * ei_nega * (zi_posi +zi) * xi*ei * dzi
      dnzi(6)  = r125 * xi_posi * ei_nega * (zi_posi +zi) * xi*ei * dzi
      dnzi(7)  = r125 * xi_posi * ei_posi * (zi_posi +zi) * xi*ei * dzi
      dnzi(8)  = r125 * xi_nega * ei_posi * (zi_posi +zi) * xi*ei * dzi
!
      dnzi(9)  = quad * xi_sqre * ei_nega * (zi_nega -zi) *    ei * dzi
      dnzi(10) = quad * xi_posi * ei_sqre * (zi_nega -zi) * xi *    dzi
      dnzi(11) = quad * xi_sqre * ei_posi * (zi_nega -zi) *    ei * dzi
      dnzi(12) = quad * xi_nega * ei_sqre * (zi_nega -zi) * xi *    dzi
!
      dnzi(13) = quad * xi_sqre * ei_nega * (zi_posi +zi) *    ei * dzi
      dnzi(14) = quad * xi_posi * ei_sqre * (zi_posi +zi) * xi *    dzi
      dnzi(15) = quad * xi_sqre * ei_posi * (zi_posi +zi) *    ei * dzi
      dnzi(16) = quad * xi_nega * ei_sqre * (zi_posi +zi) * xi *    dzi
!
      dnzi(17) =-half * xi_nega * ei_nega *            zi * xi*ei * dzi
      dnzi(18) =-half * xi_posi * ei_nega *            zi * xi*ei * dzi
      dnzi(19) =-half * xi_posi * ei_posi *            zi * xi*ei * dzi
      dnzi(20) =-half * xi_nega * ei_posi *            zi * xi*ei * dzi
!
      dnzi(21) =      - xi_nega * ei_sqre *            zi * xi *    dzi
      dnzi(22) =      - xi_posi * ei_sqre *            zi * xi *    dzi
      dnzi(23) =      - xi_sqre * ei_nega *            zi *    ei * dzi
      dnzi(24) =      - xi_sqre * ei_posi *            zi *    ei * dzi
      dnzi(25) = half * xi_sqre * ei_sqre * (zi_nega -zi) *         dzi
      dnzi(26) = half * xi_sqre * ei_sqre * (zi_posi +zi) *         dzi
!
      dnzi(27) = -two * xi_sqre * ei_sqre *            zi *         dzi
!
!
      end subroutine shape_function_dnzi_27
!
!-----------------------------------------------------------------------
!
      end module shape_func_3d_lag
