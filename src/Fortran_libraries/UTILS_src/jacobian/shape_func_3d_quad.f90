!
!      module shape_func_3d_quad
!
!     Written by H. Matsui on Sep. 2005
!
!      subroutine shape_function_an_20(an_20, xi, ei, zi,               &
!     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,      &
!     &      xi_sqre, ei_sqre, zi_sqre)
!      subroutine shape_function_dnxi_20(dnxi, xi, ei, zi,              &
!     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,      &
!     &      xi_sqre, ei_sqre, zi_sqre, dxi)
!      subroutine shape_function_dnei_20(dnei, xi, ei, zi,              &
!     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,      &
!     &      xi_sqre, ei_sqre, zi_sqre, dei)
!      subroutine shape_function_dnzi_20(dnzi, xi, ei, zi,              &
!     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,      &
!     &      xi_sqre, ei_sqre, zi_sqre, dzi)
!
!     xi, ei, zi:  \xi, \eta, \zeta
!     xi_nega:   1 - \xi
!     xi_posi:   1 + \xi
!     xi_sqre:   1 - \xi
!
      module shape_func_3d_quad
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
      subroutine shape_function_an_20(an_20, xi, ei, zi,                &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre)
!
      real (kind=kreal), intent(inout) :: an_20(20)
!
      real (kind=kreal), intent(in) :: xi, ei, zi
      real (kind=kreal), intent(in) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre, zi_sqre
!
!
      an_20(1)  = -r125 * xi_nega * ei_nega * zi_nega * (two+xi+ei+zi)
      an_20(2)  = -r125 * xi_posi * ei_nega * zi_nega * (two-xi+ei+zi)
      an_20(3)  = -r125 * xi_posi * ei_posi * zi_nega * (two-xi-ei+zi)
      an_20(4)  = -r125 * xi_nega * ei_posi * zi_nega * (two+xi-ei+zi)
      an_20(5)  = -r125 * xi_nega * ei_nega * zi_posi * (two+xi+ei-zi)
      an_20(6)  = -r125 * xi_posi * ei_nega * zi_posi * (two-xi+ei-zi)
      an_20(7)  = -r125 * xi_posi * ei_posi * zi_posi * (two-xi-ei-zi)
      an_20(8)  = -r125 * xi_nega * ei_posi * zi_posi * (two+xi-ei-zi)
!
      an_20(9)  =  quad * xi_sqre * ei_nega * zi_nega
      an_20(10) =  quad * xi_posi * ei_sqre * zi_nega
      an_20(11) =  quad * xi_sqre * ei_posi * zi_nega
      an_20(12) =  quad * xi_nega * ei_sqre * zi_nega
!
      an_20(13) =  quad * xi_sqre * ei_nega * zi_posi
      an_20(14) =  quad * xi_posi * ei_sqre * zi_posi
      an_20(15) =  quad * xi_sqre * ei_posi * zi_posi
      an_20(16) =  quad * xi_nega * ei_sqre * zi_posi
!
      an_20(17) =  quad * xi_nega * ei_nega * zi_sqre
      an_20(18) =  quad * xi_posi * ei_nega * zi_sqre
      an_20(19) =  quad * xi_posi * ei_posi * zi_sqre
      an_20(20) =  quad * xi_nega * ei_posi * zi_sqre
!
      end subroutine shape_function_an_20
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnxi_20(dnxi, xi, ei, zi,               &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, dxi)
!
      real (kind=kreal), dimension(20) :: dnxi
!
      real (kind=kreal), intent(in) :: xi, ei, zi
      real (kind=kreal), intent(in) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre, zi_sqre
      real (kind=kreal), intent(in) :: dxi
!
!
      dnxi(1)  =  r125 * (one+two*xi+ei+zi) * ei_nega * zi_nega * dxi
      dnxi(2)  = -r125 * (one-two*xi+ei+zi) * ei_nega * zi_nega * dxi
      dnxi(3)  = -r125 * (one-two*xi-ei+zi) * ei_posi * zi_nega * dxi
      dnxi(4)  =  r125 * (one+two*xi-ei+zi) * ei_posi * zi_nega * dxi
      dnxi(5)  =  r125 * (one+two*xi+ei-zi) * ei_nega * zi_posi * dxi
      dnxi(6)  = -r125 * (one-two*xi+ei-zi) * ei_nega * zi_posi * dxi
      dnxi(7)  = -r125 * (one-two*xi-ei-zi) * ei_posi * zi_posi * dxi
      dnxi(8)  =  r125 * (one+two*xi-ei-zi) * ei_posi * zi_posi * dxi
!
      dnxi(9)  = -half * xi * ei_nega * zi_nega * dxi
      dnxi(10) =  quad      * ei_sqre * zi_nega * dxi
      dnxi(11) = -half * xi * ei_posi * zi_nega * dxi
      dnxi(12) = -quad      * ei_sqre * zi_nega * dxi
!
      dnxi(13) = -half * xi * ei_nega * zi_posi * dxi
      dnxi(14) =  quad      * ei_sqre * zi_posi * dxi
      dnxi(15) = -half * xi * ei_posi * zi_posi * dxi
      dnxi(16) = -quad      * ei_sqre * zi_posi * dxi
!
      dnxi(17) = -quad      * ei_nega * zi_sqre * dxi
      dnxi(18) =  quad      * ei_nega * zi_sqre * dxi
      dnxi(19) =  quad      * ei_posi * zi_sqre * dxi
      dnxi(20) = -quad      * ei_posi * zi_sqre * dxi
!
      end subroutine shape_function_dnxi_20
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnei_20(dnei, xi, ei, zi,               &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, dei)
!
      real (kind=kreal), intent(inout) :: dnei(20)
!
      real (kind=kreal), intent(in) :: xi, ei, zi
      real (kind=kreal), intent(in) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre, zi_sqre
      real (kind=kreal), intent(in) :: dei
!
!
      dnei(1)  =  r125 * xi_nega * (one+xi+two*ei+zi) * zi_nega * dei
      dnei(2)  =  r125 * xi_posi * (one-xi+two*ei+zi) * zi_nega * dei
      dnei(3)  = -r125 * xi_posi * (one-xi-two*ei+zi) * zi_nega * dei
      dnei(4)  = -r125 * xi_nega * (one+xi-two*ei+zi) * zi_nega * dei
      dnei(5)  =  r125 * xi_nega * (one+xi+two*ei-zi) * zi_posi * dei
      dnei(6)  =  r125 * xi_posi * (one-xi+two*ei-zi) * zi_posi * dei
      dnei(7)  = -r125 * xi_posi * (one-xi-two*ei-zi) * zi_posi * dei
      dnei(8)  = -r125 * xi_nega * (one+xi-two*ei-zi) * zi_posi * dei
!
      dnei(9)  = -quad * xi_sqre      * zi_nega * dei
      dnei(10) = -half * xi_posi * ei * zi_nega * dei
      dnei(11) =  quad * xi_sqre      * zi_nega * dei
      dnei(12) = -half * xi_nega * ei * zi_nega * dei
!
      dnei(13) = -quad * xi_sqre      * zi_posi * dei
      dnei(14) = -half * xi_posi * ei * zi_posi * dei
      dnei(15) =  quad * xi_sqre      * zi_posi * dei
      dnei(16) = -half * xi_nega * ei * zi_posi * dei
!
      dnei(17) = -quad * xi_nega      * zi_sqre * dei
      dnei(18) = -quad * xi_posi      * zi_sqre * dei
      dnei(19) =  quad * xi_posi      * zi_sqre * dei
      dnei(20) =  quad * xi_nega      * zi_sqre * dei
!
      end subroutine shape_function_dnei_20
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnzi_20(dnzi, xi, ei, zi,               &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi,       &
     &      xi_sqre, ei_sqre, zi_sqre, dzi)
!
      real (kind=kreal), intent(inout) :: dnzi(20)
!
      real (kind=kreal), intent(in) :: xi, ei, zi
      real (kind=kreal), intent(in) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi, zi_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre, zi_sqre
      real (kind=kreal), intent(in) :: dzi
!
!
      dnzi(1)  =  r125 * xi_nega * ei_nega * (one+xi+ei+two*zi) * dzi
      dnzi(2)  =  r125 * xi_posi * ei_nega * (one-xi+ei+two*zi) * dzi
      dnzi(3)  =  r125 * xi_posi * ei_posi * (one-xi-ei+two*zi) * dzi
      dnzi(4)  =  r125 * xi_nega * ei_posi * (one+xi-ei+two*zi) * dzi
      dnzi(5)  = -r125 * xi_nega * ei_nega * (one+xi+ei-two*zi) * dzi
      dnzi(6)  = -r125 * xi_posi * ei_nega * (one-xi+ei-two*zi) * dzi
      dnzi(7)  = -r125 * xi_posi * ei_posi * (one-xi-ei-two*zi) * dzi
      dnzi(8)  = -r125 * xi_nega * ei_posi * (one+xi-ei-two*zi) * dzi
!
      dnzi(9)  = -quad * xi_sqre * ei_nega * dzi
      dnzi(10) = -quad * xi_posi * ei_sqre * dzi
      dnzi(11) = -quad * xi_sqre * ei_posi * dzi
      dnzi(12) = -quad * xi_nega * ei_sqre * dzi
!
      dnzi(13) =  quad * xi_sqre * ei_nega * dzi
      dnzi(14) =  quad * xi_posi * ei_sqre * dzi
      dnzi(15) =  quad * xi_sqre * ei_posi * dzi
      dnzi(16) =  quad * xi_nega * ei_sqre * dzi
!
      dnzi(17) = -half * xi_nega * ei_nega * zi * dzi
      dnzi(18) = -half * xi_posi * ei_nega * zi * dzi
      dnzi(19) = -half * xi_posi * ei_posi * zi * dzi
      dnzi(20) = -half * xi_nega * ei_posi * zi * dzi
!
      end subroutine shape_function_dnzi_20
!
!-----------------------------------------------------------------------
!
      end module shape_func_3d_quad
