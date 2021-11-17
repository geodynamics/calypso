!
!      module shape_func_2d_quad
!
!     Written by H. Matsui on Sep. 2005
!
!      subroutine shape_function_an_sf20(an_20, xi, ei,                 &
!     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre)
!      subroutine shape_function_dnxi_sf20(dnxi, xi, ei,                &
!     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,  &
!     &          dxi)
!      subroutine shape_function_dnei_sf20(dnei, xi, ei,                &
!     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,  &
!     &          dei)
!
      module shape_func_2d_quad
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
      subroutine shape_function_an_sf20(an_20, xi, ei,                  &
     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre)
!
      real (kind=kreal), intent(inout) :: an_20(8)
!
      real (kind=kreal), intent(in) :: xi, ei
      real (kind=kreal), intent(in) :: xi_nega, ei_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre
!
!
      an_20(1)  = quad * xi_nega * ei_nega * (-xi-ei-one)
      an_20(2)  = quad * xi_posi * ei_nega * ( xi-ei-one)
      an_20(3)  = quad * xi_posi * ei_posi * ( xi+ei-one)
      an_20(4)  = quad * xi_nega * ei_posi * (-xi+ei-one)
!
      an_20(5)  =  half * xi_sqre * ei_nega
      an_20(6)  =  half * xi_posi * ei_sqre
      an_20(7)  =  half * xi_sqre * ei_posi
      an_20(8)  =  half * xi_nega * ei_sqre
!
      end subroutine shape_function_an_sf20
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnxi_sf20(dnxi, xi, ei,                 &
     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,   &
     &          dxi)
!
      real (kind=kreal), intent(inout) :: dnxi(8)
!
      real (kind=kreal), intent(in) :: xi, ei
      real (kind=kreal), intent(in) :: xi_nega, ei_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre
      real (kind=kreal), intent(in) :: dxi
!
!
      dnxi(1)  = -quad * (-two*xi-ei) * ei_nega * dxi
      dnxi(2)  =  quad * ( two*xi-ei) * ei_nega * dxi
      dnxi(3)  =  quad * ( two*xi+ei) * ei_posi * dxi
      dnxi(4)  = -quad * (-two*xi+ei) * ei_posi * dxi
!
      dnxi(5)  =       - xi * ei_nega * dxi
      dnxi(6)  =  half      * ei_sqre * dxi
      dnxi(7)  =       - xi * ei_posi * dxi
      dnxi(8)  = -half      * ei_sqre * dxi
!
      end subroutine shape_function_dnxi_sf20
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnei_sf20(dnei, xi, ei,                 &
     &          xi_nega, ei_nega, xi_posi, ei_posi, xi_sqre, ei_sqre,   &
     &          dei)
!
      real (kind=kreal), intent(inout) :: dnei(8)
!
      real (kind=kreal), intent(in) :: xi, ei
      real (kind=kreal), intent(in) :: xi_nega, ei_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi
      real (kind=kreal), intent(in) :: xi_sqre, ei_sqre
      real (kind=kreal), intent(in) :: dei
!
!
      dnei(1)  = -quad * xi_nega * (-xi-two*ei) * dei
      dnei(2)  = -quad * xi_posi * ( xi-two*ei) * dei
      dnei(3)  =  quad * xi_posi * ( xi+two*ei) * dei
      dnei(4)  =  quad * xi_nega * (-xi+two*ei) * dei
!
      dnei(5)  = - half * xi_sqre *      dei
      dnei(6)  =        - xi_posi * ei * dei
      dnei(7)  =   half * xi_sqre *      dei
      dnei(8)  =        - xi_nega * ei * dei
!
      end subroutine shape_function_dnei_sf20
!
!-----------------------------------------------------------------------
!
      end module shape_func_2d_quad
