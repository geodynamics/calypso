!
!      module shape_func_2d_linear
!
!     Written by H. Matsui on Sep. 2005
!
!      subroutine shape_function_an_sf_1(an_1, xi_nega, ei_nega,        &
!     &          xi_posi, ei_posi)
!      subroutine shape_function_dnxi_sf_1(dnxi, ei_nega, ei_posi, dxi)
!      subroutine shape_function_dnei_sf_1(dnei, xi_nega, xi_posi, dei)
!
      module shape_func_2d_linear
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
      subroutine shape_function_an_sf_1(an_1, xi_nega, ei_nega,         &
     &          xi_posi, ei_posi)
!
      real (kind=kreal), intent(inout) :: an_1(4)
!
      real (kind=kreal), intent(in) :: xi_nega, xi_posi
      real (kind=kreal), intent(in) :: ei_nega, ei_posi
!
!
      an_1(1)  = quad * xi_nega * ei_nega
      an_1(2)  = quad * xi_posi * ei_nega
      an_1(3)  = quad * xi_posi * ei_posi
      an_1(4)  = quad * xi_nega * ei_posi
!
!
      end subroutine shape_function_an_sf_1
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnxi_sf_1(dnxi, ei_nega, ei_posi, dxi)
!
      real (kind=kreal), intent(inout) :: dnxi(4)
!
      real (kind=kreal), intent(in) :: ei_nega, ei_posi
      real (kind=kreal), intent(in) :: dxi
!
!
      dnxi(1) = -quad * ei_nega * dxi
      dnxi(2) =  quad * ei_nega * dxi
      dnxi(3) =  quad * ei_posi * dxi
      dnxi(4) = -quad * ei_posi * dxi
!
      end subroutine shape_function_dnxi_sf_1
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnei_sf_1(dnei, xi_nega, xi_posi, dei)
!
      real (kind=kreal), intent(inout) :: dnei(4)
!
      real (kind=kreal), intent(in) :: xi_nega, xi_posi
      real (kind=kreal), intent(in) :: dei
!
!
      dnei(1) = -quad * xi_nega * dei
      dnei(2) = -quad * xi_posi * dei
      dnei(3) =  quad * xi_posi * dei
      dnei(4) =  quad * xi_nega * dei
!
      end subroutine shape_function_dnei_sf_1
!
!-----------------------------------------------------------------------
!
      end module shape_func_2d_linear
