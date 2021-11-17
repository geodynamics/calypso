!
!      module shape_func_3d_linear
!
!     Written by H. Matsui on Sep. 2005
!
!      subroutine shape_function_an_1(an_1,                             &
!     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi)
!      subroutine shape_function_dnxi_1(dnxi_1,                         &
!     &          ei_nega, zi_nega, ei_posi, zi_posi, dxi)
!      subroutine shape_function_dnei_1(dnei_1,                         &
!     &          xi_nega, zi_nega, xi_posi, zi_posi, dei)
!      subroutine shape_function_dnzi_1(dnzi_1,                         &
!     &          xi_nega, ei_nega, xi_posi, ei_posi, dzi)
!
      module shape_func_3d_linear
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
      subroutine shape_function_an_1(an_1,                              &
     &      xi_nega, ei_nega, zi_nega, xi_posi, ei_posi, zi_posi)
!
      real (kind=kreal), intent(inout) :: an_1(8)
!
      real (kind=kreal), intent(in) :: xi_nega, ei_nega, zi_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi, zi_posi
!
!
      an_1(1)  = r125 * xi_nega * ei_nega * zi_nega
      an_1(2)  = r125 * xi_posi * ei_nega * zi_nega
      an_1(3)  = r125 * xi_posi * ei_posi * zi_nega
      an_1(4)  = r125 * xi_nega * ei_posi * zi_nega
      an_1(5)  = r125 * xi_nega * ei_nega * zi_posi
      an_1(6)  = r125 * xi_posi * ei_nega * zi_posi
      an_1(7)  = r125 * xi_posi * ei_posi * zi_posi
      an_1(8)  = r125 * xi_nega * ei_posi * zi_posi
!
      end subroutine shape_function_an_1
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnxi_1(dnxi_1,                          &
     &          ei_nega, zi_nega, ei_posi, zi_posi, dxi)
!
      real (kind=kreal), intent(inout) :: dnxi_1(8)
!
      real (kind=kreal), intent(in) :: ei_nega, zi_nega
      real (kind=kreal), intent(in) :: ei_posi, zi_posi
      real (kind=kreal), intent(in) :: dxi
!
!
      dnxi_1(1) = -r125 * ei_nega * zi_nega * dxi
      dnxi_1(2) =  r125 * ei_nega * zi_nega * dxi
      dnxi_1(3) =  r125 * ei_posi * zi_nega * dxi
      dnxi_1(4) = -r125 * ei_posi * zi_nega * dxi
      dnxi_1(5) = -r125 * ei_nega * zi_posi * dxi
      dnxi_1(6) =  r125 * ei_nega * zi_posi * dxi
      dnxi_1(7) =  r125 * ei_posi * zi_posi * dxi
      dnxi_1(8) = -r125 * ei_posi * zi_posi * dxi
!
      end subroutine shape_function_dnxi_1
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnei_1(dnei_1,                          &
     &          xi_nega, zi_nega, xi_posi, zi_posi, dei)
!
      real (kind=kreal), intent(inout):: dnei_1(8)
!
      real (kind=kreal), intent(in) :: xi_nega, zi_nega
      real (kind=kreal), intent(in) :: xi_posi, zi_posi
      real (kind=kreal), intent(in) :: dei
!
!
      dnei_1(1) = -r125 * xi_nega * zi_nega * dei
      dnei_1(2) = -r125 * xi_posi * zi_nega * dei
      dnei_1(3) =  r125 * xi_posi * zi_nega * dei
      dnei_1(4) =  r125 * xi_nega * zi_nega * dei
      dnei_1(5) = -r125 * xi_nega * zi_posi * dei
      dnei_1(6) = -r125 * xi_posi * zi_posi * dei
      dnei_1(7) =  r125 * xi_posi * zi_posi * dei
      dnei_1(8) =  r125 * xi_nega * zi_posi * dei
!
      end subroutine shape_function_dnei_1
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnzi_1(dnzi_1,                          &
     &          xi_nega, ei_nega, xi_posi, ei_posi, dzi)
!
      real (kind=kreal), intent(inout) :: dnzi_1(8)
!
      real (kind=kreal), intent(in) :: xi_nega, ei_nega
      real (kind=kreal), intent(in) :: xi_posi, ei_posi
      real (kind=kreal), intent(in) :: dzi
!
!
      dnzi_1(1) = -r125 * xi_nega * ei_nega * dzi
      dnzi_1(2) = -r125 * xi_posi * ei_nega * dzi
      dnzi_1(3) = -r125 * xi_posi * ei_posi * dzi
      dnzi_1(4) = -r125 * xi_nega * ei_posi * dzi
      dnzi_1(5) =  r125 * xi_nega * ei_nega * dzi
      dnzi_1(6) =  r125 * xi_posi * ei_nega * dzi
      dnzi_1(7) =  r125 * xi_posi * ei_posi * dzi
      dnzi_1(8) =  r125 * xi_nega * ei_posi * dzi
!
      end subroutine shape_function_dnzi_1
!
!-----------------------------------------------------------------------
!
      end module shape_func_3d_linear
