!
!      module shape_func_1d_quad
!
!     Written by H. Matsui on Sep. 2005
!
!      subroutine shape_function_an_1d_20(an_20, xi,                    &
!     &          xi_nega, xi_posi, xi_sqre)
!      subroutine shape_function_dnxi_1d_20(dnxi, xi, dxi)
!
!     xi:  \xi
!     xi_nega:   1 - \xi
!     xi_posi:   1 + \xi
!     xi_sqre:   1 - \xi
!
      module shape_func_1d_quad
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_an_1d_20(an_20, xi,                     &
     &          xi_nega, xi_posi, xi_sqre)
!
      use m_constants
!
      real (kind=kreal), intent(inout) :: an_20(3)
!
      real (kind=kreal), intent(in) :: xi
      real (kind=kreal), intent(in) :: xi_nega, xi_posi, xi_sqre
!
!
      an_20(1)  = -half * xi * xi_nega
      an_20(2)  =  xi_sqre
      an_20(3)  =  half * xi * xi_posi
!
      end subroutine shape_function_an_1d_20
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnxi_1d_20(dnxi, xi, dxi)
!
      use m_constants
!
      real (kind=kreal), intent(inout) :: dnxi(3)
      real (kind=kreal), intent(in) :: xi, dxi
!
!
      dnxi(1)  = -half * (one - two*xi) * dxi
      dnxi(2)  = -two  * xi * dxi
      dnxi(3)  =  half * (one + two*xi) * dxi
!
      end subroutine shape_function_dnxi_1d_20
!
!-----------------------------------------------------------------------
!
      end module shape_func_1d_quad
