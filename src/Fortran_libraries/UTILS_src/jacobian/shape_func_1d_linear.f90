!
!      module shape_func_1d_linear
!
!     Written by H. Matsui on Sep. 2005
!
!      subroutine shape_function_an_1d_1(an_1, xi_nega, xi_posi)
!      subroutine shape_function_dnxi_1d_1(dnxi, dxi)
!
!     xi:  \xi
!     xi_nega:   1 - \xi
!     xi_posi:   1 + \xi
!
      module shape_func_1d_linear
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
      subroutine shape_function_an_1d_1(an_1, xi_nega, xi_posi)
!
      real (kind=kreal), intent(inout) :: an_1(2)
!
      real (kind=kreal), intent(in)  :: xi_nega, xi_posi
!
!
      an_1(1)  = half * xi_nega
      an_1(2)  = half * xi_posi
!
!
      end subroutine shape_function_an_1d_1
!
!-----------------------------------------------------------------------
!
      subroutine shape_function_dnxi_1d_1(dnxi, dxi)
!
      real (kind=kreal), intent(inout) :: dnxi(2)
      real (kind=kreal), intent(in)    :: dxi
!
!
      dnxi(1) = -half * dxi
      dnxi(2) =  half * dxi
!
      end subroutine shape_function_dnxi_1d_1
!
!-----------------------------------------------------------------------
!
      end module shape_func_1d_linear
