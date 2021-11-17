!>@file  cal_shape_function_1d.f90
!!       module cal_shape_function_1d
!!
!!@author H. Matsui
!!@date   Programmed in Dec., 2008
!
!> @brief  caliculate shape function and differences at Gauss points
!!
!!@verbatim
!!      subroutine s_cal_shape_function_1d_linear(ntot_int_1d,          &
!!     &          an, dnxi, xi)
!!      subroutine s_cal_shape_function_1d_quad(ntot_int_1d,            &
!!     &          an, dnxi, xi)
!!@endverbatim
!
      module cal_shape_function_1d
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use shape_func_elements
!
      implicit none
!
      real (kind=kreal) :: xi_nega, xi_posi, xi_sqre
      private :: xi_nega, xi_posi, xi_sqre
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_function_1d_linear(ntot_int_1d, an, dnxi,  &
     &          xi)
!
      use shape_func_1d_linear
!
      integer (kind=kint), intent(in) :: ntot_int_1d
      real(kind=kreal), intent(in) :: xi(ntot_int_1d)
!
      real(kind=kreal), intent(inout)                                   &
     &        :: an(num_linear_edge,ntot_int_1d)
      real(kind=kreal), intent(inout)                                   &
     &        :: dnxi(num_linear_edge,ntot_int_1d)
! 
      integer (kind=kint) :: ix
! 
!
      do ix = 1, ntot_int_1d
        call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi(ix) )
!
        call shape_function_an_1d_1( an(1,ix), xi_nega, xi_posi)
        call shape_function_dnxi_1d_1( dnxi(1,ix), one)
      end do
!
      end subroutine s_cal_shape_function_1d_linear
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_shape_function_1d_quad(ntot_int_1d, an, dnxi,    &
     &          xi)
!
      use shape_func_1d_quad
!
      integer (kind=kint), intent(in) :: ntot_int_1d
      real(kind=kreal), intent(in) :: xi(ntot_int_1d)
!
      real(kind=kreal), intent(inout) :: an(num_quad_edge,ntot_int_1d)
      real(kind=kreal), intent(inout) :: dnxi(num_quad_edge,ntot_int_1d)
!
      integer (kind=kint) :: ix
!
!
      do ix = 1, ntot_int_1d
        call s_shape_elenents_aw_1d(xi_nega, xi_posi, xi_sqre, xi(ix) )
!
        call shape_function_an_1d_20( an(1,ix), xi(ix),                 &
     &      xi_nega, xi_posi, xi_sqre)
        call shape_function_dnxi_1d_20(dnxi(1,ix), xi(ix), one )
      end do
!
      end subroutine s_cal_shape_function_1d_quad
!
!-----------------------------------------------------------------------
!
      end module cal_shape_function_1d
