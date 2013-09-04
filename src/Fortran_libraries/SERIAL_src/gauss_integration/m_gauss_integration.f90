!>@file   m_gauss_integration.f90
!!        module m_gauss_integration
!!
!! @author H. Matsui
!! @date   Programmed in 2003
!!
!
!> @brief Constants for Gauss-Legendre integration
!!
!!@verbatim
!!      subroutine allocate_work_4_integration
!!      subroutine deallocate_work_4_integration
!!
!!      subroutine set_points_4_integration(xst, xed)
!!         set gauss points in integration area from xst to xed
!!
!!      subroutine set_points_4_elevation
!!         set gauss points in meridional direction from pi to 0
!!
!!      subroutine gaussian_integration(x)
!!         take numerical integration
!!                 x: solution
!!@endverbatim
!!
!!@n @param xst   start position of Gauss integration
!!@n @param xed   end position of Gauss integration
!!@n @param x     solution by gauss integration
!
      module m_gauss_integration
!
      use m_precision
      use m_constants
!
      implicit none
!
!>        number of functions to integrate
      integer(kind = kint) :: num_inte
!>        coefficient due to changing integration area
      real(kind = kreal) :: coef_gauss
!>        gauss points in integration area
      real(kind = kreal), dimension(:), allocatable :: x_point
!>        function values on integration points
      real(kind = kreal), dimension(:,:), allocatable :: f_point
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_work_4_integration
!
      use m_gauss_points
!
!
      allocate( x_point(n_point) )
      allocate( f_point(num_inte,n_point) )
!
      x_point = 0.0d0
      f_point = 0.0d0
!
      end subroutine allocate_work_4_integration
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_work_4_integration
!
      deallocate( x_point )
      deallocate( f_point )
!
      end subroutine deallocate_work_4_integration
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_points_4_integration(xst, xed)
!
      use m_gauss_points
!
      real(kind = kreal), intent(in) :: xst, xed
!
      integer (kind = kint) :: i
!
!
      do i = 1, n_point
        x_point(i) = half*( xst + xed + (xed-xst) * w_point(i) )
      end do
      coef_gauss = half *(xed - xst)
!
      end subroutine set_points_4_integration
!
! -----------------------------------------------------------------------
!
      subroutine set_points_4_elevation
!
      use m_gauss_points
!
      integer (kind = kint) :: i
      real(kind = kreal) :: pi
!
!
      pi = four * atan(one)
!
      do i = 1, n_point
        x_point(i) = w_colat(i)
      end do
      coef_gauss = - half * pi
!
      end subroutine set_points_4_elevation
!
! -----------------------------------------------------------------------
!
      subroutine gaussian_integration(x)
!
      use m_gauss_points
!
!
      real(kind = kreal), intent(inout) :: x(num_inte)
!
      integer (kind = kint) :: i, j
!
      x = 0.0d0
!
!$omp parallel do
      do j = 1, num_inte
        do i = 1, n_point
          x(j) = x(j) + w_coefs(i) * f_point(j,i)
        end do
      end do
!$omp end parallel do
!
      do j = 1, num_inte
        x(j) = coef_gauss * x(j)
      end do
!
      end subroutine gaussian_integration
!
! -----------------------------------------------------------------------
!
      end module m_gauss_integration
