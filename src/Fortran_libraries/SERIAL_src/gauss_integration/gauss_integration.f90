!>@file   gauss_integration.f90
!!        module gauss_integration
!!
!! @author H. Matsui
!! @date   Programmed in 2003
!!
!
!> @brief Constants for Gauss-Legendre integration
!!
!!@verbatim
!!      subroutine const_gauss_points_coefs(n_point, w_point, w_coefs)
!!          construct points and coefficients for 
!!          Gauss-Legendre integration
!!          Integration area:  -1 < x < 1
!!                n_point:  Number of Gauss points
!!                w_point:  Gauss points
!!                w_point:  Coefficients for Gauss integration
!!
!!      subroutine set_gauss_points_sph(n_point, w_point,               &
!!     &          w_colat, w_col_deg, w_azim, w_azim_deg)
!!          construct position of Gauss-Legendre points
!!                w_colat:    Gauss-Legendre colatitude (Radian)
!!                w_azim:     Gauss-Legendre longitude (Radian)
!!                w_col_deg:  Gauss-Legendre colatitude (Degree)
!!                w_azim_deg: Gauss-Legendre longitude (Degree)
!!
!!      subroutine set_gauss_points_integration                         &
!!     &         (xst, xed, n_point, w_point, x_point, coef_gauss)
!!         set gauss points in integration area from xst to xed
!!                xst:    Start point for integration
!!                xed:    End point for integration
!!                x_point: Gauss points between xst and xed
!!                coef_gauss: Coefficients for length adjustment
!!
!!      subroutine set_gauss_colat_integration                          &
!!     &         (n_point, w_colat, x_point, coef_gauss)
!!         set gauss points in meridional direction from pi to 0
!!
!!      subroutine cal_gauss_integration                                &
!!     &         (num_inte, n_point, w_coefs, f_point, coef_gauss, x)
!!         take numerical integration
!!                 num_inte:          number of data for integration
!!                 f_point(num_inte): field data at gauss points
!!                 x(num_inte):       solution
!!@endverbatim
!
      module gauss_integration
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_gauss_points_coefs(n_point, w_point, w_coefs)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: w_point(n_point)
      real(kind = kreal), intent(inout) :: w_coefs(n_point)
!
      integer(kind=kint) :: n, m, nl, j, i
      real(kind = kreal) :: x1, x2
      real(kind = kreal) :: z, z1, xm, xl, pp, p1, p2, p3
      real(kind = kreal) :: pi
!
!
      pi = four * atan(one)
!
      x1 = zero
      x2 = one
!
      w_point(1) = half
      w_coefs(1) = one
!      do i = 1, 1
!       write(*,*) 1,i, w_coefs(i), w_point(i)
!      end do
!
!
      nl = ione
!
      do n = 2, n_point
        m = ( n+ione-mod(n+ione,itwo) ) / itwo
        xm = half * (x2+x1)
        xl = half * (x2-x1)
!
        do i = 1, m
          z = cos ( pi*(dble(i)-quad)/(dble(n)+half) )
!
          do
!
            p1 = one
            p2 = zero
!
            do j = 1, n
              p3 = p2
              p2 = p1
              p1 = ((two*dble(j)-one)*z*p2-(dble(j)-one)*p3) / dble(j)
            end do
!
            pp = dble(n)*(z*p1-p2)/(z*z-one)
            z1 = z
            z = z1 - p1/pp
            if ( abs(z-z1) .lt. 3.0e-12 ) exit
!
          end do
!
          w_point(i) = xm-xl*z
          w_point(n-i+1) = xm+xl*z
          w_coefs(i) = two*xl / ((one - z*z)*pp*pp)
          w_coefs(n-i+1) = w_coefs(i)
!          write(*,*) n, i, w_point(i), w_coefs(i)
!          write(*,*) n, n-i+1, w_point(n-i+1),  w_coefs(n-i+1)
!
        end do
!
!        do i = 1, n
!          write(*,*) n,i, w_coefs(i), w_point(i)
!        end do
!
      end do
!
      do i = 1, n_point
        w_point(i) = w_point(i)*two - one
        w_coefs(i) = w_coefs(i)*two
      end do
!
      return
      end subroutine const_gauss_points_coefs
!
! -----------------------------------------------------------------------
!
      subroutine set_gauss_points_sph(n_point, w_point,                 &
     &          w_colat, w_col_deg, w_azim, w_azim_deg)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: w_point(n_point)
!
      real(kind = kreal), intent(inout) :: w_colat(n_point)
      real(kind = kreal), intent(inout) :: w_col_deg(n_point)
      real(kind = kreal), intent(inout) :: w_azim(2*n_point)
      real(kind = kreal), intent(inout) :: w_azim_deg(2*n_point)
!
      real(kind = kreal) :: pi
      integer (kind = kint) :: i
!
!
      pi = four * atan(one)
!
      do i = 1, n_point
        w_colat(i) = acos(w_point(i))
        w_col_deg(i) = 180.d0 * w_colat(i) / pi
      end do
!
      do i = 1, 2*n_point
        w_azim(i)     = pi*dble(i-1) / dble(n_point)
        w_azim_deg(i) = 180.0d0*dble(i-1) / dble(n_point)
      end do
!
      end subroutine set_gauss_points_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_gauss_points_integration                           &
     &         (xst, xed, n_point, w_point, x_point, coef_gauss)
!
      real(kind = kreal), intent(in) :: xst, xed
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: w_point(n_point)
!
      real(kind = kreal), intent(inout) :: x_point(n_point)
      real(kind = kreal), intent(inout) :: coef_gauss
!
      integer (kind = kint) :: i
!
!
      do i = 1, n_point
        x_point(i) = half*( xst + xed + (xed-xst) * w_point(i) )
      end do
      coef_gauss = half *(xed - xst)
!
      end subroutine set_gauss_points_integration
!
! -----------------------------------------------------------------------
!
      subroutine set_gauss_colat_integration                            &
     &         (n_point, w_colat, x_point, coef_gauss)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: w_colat(n_point)
!
      real(kind = kreal), intent(inout) :: x_point(n_point)
      real(kind = kreal), intent(inout) :: coef_gauss
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
      end subroutine set_gauss_colat_integration
!
! -----------------------------------------------------------------------
!
      subroutine cal_gauss_integration                                  &
     &         (num_inte, n_point, w_coefs, f_point, coef_gauss, x)
!
      integer(kind = kint), intent(in) :: num_inte, n_point
      real(kind = kreal), intent(in) :: w_coefs(n_point)
      real(kind = kreal), intent(in) :: f_point(num_inte, n_point)
      real(kind = kreal), intent(in) :: coef_gauss
!
      real(kind = kreal), intent(inout) :: x(num_inte)
!
      integer (kind = kint) :: i, j
!
!
!$omp parallel do private(i,j)
      do j = 1, num_inte
        x(j) = 0.0d0
        do i = 1, n_point
          x(j) = x(j) + w_coefs(i) * f_point(j,i)
        end do
        x(j) = coef_gauss * x(j)
      end do
!$omp end parallel do
!
      end subroutine cal_gauss_integration
!
! -----------------------------------------------------------------------
!
      end module gauss_integration
