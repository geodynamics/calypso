!>@file   m_gauss_points.f90
!!        module m_gauss_points
!!
!! @author H. Matsui
!! @date   Programmed in 2003
!!
!
!> @brief Constants for Gauss-Legendre integration
!!
!!@verbatim
!!       subroutine allocate_gauss_points
!!       subroutine allocate_gauss_colatitude
!!       subroutine deallocate_gauss_points
!!       subroutine deallocate_gauss_colatitude
!!
!! *************************************************
!!
!!      subroutine construct_gauss_coefs
!!
!! construct points and coefficients for 
!! Gauss-Legendre integration
!!
!!    Integration area:  -1 < x < 1
!!
!! *************************************************
!!
!!      subroutine set_gauss_colatitude
!!
!! construct position of Gauss-Legendre points
!!
!! *************************************************
!!@endverbatim
!
      module m_gauss_points
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Number of Gauss points
      integer(kind=kint) :: n_point = 400
!>      Position of Gauss points (@$f-1<x<1@$f)
      real(kind = kreal), dimension(:), allocatable :: w_point
!>      Coefficients of Gauss integration
      real(kind = kreal), dimension(:), allocatable :: w_coefs
!
!>      Position of Gauss-Legendre colatitude
      real(kind = kreal), dimension(:), allocatable :: w_colat
!>      Position of Gauss-Legendre colatitude in degree
      real(kind = kreal), dimension(:), allocatable :: w_col_deg
!
!>      longitude of spherical grid
      real(kind = kreal), dimension(:), allocatable :: w_azim
!>      longitude of spherical grid in degree
      real(kind = kreal), dimension(:), allocatable :: w_azim_deg
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine allocate_gauss_points
!
       allocate(w_point(n_point))
       allocate(w_coefs(n_point))
!
       w_point = 0.0d0
       w_coefs = 0.0d0
!
       end subroutine allocate_gauss_points
!
! -----------------------------------------------------------------------
!
       subroutine allocate_gauss_colatitude
!
       allocate(w_colat(n_point))
       allocate(w_col_deg(n_point))
       allocate(w_azim(2*n_point))
       allocate(w_azim_deg(2*n_point))
!
       w_colat = 0.0d0
       w_col_deg = 0.0d0
       w_azim = 0.0d0
       w_azim_deg = 0.0d0
!
       end subroutine allocate_gauss_colatitude
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_gauss_points
!
       deallocate(w_point)
       deallocate(w_coefs)
!
       end subroutine deallocate_gauss_points
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_gauss_colatitude
!
       deallocate(w_colat)
       deallocate(w_col_deg)
       deallocate(w_azim)
       deallocate(w_azim_deg)
!
       end subroutine deallocate_gauss_colatitude
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine construct_gauss_coefs
!
!
      real(kind = kreal) :: x1, x2
      real(kind = kreal) :: z, z1, xm, xl, pp, p1, p2, p3
      integer(kind=kint) :: n, m, nl, j, i
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
      end subroutine construct_gauss_coefs 
!
! -----------------------------------------------------------------------
!
      subroutine set_gauss_colatitude
!
      real(kind = kreal) :: pi
!
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
      end subroutine set_gauss_colatitude
!
! -----------------------------------------------------------------------
!
      end module m_gauss_points
