!>@file   t_gauss_points.f90
!!        module t_gauss_points
!!
!! @author H. Matsui
!! @date   Programmed in 2003
!!
!
!> @brief Constants for Gauss-Legendre integration
!!
!!@verbatim
!!      subroutine const_gauss_colatitude(num_gauss, gauss)
!!      subroutine construct_gauss_coefs(num_gauss, gauss)
!! *************************************************
!! construct points and coefficients for 
!! Gauss-Legendre integration
!!
!!    Integration area:  -1 < x < 1
!!
!!      subroutine set_gauss_colatitude(gauss)
!! *************************************************
!! construct position of Gauss-Legendre points
!!      subroutine alloc_work_4_integration(num_inte, n_point, g_int)
!!
!!      subroutine dealloc_gauss_points(gauss)
!!      subroutine dealloc_gauss_colatitude(gauss)
!!      subroutine dealloc_work_4_integration(g_int)
!!
!!      subroutine set_points_4_integration(xst, xed, gauss, g_int)
!!      subroutine set_points_4_elevation(gauss, g_int)
!!
!!      subroutine cal_gauss_integrals(gauss, g_int, x)
!!
!!      subroutine check_gauss_points(gauss)
!!@endverbatim
!
      module t_gauss_points
!
      use m_precision
      use m_constants
!
      implicit none
!
      type gauss_points
!>        Number of Gauss points
        integer(kind=kint) :: n_point = 400
!>        Position of Gauss points (@$f-1<x<1@$f)
        real(kind = kreal), allocatable :: point(:)
!>        Coefficients of Gauss integration
        real(kind = kreal), allocatable :: weight(:)
!
!>        Position of Gauss-Legendre colatitude
        real(kind = kreal), allocatable :: colat(:)
!>        Position of Gauss-Legendre colatitude in degree
        real(kind = kreal), allocatable :: colat_deg(:)
!
!>        longitude of spherical grid
        real(kind = kreal), allocatable :: azimuth(:)
!>        longitude of spherical grid in degree
        real(kind = kreal), allocatable :: azim_deg(:)
      end type gauss_points
!
      type gauss_integrations
!>        number of functions to integrate
        integer(kind = kint) :: num_inte
!>        coefficient due to changing integration area
        real(kind = kreal) :: coef_len
!>        gauss points in integration area
        real(kind = kreal), allocatable :: x_point(:)
!>        function values on integration points
        real(kind = kreal), allocatable :: f_point(:,:)
      end type gauss_integrations
!
      private :: alloc_gauss_points, alloc_gauss_colatitude
      private :: dealloc_gauss_colats
      private :: set_gauss_colatitude
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_gauss_colatitude(num_gauss, gauss)
!
      integer(kind = kint), intent(in) :: num_gauss 
      type(gauss_points), intent(inout) :: gauss
!
!
      call construct_gauss_coefs(num_gauss, gauss)
      call set_gauss_colatitude(gauss)
!
      end subroutine const_gauss_colatitude
!
! -----------------------------------------------------------------------
!
      subroutine construct_gauss_coefs(num_gauss, gauss)
!
      use gauss_integration
!
      integer(kind = kint), intent(in) :: num_gauss
      type(gauss_points), intent(inout) :: gauss
!
!
      call alloc_gauss_points(num_gauss, gauss)
      call const_gauss_points_coefs                                     &
     &   (gauss%n_point, gauss%point, gauss%weight)
!
      end subroutine construct_gauss_coefs
!
! -----------------------------------------------------------------------
!
      subroutine set_gauss_colatitude(gauss)
!
      use gauss_integration
!
      type(gauss_points), intent(inout) :: gauss
!
!
      call alloc_gauss_colatitude(gauss)
      call set_gauss_points_sph(gauss%n_point, gauss%point,             &
     &    gauss%colat, gauss%colat_deg, gauss%azimuth, gauss%azim_deg)
!
      end subroutine set_gauss_colatitude
!
! -----------------------------------------------------------------------
!
      subroutine alloc_work_4_integration(num_inte, n_point, g_int)
!
      integer(kind = kint), intent(in) :: num_inte, n_point
      type(gauss_integrations), intent(inout) :: g_int
!
!
      g_int%num_inte = num_inte
      allocate( g_int%x_point(n_point) )
      allocate( g_int%f_point(g_int%num_inte,n_point) )
!
      g_int%x_point = 0.0d0
      g_int%f_point = 0.0d0
!
      end subroutine alloc_work_4_integration
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_colatitude(gauss)
!
      type(gauss_points), intent(inout) :: gauss
!
!
      call dealloc_gauss_colats(gauss)
      call dealloc_gauss_points(gauss)
!
      end subroutine dealloc_gauss_colatitude
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_points(gauss)
!
      type(gauss_points), intent(inout) :: gauss
!
!
      deallocate(gauss%point, gauss%weight)
!
      end subroutine dealloc_gauss_points
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_colats(gauss)
!
      type(gauss_points), intent(inout) :: gauss
!
!
      deallocate(gauss%colat)
      deallocate(gauss%colat_deg)
      deallocate(gauss%azimuth)
      deallocate(gauss%azim_deg)
!
      end subroutine dealloc_gauss_colats
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_work_4_integration(g_int)
!
      type(gauss_integrations), intent(inout) :: g_int
!
      deallocate( g_int%x_point, g_int%f_point )
!
      end subroutine dealloc_work_4_integration
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_points(num_gauss, gauss)
!
      integer(kind = kint), intent(in) :: num_gauss
      type(gauss_points), intent(inout) :: gauss
!
!
      gauss%n_point = num_gauss
      allocate(gauss%point(gauss%n_point))
      allocate(gauss%weight(gauss%n_point))
!
      gauss%point =  0.0d0
      gauss%weight = 0.0d0
!
      end subroutine alloc_gauss_points
!
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_colatitude(gauss)
!
      type(gauss_points), intent(inout) :: gauss
!
!
      allocate(gauss%colat(gauss%n_point))
      allocate(gauss%colat_deg(gauss%n_point))
      allocate(gauss%azimuth(2*gauss%n_point))
      allocate(gauss%azim_deg(2*gauss%n_point))
!
      gauss%colat =      0.0d0
      gauss%colat_deg  = 0.0d0
      gauss%azimuth =    0.0d0
      gauss%azim_deg =   0.0d0
!
      end subroutine alloc_gauss_colatitude
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_points_4_integration(xst, xed, gauss, g_int)
!
      use gauss_integration
!
      real(kind = kreal), intent(in) :: xst, xed
      type(gauss_points), intent(in) :: gauss
!
      type(gauss_integrations), intent(inout) :: g_int
!
!
      call set_gauss_points_integration(xst, xed, gauss%n_point,        &
     &    gauss%point, g_int%x_point, g_int%coef_len)
!
      end subroutine set_points_4_integration
!
! -----------------------------------------------------------------------
!
      subroutine set_points_4_elevation(gauss, g_int)
!
      use gauss_integration
!
      type(gauss_points), intent(in) :: gauss
!
      type(gauss_integrations), intent(inout) :: g_int
!
!
      call set_gauss_colat_integration                                  &
     &   (gauss%n_point, gauss%colat, g_int%x_point, g_int%coef_len)
!
      end subroutine set_points_4_elevation
!
! -----------------------------------------------------------------------
!
      subroutine cal_gauss_integrals(gauss, g_int, x)
!
      use gauss_integration
!
      type(gauss_points), intent(in) :: gauss
      type(gauss_integrations), intent(in) :: g_int
!
      real(kind = kreal), intent(inout) :: x(g_int%num_inte)
!
      call cal_gauss_integration(g_int%num_inte, gauss%n_point,         &
     &    gauss%weight, g_int%f_point, g_int%coef_len, x)
!
      end subroutine cal_gauss_integrals
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_gauss_points(gauss)
!
      type(gauss_points), intent(in) :: gauss
!
      integer (kind = kint) :: i
!
!
      write(*,*) 'gaussian points and coefficients'
      do i = 1, gauss%n_point
        write(*,'(i5,1p2E25.15e3)')                                     &
     &         i, gauss%point(i), gauss%weight(i)
      end do
!
      write(*,*) 'Gauss-Legendre colatitude'
      do i = 1, gauss%n_point
        write(*,'(i5,1p3E25.15e3)') i, gauss%point(i),                  &
     &         gauss%colat(i), gauss%colat_deg(i)
      end do
!
      write(*,*) 'Azimuth'
      do i = 1, 2 * gauss%n_point
        write(*,'(i5,1p2E25.15e3)')                                     &
     &         i, gauss%azimuth(i), gauss%azim_deg(i)
      end do
!
      end subroutine check_gauss_points
!
! -----------------------------------------------------------------------
!
      end module t_gauss_points
