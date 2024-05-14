!>@file   colormap_space.F90
!!@brief  module colormap_space
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2024
!
!>@brief  Colormapping
!!
!!@verbatim
!!      subroutine s_colormap_space(rnorm, r, g, b)
!!        real(kind = kreal), intent(in) :: rnorm
!!        real(kind = kreal), intent(inout) ::  r, g, b
!!@endverbatim
!
      module colormap_space
!
      use m_precision
      use m_constants
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine s_colormap_space(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: c_r1 = 37067.0 / 158860.0
      real(kind = kreal), parameter :: c_r2 = 85181.0 / 230350.0
      real(kind = kreal), parameter                                     &
     &         :: c_r3 = (sqrt(3196965649.0) + 83129.0) / 310480.0
      real(kind = kreal), parameter :: c_r4 = 231408.0 / 362695.0
      real(kind = kreal), parameter :: c_r5 = 152073.0 / 222340.0
      real(kind = kreal), parameter :: c_r6 = 294791.0 / 397780.0
      real(kind = kreal), parameter :: c_r7 = 491189.0 / 550980.0
!
      real(kind = kreal), parameter                                     &
     &         :: c_g1 = (-sqrt(166317494.0) + 39104.0) / 183830.0
      real(kind = kreal), parameter                                     &
     &         :: c_g3 = (3.0 * sqrt(220297369.0) + 58535.0) / 155240.0
!
      real(kind = kreal), parameter :: c_b1 = 51987.0 / 349730.0
!
      real(kind = kreal) :: x, xx
!
!
      x = rnorm
      if (x .lt. c_r1) then
        r = 0.0
      else if (x .lt. c_r2) then
        xx = x - c_r1
        r = (780.25 * xx + 319.71) * xx / 255.0
      else if (x .lt. c_r3) then
        r = ((1035.33580904442 * x - 82.5380748768798) * x              &
     &     - 52.8985266363332) / 255.0
      else if (x .lt. c_r4) then
        r = (339.41 * x - 33.194) / 255.0
      else if (x .lt. c_r5) then
        r = (1064.8 * x - 496.01) / 255.0
      else if (x .lt. c_r6) then
        r = (397.78 * x - 39.791) / 255.0
      else if (x .lt. c_r7) then
        r = 1.0
      else if (x .lt. one) then
        r = (5509.8 * x + 597.91) * x / 255.0
      else
        r = 1.0
      end if

      if (x .lt. zero)  then
        g = 0.0
      else if (x .lt. c_g1) then
        g = (-1838.3 * x + 464.36) * x / 255.0
      else if (x .lt. c_r1) then
        g = (-317.72 * x + 74.134) / 255.0
      else if (x .lt. c_g3) then
        g = 0.0
      else if (x .lt. c_r6) then
        xx = x - c_g3
        g = (-1945.0 * xx + 1430.2) * xx / 255.0
      else if (x .lt. c_r7) then
        g = ((-1770.0 * x + 3.92813840044638e3) * x                     &
     &     - 1.84017494792245e3) / 255.0
      else
        g = 1.0
      end if

      if (x .lt. zero) then
        b = 0.0
      else if (x .lt. c_b1) then
        b = (458.79 * x) / 255.0
      else if (x .lt. c_r2) then
        b = (109.06 * x + 51.987) / 255.0
      else if (x .lt. c_r3) then
        b = (339.41 * x - 33.194) / 255.0
      else if (x .lt. c_g3) then
        b = ((-1552.4 * x + 1170.7) * x - 92.996) / 255.0
      else if (x .lt. 27568.0 / 38629.0) then
        b = 0.0
      else if (x .lt. 81692.0 / 96241.0) then
        b = (386.29 * x - 275.68) / 255.0
      else if (x .lt. 1.0) then
        b = (1348.7 * x - 1092.6) / 255.0
      else
        b = 1.0
      end if
!
      end subroutine s_colormap_space
!
! ----------------------------------------------------------------------
!
      end module colormap_space
