!>@file   colormap_two_colors.F90
!!@brief  module colormap_two_colors
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2024
!
!>@brief  Colormapping with two colors
!!
!!@verbatim
!!      subroutine s_colormap_redblue(rnorm, r, g, b)
!!      subroutine s_colormap_orangecyan(rnorm, r, g, b)
!!        real(kind = kreal), intent(in) :: rnorm
!!        real(kind = kreal), intent(inout) ::  r, g, b
!!@endverbatim
!
      module colormap_two_colors
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
      subroutine s_colormap_redblue(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: abyss = zero
      real(kind = kreal), parameter :: blue =  0.1d0
      real(kind = kreal), parameter :: white = half
      real(kind = kreal), parameter :: red =   0.9d0
      real(kind = kreal), parameter :: blood =  one
!
!
      if (rnorm .lt. abyss ) then
        r = zero
        g = 0.2d0
        b = 0.8d0
      else if (rnorm .ge. abyss .and. rnorm.lt.blue) then
        r = zero
        g = 2.0d0 * (blue - rnorm)
        b = 0.8d0 + 2.0d0 * rnorm
      else if (rnorm .ge. blue .and. rnorm.lt.white) then
        r = (rnorm - blue) * 2.0d0
        g = (rnorm - blue) * 2.0d0
        b = one - (rnorm - blue) * 0.25
      else if (rnorm .ge. white .and. rnorm.lt.red) then
        r = one - (red - rnorm) * 0.25
        g = (red - rnorm) * 2.0d0
        b = (red - rnorm) * 2.0d0
      else if (rnorm .ge. red .and. rnorm.lt. blood) then
        r = one - (rnorm - red) * 2.0d0
        g = zero
        b = zero
      else if (rnorm .ge. blood) then
        r = 0.8d0
        g = zero
        b = zero
      end if
!
      end subroutine s_colormap_redblue
!
! ----------------------------------------------------------------------
!
      subroutine s_colormap_orangecyan(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: blue = zero
      real(kind = kreal), parameter :: white = half
      real(kind = kreal), parameter :: red =  one
!
!
      if (rnorm .lt. blue ) then
        r = 0.0d0
        g = 1.0d0
        b = 1.0d0
      else if (rnorm .ge. blue .and. rnorm.lt.white) then
        r = rnorm * 2.0d0
        g = 1.0d0
        b = 1.0d0 - rnorm * 0.5d0
      else if (rnorm .ge. white .and. rnorm.lt.red) then
        r = 1.0
        g = (red - rnorm) + 0.5d0
        b = (red - rnorm) * 1.5d0
      else if (rnorm .ge. red) then
        r = 1.0d0
        g = 0.5d0
        b = 0.0d0
      end if
!
      end subroutine s_colormap_orangecyan
!
! ----------------------------------------------------------------------
!
      end module colormap_two_colors
