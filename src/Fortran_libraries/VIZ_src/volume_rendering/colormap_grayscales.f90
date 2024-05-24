!>@file   colormap_grayscales.F90
!!@brief  module colormap_grayscales
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2024
!
!>@brief  Colormapping for grayscales
!!
!!@verbatim
!!      subroutine s_colormap_grayscale(rnorm, r, g, b)
!!      subroutine s_colormap_sym_grayscale(rnorm, r, g, b)
!!        real(kind = kreal), intent(in) :: rnorm
!!        real(kind = kreal), intent(inout) ::  r, g, b
!!@endverbatim
!
      module colormap_grayscales
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
      subroutine s_colormap_grayscale(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: black = zero
      real(kind = kreal), parameter :: white = one
!
!
      if (rnorm .lt. zero ) then
        r = zero
        g = zero
        b = zero
      else if (rnorm .ge. zero .and. rnorm.lt.white) then
        r = 0.85d0*rnorm
        g = 0.85d0*rnorm
        b = 0.85d0*rnorm
      else if (rnorm .ge. white ) then
        r = 0.85d0
        g = 0.85d0
        b = 0.85d0
      end if
!
      end subroutine s_colormap_grayscale
!
! ----------------------------------------------------------------------
!
      subroutine s_colormap_sym_grayscale(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: black = zero
      real(kind = kreal), parameter :: white = one
      real(kind = kreal), parameter :: half = one / two
!
!
      if (rnorm .lt. zero ) then
        r = zero
        g = zero
        b = zero
      else if (rnorm .ge. zero .and. rnorm.lt.half) then
        r = 0.85d0*two*rnorm
        g = 0.85d0*two*rnorm
        b = 0.85d0*two*rnorm
      else if (rnorm .ge. half .and. rnorm.lt.white) then
        r = 0.85d0*two*(one - rnorm)
        g = 0.85d0*two*(one - rnorm)
        b = 0.85d0*two*(one - rnorm)
      else if (rnorm .ge. white ) then
        r = zero
        g = zero
        b = zero
      end if
!
      end subroutine s_colormap_sym_grayscale
!
! ----------------------------------------------------------------------
!
      end module colormap_grayscales
