!>@file   colormap_rainbow.F90
!!@brief  module colormap_rainbow
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2024
!
!>@brief  Rainbow colormapping
!!
!!@verbatim
!!      subroutine s_colormap_rainbow(rnorm, r, g, b)
!!        real(kind = kreal), intent(in) :: rnorm
!!        real(kind = kreal), intent(inout) ::  r, g, b
!!@endverbatim
!
      module colormap_rainbow
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
      subroutine s_colormap_rainbow(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: purple = zero
      real(kind = kreal), parameter :: blue =   0.1e0
      real(kind = kreal), parameter :: ocean =  0.325e0
      real(kind = kreal), parameter :: green =  0.55e0
      real(kind = kreal), parameter :: yellow = 0.775e0
      real(kind = kreal), parameter :: red =    one
      real(kind = kreal), parameter :: forty =  four*ten
!
!
      if (rnorm .lt. purple ) then
        r = half
        g = zero
        b = one
      else if (rnorm .ge. purple .and. rnorm.lt.blue) then
        r = half - five*rnorm
        g = zero
        b = one
      else if (rnorm .ge. blue .and. rnorm.lt.ocean) then
        r = zero
        g = forty*(rnorm-blue) / dnine
        b = one
      else if (rnorm .ge. ocean .and. rnorm.lt.green) then
        r = zero
        g = one
        b = one - forty*(rnorm-ocean) / dnine
      else if (rnorm .ge. green .and. rnorm.lt.yellow) then
        r = forty*(rnorm-green) / dnine
        g = one
        b = zero
      else if (rnorm .ge. yellow .and. rnorm.lt. red) then
        r = one
        g = one - forty*(rnorm-yellow) / dnine
        b = zero
      else if (rnorm .ge. red ) then
        r = one
        g = zero
        b = zero
      end if
!
      end subroutine s_colormap_rainbow
!
! ----------------------------------------------------------------------
!
      end module colormap_rainbow
