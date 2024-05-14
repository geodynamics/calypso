!>@file   colormap_metal.F90
!!@brief  module colormap_metal
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2024
!
!>@brief  Colormapping for molten metal
!!
!!@verbatim
!!      subroutine s_colormap_metal(rnorm, r, g, b)
!!        real(kind = kreal), intent(in) :: rnorm
!!        real(kind = kreal), intent(inout) ::  r, g, b
!!@endverbatim
!
      module colormap_metal
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
      subroutine s_colormap_metal(rnorm, r, g, b)
!
      real(kind = kreal), intent(in) :: rnorm
      real(kind = kreal), intent(inout) ::  r, g, b
!
      real(kind = kreal), parameter :: c_g1 =  0.6
      real(kind = kreal), parameter :: r_mul = one / c_g1
      real(kind = kreal), parameter :: g_mul = one / (one - c_g1)
!
      real(kind = kreal) :: x
!
!
      x = rnorm
      if (x .lt. zero) then
        r = zero
      else if(r .lt. c_g1) then
        r = x * r_mul
      else
        r = one
      end if
!
      if (x .lt. c_g1) then
        g = zero
      else if(r .lt. one) then
        g = (x - c_g1) * g_mul
      else
        g = one
      end if
!
      b = zero
!
      end subroutine s_colormap_metal
!
! ----------------------------------------------------------------------
!
      end module colormap_metal
