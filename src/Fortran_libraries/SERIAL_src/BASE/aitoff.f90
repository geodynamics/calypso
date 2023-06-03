!>@file   aitoff.f90
!!@brief  module aitoff
!!
!!@date  Programmed by H.Matsui in March, 2009
!
!>@brief  Program for Aitoff projection
!!
!!@verbatim
!!      subroutine s_aitoff(sin_t, cos_t, phi, xg, yg)
!!        real(kind = kreal), intent(in) :: sin_t, cos_t, phi
!!        real(kind = kreal), intent(inout) :: xg, yg
!!      subroutine reverse_aitoff(xg, yg, theta, phi)
!!        real(kind = kreal), intent(in) :: xg, yg
!!        real(kind = kreal), intent(inout) :: theta, phi
!!*************************************************
!!          map projection using the Hammer-Aitoff equal-area projection
!!*
!!*   make grid data for surface mapping
!!*
!!*************************************************
!!*
!!*  sin_t, cos_t : theta of spherical coordinate (rad)
!!*  phi : phi of spherical coordinate  (rad)
!!*
!!*************************************************
!
      module aitoff
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
       subroutine s_aitoff(sin_t, cos_t, phi, xg, yg)
!*
      real(kind = kreal), intent(in) :: sin_t, cos_t, phi
      real(kind = kreal), intent(inout) :: xg, yg
!
      real(kind = kreal) :: xl2, den
!*
!
      xl2 = half * phi
      den = sqrt( one + sin_t*sin(xl2) )
      xg = -real( two * sin_t * cos(xl2) / den)
      yg =  real( cos_t / den)
!*
      end subroutine s_aitoff
!
! ----------------------------------------------------------------------
!
      subroutine reverse_aitoff(xg, yg, theta, phi)
!*
      real(kind = kreal), intent(in) :: xg, yg
      real(kind = kreal), intent(inout) :: theta, phi
!
      real(kind = kreal) :: A, cosp, cost, sint, pi
!*
!
      pi = two*two*atan(one)
      theta = -one
      phi =    zero
!
      A = one - half*half * xg*xg - yg*yg
      if(A .le. zero) return
!
      cost = yg * sqrt(A + one)
      sint = sqrt(one - yg*yg * (A + one))
!
      if(sint .eq. zero) then
        if(yg .gt. zero) then
          theta = zero
          phi =   zero
        else
          theta = pi
          phi =   zero
        end if
      end if
      if(cost .lt. -one .or. cost .gt. one) return
      if(sint .lt. -one .or. sint .gt. one) return
!
      cosp = A / sint
      if(cosp .lt. -one) cosp = -one
      if(cosp .gt.  one) cosp = one
!
      theta = acos(cost)
      if(xg .le. zero) then
        phi = two * acos(-cosp) - pi
      else
        phi = pi - two * acos(-cosp)
      end if
!
      end subroutine reverse_aitoff
!
! ----------------------------------------------------------------------
!
      end module aitoff
