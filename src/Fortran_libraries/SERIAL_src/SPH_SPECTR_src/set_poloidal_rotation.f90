!>@file   set_poloidal_rotation.f90
!!@brief  module set_poloidal_rotation
!!
!!@author H. Matsui
!!@date Programmed on June., 1994
!!@n    Modified on Apr., 2009
!!@n    Modified on Apr., 2012
!
!>@brief  Set Rotation data for Coriolis term
!
!!@verbatim
!!      subroutine set_rot_earth_4_sph(rotate)
!!***********************************************************************
!!*
!!*     rot_e(k,j) : rotation of earth  (output)
!!*     rot_e(k,j) : d \Omega / dr
!!*     rot_e(k,j) : d^2 \Omega / dr^2
!!*
!!*                       1
!!*         rot_e(k,j) = --- r^2
!!*                       2
!!*
!!*                     dom(k,0)
!!*       drot_e(k,j) = ---------
!!*                        dr
!!*                   = r(k)
!!*
!!*                      dom(k,0)
!!*       d2rot_e(k,j) = ---------
!!*                         dr
!!*                    = 1.0
!!*
!!***********************************************************************
!!@endverbatim
!!
!!@n @param rotate  rotation vector of system
!
      module set_poloidal_rotation
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
      use m_spheric_parameter
!
      implicit none
!
      private :: set_3dir_rot_earth_4_sph, set_rotatio_spectr
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_rot_earth_4_sph(rotate)
!
      use m_sph_spectr_data
!
      real(kind = kreal), intent(in) :: rotate(3)
!
!
      call set_3dir_rot_earth_4_sph(nidx_rj(1), radius_1d_rj_r,         &
     &    rotate, omega_rj)
      call set_3dir_rot_earth_4_sph(nidx_rlm(1), radius_1d_rlm_r,       &
     &    rotate, omega_rlm)
!
      end subroutine set_rot_earth_4_sph
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_3dir_rot_earth_4_sph(nri, r, rotate, omega)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rotate(3)
      real(kind = kreal), intent(in) :: r(nri)
!
      real(kind = kreal), intent(inout) :: omega(nri,0:2,3)
!
!
      call set_rotatio_spectr(nri, r, rotate(2),                        &
      &         omega(1,0,1), omega(1,1,1), omega(1,2,1))
      call set_rotatio_spectr(nri, r, rotate(3),                        &
      &         omega(1,0,2), omega(1,1,2), omega(1,2,2))
      call set_rotatio_spectr(nri, r, rotate(1),                        &
      &         omega(1,0,3), omega(1,1,3), omega(1,2,3))
!*
      end subroutine set_3dir_rot_earth_4_sph
!
!  -------------------------------------------------------------------
!
      subroutine set_rotatio_spectr(nri, r, rotate,                     &
      &         rot_e, drot_e, d2rot_e)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rotate
      real(kind = kreal), intent(in) :: r(nri)
!
      real(kind = kreal), intent(inout) :: rot_e(nri)
      real(kind = kreal), intent(inout) :: drot_e(nri)
      real(kind = kreal), intent(inout) :: d2rot_e(nri)
!
      integer(kind = kint) :: k
!
!
      do k = 1, nri
        rot_e(k) = half * rotate * r(k)**2
        drot_e(k) =       rotate * r(k)
        d2rot_e(k) =      rotate
      end do
!*
      end subroutine set_rotatio_spectr
!
!  -------------------------------------------------------------------
!
      end module set_poloidal_rotation
