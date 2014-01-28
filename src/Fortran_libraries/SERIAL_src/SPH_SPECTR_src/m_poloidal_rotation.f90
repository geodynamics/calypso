!>@file   m_poloidal_rotation.f90
!!@brief  module m_poloidal_rotation
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
!!
!!      subroutine deallocate_rot_rlm_data
!!      subroutine deallocate_rot_rj_data
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
      module m_poloidal_rotation
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
!>     rotation spectr in @f$ f(r,l,m) @f$
!!@verbatim
!!        omega(kr,0) ... Omaga_z
!!        omega(kr,1) ... d Omaga_z / dr
!!        omega(kr,2) ... d^2 Omaga_z / dr^2
!!@endverbatim
      real(kind = kreal), allocatable :: omega_rlm(:,:)
!
!>     rotation spectr in @f$ f(r,j) @f$
!!@verbatim
!!        omega(kr,0,1) ... Omaga_x
!!        omega(kr,1,1) ... d Omaga_x / dr
!!        omega(kr,2,1) ... d^2 Omaga_x / dr^2
!!        omega(kr,0,2) ... Omaga_z
!!        omega(kr,1,2) ... d Omaga_z / dr
!!        omega(kr,2,2) ... d^2 Omaga_z / dr^2
!!        omega(kr,0,3) ... Omaga_y
!!        omega(kr,1,3) ... d Omaga_y / dr
!!        omega(kr,2,3) ... d^2 Omaga_y / dr^2
!!@endverbatim
      real(kind = kreal), allocatable :: omega_rj(:,:,:)
!
!
      private :: allocate_rot_rlm_data, allocate_rot_rj_data
      private :: set_3dir_rot_earth_4_sph, set_rotatio_spectr
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine allocate_rot_rlm_data(nri_rlm)
!
      integer(kind = kint), intent(in) :: nri_rlm
!
!
      allocate( omega_rlm(nri_rlm,0:2) )
      omega_rlm = 0.0d0
!
      end subroutine allocate_rot_rlm_data
!
!  --------------------------------------------------------------------
!
      subroutine allocate_rot_rj_data(nri_rj)
!
      integer(kind = kint), intent(in) :: nri_rj
!
!
      allocate( omega_rj(nri_rj,0:2,3) )
      omega_rj =  0.0d0
!
      end subroutine allocate_rot_rj_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine deallocate_rot_rlm_data
!
      deallocate(omega_rlm)
!
      end subroutine deallocate_rot_rlm_data
!
!  --------------------------------------------------------------------
!
      subroutine deallocate_rot_rj_data
!
      deallocate(omega_rj)
!
      end subroutine deallocate_rot_rj_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine set_rot_earth_4_sph(rotate)
!
      use m_sph_spectr_data
!
      real(kind = kreal), intent(in) :: rotate(3)
!
!
      call allocate_rot_rlm_data(nidx_rlm(1))
      call allocate_rot_rj_data(nidx_rj(1))
!
      call set_3dir_rot_earth_4_sph(nidx_rj(1), radius_1d_rj_r, rotate, &
     &    omega_rj)
      call set_rotatio_spectr(nidx_rlm(1), radius_1d_rlm_r, one,        &
     &    omega_rlm(1,0), omega_rlm(1,1), omega_rlm(1,2))
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
      end module m_poloidal_rotation
