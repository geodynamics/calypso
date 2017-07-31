!>@file   t_poloidal_rotation.f90
!!@brief  module t_poloidal_rotation
!!
!!@author H. Matsui
!!@date Programmed on June., 1994
!!@n    Modified on Apr., 2009
!!@n    Modified on Apr., 2012
!
!>@brief  Set Rotation data for Coriolis term
!
!!@verbatim
!!      subroutine set_rot_earth_4_sph                                  &
!!     &         (sph_rlm, sph_rj, fl_prop, omega_sph)
!!      subroutine set_3dir_rot_earth_sph_rj                            &
!!     &         (sph_rj, fl_prop, omega_sph)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_rotation), intent(inout) :: omega_sph
!!
!!      subroutine dealloc_rot_rlm_data
!!      subroutine dealloc_rot_rj_data
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
      module t_poloidal_rotation
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
!
      implicit none
!
      type sph_rotation
!>        Number of radial points for rotatin vector
        integer(kind = kint) :: nri_w_rlm
!>        Number of radial points for rotatin vector
        integer(kind = kint) :: nri_w_rj
!>        rotation spectr in @f$ f(r,l,m) @f$
!!@verbatim
!!        omega(kr,0) ... Omaga_z
!!        omega(kr,1) ... d Omaga_z / dr
!!        omega(kr,2) ... d^2 Omaga_z / dr^2
!!@endverbatim
          real(kind = kreal), allocatable :: ws_rlm(:,:)
!
!>        rotation spectr in @f$ f(r,j) @f$
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
          real(kind = kreal), allocatable :: ws_rj(:,:,:)
      end type sph_rotation
!
!
      private :: alloc_rot_rlm_data, alloc_rot_rj_data
      private :: set_3dir_rot_earth_4_sph, set_rotation_spectr
      private :: set_zdir_rot_earth_4_sph
!
!  -------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine set_rot_earth_4_sph                                    &
     &         (sph_rlm, sph_rj, fl_prop, omega_sph)
!
      use t_spheric_rlm_data
      use t_spheric_rj_data
      use t_physical_property
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
!
      type(sph_rotation), intent(inout) :: omega_sph
!
!
      call alloc_rot_rlm_data(sph_rlm%nidx_rlm(1), omega_sph)
      call alloc_rot_rj_data(sph_rj%nidx_rj(1), omega_sph)
!
      call set_3dir_rot_earth_4_sph                                     &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                     &
     &    fl_prop%sys_rot, omega_sph%ws_rj)
      call set_zdir_rot_earth_4_sph                                     &
     &   (sph_rlm%nidx_rlm(1), sph_rlm%radius_1d_rlm_r,                 &
     &    one, omega_sph%ws_rlm)
!
      end subroutine set_rot_earth_4_sph
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_3dir_rot_earth_sph_rj                              &
     &         (sph_rj, fl_prop, omega_sph)
!
      use t_spheric_rj_data
      use t_physical_property
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
!
      type(sph_rotation), intent(inout) :: omega_sph
!
!
      call alloc_rot_rj_data(sph_rj%nidx_rj(1), omega_sph)
      call set_3dir_rot_earth_4_sph                                     &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                     &
     &    fl_prop%sys_rot, omega_sph%ws_rj)
!
      end subroutine set_3dir_rot_earth_sph_rj
!
!  -------------------------------------------------------------------
!
      subroutine dealloc_rot_rlm_data(omega_sph)
!
      type(sph_rotation), intent(inout) :: omega_sph
!
      deallocate(omega_sph%ws_rlm)
!
      end subroutine dealloc_rot_rlm_data
!
!  --------------------------------------------------------------------
!
      subroutine dealloc_rot_rj_data(omega_sph)
!
      type(sph_rotation), intent(inout) :: omega_sph
!
      deallocate(omega_sph%ws_rj)
!
      end subroutine dealloc_rot_rj_data
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine alloc_rot_rlm_data(nri_rlm, omega_sph)
!
      integer(kind = kint), intent(in) :: nri_rlm
      type(sph_rotation), intent(inout) :: omega_sph
!
!
      omega_sph%nri_w_rlm = nri_rlm
      allocate( omega_sph%ws_rlm(omega_sph%nri_w_rlm,0:2) )
      omega_sph%ws_rlm = 0.0d0
!
      end subroutine alloc_rot_rlm_data
!
!  --------------------------------------------------------------------
!
      subroutine alloc_rot_rj_data(nri_rj, omega_sph)
!
      integer(kind = kint), intent(in) :: nri_rj
      type(sph_rotation), intent(inout) :: omega_sph
!
!
      omega_sph%nri_w_rj = nri_rj
      allocate( omega_sph%ws_rj(omega_sph%nri_w_rj,0:2,3) )
      omega_sph%ws_rj =  0.0d0
!
      end subroutine alloc_rot_rj_data
!
!  --------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_zdir_rot_earth_4_sph(nri, r, rotate, omega)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rotate
      real(kind = kreal), intent(in) :: r(nri)
!
      real(kind = kreal), intent(inout) :: omega(nri,0:2)
!
!
      call set_rotation_spectr(nri, r, rotate,                          &
     &    omega(1,0), omega(1,1), omega(1,2))
!*
      end subroutine set_zdir_rot_earth_4_sph
!
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
      call set_rotation_spectr(nri, r, rotate(2),                       &
     &    omega(1,0,1), omega(1,1,1), omega(1,2,1))
      call set_rotation_spectr(nri, r, rotate(3),                       &
     &    omega(1,0,2), omega(1,1,2), omega(1,2,2))
      call set_rotation_spectr(nri, r, rotate(1),                       &
     &   omega(1,0,3), omega(1,1,3), omega(1,2,3))
!*
      end subroutine set_3dir_rot_earth_4_sph
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_rotation_spectr(nri, r, rotate,                    &
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
      end subroutine set_rotation_spectr
!
!  -------------------------------------------------------------------
!
      end module t_poloidal_rotation
