!>@file   m_control_params_sph_MHD.f90
!!@brief  module m_control_params_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Field and spectr data
!!
!!
!!@verbatim
!!      subroutine allocate_vsp_bc_array(jmax)
!!      subroutine allocate_temp_bc_array(jmax)
!!      subroutine allocate_dscalar_bc_array(jmax)
!!
!!      subroutine deallocate_vsp_bc_array
!!      subroutine deallocate_temp_bc_array
!!      subroutine deallocate_dscalar_bc_array
!!@endverbatim
!!
!!@n @param jmax  number of modes for spherical harmonics @f$L*(L+2)@f$
!
      module m_control_params_sph_MHD
!
      use m_precision
!
      implicit none
!
!
!>      integer flag for fixed velocity boundary at inner core
      integer(kind = kint), parameter :: iflag_fixed_velo = 0
!>      integer flag for free-slip boundary at inner core
      integer(kind = kint), parameter :: iflag_free_slip =  1
!>      integer flag for rotatable inner core
      integer(kind = kint), parameter :: iflag_rotatable_ic = 10
!
!>      integer flag for insulated magnetic boundary
      integer(kind = kint), parameter :: iflag_sph_insulator =   0
!>      integer flag for pseudo vacuum magnetic boundary
      integer(kind = kint), parameter :: iflag_radial_magne =   11
!
!>      Fixed poloidal velocity spectrum for ICB
      real(kind= kreal), allocatable :: vp_ICB_bc(:)
!>      Fixed toroidal velocity spectrum for ICB
      real(kind= kreal), allocatable :: vt_ICB_bc(:)
!>      Fixed poloidal velocity spectrum for CMB
      real(kind= kreal), allocatable :: vp_CMB_bc(:)
!>      Fixed toroidal velocity spectrum for CMB
      real(kind= kreal), allocatable :: vt_CMB_bc(:)
!
      integer(kind = kint) :: iflag_sph_coriolis_file = 0
!
!>      Number of grid points in zonal direction for dynamo benchmark
      integer(kind = kint) :: mphi_mid_eq = -1
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_vsp_bc_array(jmax)
!
      integer(kind= kint), intent(in) :: jmax
!
      allocate(vp_ICB_bc(jmax))
      allocate(vt_ICB_bc(jmax))
      allocate(vp_CMB_bc(jmax))
      allocate(vt_CMB_bc(jmax))
      vp_ICB_bc = 0.0d0
      vt_ICB_bc = 0.0d0
      vp_CMB_bc = 0.0d0
      vt_CMB_bc = 0.0d0
!
      end subroutine allocate_vsp_bc_array
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_vsp_bc_array
!
      deallocate(vp_ICB_bc, vt_ICB_bc)
      deallocate(vp_CMB_bc, vt_CMB_bc)
!
      end subroutine deallocate_vsp_bc_array
!
! -----------------------------------------------------------------------
!
      end module m_control_params_sph_MHD
