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
!>      integer flag for whole sphere model
      integer(kind = kint), parameter :: iflag_sph_fill_center = 41
!>      integer flag for whole sphere model
      integer(kind = kint), parameter :: iflag_sph_fix_center =  42
!
!>      integer flag for fixed velocity boundary
      integer(kind = kint), parameter :: iflag_fixed_field = 0
!>      integer flag for free-slip boundary
      integer(kind = kint), parameter :: iflag_fixed_flux =  1
!
!>      integer flag for insulated magnetic boundary
      integer(kind = kint), parameter :: iflag_sph_insulator =   0
!>      integer flag for pseudo vacuum magnetic boundary
      integer(kind = kint), parameter :: iflag_radial_magne =   11
!
!>      boundary condition flag for velocity at ICB
      integer(kind = kint) :: iflag_icb_velocity = iflag_fixed_velo
!>      boundary condition flag for velocity at CMB
      integer(kind = kint) :: iflag_cmb_velocity = iflag_fixed_velo
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
!>      boundary condition flag for tempeture at ICB
      integer(kind = kint) :: iflag_icb_temp = iflag_fixed_field
!>      boundary condition flag for tempeture at CMB
      integer(kind = kint) :: iflag_cmb_temp = iflag_fixed_field
!
!>      Fixed temperature spectrum for ICB
      real(kind= kreal), allocatable :: temp_ICB_bc(:)
!>      Fixed temperature spectrum for CMB
      real(kind= kreal), allocatable :: temp_CMB_bc(:)
!>      Fixed heat flux spectrum for ICB
      real(kind= kreal), allocatable :: h_flux_ICB_bc(:)
!>      Fixed heat flux spectrum for CMB
      real(kind= kreal), allocatable :: h_flux_CMB_bc(:)
!
!>      boundary condition flag for composition variation at ICB
      integer(kind = kint) :: iflag_icb_composition = iflag_fixed_field
!>      boundary condition flag for composition variation at CMB
      integer(kind = kint) :: iflag_cmb_composition = iflag_fixed_field
!
!>      Fixed composition spectrum for ICB
      real(kind= kreal), allocatable :: composition_ICB_bc(:)
!>      Fixed composition spectrum for CMB
      real(kind= kreal), allocatable :: composition_CMB_bc(:)
!>      Fixed composition flux spectrum for ICB
      real(kind= kreal), allocatable :: c_flux_ICB_bc(:)
!>      Fixed composition flux spectrum for CMB
      real(kind= kreal), allocatable :: c_flux_CMB_bc(:)
!
!
!>      boundary condition flag for magnetic field at ICB
      integer(kind = kint) :: iflag_icb_magne = iflag_sph_insulator
!>      boundary condition flag for magnetic field at CMB
      integer(kind = kint) :: iflag_cmb_magne = iflag_sph_insulator
!
!
      integer(kind = kint) :: iflag_sph_coriolis_file = 0
!
!>      Start radial address of fluid shell for @f$ f(r,j) @f$
      integer(kind = kint) :: kr_rj_fluid_start =   1
!>      End radial address of fluid shell for @f$ f(r,j) @f$
      integer(kind = kint) :: kr_rj_fluid_end =     1
!>      Start radial address to solve heat equation for @f$ f(r,j) @f$
      integer(kind = kint) :: kr_rj_thermal_start = 1
!>      End radial address to solve heat equation for @f$ f(r,j) @f$
      integer(kind = kint) :: kr_rj_thermal_end =   1
!>      Start radial address to solve induction for @f$ f(r,j) @f$
      integer(kind = kint) :: kr_rj_conduct_start = 1
!>      End radial address to solve induction for @f$ f(r,j) @f$
      integer(kind = kint) :: kr_rj_conduct_end =   1
!>      Start radial address to solve comopsition for @f$ f(r,j) @f$
      integer(kind = kint) :: kr_rj_light_start =   1
!>      End radial address to solve comopsition for @f$ f(r,j) @f$
      integer(kind = kint) :: kr_rj_light_end =     1
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
!
      subroutine allocate_temp_bc_array(jmax)
!
      integer(kind= kint), intent(in) :: jmax
!
      allocate(temp_ICB_bc(jmax))
      allocate(temp_CMB_bc(jmax))
      allocate(h_flux_ICB_bc(jmax))
      allocate(h_flux_CMB_bc(jmax))
      temp_ICB_bc = 0.0d0
      temp_CMB_bc = 0.0d0
      h_flux_ICB_bc = 0.0d0
      h_flux_CMB_bc = 0.0d0
!
      end subroutine allocate_temp_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine allocate_dscalar_bc_array(jmax)
!
      integer(kind= kint), intent(in) :: jmax
!
      allocate(composition_ICB_bc(jmax))
      allocate(composition_CMB_bc(jmax))
      allocate(c_flux_ICB_bc(jmax))
      allocate(c_flux_CMB_bc(jmax))
      composition_ICB_bc = 0.0d0
      composition_CMB_bc = 0.0d0
      c_flux_ICB_bc = 0.0d0
      c_flux_CMB_bc = 0.0d0
!
      end subroutine allocate_dscalar_bc_array
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
      subroutine deallocate_temp_bc_array
!
      deallocate(temp_ICB_bc,   temp_CMB_bc)
      deallocate(h_flux_ICB_bc, h_flux_CMB_bc)
!
      end subroutine deallocate_temp_bc_array
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_dscalar_bc_array
!
      deallocate(composition_ICB_bc, composition_CMB_bc)
      deallocate(c_flux_ICB_bc,  c_flux_CMB_bc)
!
      end subroutine deallocate_dscalar_bc_array
!
! -----------------------------------------------------------------------
!
      end module m_control_params_sph_MHD
