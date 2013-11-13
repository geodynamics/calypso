!>@file   m_boundary_params_sph_MHD.f90
!!@brief  module m_boundary_params_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Structure for basic boundary conditions for spherical dynamo
!!
!!
!!@verbatim
!!      subroutine set_radial_range_by_BC(iflag_icb_bc, sph_bc)
!!        type(sph_boundary_type), intent(inout) :: bc_param
!!@endverbatim
!!
!!@n @param jmax    number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param nri     number of radial grid points
!!@n @param radius  radius
!
      module m_boundary_params_sph_MHD
!
      use m_precision
      use t_boundary_params_sph_MHD
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
!
!>      Structure for basic velocity boundary condition parameters
      type(sph_boundary_type), save :: sph_bc_U
!>      Structure for basic magnetic boundary condition parameters
      type(sph_boundary_type), save :: sph_bc_B
!>      Structure for basic thermal boundary condition parameters
      type(sph_boundary_type), save :: sph_bc_T
!>      Structure for basic compositional boundary condition parameters
      type(sph_boundary_type), save :: sph_bc_C
!
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
! -----------------------------------------------------------------------
!
      subroutine set_radial_range_by_BC(sph_bc)
!
      use m_spheric_parameter
!
      type(sph_boundary_type), intent(inout) :: sph_bc
!
!
      if      (sph_bc%iflag_icb .eq. iflag_sph_fill_center              &
     &    .or. sph_bc%iflag_icb .eq. iflag_sph_fix_center) then
        sph_bc%kr_in = nlayer_2_center
      else
        sph_bc%kr_in = nlayer_ICB
      end if
      sph_bc%kr_out =  nlayer_CMB
!
      sph_bc%r_ICB(0) = radius_1d_rj_r(sph_bc%kr_in)
      sph_bc%r_ICB(1) = ar_1d_rj(sph_bc%kr_in,1)
      sph_bc%r_ICB(2) = ar_1d_rj(sph_bc%kr_in,2)
      sph_bc%r_CMB(0) = radius_1d_rj_r(sph_bc%kr_out)
      sph_bc%r_CMB(1) = ar_1d_rj(sph_bc%kr_out,1)
      sph_bc%r_CMB(2) = ar_1d_rj(sph_bc%kr_out,2)
!
      end subroutine set_radial_range_by_BC
!
! -----------------------------------------------------------------------
!
      end module m_boundary_params_sph_MHD
