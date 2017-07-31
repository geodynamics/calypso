!>@file   t_boundary_data_sph_MHD.f90
!!@brief  module t_boundary_data_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Structure for basic boundary conditions for spherical dynamo
!!
!!
!!@verbatim
!!      subroutine alloc_vsp_bc_array(jmax, sph_MHD_bc)
!!      subroutine dealloc_vsp_bc_array(sph_MHD_bc)
!!@endverbatim
!!
!!@n @param jmax    number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param nri     number of radial grid points
!!@n @param radius  radius
!
      module t_boundary_data_sph_MHD
!
      use m_precision
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
      use t_coef_fdm4_MHD_boundaries
!
      implicit none
!
!
!>      Structure for boundary velocity spectr
      type sph_velocity_BC_spectr
!>        Fixed poloidal velocity spectrum for ICB
        real(kind= kreal), allocatable :: vp_ICB_bc(:)
!>        Fixed toroidal velocity spectrum for ICB
        real(kind= kreal), allocatable :: vt_ICB_bc(:)
!>        Fixed poloidal velocity spectrum for CMB
        real(kind= kreal), allocatable :: vp_CMB_bc(:)
!>        Fixed toroidal velocity spectrum for CMB
        real(kind= kreal), allocatable :: vt_CMB_bc(:)
      end type sph_velocity_BC_spectr
!
!>      Structure for boundary conditions
      type sph_MHD_boundary_data
!>        Structure for basic velocity boundary condition parameters
        type(sph_boundary_type) :: sph_bc_U
!>        Structure for basic magnetic boundary condition parameters
        type(sph_boundary_type) :: sph_bc_B
!>        Structure for basic thermal boundary condition parameters
        type(sph_boundary_type) :: sph_bc_T
!>        Structure for basic compositional boundary condition parameters
        type(sph_boundary_type) :: sph_bc_C
!
!>        Structure for boundary velocity spectr
        type(sph_velocity_BC_spectr) :: bc_Uspectr
!
!
!>        Structure for FDM matrix of center
        type(fdm2_center_mat) :: fdm2_center
!>        Structure for FDM matrix of free slip boundary at ICB
        type(fdm2_free_slip) :: fdm2_free_ICB
!>        Structure for FDM matrix of free slip boundary at CMB
        type(fdm2_free_slip) :: fdm2_free_CMB
!
!>        Structure for 4th order FDM matrix of non-slip boundary at ICB
        type(fdm4_ICB_vpol) :: fdm4_noslip_ICB
!>        Structure for 4th order FDM matrix of free slip boundary at ICB
        type(fdm4_ICB_vpol) :: fdm4_free_ICB
!
!>        Structure for 4th order FDM matrix of non-slip boundary at CMB
        type(fdm4_CMB_vpol) :: fdm4_noslip_CMB
!>        Structure for 4th order FDM matrix of free slip boundary at CMB
        type(fdm4_CMB_vpol) :: fdm4_free_CMB
      end type sph_MHD_boundary_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_vsp_bc_array(jmax, bc_Uspectr)
!
      integer(kind= kint), intent(in) :: jmax
      type(sph_velocity_BC_spectr), intent(inout) :: bc_Uspectr
!
      allocate(bc_Uspectr%vp_ICB_bc(jmax))
      allocate(bc_Uspectr%vt_ICB_bc(jmax))
      allocate(bc_Uspectr%vp_CMB_bc(jmax))
      allocate(bc_Uspectr%vt_CMB_bc(jmax))
!
      if(jmax .le. 0) return
      bc_Uspectr%vp_ICB_bc = 0.0d0
      bc_Uspectr%vt_ICB_bc = 0.0d0
      bc_Uspectr%vp_CMB_bc = 0.0d0
      bc_Uspectr%vt_CMB_bc = 0.0d0
!
      end subroutine alloc_vsp_bc_array
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_vsp_bc_array(bc_Uspectr)
!
      type(sph_velocity_BC_spectr), intent(inout) :: bc_Uspectr
!
      deallocate(bc_Uspectr%vp_ICB_bc, bc_Uspectr%vt_ICB_bc)
      deallocate(bc_Uspectr%vp_CMB_bc, bc_Uspectr%vt_CMB_bc)
!
      end subroutine dealloc_vsp_bc_array
!
! -----------------------------------------------------------------------
!
      end module t_boundary_data_sph_MHD
