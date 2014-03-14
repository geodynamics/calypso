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
!
      end module m_boundary_params_sph_MHD
