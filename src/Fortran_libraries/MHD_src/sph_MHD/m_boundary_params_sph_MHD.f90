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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_radial_range_by_BC(iflag_icb_bc, sph_bc)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: iflag_icb_bc
      type(sph_boundary_type), intent(inout) :: sph_bc
!
!
      if      (iflag_icb_bc .eq. iflag_sph_fill_center                  &
     &    .or. iflag_icb_bc .eq. iflag_sph_fix_center) then
        sph_bc%kr_in = nlayer_2_center
      else
        sph_bc%kr_in = nlayer_ICB
      end if
      sph_bc%kr_out =  nlayer_CMB
!
      end subroutine set_radial_range_by_BC
!
! -----------------------------------------------------------------------
!
      end module m_boundary_params_sph_MHD
