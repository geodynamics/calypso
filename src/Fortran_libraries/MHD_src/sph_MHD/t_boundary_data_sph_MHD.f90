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
      use t_boundary_sph_spectr
      use t_coef_sph_velocity_BCs
!
      use t_coef_fdm2_centre
      use t_coef_fdm4_vpol_centre
      use t_spheric_parameter
      use t_control_parameter
      use t_time_data
!
      implicit none
!
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
!>        Structure for boundary velocity field spectr
        type(sph_vector_boundary_data) :: bcs_U
!>        Structure for boundary magnetic field spectr
        type(sph_vector_boundary_data) :: bcs_B
!>        Structure for boundary temperature spectr
        type(sph_scalar_boundary_data) :: bcs_T
!>        Structure for boundary composition spectr
        type(sph_scalar_boundary_data) :: bcs_C
!
!>        Structure for Additional velocity boundary condition matrices
        type(velocity_boundary_FDMs) :: bc_fdms_U
!>        Structure for FDM matrix of center
        type(fdm2_center_mat) :: fdm2_center
      end type sph_MHD_boundary_data
!
! ----------------------------------------------------------------------
!
!      contains
!
! ----------------------------------------------------------------------
!
      end module t_boundary_data_sph_MHD
