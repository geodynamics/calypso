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
!!      subroutine set_MHD_evolved_boundaries                           &
!!     &         (time_d, sph, MHD_prop, sph_MHD_bc)
!!      subroutine set_cv_evolved_boundaries                            &
!!     &         (time_d, sph, MHD_prop, sph_MHD_bc)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) :: sph
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
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
      use t_coef_fdm2_MHD_boundaries
      use t_coef_fdm4_MHD_boundaries
      use t_time_data
      use t_spheric_parameter
      use t_control_parameter
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
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_MHD_evolved_boundaries                             &
     &         (time_d, sph, MHD_prop, sph_MHD_bc)
!
      use set_evoluved_boundaries
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
!
      call set_cv_evolved_boundaries(time_d, sph, MHD_prop, sph_MHD_bc)
!
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call set_evo_vector_boundaries(time_d%time, sph%sph_rj,         &
     &      sph_MHD_bc%sph_bc_B, sph_MHD_bc%bcs_B)
      end if
!
      end subroutine set_MHD_evolved_boundaries
!
! ----------------------------------------------------------------------
!
      subroutine set_cv_evolved_boundaries                              &
     &         (time_d, sph, MHD_prop, sph_MHD_bc)
!
      use set_evoluved_boundaries
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call set_evo_vector_boundaries(time_d%time, sph%sph_rj,         &
     &      sph_MHD_bc%sph_bc_U, sph_MHD_bc%bcs_U)
      end if
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        call set_evo_scalar_boundaries(time_d%time, sph%sph_rj,         &
     &      sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T)
      end if
      if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        call set_evo_scalar_boundaries(time_d%time, sph%sph_rj,         &
     &      sph_MHD_bc%sph_bc_C, sph_MHD_bc%bcs_C)
      end if
!
      end subroutine set_cv_evolved_boundaries
!
! ----------------------------------------------------------------------
!
      end module t_boundary_data_sph_MHD
