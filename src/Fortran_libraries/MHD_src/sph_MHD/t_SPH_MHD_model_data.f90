!>@file   t_SPH_MHD_model_data.f90
!!@brief  module t_SPH_MHD_model_data
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!!
!!@verbatim
!!@endverbatim
!
      module t_SPH_MHD_model_data
!
      use m_precision
      use t_control_parameter
      use t_poloidal_rotation
      use t_radial_reference_field
      use t_boundary_data_sph_MHD
      use t_bc_data_list
      use t_sph_boundary_input_data
!
      implicit  none
!
!
!
!>      Parameters for spectr dynamo model
      type SPH_MHD_model_data
        type(MHD_evolution_param) :: MHD_prop
!
!>        Structure for rotatin vector
        type(sph_rotation) :: omega_sph
!
!>        Structure of reference temperature
        type(radial_reference_field) :: refs
!
!>        Structure of boundary condition data
        type(sph_MHD_boundary_data) :: sph_MHD_bc
!>        Structure for boundary condition lists for MHD
        type(MHD_BC_lists) :: MHD_BC
!>         Structures for boundary conditions
        type(boundary_spectra) :: bc_IO
      end type SPH_MHD_model_data
!
      end module t_SPH_MHD_model_data
