!>@file   t_control_parameter.f90
!!@brief  module t_control_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!!@verbatim
!!      subroutine allocate_force_list
!!      subroutine deallocate_force_list
!!@endverbatim
!
      module t_control_parameter
!
      use m_precision
      use t_physical_property
      use t_reference_scalar_param
      use t_coef_parameters_list
      use t_ctl_param_val_density
      use t_ctl_param_val_diffusion
!
      implicit  none
!
!
!>      Structure for evolution parameter for valocity
      type MHD_evolution_param
!>        Time integeration flag
        integer (kind=kint) :: iflag_all_scheme = id_Crank_nicolson
!
!>        Structure for fluid property
        type(fluid_property) :: fl_prop
!>        flag for polytrope
        logical :: flag_ref_density_valiation = .FALSE.
!>        Structure for polytrope
        type(polytrope_parameters) :: polytrope_param
!>        flag for valuable viscosity
        logical :: flag_viscous_variation = .FALSE.
!>        Structure for valuable diffusivity
        type(val_diffuse_parameters) :: val_viscous_param
!
!>        Structure for manetic property
        type(conductive_property) :: cd_prop
!>        flag for valuable magnetic diffusivity
        logical :: flag_mag_diffuse_variation = .FALSE.
!>        Structure for valuable magnetic diffusivity
        type(val_diffuse_parameters) :: val_mag_diffuse_param
!
!>        Structure for thermal property
        type(scalar_property) :: ht_prop
!>        reference paramter for temperature
        type(reference_scalar_param) :: ref_param_T
!>        Takepiro stratified temperature
        type(takepiro_model_param) :: takepito_T
!>        flag for valuable thermal diffusivity
        logical :: flag_term_diffuse_variation = .FALSE.
!>        Structure for valuable thermal diffusivity
        type(val_diffuse_parameters) :: val_thermal_diffuse_param
!
!>        Structure for compositon property
        type(scalar_property) :: cp_prop
!>        reference paramter for composition
        type(reference_scalar_param) :: ref_param_C
!>        Takepiro stratified composition
        type(takepiro_model_param) :: takepito_C
!>        flag for valuable compositional diffusivity
        logical :: flag_comp_diffuse_variation = .FALSE.
!>        Structure for valuable compositional diffusivity
        type(val_diffuse_parameters) :: val_comp_diffuse_param
!
!>        Structure for normalization parameters
        type(coef_parameters_list) :: MHD_coef_list
      end type MHD_evolution_param
!
!
      end module t_control_parameter
