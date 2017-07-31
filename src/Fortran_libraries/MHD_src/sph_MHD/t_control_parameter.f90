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
!
!>        Structure for manetic property
        type(conductive_property) :: cd_prop
!
!>        Structure for thermal property
        type(scalar_property) :: ht_prop
!>        reference paramter for temperature
        type(reference_scalar_param) :: ref_param_T
!>        Takepiro stratified temperature
        type(takepiro_model_param) :: takepito_T
!
!>        Structure for compositon property
        type(scalar_property) :: cp_prop
!>        reference paramter for composition
        type(reference_scalar_param) :: ref_param_C
!>        Takepiro stratified composition
        type(takepiro_model_param) :: takepito_C
      end type MHD_evolution_param
!
!
      end module t_control_parameter
