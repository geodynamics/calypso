!>@file   t_ctl_data_MHD_model.f90
!!@brief  module t_ctl_data_MHD_model
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine dealloc_sph_mhd_model(model_ctl)
!!        type(mhd_model_control), intent(inout) :: model_ctl
!!@endverbatim
!
      module t_ctl_data_MHD_model
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_fields
      use t_ctl_data_mhd_evolution
      use t_ctl_data_mhd_evo_area
      use t_ctl_data_node_boundary
      use t_ctl_data_surf_boundary
      use t_ctl_data_mhd_normalize
      use t_ctl_data_mhd_forces
      use t_ctl_data_coriolis_force
      use t_ctl_data_gravity
      use t_ctl_data_mhd_magne
      use t_ctl_data_magnetic_scale
      use t_ctl_data_temp_model
      use t_ctl_data_dimless_numbers
!
      use skip_comment_f
!
      implicit none
!
      type mhd_model_control
!>        Block name
        character(len=kchara) :: block_name = 'model'
!
!>        Structure for field information control
        type(field_control) :: fld_ctl
!
!>        Structure for evolution fields control
        type(mhd_evolution_control) :: evo_ctl
!>        Structure for domain area controls
        type(mhd_evo_area_control) :: earea_ctl
!
!>        Structure for nodal boundary conditions
        type(node_bc_control) :: nbc_ctl
!>        Structure for surface boundary conditions
        type(surf_bc_control) :: sbc_ctl
!
!>        Structure for list of dimensionless numbers
        type(dimless_control) :: dless_ctl
!>        Structure for coefficients of governing equations
        type(equations_control) :: eqs_ctl
!
!>        Structure for force list
        type(forces_control) :: frc_ctl
!>        Structure for gravity definistion
        type(gravity_control) :: g_ctl
!>        Structure for Coriolis force
        type(coriolis_control) :: cor_ctl
!>        Structure for external magnetic field
        type(magneto_convection_control) :: mcv_ctl
!>        Structure for magnetic field scaling
        type(magnetic_field_scale_control) :: bscale_ctl
!
!>        Structures for reference tempearature
        type(reference_temperature_ctl) :: reft_ctl
!>        Structures for reference composition
        type(reference_temperature_ctl) :: refc_ctl
!
        integer (kind=kint) :: i_model = 0
      end type mhd_model_control
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_mhd_model(model_ctl)
!
      type(mhd_model_control), intent(inout) :: model_ctl
!
!
      call dealloc_phys_control(model_ctl%fld_ctl)
      call dealloc_t_evo_name_ctl(model_ctl%evo_ctl)
      call dealloc_ele_area_grp_ctl(model_ctl%earea_ctl)
!
      call dealloc_bc_4_node_ctl(model_ctl%nbc_ctl)
      call dealloc_bc_4_surf_ctl(model_ctl%sbc_ctl)
!
      call dealloc_dimless_ctl(model_ctl%dless_ctl)
      call dealloc_coef_term_ctl(model_ctl%eqs_ctl)
      call dealloc_name_force_ctl(model_ctl%frc_ctl)
      call dealloc_gravity_ctl(model_ctl%g_ctl)
      call dealloc_coriolis_ctl(model_ctl%cor_ctl)
      call dealloc_magneto_ctl(model_ctl%mcv_ctl)
      call dealloc_magnetic_scale_ctl(model_ctl%bscale_ctl)
!
      model_ctl%i_model = 0
!
      end subroutine dealloc_sph_mhd_model
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_MHD_model
