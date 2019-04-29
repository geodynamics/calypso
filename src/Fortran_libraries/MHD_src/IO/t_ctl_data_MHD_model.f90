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
!!      subroutine read_sph_mhd_model(hd_block, iflag, Dmodel_ctl)
!!      subroutine bcast_sph_mhd_model(Dmodel_ctl)
!!      subroutine dealloc_sph_mhd_model(Dmodel_ctl)
!!        type(mhd_DNS_model_control), intent(inout) :: Dmodel_ctl
!!@endverbatim
!
      module t_ctl_data_MHD_model
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_fields
      use t_ctl_data_mhd_evolution
      use t_ctl_data_node_boundary
      use t_ctl_data_surf_boundary
      use t_ctl_data_mhd_normalize
      use t_ctl_data_mhd_forces
      use t_ctl_data_temp_model
!
      use skip_comment_f
!
      implicit none
!
      type mhd_DNS_model_control
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
!>        Structure for Coriolis force
        type(magneto_convection_control) :: mcv_ctl
!
!>        Structures for reference tempearature
        type(reference_temperature_ctl) :: reft_ctl
!>        Structures for reference composition
        type(reference_temperature_ctl) :: refc_ctl
      end type mhd_DNS_model_control
!
!    label for entry of group
!
!
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
      integer (kind=kint) :: i_phys_values =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_evo =     'time_evolution_ctl'
      character(len=kchara), parameter :: hd_layers_ctl = 'layers_ctl'
      integer (kind=kint) :: i_time_evo =      0
      integer (kind=kint) :: i_layers_ctl =    0
!
      character(len=kchara), parameter                                  &
     &      :: hd_bc_4_node =          'bc_4_node'
      character(len=kchara), parameter                                  &
     &      :: hd_boundary_condition = 'boundary_condition'
      integer (kind=kint) :: i_bc_4_node =     0
!
      character(len=kchara), parameter                                  &
     &      :: hd_dimless_ctl =  'dimensionless_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_term_ctl ='coefficients_ctl'
      integer (kind=kint) :: i_dimless_ctl =   0
      integer (kind=kint) :: i_coef_term_ctl = 0
!
      character(len=kchara), parameter                                  &
     &      :: hd_forces_ctl =   'forces_define'
      character(len=kchara), parameter                                  &
     &      :: hd_gravity_ctl =  'gravity_define'
      character(len=kchara), parameter                                  &
     &      :: hd_coriolis_ctl = 'Coriolis_define'
      character(len=kchara), parameter                                  &
     &      :: hd_magneto_ctl =  'Magneto_convection_def'
      integer (kind=kint) :: i_forces_ctl =    0
      integer (kind=kint) :: i_gravity_ctl =   0
      integer (kind=kint) :: i_coriolis_ctl =  0
      integer (kind=kint) :: i_magneto_ctl =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_temp_def =     'temperature_define'
      character(len=kchara), parameter                                  &
     &      :: hd_comp_def =     'composition_define'
      character(len=kchara), parameter                                  &
     &      :: hd_bc_4_surf =    'bc_4_surface'
      integer (kind=kint) :: i_temp_def =      0
      integer (kind=kint) :: i_comp_def =      0
      integer (kind=kint) :: i_bc_4_surf =     0
!
!      character(len=kchara), parameter :: hd_sgs_ctl = 'SGS_control'
!      integer (kind=kint) :: i_sgs_ctl =       0
!
!
      private :: hd_phys_values, i_phys_values
!
      private :: hd_time_evo, hd_layers_ctl
      private :: i_time_evo,  i_layers_ctl
!
      private :: hd_bc_4_node, hd_boundary_condition, i_bc_4_node
      private :: hd_bc_4_surf, i_bc_4_surf
!
      private :: hd_dimless_ctl, hd_coef_term_ctl
      private :: i_dimless_ctl,  i_coef_term_ctl
!
      private :: hd_forces_ctl, i_forces_ctl
      private :: hd_gravity_ctl, hd_coriolis_ctl, hd_magneto_ctl
      private :: i_gravity_ctl,  i_coriolis_ctl,  i_magneto_ctl
!
      private :: hd_temp_def, i_temp_def
      private :: hd_comp_def, i_comp_def
!      private :: hd_sgs_ctl, i_sgs_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_model(hd_block, iflag, Dmodel_ctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(mhd_DNS_model_control), intent(inout) :: Dmodel_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        iflag = find_control_end_flag(hd_block)
        if(iflag .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (hd_phys_values, i_phys_values, Dmodel_ctl%fld_ctl)
!
        call read_mhd_time_evo_ctl                                      &
     &     (hd_time_evo, i_time_evo, Dmodel_ctl%evo_ctl)
        call read_mhd_layer_ctl                                         &
     &     (hd_layers_ctl, i_layers_ctl, Dmodel_ctl%earea_ctl)
!
        call read_bc_4_node_ctl                                         &
     &     (hd_boundary_condition, i_bc_4_node, Dmodel_ctl%nbc_ctl)
        call read_bc_4_node_ctl                                         &
     &     (hd_bc_4_node, i_bc_4_node, Dmodel_ctl%nbc_ctl)
        call read_bc_4_surf_ctl                                         &
     &     (hd_bc_4_surf, i_bc_4_surf, Dmodel_ctl%sbc_ctl)
!
        call read_forces_ctl                                            &
     &     (hd_forces_ctl, i_forces_ctl, Dmodel_ctl%frc_ctl)
        call read_dimless_ctl                                           &
     &     (hd_dimless_ctl, i_dimless_ctl, Dmodel_ctl%dless_ctl)
        call read_coef_term_ctl                                         &
     &     (hd_coef_term_ctl, i_coef_term_ctl, Dmodel_ctl%eqs_ctl)
!
        call read_gravity_ctl                                           &
     &     (hd_gravity_ctl, i_gravity_ctl, Dmodel_ctl%g_ctl)
        call read_coriolis_ctl                                          &
     &     (hd_coriolis_ctl, i_coriolis_ctl, Dmodel_ctl%cor_ctl)
        call read_magneto_ctl                                           &
     &     (hd_magneto_ctl, i_magneto_ctl, Dmodel_ctl%mcv_ctl)
        call read_reftemp_ctl                                           &
     &     (hd_temp_def, i_temp_def, Dmodel_ctl%reft_ctl)
        call read_reftemp_ctl                                           &
     &     (hd_comp_def, i_comp_def, Dmodel_ctl%refc_ctl)
      end do
!
      end subroutine read_sph_mhd_model
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_model(Dmodel_ctl)
!
      use bcast_4_field_ctl
!
      type(mhd_DNS_model_control), intent(inout) :: Dmodel_ctl
!
!
      call bcast_phys_data_ctl(Dmodel_ctl%fld_ctl)
      call bcast_mhd_time_evo_ctl(Dmodel_ctl%evo_ctl)
      call bcast_mhd_layer_ctl(Dmodel_ctl%earea_ctl)
!
      call bcast_bc_4_node_ctl(Dmodel_ctl%nbc_ctl)
      call bcast_bc_4_surf_ctl(Dmodel_ctl%sbc_ctl)
!
      call bcast_dimless_ctl(Dmodel_ctl%dless_ctl)
      call bcast_coef_term_ctl(Dmodel_ctl%eqs_ctl)
      call bcast_forces_ctl(Dmodel_ctl%frc_ctl)
      call bcast_gravity_ctl(Dmodel_ctl%g_ctl)
      call bcast_coriolis_ctl(Dmodel_ctl%cor_ctl)
      call bcast_magneto_ctl(Dmodel_ctl%mcv_ctl)
      call bcast_ref_scalar_ctl(Dmodel_ctl%reft_ctl)
      call bcast_ref_scalar_ctl(Dmodel_ctl%refc_ctl)
!
      end subroutine bcast_sph_mhd_model
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_mhd_model(Dmodel_ctl)
!
      type(mhd_DNS_model_control), intent(inout) :: Dmodel_ctl
!
!
      call dealloc_phys_control(Dmodel_ctl%fld_ctl)
      call dealloc_t_evo_name_ctl(Dmodel_ctl%evo_ctl)
      call dealloc_ele_area_grp_ctl(Dmodel_ctl%earea_ctl)
!
      call dealloc_bc_4_node_ctl(Dmodel_ctl%nbc_ctl)
      call dealloc_bc_4_surf_ctl(Dmodel_ctl%sbc_ctl)
!
      call dealloc_dimless_ctl(Dmodel_ctl%dless_ctl)
      call dealloc_coef_term_ctl(Dmodel_ctl%eqs_ctl)
      call dealloc_name_force_ctl(Dmodel_ctl%frc_ctl)
      call dealloc_gravity_ctl(Dmodel_ctl%g_ctl)
      call dealloc_coriolis_ctl(Dmodel_ctl%cor_ctl)
      call dealloc_magneto_ctl(Dmodel_ctl%mcv_ctl)
!
      end subroutine dealloc_sph_mhd_model
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_MHD_model
