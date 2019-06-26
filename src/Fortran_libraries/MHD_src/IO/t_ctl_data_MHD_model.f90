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
!!      subroutine read_sph_mhd_model                                   &
!!     &         (id_control, hd_block, Dmodel_ctl, c_buf)
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
      use t_read_control_elements
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
!
        integer (kind=kint) :: i_model = 0
      end type mhd_DNS_model_control
!
!    label for entry of group
!
!
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_evo =     'time_evolution_ctl'
      character(len=kchara), parameter :: hd_layers_ctl = 'layers_ctl'
!
      character(len=kchara), parameter                                  &
     &      :: hd_bc_4_node =          'bc_4_node'
      character(len=kchara), parameter                                  &
     &      :: hd_boundary_condition = 'boundary_condition'
!
      character(len=kchara), parameter                                  &
     &      :: hd_dimless_ctl =  'dimensionless_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_term_ctl ='coefficients_ctl'
!
      character(len=kchara), parameter                                  &
     &      :: hd_forces_ctl =   'forces_define'
      character(len=kchara), parameter                                  &
     &      :: hd_gravity_ctl =  'gravity_define'
      character(len=kchara), parameter                                  &
     &      :: hd_coriolis_ctl = 'Coriolis_define'
      character(len=kchara), parameter                                  &
     &      :: hd_magneto_ctl =  'Magneto_convection_def'
!
      character(len=kchara), parameter                                  &
     &      :: hd_temp_def =     'temperature_define'
      character(len=kchara), parameter                                  &
     &      :: hd_comp_def =     'composition_define'
      character(len=kchara), parameter                                  &
     &      :: hd_bc_4_surf =    'bc_4_surface'
!
      private :: hd_phys_values, hd_time_evo, hd_layers_ctl
      private :: hd_bc_4_node, hd_boundary_condition, hd_bc_4_surf
      private :: hd_dimless_ctl, hd_coef_term_ctl
!
      private :: hd_forces_ctl
      private :: hd_gravity_ctl, hd_coriolis_ctl, hd_magneto_ctl
!
      private :: hd_temp_def, hd_comp_def
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_model                                     &
     &         (id_control, hd_block, Dmodel_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_DNS_model_control), intent(inout) :: Dmodel_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(Dmodel_ctl%i_model .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, Dmodel_ctl%fld_ctl, c_buf)
!
        call read_mhd_time_evo_ctl                                      &
     &     (id_control, hd_time_evo, Dmodel_ctl%evo_ctl, c_buf)
        call read_mhd_layer_ctl                                         &
     &     (id_control, hd_layers_ctl, Dmodel_ctl%earea_ctl, c_buf)
!
        call read_bc_4_node_ctl(id_control, hd_boundary_condition,      &
     &      Dmodel_ctl%nbc_ctl, c_buf)
        call read_bc_4_node_ctl(id_control, hd_bc_4_node,               &
     &      Dmodel_ctl%nbc_ctl, c_buf)
        call read_bc_4_surf_ctl                                         &
     &     (id_control, hd_bc_4_surf, Dmodel_ctl%sbc_ctl, c_buf)
!
        call read_forces_ctl                                            &
     &     (id_control, hd_forces_ctl, Dmodel_ctl%frc_ctl, c_buf)
        call read_dimless_ctl                                           &
     &     (id_control, hd_dimless_ctl, Dmodel_ctl%dless_ctl, c_buf)
        call read_coef_term_ctl                                         &
     &     (id_control, hd_coef_term_ctl, Dmodel_ctl%eqs_ctl, c_buf)
!
        call read_gravity_ctl                                           &
     &     (id_control, hd_gravity_ctl, Dmodel_ctl%g_ctl, c_buf)
        call read_coriolis_ctl                                          &
     &     (id_control, hd_coriolis_ctl, Dmodel_ctl%cor_ctl, c_buf)
        call read_magneto_ctl                                           &
     &     (id_control, hd_magneto_ctl, Dmodel_ctl%mcv_ctl, c_buf)
        call read_reftemp_ctl                                           &
     &     (id_control, hd_temp_def, Dmodel_ctl%reft_ctl, c_buf)
        call read_refcomp_ctl                                           &
     &     (id_control, hd_comp_def, Dmodel_ctl%refc_ctl, c_buf)
      end do
      Dmodel_ctl%i_model = 1
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
      call MPI_BCAST(Dmodel_ctl%i_model, 1,                             &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
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
      Dmodel_ctl%i_model = 0
!
      end subroutine dealloc_sph_mhd_model
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_MHD_model
