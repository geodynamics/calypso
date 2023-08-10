!>@file   m_sph_node_group_types.f90
!!@brief  module m_sph_node_group_types
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui in Sep. 2005
!
!> @brief set surface boundary condition flags from conterol input
!!
!!@verbatim
!!      subroutine set_bc_group_types_scalar(bc_type_ctl, ibc_type)
!!      subroutine set_bc_group_types_rotation(bc_type_ctl, ibc_type)
!!      subroutine set_bc_group_types_sph_center(bc_type_ctl, ibc_type)
!!      subroutine set_bc_group_types_sph_velo(bc_type_ctl, ibc_type)
!!      subroutine set_bc_group_types_sph_magne(bc_type_ctl, ibc_type)
!!      subroutine set_bc_group_types_fluxes(bc_type_ctl, ibc_type)
!!        character (len=kchara), intent(in) :: bc_type_ctl
!!        integer(kind = kint), intent(inout) :: ibc_type
!!
!!      subroutine set_label_sph_thermal_bc(array_c)
!!      subroutine set_label_sph_momentum_bc(array_c)
!!      subroutine set_label_sph_induction_bc(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
!!
!!      subroutine add_bc_group_types_scalar(array_c)
!!      subroutine add_bc_group_types_vector(array_c)
!!      subroutine add_bc_group_types_rotation(array_c)
!!      subroutine add_bc_group_types_sph_center(array_c)
!!      subroutine add_bc_group_types_sph_velo(array_c)
!!      subroutine add_bc_group_types_sph_magne(array_c)
!!      subroutine add_bc_group_types_fluxes(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
!!@endverbatim
!
      module m_sph_node_group_types
!
      use m_precision
      use m_boundary_condition_IDs
      use skip_comment_f
!
      implicit none
!
!>      control name for fixed field by control
      character(len = kchara), parameter, private                       &
     &                           :: fixed_bc =     'fixed'
!>      control name for fixed field by control
      character(len = kchara), parameter, private                       &
     &                           :: fixed_ctl_bc = 'fixed_ctl'
!>      control name for fixed field by external file
      character(len = kchara), parameter, private                       &
     &                           :: fixed_file =    'file'
!>      control name for fixed field by external file
      character(len = kchara), parameter, private                       &
     &                           :: fixed_file_bc = 'fixed_file'
!>      control name for evolved field by external file
      character(len = kchara), parameter, private                       &
     &                           :: evo_field_file_bc = 'evolved_field'
!>      control name for evolved flux by external file
      character(len = kchara), parameter, private                       &
     &                           :: evo_flux_file_bc =  'evolved_flux'
!
!>      control name for fixed rotation around x-axis boundary
      character(len = kchara), parameter, private:: fix_rot_x = 'rot_x'
!>      control name for fixed rotation around y-axis boundary
      character(len = kchara), parameter, private:: fix_rot_y = 'rot_y'
!>      control name for fixed rotation around z-axis boundary
      character(len = kchara), parameter, private:: fix_rot_z = 'rot_z'
!
!>      control name for free slip boundary for spherical shell
      character(len = kchara), parameter, private                       &
     &                :: free_slip_sph = 'free_slip_sph'
!>      control name for non-slip boundary for spherical shell
      character(len = kchara), parameter, private                       &
     &                :: non_slip_sph = 'non_slip_sph'
!>      control name for rotetable inner core for spherical shell
      character(len = kchara), parameter, private                       &
     &                :: rot_inner_core = 'rot_inner_core'
!
!>      control name for insulated for spherical shell
      character(len = kchara), parameter, private                       &
     &                :: insulator_sph =     'insulator'
!>      control name for psuedo vacuum for spherical shell
      character(len = kchara), parameter, private                       &
     &                :: pseudo_vacuum_sph = 'pseudo_vacuum'
!
!>      control name to filling to center
      character(len = kchara), parameter, private                       &
     &               :: fill_sph_center = 'sph_to_center'
!>      control name to filling to center
      character(len = kchara), parameter, private                       &
     &               :: fix_sph_center = 'fix_at_center'
!
!>      control name for fixed flux by control
      character(len = kchara), parameter, private                       &
     &               :: flux_bc = 'fixed_flux'
!>      control name for fixed flux by external file
      character(len = kchara), parameter, private                       &
     &               :: flux_file_bc = 'fixed_flux_file'
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_scalar(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if     (cmp_no_case(bc_type_ctl, fixed_bc)                        &
     &   .or. cmp_no_case(bc_type_ctl, fixed_ctl_bc)) then
        ibc_type =  iflag_bc_fix_s
      else if(cmp_no_case(bc_type_ctl, fixed_file)                      &
     &   .or. cmp_no_case(bc_type_ctl, fixed_file_bc)) then
        ibc_type =  iflag_bc_file_s
      else if (cmp_no_case(bc_type_ctl, evo_field_file_bc)) then
        ibc_type = iflag_bc_evo_field
      end if
!
      end subroutine set_bc_group_types_scalar
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_vector(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if (cmp_no_case(bc_type_ctl, fixed_file)                          &
     &    .or. cmp_no_case(bc_type_ctl, fixed_file_bc)) then
        ibc_type = iflag_bc_file_s
      else if (cmp_no_case(bc_type_ctl, evo_field_file_bc)) then
        ibc_type = iflag_bc_evo_field
      end if
!
      end subroutine set_bc_group_types_vector
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_rotation(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      (cmp_no_case(bc_type_ctl, fix_rot_x)) then
        ibc_type = iflag_bc_rot + 1
      else if (cmp_no_case(bc_type_ctl, fix_rot_y)) then
        ibc_type = iflag_bc_rot + 2
      else if (cmp_no_case(bc_type_ctl, fix_rot_z)) then
        ibc_type = iflag_bc_rot + 3
      end if
!
      end subroutine set_bc_group_types_rotation
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_sph_center(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      (cmp_no_case(bc_type_ctl, fill_sph_center)) then
        ibc_type = iflag_sph_2_center
      else if (cmp_no_case(bc_type_ctl, fix_sph_center)) then
        ibc_type = iflag_sph_clip_center
      end if
!
      end subroutine set_bc_group_types_sph_center
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_sph_velo(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      (cmp_no_case(bc_type_ctl, free_slip_sph)) then
        ibc_type = iflag_free_sph
      else if (cmp_no_case(bc_type_ctl, non_slip_sph)) then
        ibc_type = iflag_non_slip_sph
      else if (cmp_no_case(bc_type_ctl, rot_inner_core)) then
        ibc_type = iflag_rotatable_icore
      end if
!
      end subroutine set_bc_group_types_sph_velo
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_sph_magne(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      (cmp_no_case(bc_type_ctl, insulator_sph)) then
        ibc_type = iflag_insulator
      else if (cmp_no_case(bc_type_ctl, pseudo_vacuum_sph)) then
        ibc_type = iflag_pseudo_vacuum
      end if
!
      end subroutine set_bc_group_types_sph_magne
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_fluxes(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      (cmp_no_case(bc_type_ctl, flux_bc)) then
        ibc_type = iflag_bc_fix_flux
      else if (cmp_no_case(bc_type_ctl, flux_file_bc)) then
        ibc_type = iflag_bc_file_flux
      else if (cmp_no_case(bc_type_ctl, evo_flux_file_bc)) then
        ibc_type = iflag_bc_evo_flux
      end if
!
      end subroutine set_bc_group_types_fluxes
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_label_sph_thermal_bc(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call add_bc_group_types_scalar(array_c)
      call add_bc_group_types_sph_center(array_c)
      call add_bc_group_types_fluxes(array_c)
!
      end subroutine set_label_sph_thermal_bc
!
!-----------------------------------------------------------------------
!
      subroutine set_label_sph_momentum_bc(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call add_bc_group_types_sph_velo(array_c)
      call add_bc_group_types_sph_center(array_c)
      call add_bc_group_types_rotation(array_c)
!
      end subroutine set_label_sph_momentum_bc
!
!-----------------------------------------------------------------------
!
      subroutine set_label_sph_induction_bc(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call add_bc_group_types_sph_magne(array_c)
      call add_bc_group_types_sph_center(array_c)
!
      end subroutine set_label_sph_induction_bc
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_scalar(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(fixed_bc,          array_c)
      call append_c_to_ctl_array(fixed_file,        array_c)
      call append_c_to_ctl_array(evo_field_file_bc, array_c)
!
      end subroutine add_bc_group_types_scalar
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_vector(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(fixed_file,        array_c)
      call append_c_to_ctl_array(evo_field_file_bc, array_c)
!
      end subroutine add_bc_group_types_vector
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_rotation(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(fix_rot_x, array_c)
      call append_c_to_ctl_array(fix_rot_y, array_c)
      call append_c_to_ctl_array(fix_rot_z, array_c)
!
      end subroutine add_bc_group_types_rotation
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_sph_center(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(fill_sph_center, array_c)
      call append_c_to_ctl_array(fix_sph_center, array_c)
!
      end subroutine add_bc_group_types_sph_center
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_sph_velo(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(non_slip_sph,   array_c)
      call append_c_to_ctl_array(free_slip_sph,  array_c)
      call append_c_to_ctl_array(rot_inner_core, array_c)
!
      end subroutine add_bc_group_types_sph_velo
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_sph_magne(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(insulator_sph,     array_c)
      call append_c_to_ctl_array(pseudo_vacuum_sph, array_c)
!
      end subroutine add_bc_group_types_sph_magne
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_fluxes(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(flux_bc,          array_c)
      call append_c_to_ctl_array(flux_file_bc,     array_c)
      call append_c_to_ctl_array(evo_flux_file_bc, array_c)
!
      end subroutine add_bc_group_types_fluxes
!
! ----------------------------------------------------------------------
!
      end module m_sph_node_group_types
