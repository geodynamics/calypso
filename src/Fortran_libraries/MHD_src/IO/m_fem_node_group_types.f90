!>@file   m_fem_node_group_types.f90
!!@brief  module m_fem_node_group_types
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui in Sep. 2005
!
!> @brief set surface boundary condition flags from conterol input
!!
!!@verbatim
!!      subroutine set_bc_group_types_each_dir(bc_type_ctl, ibc_type)
!!      subroutine set_bc_group_types_sgs_scalar(bc_type_ctl, ibc_type)
!!      subroutine set_bc_group_types_sgs_vect(bc_type_ctl, ibc_type)
!!
!!      subroutine set_label_thermal_bc(names)
!!      subroutine set_label_momentum_bc(array_c)
!!      subroutine set_label_induction_bc(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
!!
!!      subroutine set_label_potential_bc(array_c)
!!      subroutine set_label_vector_p_bc(array_c)
!!      subroutine set_label_current_bc(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
!!@endverbatim
!
      module m_fem_node_group_types
!
      use m_precision
      use m_boundary_condition_IDs
      use m_sph_node_group_types
      use skip_comment_f
!
      implicit none
!
!>      control name for fixed field in SGS model
      character(len = kchara), parameter :: fixed_SGS =  'sgs'
!
!>      control name for fixed x-componenet by control
      character(len = kchara), parameter :: fixed_x = 'fix_x'
!>      control name for fixed y-componenet by control
      character(len = kchara), parameter :: fixed_y = 'fix_y'
!>      control name for fixed z-componenet by control
      character(len = kchara), parameter :: fixed_z = 'fix_z'
!>      control name for fixed x-componenet by control
      character(len = kchara), parameter :: fix_ctl_x = 'fix_ctl_x'
!>      control name for fixed y-componenet by control
      character(len = kchara), parameter :: fix_ctl_y = 'fix_ctl_y'
!>      control name for fixed z-componenet by control
      character(len = kchara), parameter :: fix_ctl_z = 'fix_ctl_z'
!
!>      control name for fixed x-componenet by external file
      character(len = kchara), parameter :: bc_file_x = 'file_x'
!>      control name for fixed y-componenet by external file
      character(len = kchara), parameter :: bc_file_y = 'file_y'
!>      control name for fixed z-componenet by external file
      character(len = kchara), parameter :: bc_file_z = 'file_z'
!>      control name for fixed x-componenet by external file
      character(len = kchara), parameter :: fix_file_x = 'fix_file_x'
!>      control name for fixed y-componenet by external file
      character(len = kchara), parameter :: fix_file_y = 'fix_file_y'
!>      control name for fixed z-componenet by external file
      character(len = kchara), parameter :: fix_file_z = 'fix_file_z'
!
!>      control name for fixed x-componenet in SGS model
      character(len = kchara), parameter :: fix_SGS_x = 'sgs_x'
!>      control name for fixed y-componenet in SGS model
      character(len = kchara), parameter :: fix_SGS_y = 'sgs_y'
!>      control name for fixed z-componenet in SGS model
      character(len = kchara), parameter :: fix_SGS_z = 'sgs_z'
!
!
!>      control name for equator
      character(len = kchara), parameter, private                       &
     &               :: equator_bc = 'equator'
!
!>      control name for no radial flow
      character(len = kchara), parameter, private                       &
     &               :: no_radial_comp_bc = 'vr_0'
!>      control name for special condition
      character(len = kchara), parameter, private                       &
     &               :: special_bc = 'special'
!
!>      control name for equator
      character(len = kchara), parameter, private                       &
     &               :: insulate_shell_bc = 'insulate_shell'
!
      private :: fixed_x, fix_ctl_x, bc_file_x, fix_file_x
      private :: fixed_y, fix_ctl_y, bc_file_y, fix_file_y
      private :: fixed_z, fix_ctl_z, bc_file_z, fix_file_z
      private :: fixed_SGS, fix_SGS_x, fix_SGS_y, fix_SGS_z
!
      private :: add_bc_group_types_equator
      private :: add_bc_group_special_velocity
      private :: add_bc_group_insulate_shell
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_each_dir(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      (cmp_no_case(bc_type_ctl, fixed_x)                        &
     &    .or. cmp_no_case(bc_type_ctl, fix_ctl_x) ) then
        ibc_type = iflag_bc_fix_x
      else if (cmp_no_case(bc_type_ctl, fixed_y)                        &
     &    .or. cmp_no_case(bc_type_ctl, fix_ctl_y) ) then
        ibc_type = iflag_bc_fix_y
      else if (cmp_no_case(bc_type_ctl, fixed_z)                        &
     &    .or. cmp_no_case(bc_type_ctl, fix_ctl_z) ) then
        ibc_type = iflag_bc_fix_z
      else if (cmp_no_case(bc_type_ctl, bc_file_x)                      &
     &    .or. cmp_no_case(bc_type_ctl, fix_file_x)) then
        ibc_type = iflag_bc_file_x
      else if (cmp_no_case(bc_type_ctl, bc_file_y)                      &
     &    .or. cmp_no_case(bc_type_ctl, fix_file_y)) then
        ibc_type = iflag_bc_file_y
      else if (cmp_no_case(bc_type_ctl, bc_file_z)                      &
     &    .or. cmp_no_case(bc_type_ctl, fix_file_z)) then
        ibc_type = iflag_bc_file_z
      end if
!
      end subroutine set_bc_group_types_each_dir
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_sgs_scalar(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if(cmp_no_case(bc_type_ctl, fixed_SGS)) then
        ibc_type = iflag_bc_sgs_s
      end if
!
      end subroutine set_bc_group_types_sgs_scalar
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_sgs_vect(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      (cmp_no_case(bc_type_ctl, fix_SGS_x)) then
        ibc_type = iflag_bc_sgs_x
      else if (cmp_no_case(bc_type_ctl, fix_SGS_y)) then
        ibc_type = iflag_bc_sgs_y
      else if (cmp_no_case(bc_type_ctl, fix_SGS_z)) then
        ibc_type = iflag_bc_sgs_z
      end if
!
      end subroutine set_bc_group_types_sgs_vect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_types_equator(bc_type_ctl, iflag_4_hemi)
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: iflag_4_hemi
!
      if(cmp_no_case(bc_type_ctl, equator_bc)) iflag_4_hemi= 1
      end subroutine set_bc_group_types_equator
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_special_velocity(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if      (cmp_no_case(bc_type_ctl, no_radial_comp_bc)) then
        ibc_type = iflag_no_vr
      else if (cmp_no_case(bc_type_ctl, special_bc)) then
        ibc_type = iflag_bc_special
      end if
!
      end subroutine set_bc_group_special_velocity
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_group_insulate_shell(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
      if(cmp_no_case(bc_type_ctl, insulate_shell_bc)) then
        ibc_type = iflag_insulator
      end if
!
      end subroutine set_bc_group_insulate_shell
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_label_thermal_bc(array_c)
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
      call add_bc_group_types_sgs_scalar(array_c)
!
      end subroutine set_label_thermal_bc
!
!-----------------------------------------------------------------------
!
      subroutine set_label_momentum_bc(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call add_bc_group_types_sph_velo(array_c)
      call add_bc_group_types_sph_center(array_c)
      call add_bc_group_types_vector(array_c)
      call add_bc_group_types_each_dir(array_c)
      call add_bc_group_types_sgs_vect(array_c)
      call add_bc_group_types_rotation(array_c)
      call add_bc_group_special_velocity(array_c)
      call add_bc_group_types_equator(array_c)
!
      end subroutine set_label_momentum_bc
!
!-----------------------------------------------------------------------
!
      subroutine set_label_induction_bc(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call add_bc_group_types_sph_magne(array_c)
      call add_bc_group_types_sph_center(array_c)
      call add_bc_group_types_vector(array_c)
      call add_bc_group_types_each_dir(array_c)
      call add_bc_group_types_sgs_vect(array_c)
!
      end subroutine set_label_induction_bc
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_label_potential_bc(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call add_bc_group_types_scalar(array_c)
      call add_bc_group_types_sgs_scalar(array_c)
!
      end subroutine set_label_potential_bc
!
! ----------------------------------------------------------------------
!
      subroutine set_label_vector_p_bc(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call add_bc_group_insulate_shell(array_c)
      call add_bc_group_types_vector(array_c)
      call add_bc_group_types_each_dir(array_c)
      call add_bc_group_types_sgs_vect(array_c)
!
      end subroutine set_label_vector_p_bc
!
! ----------------------------------------------------------------------
!
      subroutine set_label_current_bc(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call add_bc_group_types_vector(array_c)
      call add_bc_group_types_each_dir(array_c)
      end subroutine set_label_current_bc
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_each_dir(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(fixed_x, array_c)
      call append_c_to_ctl_array(fixed_y, array_c)
      call append_c_to_ctl_array(fixed_z, array_c)
!
      call append_c_to_ctl_array(bc_file_x, array_c)
      call append_c_to_ctl_array(bc_file_y, array_c)
      call append_c_to_ctl_array(bc_file_z, array_c)
!
      end subroutine add_bc_group_types_each_dir
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_sgs_scalar(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(fixed_SGS, array_c)
      end subroutine add_bc_group_types_sgs_scalar
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_sgs_vect(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(fix_SGS_x, array_c)
      call append_c_to_ctl_array(fix_SGS_y, array_c)
      call append_c_to_ctl_array(fix_SGS_z, array_c)
!
      end subroutine add_bc_group_types_sgs_vect
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_types_equator(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(equator_bc, array_c)
      end subroutine add_bc_group_types_equator
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_special_velocity(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(no_radial_comp_bc, array_c)
      call append_c_to_ctl_array(special_bc, array_c)
      end subroutine add_bc_group_special_velocity
!
! ----------------------------------------------------------------------
!
      subroutine add_bc_group_insulate_shell(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call append_c_to_ctl_array(insulate_shell_bc, array_c)
      end subroutine add_bc_group_insulate_shell
!
!-----------------------------------------------------------------------
!
      end module m_fem_node_group_types
