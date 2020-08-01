!>@file   check_workarea_4_explicit.f90
!!        module check_workarea_4_explicit
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Add required field for work area
!!
!!@verbatim
!!      subroutine add_field_ctl_4_check_evo(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!!
      module check_workarea_4_explicit
!
      use m_precision
      use t_control_array_character3
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_check_evo(field_ctl)
!
      use m_base_field_labels
      use m_explicit_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(check_momentum_2, field_ctl))             &
     &   call add_phys_name_ctl(check_momentum, field_ctl)
      if(check_field_list_ctl(check_induction_2, field_ctl))            &
     &   call add_phys_name_ctl(check_induction, field_ctl)
      if(check_field_list_ctl(check_heat_2, field_ctl))                 &
     &   call add_phys_name_ctl(check_heat, field_ctl)
      if(check_field_list_ctl(check_composition_2, field_ctl))          &
     &   call add_phys_name_ctl(check_composition, field_ctl)
      if(check_field_list_ctl(check_pressure_2, field_ctl))             &
     &   call add_phys_name_ctl(check_pressure, field_ctl)
      if(check_field_list_ctl(check_potential_2, field_ctl))            &
     &   call add_phys_name_ctl(check_potential, field_ctl)
!
!
      if(check_field_list_ctl(check_momentum, field_ctl))               &
     &   call add_phys_name_ctl(velocity, field_ctl)
!
      if(check_field_list_ctl(check_heat, field_ctl))                   &
     &   call add_phys_name_ctl(temperature, field_ctl)
      if(check_field_list_ctl(check_composition, field_ctl))            &
     &   call add_phys_name_ctl(composition, field_ctl)
      if(check_field_list_ctl(check_pressure, field_ctl))               &
     &   call add_phys_name_ctl(pressure, field_ctl)
      if(check_field_list_ctl(check_potential, field_ctl))              &
     &   call add_phys_name_ctl(magnetic_potential, field_ctl)
!
      end subroutine add_field_ctl_4_check_evo
!
! -----------------------------------------------------------------------
!
      end module check_workarea_4_explicit
