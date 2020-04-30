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
!!      subroutine add_field_ctl_4_check_evo(cd_prop, field_ctl)
!!        type(conductive_property), intent(in) :: cd_prop
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
      subroutine add_field_ctl_4_check_evo(cd_prop, field_ctl)
!
      use t_physical_property
      use t_explicit_term_labels
      use t_base_field_labels
      use add_nodal_fields_ctl
!
      type(conductive_property), intent(in) :: cd_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(check_momentum_2%name, field_ctl))        &
     &   call add_phys_name_ctl(check_momentum%name, field_ctl)
      if(check_field_list_ctl(check_induction_2%name, field_ctl))       &
     &   call add_phys_name_ctl(check_induction%name, field_ctl)
      if(check_field_list_ctl(check_heat_2%name, field_ctl))            &
     &   call add_phys_name_ctl(check_heat%name, field_ctl)
      if(check_field_list_ctl(check_composition_2%name, field_ctl))     &
     &   call add_phys_name_ctl(check_composition%name, field_ctl)
      if(check_field_list_ctl(check_pressure_2%name, field_ctl))        &
     &   call add_phys_name_ctl(check_pressure%name, field_ctl)
      if(check_field_list_ctl(check_potential_2%name, field_ctl))       &
     &   call add_phys_name_ctl(check_potential%name, field_ctl)
!
!
      if(check_field_list_ctl(check_momentum%name, field_ctl))          &
     &   call add_phys_name_ctl(velocity%name, field_ctl)
!
      if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(check_field_list_ctl(check_induction%name, field_ctl))       &
     &     call add_phys_name_ctl(vector_potential%name, field_ctl)
      else
        if(check_field_list_ctl(check_induction%name, field_ctl))       &
     &     call add_phys_name_ctl(magnetic_field%name, field_ctl)
      end if
!
      if(check_field_list_ctl(check_heat%name, field_ctl))              &
     &   call add_phys_name_ctl(temperature%name, field_ctl)
      if(check_field_list_ctl(check_composition%name, field_ctl))       &
     &   call add_phys_name_ctl(composition%name, field_ctl)
      if(check_field_list_ctl(check_pressure%name, field_ctl))          &
     &   call add_phys_name_ctl(pressure%name, field_ctl)
      if(check_field_list_ctl(check_potential%name, field_ctl))         &
     &   call add_phys_name_ctl(magnetic_potential%name, field_ctl)
!
      end subroutine add_field_ctl_4_check_evo
!
! -----------------------------------------------------------------------
!
      end module check_workarea_4_explicit
