!>@file   check_field_w_symmetry.f90
!!        module check_field_w_symmetry
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Check Dependecies for fields with symmetry
!!
!!@verbatim
!!      subroutine add_field_w_symmetry_ctl(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_field_w_symmetry
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use m_field_w_symmetry_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_w_symmetry_ctl(field_ctl)
!
      use t_control_array_character3
      use m_base_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      check_field_list_ctl(sym_velocity, field_ctl)            &
     &    .or. check_field_list_ctl(asym_velocity, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
      end if
      if( check_field_list_ctl(sym_vorticity, field_ctl)                &
     &    .or. check_field_list_ctl(asym_vorticity, field_ctl)) then
        call add_phys_name_ctl(vorticity, field_ctl)
      end if
      if( check_field_list_ctl(sym_magnetic_field, field_ctl)           &
     &    .or. check_field_list_ctl(asym_magnetic_field,                &
     &                              field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
      if( check_field_list_ctl(sym_vector_potential, field_ctl)         &
     &    .or. check_field_list_ctl(asym_vector_potential,              &
     &                              field_ctl)) then
        call add_phys_name_ctl(vector_potential, field_ctl)
      end if
      if( check_field_list_ctl(sym_current_density, field_ctl)          &
     &    .or. check_field_list_ctl(asym_current_density,               &
     &                              field_ctl)) then
        call add_phys_name_ctl(current_density, field_ctl)
      end if
!
      if( check_field_list_ctl(sym_pressure, field_ctl)                 &
     &    .or. check_field_list_ctl(asym_pressure, field_ctl)) then
        call add_phys_name_ctl(pressure, field_ctl)
      end if
      if( check_field_list_ctl(sym_magnetic_potential, field_ctl)       &
     &    .or. check_field_list_ctl(asym_magnetic_potential, field_ctl) &
     &   ) then
        call add_phys_name_ctl(magnetic_potential, field_ctl)
      end if
      if( check_field_list_ctl(sym_scalar_potential, field_ctl)         &
     &    .or. check_field_list_ctl(asym_scalar_potential, field_ctl)   &
     &   ) then
        call add_phys_name_ctl(scalar_potential, field_ctl)
      end if
!
      if( check_field_list_ctl(sym_temperature, field_ctl)              &
     &    .or. check_field_list_ctl(asym_temperature, field_ctl)) then
        call add_phys_name_ctl(temperature, field_ctl)
      end if
      if( check_field_list_ctl(sym_composition, field_ctl)              &
     &    .or. check_field_list_ctl(asym_composition, field_ctl)) then
        call add_phys_name_ctl(composition, field_ctl)
      end if
      if( check_field_list_ctl(sym_density, field_ctl)                  &
     &    .or. check_field_list_ctl(asym_density, field_ctl)) then
        call add_phys_name_ctl(density, field_ctl)
      end if
      if( check_field_list_ctl(sym_entropy, field_ctl)                  &
     &    .or. check_field_list_ctl(asym_entropy, field_ctl)) then
        call add_phys_name_ctl(entropy, field_ctl)
      end if
!
      if( check_field_list_ctl(sym_perturbation_temp, field_ctl)        &
     &    .or. check_field_list_ctl(asym_perturbation_temp, field_ctl)  &
     &   ) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
      end if
      if( check_field_list_ctl(sym_perturbation_composition, field_ctl) &
     &  .or. check_field_list_ctl(asym_perturbation_composition,        &
     &                              field_ctl)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
      end if
      if( check_field_list_ctl(sym_perturbation_density, field_ctl)     &
     &    .or. check_field_list_ctl(asym_perturbation_density,          &
     &                              field_ctl)) then
        call add_phys_name_ctl(perturbation_density, field_ctl)
      end if
      if( check_field_list_ctl(sym_perturbation_entropy, field_ctl)     &
     &    .or. check_field_list_ctl(asym_perturbation_entropy,          &
     &                              field_ctl)) then
        call add_phys_name_ctl(perturbation_entropy, field_ctl)
      end if
!
      end subroutine add_field_w_symmetry_ctl
!
! -----------------------------------------------------------------------
!
      end module check_field_w_symmetry
