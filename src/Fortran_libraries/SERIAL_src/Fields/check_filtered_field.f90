!>@file   check_filtered_field.f90
!!        module check_filtered_field
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Check Dependecies for filtered fields
!!
!!@verbatim
!!      subroutine add_field_ctl_4_filterd_field(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_filtered_field
!
      use m_precision
      use m_constants
      use t_base_field_labels
      use m_filtered_field_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_filterd_field(field_ctl)
!
      use t_control_array_character3
      use m_base_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(filter_vorticity, field_ctl)) then
        call add_phys_name_ctl(filter_velocity, field_ctl)
      end if
      if(check_field_list_ctl(filter_current, field_ctl)) then
        call add_phys_name_ctl(filter_magne, field_ctl)
      end if
!
      if(check_field_list_ctl(filter_velocity, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
      end if
      if(check_field_list_ctl(filter_magne, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
      if(check_field_list_ctl(filter_vector_potential, field_ctl)) then
        call add_phys_name_ctl(vector_potential, field_ctl)
      end if
!
      if(check_field_list_ctl(filter_temperature, field_ctl)) then
        call add_phys_name_ctl(temperature, field_ctl)
      end if
      if(check_field_list_ctl(filter_composition, field_ctl)) then
        call add_phys_name_ctl(composition, field_ctl)
      end if
      if(check_field_list_ctl(filter_density, field_ctl)) then
        call add_phys_name_ctl(density, field_ctl)
      end if
      if(check_field_list_ctl(filter_entropy, field_ctl)) then
        call add_phys_name_ctl(entropy, field_ctl)
      end if
!
      if(check_field_list_ctl(filter_pert_temperature, field_ctl)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
      end if
      if(check_field_list_ctl(filter_pert_composition, field_ctl)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
      end if
      if(check_field_list_ctl(filter_pert_density, field_ctl)) then
        call add_phys_name_ctl(perturbation_density, field_ctl)
      end if
      if(check_field_list_ctl(filter_pert_entropy, field_ctl)) then
        call add_phys_name_ctl(perturbation_entropy, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_filterd_field
!
! -----------------------------------------------------------------------
!
      end module check_filtered_field
