!>@file   check_base_field.f90
!!        module check_base_field
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Check Dependecies for basic fields
!!
!!@verbatim
!!      subroutine add_field_ctl_4_base_field(field_ctl)
!!      subroutine add_field_ctl_4_grad_field(field_ctl)
!!      subroutine add_field_ctl_4_diff_vector(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_base_field
!
      use m_precision
      use m_constants
      use t_base_field_labels
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
      subroutine add_field_ctl_4_base_field(field_ctl)
!
      use m_base_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(perturbation_temp, field_ctl)) then
        call add_phys_name_ctl(reference_temperature, field_ctl)
      end if
      if(check_field_list_ctl(perturbation_composition,                 &
     &                        field_ctl)) then
        call add_phys_name_ctl(reference_composition, field_ctl)
      end if
      if(   check_field_list_ctl(perturbation_entropy, field_ctl)) then
        call add_phys_name_ctl(reference_entropy, field_ctl)
      end if
      if(   check_field_list_ctl(perturbation_density, field_ctl)) then
        call add_phys_name_ctl(reference_density, field_ctl)
      end if
!
      if(   check_field_list_ctl(reference_temperature, field_ctl)      &
     & .or. check_field_list_ctl(heat_source, field_ctl)) then
        call add_phys_name_ctl(temperature, field_ctl)
      end if
      if(   check_field_list_ctl(reference_composition, field_ctl)      &
     & .or. check_field_list_ctl(composition_source, field_ctl)) then
        call add_phys_name_ctl(composition, field_ctl)
      end if
      if(   check_field_list_ctl(reference_entropy, field_ctl)          &
     & .or. check_field_list_ctl(entropy_source, field_ctl)) then
        call add_phys_name_ctl(entropy, field_ctl)
      end if
      if(check_field_list_ctl(reference_density, field_ctl)) then 
        call add_phys_name_ctl(density, field_ctl)
      end if
!
      if( check_field_list_ctl(vector_potential, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(magnetic_potential, field_ctl)
      end if
!
      if(   check_field_list_ctl(current_density, field_ctl)            &
     & .or. check_field_list_ctl(magnetic_potential, field_ctl)         &
     & .or. check_field_list_ctl(scalar_potential, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
!
      if(check_field_list_ctl(density, field_ctl)) then 
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(composition, field_ctl)
      end if
!
      if(   check_field_list_ctl(vorticity, field_ctl)                  &
     & .or. check_field_list_ctl(pressure, field_ctl)                   &
     & .or. check_field_list_ctl(magnetic_field, field_ctl)             &
     & .or. check_field_list_ctl(temperature, field_ctl)                &
     & .or. check_field_list_ctl(composition, field_ctl)                &
     & .or. check_field_list_ctl(density, field_ctl)                    &
     & .or. check_field_list_ctl(entropy, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_base_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_grad_field(field_ctl)
!
      use t_grad_field_labels
      use m_base_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_velocity, field_ctl))                 &
     &   call add_phys_name_ctl(velocity, field_ctl)
      if(check_field_list_ctl(div_magnetic, field_ctl))                 &
     &   call add_phys_name_ctl(magnetic_field, field_ctl)
      if(check_field_list_ctl(div_vector_potential, field_ctl))         &
     &   call add_phys_name_ctl(vector_potential, field_ctl)
!
      if(check_field_list_ctl(grad_temp, field_ctl))                    &
     &   call add_phys_name_ctl(temperature, field_ctl)
      if(check_field_list_ctl(grad_pert_temp, field_ctl))               &
     &   call add_phys_name_ctl(perturbation_temp, field_ctl)
      if(check_field_list_ctl(grad_reference_temp, field_ctl))          &
     &   call add_phys_name_ctl(reference_temperature, field_ctl)
!
      if(check_field_list_ctl(grad_composition, field_ctl))             &
     &   call add_phys_name_ctl(composition, field_ctl)
      if(check_field_list_ctl(grad_pert_composition, field_ctl))        &
     &   call add_phys_name_ctl(perturbation_composition, field_ctl)
      if(check_field_list_ctl(grad_reference_composition, field_ctl))   &
     &   call add_phys_name_ctl(reference_composition, field_ctl)
!
      if(check_field_list_ctl(grad_density, field_ctl))                 &
     &   call add_phys_name_ctl(density, field_ctl)
      if(check_field_list_ctl(grad_pert_density, field_ctl))            &
     &   call add_phys_name_ctl(perturbation_density, field_ctl)
      if(check_field_list_ctl(grad_reference_density, field_ctl))       &
     &   call add_phys_name_ctl(reference_density, field_ctl)
!
      if(check_field_list_ctl(grad_entropy, field_ctl))                 &
     &   call add_phys_name_ctl(entropy, field_ctl)
      if(check_field_list_ctl(grad_pert_entropy, field_ctl))            &
     &   call add_phys_name_ctl(perturbation_entropy, field_ctl)
      if(check_field_list_ctl(grad_reference_entropy, field_ctl))       &
     &   call add_phys_name_ctl(reference_entropy, field_ctl)
!
      end subroutine add_field_ctl_4_grad_field
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_diff_vector(field_ctl)
!
      use t_diff_vector_labels
      use m_base_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(   check_field_list_ctl(grad_v_1, field_ctl)                   &
     & .or. check_field_list_ctl(grad_v_2, field_ctl)                   &
     & .or. check_field_list_ctl(grad_v_3, field_ctl))                  &
     &  call add_phys_name_ctl(velocity, field_ctl)
      if(   check_field_list_ctl(grad_w_1, field_ctl)                   &
     & .or. check_field_list_ctl(grad_w_2, field_ctl)                   &
     & .or. check_field_list_ctl(grad_w_3, field_ctl))                  &
     &  call add_phys_name_ctl(vorticity, field_ctl)
!
      if(   check_field_list_ctl(grad_b_1, field_ctl)                   &
     & .or. check_field_list_ctl(grad_b_2, field_ctl)                   &
     & .or. check_field_list_ctl(grad_b_3, field_ctl))                  &
     &  call add_phys_name_ctl(magnetic_field, field_ctl)
      if(   check_field_list_ctl(grad_a_1, field_ctl)                   &
     & .or. check_field_list_ctl(grad_a_2, field_ctl)                   &
     & .or. check_field_list_ctl(grad_a_3, field_ctl))                  &
     &  call add_phys_name_ctl(vector_potential, field_ctl)
      if(   check_field_list_ctl(grad_j_1, field_ctl)                   &
     & .or. check_field_list_ctl(grad_j_2, field_ctl)                   &
     & .or. check_field_list_ctl(grad_j_3, field_ctl))                  &
     &  call add_phys_name_ctl(current_density, field_ctl)
!
      end subroutine add_field_ctl_4_diff_vector
!
! -----------------------------------------------------------------------
!
      end module check_base_field
