!>@file   check_filtered_forces.f90
!!        module check_filtered_forces
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!>@brief Check Dependecies for filtered forces
!!
!!@verbatim
!!      subroutine add_field_ctl_4_filter_forces(field_ctl)
!!      subroutine add_field_ctl_4_fil_ene_flux(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!!
      module check_filtered_forces
!
      use m_precision
      use m_constants
!
      use t_control_array_character3
      use m_filtered_field_labels
      use m_filtered_force_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_filter_forces(field_ctl)
!
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
!
      if(check_field_list_ctl(magnetic_induction_by_filtered,           &
     &                        field_ctl))                               &
     &   call add_phys_name_ctl(vecp_induction_by_filtered, field_ctl)
!
!
      if(check_field_list_ctl(inertia_by_filtered, field_ctl)) then
         call add_phys_name_ctl(filter_velocity, field_ctl)
         call add_phys_name_ctl(filter_vorticity, field_ctl)
      end if
      if(check_field_list_ctl(momentum_flux_by_filtered, field_ctl))    &
     &   call add_phys_name_ctl(filter_velocity, field_ctl)
!
      if(check_field_list_ctl(Lorentz_force_by_filtered,                &
     &                        field_ctl)) then
         call add_phys_name_ctl(filter_magne, field_ctl)
         call add_phys_name_ctl(filter_current, field_ctl)
      end if
      if(     check_field_list_ctl(magnetic_tension_by_filtered,        &
     &                             field_ctl)                           &
     &   .or. check_field_list_ctl(maxwell_tensor_by_filtered,          &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_magne, field_ctl)
      end if
!
      if(check_field_list_ctl(filtered_buoyancy, field_ctl))            &
     &   call add_phys_name_ctl(filter_temperature, field_ctl)
      if(check_field_list_ctl(filtered_comp_buoyancy, field_ctl))       &
     &   call add_phys_name_ctl(filter_composition, field_ctl)
!
      if(     check_field_list_ctl(vecp_induction_by_filtered,          &
     &                             field_ctl)                           &
     &   .or. check_field_list_ctl(magnetic_stretch_by_filtered,        &
     &                             field_ctl)                           &
     &   .or. check_field_list_ctl(induction_tensor_by_filtered,        &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_velocity, field_ctl)
         call add_phys_name_ctl(filter_magne, field_ctl)
      end if
!
      if(check_field_list_ctl(heat_advect_by_filtered, field_ctl)       &
     &   .or. check_field_list_ctl(heat_flux_by_filtered,               &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_velocity, field_ctl)
         call add_phys_name_ctl(filter_temperature, field_ctl)
      end if
      if(     check_field_list_ctl(pert_h_advect_by_filtered,           &
     &                             field_ctl)                           &
     &   .or. check_field_list_ctl(pert_h_flux_by_filtered,             &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_velocity, field_ctl)
         call add_phys_name_ctl(filter_pert_temperature, field_ctl)
      end if
!
      if(check_field_list_ctl(comp_advect_by_filtered, field_ctl)       &
     &   .or. check_field_list_ctl(composite_flux_by_filtered,          &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_velocity, field_ctl)
         call add_phys_name_ctl(filter_composition, field_ctl)
      end if
      if(     check_field_list_ctl(pert_c_advect_by_filtered,           &
     &                             field_ctl)                           &
     &   .or. check_field_list_ctl(pert_c_flux_by_filtered,             &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_velocity, field_ctl)
         call add_phys_name_ctl(filter_pert_composition, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_filter_forces
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_fil_ene_flux(field_ctl)
!
      use m_base_field_labels
      use m_filtered_ene_flux_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(inertia_work_by_filtered,                 &
     &                        field_ctl)) then
         call add_phys_name_ctl(velocity, field_ctl)
         call add_phys_name_ctl(inertia_by_filtered, field_ctl)
      end if
!
      if(check_field_list_ctl(wk_against_Lorentz_by_filtered,           &
     &                        field_ctl)                                &
     &     .or. check_field_list_ctl(Lorentz_work_by_filtered,          &
     &                        field_ctl)) then
         call add_phys_name_ctl(velocity, field_ctl)
         call add_phys_name_ctl                                         &
     &      (Lorentz_force_by_filtered, field_ctl)
      end if
      if(check_field_list_ctl(mag_tension_work_by_filtered,             &
     &                        field_ctl)) then
         call add_phys_name_ctl(velocity, field_ctl)
         call add_phys_name_ctl                                         &
     &      (magnetic_tension_by_filtered, field_ctl)
      end if
!
      if(check_field_list_ctl(filtered_buoyancy_flux, field_ctl)) then
         call add_phys_name_ctl(velocity, field_ctl)
         call add_phys_name_ctl(filtered_buoyancy, field_ctl)
      end if
      if(check_field_list_ctl(filtered_comp_buoyancy_flux,              &
     &                        field_ctl)) then
         call add_phys_name_ctl(velocity, field_ctl)
         call add_phys_name_ctl(filtered_comp_buoyancy, field_ctl)
      end if
!
      if(check_field_list_ctl(mag_ene_generation_by_filtered,           &
     &                        field_ctl)) then
         call add_phys_name_ctl(magnetic_field, field_ctl)
         call add_phys_name_ctl(vecp_induction_by_filtered, field_ctl)
      end if
      if(check_field_list_ctl(mag_stretch_flux_by_filtered,             &
     &                        field_ctl)) then
         call add_phys_name_ctl(magnetic_field, field_ctl)
         call add_phys_name_ctl                                         &
     &      (magnetic_stretch_by_filtered, field_ctl)
      end if
!
      if(check_field_list_ctl(temp_generation_by_filtered,              &
     &                        field_ctl)) then
         call add_phys_name_ctl(temperature, field_ctl)
         call add_phys_name_ctl(heat_advect_by_filtered, field_ctl)
      end if
      if(check_field_list_ctl(part_temp_gen_by_filtered,                &
     &                        field_ctl)) then
         call add_phys_name_ctl(perturbation_temp, field_ctl)
         call add_phys_name_ctl(pert_h_advect_by_filtered, field_ctl)
      end if
!
      if(check_field_list_ctl(comp_generation_by_filtered,              &
     &                        field_ctl)) then
         call add_phys_name_ctl(composition, field_ctl)
         call add_phys_name_ctl(comp_advect_by_filtered, field_ctl)
      end if
      if(check_field_list_ctl(part_comp_gen_by_filtered,                &
     &                        field_ctl)) then
         call add_phys_name_ctl(perturbation_composition, field_ctl)
         call add_phys_name_ctl(pert_c_advect_by_filtered, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_fil_ene_flux
!
! ----------------------------------------------------------------------
!
      end module check_filtered_forces
