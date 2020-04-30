!>@file   check_base_forces.f90
!!        module check_base_forces
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_field_ctl_4_forces(field_ctl)
!!      subroutine add_field_ctl_4_rot_forces(field_ctl)
!!      subroutine add_field_ctl_4_div_forces(field_ctl)
!!      subroutine add_field_ctl_4_field_products(field_ctl)
!!      subroutine add_field_ctl_4_diffusions(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_base_forces
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use t_base_force_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_forces(field_ctl)
!
      use t_control_array_character3
      use t_diff_vector_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(magnetic_induction%name, field_ctl)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(vecp_induction%name, field_ctl)
      end if
!
      if(check_field_list_ctl(pressure_gradient%name, field_ctl))       &
        call add_phys_name_ctl(pressure%name, field_ctl)
      if(check_field_list_ctl(inertia%name, field_ctl)) then
        call add_phys_name_ctl(vorticity%name, field_ctl)
        call add_phys_name_ctl(pressure%name, field_ctl)
      end if
!
      if(check_field_list_ctl(Lorentz_force%name, field_ctl)) then
        call add_phys_name_ctl(current_density%name, field_ctl)
        call add_phys_name_ctl(pressure%name, field_ctl)
      end if
!
      if(check_field_list_ctl(buoyancy%name, field_ctl))                &
        call add_phys_name_ctl(temperature%name, field_ctl)
      if(check_field_list_ctl(composite_buoyancy%name, field_ctl))      &
        call add_phys_name_ctl(composition%name, field_ctl)
!
      if(      check_field_list_ctl(Coriolis_force%name, field_ctl)     &
     &    .or. check_field_list_ctl(momentum_flux%name, field_ctl))     &
        call add_phys_name_ctl(velocity%name, field_ctl)
      if(      check_field_list_ctl(magnetic_tension%name, field_ctl)   &
      &    .or. check_field_list_ctl(maxwell_tensor%name, field_ctl))   &
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
!
      if(      check_field_list_ctl(vecp_induction%name, field_ctl)     &
     &    .or. check_field_list_ctl(induction_tensor%name, field_ctl)   &
     &      ) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
      end if
      if(check_field_list_ctl(magnetic_stretch%name, field_ctl)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(grad_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_v_3%name, field_ctl)
      end if
!
      if(      check_field_list_ctl(heat_advect%name, field_ctl)        &
     &    .or. check_field_list_ctl(heat_flux%name, field_ctl)          &
     &      ) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(temperature%name, field_ctl)
      end if
      if(      check_field_list_ctl(pert_heat_advect%name, field_ctl)   &
     &    .or. check_field_list_ctl(pert_heat_flux%name, field_ctl)     &
     &      ) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(perturbation_temp%name, field_ctl)
      end if
      if(      check_field_list_ctl(composition_advect%name, field_ctl) &
     &    .or. check_field_list_ctl(composite_flux%name, field_ctl)     &
     &      ) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(composition%name, field_ctl)
      end if
      if(      check_field_list_ctl(pert_comp_advect%name, field_ctl)   &
     &    .or. check_field_list_ctl(pert_comp_flux%name, field_ctl)     &
     &      ) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(perturbation_composition%name,           &
     &                         field_ctl)
      end if
!
      end subroutine add_field_ctl_4_forces
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_rot_forces(field_ctl)
!
      use m_rot_force_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if    (check_field_list_ctl(rot_inertia%name, field_ctl))         &
     &   call add_phys_name_ctl(inertia%name, field_ctl)
      if    (check_field_list_ctl(rot_Coriolis_force%name, field_ctl))  &
     &   call add_phys_name_ctl(Coriolis_force%name, field_ctl)
      if(check_field_list_ctl(rot_Lorentz_force%name, field_ctl))       &
     &   call add_phys_name_ctl(Lorentz_force%name, field_ctl)
!
      if(check_field_list_ctl(rot_buoyancy%name, field_ctl))            &
     &   call add_phys_name_ctl(buoyancy%name, field_ctl)
      if(check_field_list_ctl(rot_composite_buoyancy%name, field_ctl))  &
     &   call add_phys_name_ctl(composite_buoyancy%name, field_ctl)
!
      end subroutine add_field_ctl_4_rot_forces
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_div_forces(field_ctl)
!
      use t_control_array_character3
      use m_div_force_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_inertia%name, field_ctl))             &
     &   call add_phys_name_ctl(inertia%name, field_ctl)
      if(check_field_list_ctl(div_Coriolis_force%name, field_ctl))      &
     &   call add_phys_name_ctl(Coriolis_force%name, field_ctl)
      if(check_field_list_ctl(div_Lorentz_force%name, field_ctl))       &
     &   call add_phys_name_ctl(Lorentz_force%name, field_ctl)
!
      if(check_field_list_ctl(div_buoyancy%name, field_ctl))            &
        call add_phys_name_ctl(buoyancy%name, field_ctl)
      if(check_field_list_ctl(div_composite_buoyancy%name, field_ctl))  &
        call add_phys_name_ctl(composite_buoyancy%name, field_ctl)
!
      if(check_field_list_ctl(div_momentum_flux%name, field_ctl))       &
        call add_phys_name_ctl(momentum_flux%name, field_ctl)
      if(check_field_list_ctl(div_maxwell_tensor%name, field_ctl))      &
        call add_phys_name_ctl(maxwell_tensor%name, field_ctl)
      if(check_field_list_ctl(div_induction_tensor%name, field_ctl))    &
        call add_phys_name_ctl(induction_tensor%name, field_ctl)
!
      if(check_field_list_ctl(div_heat_flux%name, field_ctl))           &
        call add_phys_name_ctl(div_pert_heat_flux%name, field_ctl)
      if(check_field_list_ctl(pert_heat_flux%name, field_ctl))          &
        call add_phys_name_ctl(div_composition_flux%name, field_ctl)
      if(check_field_list_ctl(composite_flux%name, field_ctl))          &
        call add_phys_name_ctl(induction_tensor%name, field_ctl)
      if(check_field_list_ctl(div_pert_composition_flux%name,           &
     &                        field_ctl))                               &
        call add_phys_name_ctl(pert_comp_flux%name, field_ctl)
!
      end subroutine add_field_ctl_4_div_forces
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_field_products(field_ctl)
!
      use t_control_array_character3
      use t_field_product_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(rest_of_geostrophic%name,                 &
     &                        field_ctl)) then
        call add_phys_name_ctl(Coriolis_force%name, field_ctl)
        call add_phys_name_ctl(pressure_gradient%name, field_ctl)
      end if
!
      if(check_field_list_ctl(poynting_flux%name, field_ctl)) then
        call add_phys_name_ctl(electric_field%name, field_ctl)
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
      end if
      if(check_field_list_ctl(electric_field%name, field_ctl)) then
        call add_phys_name_ctl(vecp_induction%name, field_ctl)
        call add_phys_name_ctl(current_density%name, field_ctl)
      end if
!
      if(check_field_list_ctl(truncated_magnetic_field%name,            &
     &                        field_ctl))                               &
     &   call add_phys_name_ctl(magnetic_field%name, field_ctl)
!
      if(check_field_list_ctl(kinetic_helicity%name, field_ctl)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(vorticity%name, field_ctl)
      end if
      if(check_field_list_ctl(magnetic_helicity%name, field_ctl)) then
        call add_phys_name_ctl(vector_potential%name, field_ctl)
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
      end if
      if(check_field_list_ctl(current_helicity%name, field_ctl)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(current_density%name, field_ctl)
      end if
      if(check_field_list_ctl(cross_helicity%name, field_ctl)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
      end if
!
      if(      check_field_list_ctl(square_velocity%name, field_ctl)    &
     &    .or. check_field_list_ctl(velocity_scale%name, field_ctl))    &
     &   call add_phys_name_ctl(velocity%name, field_ctl)
      if(check_field_list_ctl(square_vorticity%name, field_ctl))        &
     &   call add_phys_name_ctl(vorticity%name, field_ctl)
      if(      check_field_list_ctl(square_magne%name, field_ctl)       &
     &    .or. check_field_list_ctl(magnetic_scale%name, field_ctl))    &
     &   call add_phys_name_ctl(magnetic_field%name, field_ctl)
      if(check_field_list_ctl(square_vector_potential%name, field_ctl)) &
     &   call add_phys_name_ctl(vector_potential%name, field_ctl)
      if(check_field_list_ctl(square_current%name, field_ctl))          &
     &   call add_phys_name_ctl(current_density%name, field_ctl)
      if(      check_field_list_ctl(square_temperature%name, field_ctl) &
     &    .or. check_field_list_ctl(temperature_scale%name, field_ctl)) &
     &   call add_phys_name_ctl(temperature%name, field_ctl)
      if(      check_field_list_ctl(square_composition%name, field_ctl) &
     &    .or. check_field_list_ctl(composition_scale%name, field_ctl)) &
     &   call add_phys_name_ctl(composition%name, field_ctl)
!
      end subroutine add_field_ctl_4_field_products
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_diffusions(field_ctl)
!
      use t_control_array_character3
      use t_diffusion_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_viscousity%name, field_ctl))          &
     &   call add_phys_name_ctl(viscous_diffusion%name, field_ctl)
!
      if(check_field_list_ctl(viscous_diffusion%name, field_ctl))       &
     &   call add_phys_name_ctl(velocity%name, field_ctl)
      if(check_field_list_ctl(vorticity_diffusion%name, field_ctl))     &
     &   call add_phys_name_ctl(vorticity%name, field_ctl)
      if(check_field_list_ctl(magnetic_diffusion%name, field_ctl))      &
     &   call add_phys_name_ctl(magnetic_field%name, field_ctl)
      if(check_field_list_ctl(vector_potential_diffusion%name,          &
     &                        field_ctl))                               &
     &   call add_phys_name_ctl(vector_potential%name, field_ctl)
      if(check_field_list_ctl(thermal_diffusion%name, field_ctl))       &
     &   call add_phys_name_ctl(temperature%name, field_ctl)
      if(check_field_list_ctl(composition_diffusion%name, field_ctl))  &
     &   call add_phys_name_ctl(composition%name, field_ctl)
!
      end subroutine add_field_ctl_4_diffusions
!
! -----------------------------------------------------------------------
!
      end module check_base_forces
