!>@file   check_base_forces.f90
!!        module check_base_forces
!!
!!@author H. Matsui (UC Davis)
!!@n      and T. Kera (Tohoku University)
!!
!!@date   Programmed in Jan., 2020
!!@n      Modified in July, 2021
!!
!!
!>@brief Check Dependecies for basic forces
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
      use m_diff_vector_labels
      use m_base_field_labels
      use m_base_force_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(magnetic_induction, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(vecp_induction, field_ctl)
      end if
!
      if(check_field_list_ctl(pressure_gradient, field_ctl))            &
        call add_phys_name_ctl(pressure, field_ctl)
      if(check_field_list_ctl(inertia, field_ctl)) then
        call add_phys_name_ctl(vorticity, field_ctl)
        call add_phys_name_ctl(pressure, field_ctl)
      end if
!
      if(check_field_list_ctl(Lorentz_force, field_ctl)) then
        call add_phys_name_ctl(current_density, field_ctl)
        call add_phys_name_ctl(pressure, field_ctl)
      end if
!
      if(check_field_list_ctl(buoyancy, field_ctl))                     &
        call add_phys_name_ctl(temperature, field_ctl)
      if(check_field_list_ctl(composite_buoyancy, field_ctl))           &
        call add_phys_name_ctl(composition, field_ctl)
!
      if(      check_field_list_ctl(Coriolis_force, field_ctl)          &
     &    .or. check_field_list_ctl(momentum_flux, field_ctl))          &
        call add_phys_name_ctl(velocity, field_ctl)
      if(      check_field_list_ctl(magnetic_tension, field_ctl)        &
      &    .or. check_field_list_ctl(maxwell_tensor, field_ctl))        &
        call add_phys_name_ctl(magnetic_field, field_ctl)
!
      if(      check_field_list_ctl(vecp_induction, field_ctl)          &
     &    .or. check_field_list_ctl(induction_tensor, field_ctl)        &
     &      ) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
      if(check_field_list_ctl(magnetic_stretch, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(grad_v_1, field_ctl)
        call add_phys_name_ctl(grad_v_2, field_ctl)
        call add_phys_name_ctl(grad_v_3, field_ctl)
      end if
!
      if(      check_field_list_ctl(heat_advect, field_ctl)             &
     &    .or. check_field_list_ctl(heat_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(temperature, field_ctl)
      end if
      if(      check_field_list_ctl(pert_heat_advect, field_ctl)        &
     &    .or. check_field_list_ctl(pert_heat_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(perturbation_temp, field_ctl)
      end if
      if(      check_field_list_ctl(composition_advect, field_ctl)      &
     &    .or. check_field_list_ctl(composite_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(composition, field_ctl)
      end if
      if(      check_field_list_ctl(pert_comp_advect, field_ctl)        &
     &    .or. check_field_list_ctl(pert_comp_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(perturbation_composition, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_forces
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_rot_forces(field_ctl)
!
      use m_base_force_labels
      use m_rot_force_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if    (check_field_list_ctl(rot_inertia, field_ctl))              &
     &   call add_phys_name_ctl(inertia, field_ctl)
      if    (check_field_list_ctl(rot_Coriolis_force, field_ctl))       &
     &   call add_phys_name_ctl(Coriolis_force, field_ctl)
      if(check_field_list_ctl(rot_Lorentz_force, field_ctl))            &
     &   call add_phys_name_ctl(Lorentz_force, field_ctl)
!
      if(check_field_list_ctl(rot_buoyancy, field_ctl))                 &
     &   call add_phys_name_ctl(buoyancy, field_ctl)
      if(check_field_list_ctl(rot_composite_buoyancy, field_ctl))       &
     &   call add_phys_name_ctl(composite_buoyancy, field_ctl)
!
      end subroutine add_field_ctl_4_rot_forces
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_div_forces(field_ctl)
!
      use t_control_array_character3
      use m_base_force_labels
      use m_div_force_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_inertia, field_ctl))                  &
     &   call add_phys_name_ctl(inertia, field_ctl)
      if(check_field_list_ctl(div_Coriolis_force, field_ctl))           &
     &   call add_phys_name_ctl(Coriolis_force, field_ctl)
      if(check_field_list_ctl(div_Lorentz_force, field_ctl))            &
     &   call add_phys_name_ctl(Lorentz_force, field_ctl)
!
      if(check_field_list_ctl(div_buoyancy, field_ctl))                 &
        call add_phys_name_ctl(buoyancy, field_ctl)
      if(check_field_list_ctl(div_composite_buoyancy, field_ctl))       &
        call add_phys_name_ctl(composite_buoyancy, field_ctl)
!
      if(check_field_list_ctl(div_momentum_flux, field_ctl))            &
        call add_phys_name_ctl(momentum_flux, field_ctl)
      if(check_field_list_ctl(div_maxwell_tensor, field_ctl))           &
        call add_phys_name_ctl(maxwell_tensor, field_ctl)
      if(check_field_list_ctl(div_induction_tensor, field_ctl))         &
        call add_phys_name_ctl(induction_tensor, field_ctl)
!
      if(check_field_list_ctl(div_heat_flux, field_ctl))                &
        call add_phys_name_ctl(div_pert_heat_flux, field_ctl)
      if(check_field_list_ctl(pert_heat_flux, field_ctl))               &
        call add_phys_name_ctl(div_composition_flux, field_ctl)
      if(check_field_list_ctl(composite_flux, field_ctl))               &
        call add_phys_name_ctl(induction_tensor, field_ctl)
      if(check_field_list_ctl(div_pert_composition_flux, field_ctl))    &
        call add_phys_name_ctl(pert_comp_flux, field_ctl)
!
      end subroutine add_field_ctl_4_div_forces
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_field_products(field_ctl)
!
      use t_control_array_character3
      use m_base_field_labels
      use m_base_force_labels
      use m_field_product_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(rest_of_geostrophic, field_ctl)) then
        call add_phys_name_ctl(Coriolis_force, field_ctl)
        call add_phys_name_ctl(pressure_gradient, field_ctl)
      end if
!
      if(check_field_list_ctl(poynting_flux, field_ctl)) then
        call add_phys_name_ctl(electric_field, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
      if(check_field_list_ctl(electric_field, field_ctl)) then
        call add_phys_name_ctl(vecp_induction, field_ctl)
        call add_phys_name_ctl(current_density, field_ctl)
      end if
!
      if(     check_field_list_ctl(truncated_magnetic_field, field_ctl) &
     &   .or. check_field_list_ctl(magnetic_intensity, field_ctl)       &
     &   .or. check_field_list_ctl(declination, field_ctl)              &
     &   .or. check_field_list_ctl(inclination, field_ctl)              &
     &   .or. check_field_list_ctl(vgp_latitude, field_ctl)             &
     &   .or. check_field_list_ctl(vgp_longigude, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
!
      if(check_field_list_ctl(Lorentz_work_dipole, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Lorentz_force_dipole, field_ctl)
      end if
      if(check_field_list_ctl(Lorentz_force_dipole, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(current_for_dipole, field_ctl)
      end if
!
      if(check_field_list_ctl(kinetic_helicity, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(vorticity, field_ctl)
      end if
      if(check_field_list_ctl(magnetic_helicity, field_ctl)) then
        call add_phys_name_ctl(vector_potential, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
      if(check_field_list_ctl(current_helicity, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(current_density, field_ctl)
      end if
      if(check_field_list_ctl(cross_helicity, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
!
      if(      check_field_list_ctl(square_velocity, field_ctl)         &
     &    .or. check_field_list_ctl(velocity_scale, field_ctl)          &
     &    .or. check_field_list_ctl(stream_pol_velocity, field_ctl))    &
     &   call add_phys_name_ctl(velocity, field_ctl)
      if(check_field_list_ctl(square_vorticity, field_ctl))             &
     &   call add_phys_name_ctl(vorticity, field_ctl)
      if(      check_field_list_ctl(square_magne, field_ctl)            &
     &    .or. check_field_list_ctl(magnetic_scale, field_ctl)          &
     &    .or. check_field_list_ctl(stream_pol_magne, field_ctl)        &
     &    .or. check_field_list_ctl(magnetic_dipole, field_ctl))        &
     &   call add_phys_name_ctl(magnetic_field, field_ctl)
      if(check_field_list_ctl(square_vector_potential, field_ctl))      &
     &   call add_phys_name_ctl(vector_potential, field_ctl)
      if(      check_field_list_ctl(square_current, field_ctl)          &
     &    .or. check_field_list_ctl(current_for_dipole, field_ctl))     &
     &   call add_phys_name_ctl(current_density, field_ctl)
      if(      check_field_list_ctl(square_temperature, field_ctl)      &
     &    .or. check_field_list_ctl(temperature_scale, field_ctl))      &
     &   call add_phys_name_ctl(temperature, field_ctl)
      if(      check_field_list_ctl(square_composition, field_ctl)      &
     &    .or. check_field_list_ctl(composition_scale, field_ctl))      &
     &   call add_phys_name_ctl(composition, field_ctl)
!
      end subroutine add_field_ctl_4_field_products
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_diffusions(field_ctl)
!
      use t_control_array_character3
      use m_diffusion_term_labels
      use m_base_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_viscousity, field_ctl))               &
     &   call add_phys_name_ctl(viscous_diffusion, field_ctl)
!
      if(check_field_list_ctl(viscous_diffusion, field_ctl))            &
     &   call add_phys_name_ctl(velocity, field_ctl)
      if(check_field_list_ctl(vorticity_diffusion, field_ctl))          &
     &   call add_phys_name_ctl(vorticity, field_ctl)
      if(check_field_list_ctl(magnetic_diffusion, field_ctl))           &
     &   call add_phys_name_ctl(magnetic_field, field_ctl)
      if(check_field_list_ctl(vector_potential_diffusion, field_ctl))   &
     &   call add_phys_name_ctl(vector_potential, field_ctl)
      if(check_field_list_ctl(thermal_diffusion, field_ctl))            &
     &   call add_phys_name_ctl(temperature, field_ctl)
      if(check_field_list_ctl(composition_diffusion, field_ctl))        &
     &   call add_phys_name_ctl(composition, field_ctl)
!
      end subroutine add_field_ctl_4_diffusions
!
! -----------------------------------------------------------------------
!
      end module check_base_forces
