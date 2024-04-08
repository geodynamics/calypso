!>@file   check_energy_fluxes.f90
!!        module check_energy_fluxes
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Check Dependecies for energy fluxes
!!
!!@verbatim
!!      subroutine add_field_ctl_4_ene_flux(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_energy_fluxes
!
      use m_precision
      use m_constants
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_ene_flux(field_ctl)
!
      use t_control_array_character3
      use m_base_field_labels
      use m_base_force_labels
      use m_diffusion_term_labels
      use m_energy_flux_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(inertia_work, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(inertia, field_ctl)
      end if
      if(check_field_list_ctl(work_against_Lorentz, field_ctl)          &
     &   .or. check_field_list_ctl(Lorentz_work, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(current_density, field_ctl)
        call add_phys_name_ctl(Lorentz_force, field_ctl)
      end if
      if(check_field_list_ctl(mag_tension_work, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(magnetic_tension, field_ctl)
      end if
!
      if(check_field_list_ctl(buoyancy_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(buoyancy, field_ctl)
      end if
      if(check_field_list_ctl(composite_buoyancy_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(composite_buoyancy, field_ctl)
      end if
!
      if(check_field_list_ctl(magnetic_ene_generation, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(magnetic_induction, field_ctl)
      end if
      if(check_field_list_ctl(magnetic_stretch_flux, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(magnetic_stretch, field_ctl)
      end if
!
      if(check_field_list_ctl(temp_generation, field_ctl)) then
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(heat_advect, field_ctl)
      end if
      if(check_field_list_ctl(pert_temp_generation, field_ctl)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
        call add_phys_name_ctl(pert_heat_advect, field_ctl)
      end if
      if(check_field_list_ctl(comp_generation, field_ctl)) then
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(composition_advect, field_ctl)
      end if
      if(check_field_list_ctl(pert_comp_generation, field_ctl)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
        call add_phys_name_ctl(pert_comp_advect, field_ctl)
      end if
!
      if(check_field_list_ctl(viscous_ene_diffusion, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(viscous_diffusion, field_ctl)
      end if
      if(check_field_list_ctl(magnetic_ene_diffusion, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(magnetic_diffusion, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_ene_flux
!
! -----------------------------------------------------------------------
!
      end module check_energy_fluxes
