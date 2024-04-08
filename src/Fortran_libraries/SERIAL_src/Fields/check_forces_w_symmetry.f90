!>@file   check_forces_w_symmetry.f90
!!        module check_forces_w_symmetry
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!>@brief Check Dependecies for forces with symmetry
!!
!!@verbatim
!!      subroutine add_force_by_sym_asym_ctl(field_ctl)
!!      subroutine add_force_by_asym_asym_ctl(field_ctl)
!!      subroutine add_force_by_sym_sym_ctl(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_forces_w_symmetry
!
      use m_precision
      use m_constants
!
      use t_base_force_labels
      use m_field_w_symmetry_labels
      use m_force_w_sym_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_force_by_sym_sym_ctl(field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(rot_usym_x_Bsym, field_ctl)) then
        call add_phys_name_ctl(usym_x_Bsym, field_ctl)
      end if
!
      if(      check_field_list_ctl(wsym_x_usym, field_ctl)             &
     &    .or. check_field_list_ctl(m_flux_sym_sym, field_ctl) ) then
        call add_phys_name_ctl(sym_vorticity, field_ctl)
        call add_phys_name_ctl(sym_velocity, field_ctl)
      end if
      if( check_field_list_ctl(Jsym_x_Bsym, field_ctl) ) then
        call add_phys_name_ctl(sym_current_density, field_ctl)
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
      end if
      if( check_field_list_ctl(maxwell_tensor_sym_sym, field_ctl)       &
     &    .or. check_field_list_ctl(Bsym_nabla_Bsym, field_ctl) ) then
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
      end if
!
      if( check_field_list_ctl(sym_thermal_buoyancy, field_ctl)) then
        call add_phys_name_ctl(sym_temperature, field_ctl)
      end if
      if( check_field_list_ctl(sym_composite_buoyancy, field_ctl)) then
        call add_phys_name_ctl(sym_composition, field_ctl)
      end if
!
      if( check_field_list_ctl(usym_x_Bsym, field_ctl)                  &
     &    .or. check_field_list_ctl(rot_usym_x_Bsym, field_ctl)         &
     &    .or. check_field_list_ctl(Bsym_nabla_usym, field_ctl)         &
     &    .or. check_field_list_ctl(usym_Bsym, field_ctl) ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
      end if
!
      if( check_field_list_ctl(usym_nabla_Tsym, field_ctl)              &
     &    .or. check_field_list_ctl(heat_flux_sym_sym, field_ctl)) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(sym_temperature, field_ctl)
      end if
      if( check_field_list_ctl(usym_nabla_pTsym, field_ctl)             &
     &    .or. check_field_list_ctl(pert_h_flux_sym_sym, field_ctl)     &
     &  ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(sym_perturbation_temp, field_ctl)
      end if
      if( check_field_list_ctl(usym_nabla_Csym, field_ctl)              &
     &    .or. check_field_list_ctl(composite_flux_sym_sym, field_ctl)  &
     &  ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(sym_composition, field_ctl)
      end if
      if( check_field_list_ctl(usym_nabla_pCsym, field_ctl)             &
     &    .or. check_field_list_ctl(pert_c_flux_sym_sym, field_ctl)     &
     &  ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(sym_perturbation_composition, field_ctl)
      end if
!
      end subroutine add_force_by_sym_sym_ctl
!
! ----------------------------------------------------------------------
!
      subroutine add_force_by_asym_asym_ctl(field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(rot_uasym_x_Basym, field_ctl)) then
        call add_phys_name_ctl(uasym_x_Basym, field_ctl)
      end if
!
      if(      check_field_list_ctl(wasym_x_uasym, field_ctl)           &
     &    .or. check_field_list_ctl(m_flux_asym_asym, field_ctl) ) then
        call add_phys_name_ctl(asym_vorticity, field_ctl)
        call add_phys_name_ctl(asym_velocity, field_ctl)
      end if
      if( check_field_list_ctl(Jasym_x_Basym, field_ctl) ) then
        call add_phys_name_ctl(asym_current_density, field_ctl)
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
      end if
      if( check_field_list_ctl(maxwell_tensor_asym_asym, field_ctl)     &
     &    .or. check_field_list_ctl(Basym_nabla_Basym, field_ctl)) then
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
      end if
!
      if(check_field_list_ctl(asym_thermal_buoyancy, field_ctl)) then
        call add_phys_name_ctl(asym_temperature, field_ctl)
      end if
      if(check_field_list_ctl(asym_composite_buoyancy, field_ctl)) then
        call add_phys_name_ctl(asym_composition, field_ctl)
      end if
!
      if( check_field_list_ctl(uasym_x_Basym, field_ctl)                &
     &    .or. check_field_list_ctl(Basym_nabla_uasym, field_ctl)       &
     &    .or. check_field_list_ctl(uasym_Basym, field_ctl) ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
      end if
      if( check_field_list_ctl(uasym_nabla_Tasym, field_ctl)            &
     &    .or. check_field_list_ctl(heat_flux_asym_asym, field_ctl)     &
     &  ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(asym_temperature, field_ctl)
      end if
      if( check_field_list_ctl(uasym_nabla_pTasym, field_ctl)           &
     &    .or. check_field_list_ctl(pert_h_flux_asym_asym, field_ctl)   &
     &  ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(asym_perturbation_temp, field_ctl)
      end if
!
      if( check_field_list_ctl(uasym_nabla_Casym, field_ctl)            &
     &   .or. check_field_list_ctl(composite_flux_asym_asym, field_ctl) &
     &  ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(asym_composition, field_ctl)
      end if
      if( check_field_list_ctl(uasym_nabla_pCasym, field_ctl)           &
     &    .or. check_field_list_ctl(pert_c_flux_asym_asym, field_ctl)   &
     &  ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl                                          &
     &     (asym_perturbation_composition, field_ctl)
      end if
!
      end subroutine add_force_by_asym_asym_ctl
!
! ----------------------------------------------------------------------
!
      subroutine add_force_by_sym_asym_ctl(field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(rot_usym_x_Basym, field_ctl)) then
        call add_phys_name_ctl(usym_x_Basym, field_ctl)
      end if
      if(check_field_list_ctl(rot_uasym_x_Bsym, field_ctl)) then
        call add_phys_name_ctl(uasym_x_Bsym, field_ctl)
      end if
!
      if(      check_field_list_ctl(wsym_x_uasym, field_ctl)            &
     &    .or. check_field_list_ctl(m_flux_sym_asym, field_ctl) ) then
        call add_phys_name_ctl(sym_vorticity, field_ctl)
        call add_phys_name_ctl(asym_velocity, field_ctl)
      end if
      if( check_field_list_ctl(Jsym_x_Basym, field_ctl)  ) then
        call add_phys_name_ctl(sym_current_density, field_ctl)
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
      end if
!
      if( check_field_list_ctl(maxwell_tensor_sym_asym, field_ctl)      &
     &    .or. check_field_list_ctl(Bsym_nabla_Basym, field_ctl) ) then
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
      end if
!
      if( check_field_list_ctl(usym_x_Basym, field_ctl)                 &
     &    .or. check_field_list_ctl(Bsym_nabla_uasym, field_ctl)        &
     &    .or. check_field_list_ctl(usym_Basym, field_ctl) ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
      end if
!
      if( check_field_list_ctl(usym_nabla_Tasym, field_ctl)             &
     &    .or. check_field_list_ctl(heat_flux_sym_asym, field_ctl)      &
     &   ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(asym_temperature, field_ctl)
      end if
      if( check_field_list_ctl(usym_nabla_pTasym, field_ctl)            &
     &    .or. check_field_list_ctl(pert_h_flux_sym_asym, field_ctl)    &
     &   ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(asym_perturbation_temp, field_ctl)
      end if
      if( check_field_list_ctl(usym_nabla_Casym, field_ctl)             &
     &    .or. check_field_list_ctl(composite_flux_sym_asym, field_ctl) &
     &   ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(asym_composition, field_ctl)
      end if
      if( check_field_list_ctl(usym_nabla_pCasym, field_ctl)            &
     &    .or. check_field_list_ctl(pert_c_flux_sym_asym, field_ctl)    &
     &   ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl                                          &
     &     (asym_perturbation_composition, field_ctl)
      end if
!
!
      if( check_field_list_ctl(wasym_x_usym, field_ctl)) then
        call add_phys_name_ctl(asym_vorticity, field_ctl)
        call add_phys_name_ctl(sym_velocity, field_ctl)
      end if
!
      if( check_field_list_ctl(Jasym_x_Bsym, field_ctl)) then
        call add_phys_name_ctl(asym_current_density, field_ctl)
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
      end if
!
      if( check_field_list_ctl(Basym_nabla_Bsym, field_ctl)) then
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
      end if
!
      if( check_field_list_ctl(uasym_x_Bsym, field_ctl)                 &
     &    .or. check_field_list_ctl(Basym_nabla_usym, field_ctl)        &
     &    .or. check_field_list_ctl(uasym_Bsym, field_ctl)) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
      end if
!
      if( check_field_list_ctl(uasym_nabla_Tsym, field_ctl)             &
     &    .or. check_field_list_ctl(heat_flux_asym_sym, field_ctl)      &
     &  ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(asym_temperature, field_ctl)
      end if
!
      if( check_field_list_ctl(uasym_nabla_pTsym, field_ctl)            &
     &    .or. check_field_list_ctl(pert_h_flux_asym_sym, field_ctl)    &
     &  ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(sym_perturbation_temp, field_ctl)
      end if
      if( check_field_list_ctl(uasym_nabla_Csym, field_ctl)             &
     &    .or. check_field_list_ctl(composite_flux_asym_sym, field_ctl) &
     &  ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(sym_composition, field_ctl)
      end if
!
      if( check_field_list_ctl(uasym_nabla_pCsym, field_ctl)            &
     &    .or. check_field_list_ctl(pert_c_flux_asym_sym, field_ctl)    &
     &  ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(sym_perturbation_composition, field_ctl)
      end if
!
      end subroutine add_force_by_sym_asym_ctl
!
! ----------------------------------------------------------------------
!
      end module check_forces_w_symmetry
