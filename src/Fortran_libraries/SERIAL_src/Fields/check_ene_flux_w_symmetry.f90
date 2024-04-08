!>@file   check_ene_flux_w_symmetry.f90
!!        module check_ene_flux_w_symmetry
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Check Dependecies for energy flux with symmetry
!!
!!@verbatim
!!      subroutine add_ene_flux_by_sym_sym_ctl(field_ctl)
!!      subroutine add_ene_flux_by_asym_asym_ctl(field_ctl)
!!      subroutine add_ene_flux_by_sym_asym_ctl(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!!
      module check_ene_flux_w_symmetry
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use m_field_w_symmetry_labels
      use m_energy_flux_w_sym_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_ene_flux_by_sym_sym_ctl(field_ctl)
!
      use t_control_array_character3
      use m_base_field_labels
      use m_force_w_sym_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      if(     check_field_list_ctl(u_dot_wsym_x_usym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(wsym_x_usym, field_ctl)
      end if
      if(check_field_list_ctl(rev_u_dot_Jsym_x_Bsym, field_ctl)         &
     &   .or. check_field_list_ctl(u_dot_Jsym_x_Bsym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Jsym_x_Bsym, field_ctl)
      end if
      if(check_field_list_ctl(u_dot_Bsym_nabla_Bsym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Bsym_nabla_Bsym, field_ctl)
!
      end if
      if(check_field_list_ctl(sym_termal_buo_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(sym_thermal_buoyancy, field_ctl)
      end if
      if(check_field_list_ctl(sym_composite_buo_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(sym_composite_buoyancy, field_ctl)
!
      end if
      if(check_field_list_ctl(B_rot_Bsym_x_usym, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(rot_usym_x_Bsym, field_ctl)
      end if
      if(check_field_list_ctl(B_dot_Bsym_nabla_usym, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(Bsym_nabla_usym, field_ctl)
      end if
!
      if(check_field_list_ctl(T_usym_nabla_Tsym, field_ctl)) then
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(usym_nabla_Tsym, field_ctl)
      end if
      if(check_field_list_ctl(pT_usym_nabla_pTsym, field_ctl)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
        call add_phys_name_ctl(usym_nabla_pTsym, field_ctl)
      end if
!
      if(check_field_list_ctl(C_usym_nabla_Csym, field_ctl)) then
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(usym_nabla_Csym, field_ctl)
      end if
      if(check_field_list_ctl(pC_usym_nabla_pCsym, field_ctl)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
        call add_phys_name_ctl(usym_nabla_pCsym, field_ctl)
      end if
!
      end subroutine add_ene_flux_by_sym_sym_ctl
!
! ----------------------------------------------------------------------
!
      subroutine add_ene_flux_by_asym_asym_ctl(field_ctl)
!
      use t_control_array_character3
      use m_base_field_labels
      use m_force_w_sym_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(u_dot_wasym_x_uasym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(wasym_x_uasym, field_ctl)
      end if
      if(check_field_list_ctl(rev_u_dot_Jasym_x_Basym, field_ctl)       &
     &   .or. check_field_list_ctl(u_dot_Jasym_x_Basym, field_ctl)      &
     &   ) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Jasym_x_Basym, field_ctl)
      end if
      if(check_field_list_ctl(u_dot_Basym_nabla_Basym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Basym_nabla_Basym, field_ctl)
      end if
!
      if(check_field_list_ctl(asym_termal_buo_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(asym_thermal_buoyancy, field_ctl)
      end if
      if(check_field_list_ctl(asym_composite_buo_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(asym_composite_buoyancy, field_ctl)
      end if
!
      if(check_field_list_ctl(B_rot_Basym_x_uasym, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(rot_uasym_x_Basym, field_ctl)
      end if
      if(check_field_list_ctl(B_dot_Basym_nabla_uasym, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(Basym_nabla_uasym, field_ctl)
      end if
!
      if(check_field_list_ctl(T_uasym_nabla_Tasym, field_ctl)) then
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(uasym_nabla_Tasym, field_ctl)
      end if
      if(check_field_list_ctl(pT_uasym_nabla_pTasym, field_ctl)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
        call add_phys_name_ctl(uasym_nabla_pTasym, field_ctl)
      end if
!
      if(check_field_list_ctl(C_uasym_nabla_Casym, field_ctl)) then
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(uasym_nabla_Casym, field_ctl)
      end if
      if(check_field_list_ctl(pC_uasym_nabla_pCasym, field_ctl)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
        call add_phys_name_ctl(uasym_nabla_pCasym, field_ctl)
      end if
!
      end subroutine add_ene_flux_by_asym_asym_ctl
!
! ----------------------------------------------------------------------
!
      subroutine add_ene_flux_by_sym_asym_ctl(field_ctl)
!
      use t_control_array_character3
      use m_base_field_labels
      use m_force_w_sym_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(     check_field_list_ctl(u_dot_wsym_x_uasym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(wsym_x_uasym, field_ctl)
      end if
      if(check_field_list_ctl(rev_u_dot_Jsym_x_Basym, field_ctl)        &
     &   .or. check_field_list_ctl(u_dot_Jsym_x_Basym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Jsym_x_Basym, field_ctl)
      end if
      if(check_field_list_ctl(u_dot_Bsym_nabla_Basym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Bsym_nabla_Basym, field_ctl)
      end if
!
      if(check_field_list_ctl(B_rot_Bsym_x_uasym, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(rot_usym_x_Basym, field_ctl)
      end if
      if(check_field_list_ctl(B_dot_Bsym_nabla_uasym, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(Bsym_nabla_uasym, field_ctl)
      end if
!
      if(check_field_list_ctl(T_usym_nabla_Tasym, field_ctl)) then
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(usym_nabla_Tasym, field_ctl)
      end if
      if(check_field_list_ctl(pT_usym_nabla_pTasym, field_ctl)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
        call add_phys_name_ctl(usym_nabla_pTasym, field_ctl)
      end if
!
      if(check_field_list_ctl(C_usym_nabla_Casym, field_ctl)) then
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(usym_nabla_Casym, field_ctl)
      end if
      if(check_field_list_ctl(pC_usym_nabla_pCasym, field_ctl)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
        call add_phys_name_ctl(usym_nabla_pCasym, field_ctl)
      end if
!
!
      if(check_field_list_ctl(u_dot_wasym_x_usym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(wasym_x_usym, field_ctl)
      end if
!
      if(check_field_list_ctl(rev_u_dot_Jasym_x_Bsym, field_ctl)        &
     &    .or.check_field_list_ctl(u_dot_Jasym_x_Bsym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Jasym_x_Bsym, field_ctl)
      end if
      if(check_field_list_ctl(u_dot_Basym_nabla_Bsym, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Basym_nabla_Bsym, field_ctl)
      end if
!
      if(check_field_list_ctl(B_rot_Basym_x_usym, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(rot_uasym_x_Bsym, field_ctl)
      end if
      if(check_field_list_ctl(B_dot_Basym_nabla_usym, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(Basym_nabla_usym, field_ctl)
      end if
!
      if(check_field_list_ctl(T_uasym_nabla_Tsym, field_ctl)) then
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(uasym_nabla_Tsym, field_ctl)
      end if
      if(check_field_list_ctl(pT_uasym_nabla_pTsym, field_ctl)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
        call add_phys_name_ctl(uasym_nabla_pTsym, field_ctl)
      end if
!
      if(check_field_list_ctl(C_uasym_nabla_Csym, field_ctl)) then
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(uasym_nabla_Csym, field_ctl)
      end if
      if(check_field_list_ctl(pC_uasym_nabla_pCsym, field_ctl)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
        call add_phys_name_ctl(uasym_nabla_pCsym, field_ctl)
      end if
!
      end subroutine add_ene_flux_by_sym_asym_ctl
!
! ----------------------------------------------------------------------
!
      end module check_ene_flux_w_symmetry
