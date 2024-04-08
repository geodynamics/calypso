!>@file   add_nodal_fields_4_MHD.f90
!!@brief  module add_nodal_fields_4_MHD
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui Sep., 2006
!
!> @brief Add missing field for MHD dynamo to field list
!!
!!@verbatim
!!      subroutine add_dependent_field(MHD_prop, field_ctl)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!      subroutine add_field_name_4_mhd(MHD_prop, field_ctl)
!!      subroutine add_ctl_4_ref_temp                                   &
!!     &         (ref_param_T, ref_param_C, field_ctl)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_nodal_fields_4_MHD
!
      use m_precision
!
      use m_machine_parameter
      use t_control_parameter
      use t_physical_property
      use add_nodal_fields_ctl
      use calypso_mpi
!
      implicit  none
!
      private :: add_work_area_4_potentials, add_ctl_4_forces
      private :: add_data_4_previous_step, add_data_4_check_step
      private :: add_field_ctl_4_evo_magne
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_dependent_field(MHD_prop, field_ctl)
!
      use check_energy_fluxes
      use check_base_forces
      use check_base_field
      use check_field_w_symmetry
      use check_forces_w_symmetry
      use check_ene_flux_w_symmetry
      use check_workarea_4_explicit
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
!
      call add_ene_flux_by_sym_asym_ctl(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_ene_flux_by_sym_asym_ctl end'
      call add_ene_flux_by_sym_sym_ctl(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_ene_flux_by_sym_sym_ctl end'
      call add_ene_flux_by_asym_asym_ctl(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_ene_flux_by_asym_asym_ctl end'
!
      call add_force_by_sym_asym_ctl(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_force_by_sym_asym_ctl end'
      call add_force_by_asym_asym_ctl(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_force_by_asym_asym_ctl end'
      call add_force_by_sym_sym_ctl(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_force_by_sym_sym_ctl end'
!
      call add_field_w_symmetry_ctl(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_w_symmetry_ctl end'
!
      call add_field_ctl_4_check_evo(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_check_evo end'
      call add_field_ctl_4_evo_magne(MHD_prop%cd_prop, field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_evo_magne end'
!
      call add_field_ctl_4_ene_flux(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_ene_flux end'
      call add_field_ctl_4_field_products(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_field_products end'
!
      call add_field_ctl_4_diffusions(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_diffusions end'
!
      call add_field_ctl_4_div_forces(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_div_forces end'
      call add_field_ctl_4_rot_forces(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_rot_forces end'
      call add_field_ctl_4_forces(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_forces end'
!
      call add_field_ctl_4_field_comps(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_field_comps end'
!
      call add_field_ctl_4_diff_vector(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_diff_vector end'
      call add_field_ctl_4_grad_field(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_grad_field end'
!
      call add_field_ctl_4_base_field(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_base_field end'
!
      end subroutine add_dependent_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_mhd(MHD_prop, field_ctl)
!
      use t_control_array_character3
      use t_reference_scalar_param
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
!    set work fields for potentials
!
      if (iflag_debug.eq.1) write(*,*) 'add_work_area_4_potentials'
      call add_work_area_4_potentials                                   &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop, field_ctl)
!
!    set work fields for reference temperature
!
      if (iflag_debug.eq.1) write(*,*) 'add_ctl_4_forces'
      call add_ctl_4_forces                                             &
     &   (MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C, &
     &    field_ctl)
!
!     set work fields for adams-bashforth
!
      if (iflag_debug.eq.1) write(*,*) 'add_data_4_previous_step'
      call add_data_4_previous_step                                     &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, field_ctl)
!
!     set work fields for evolution check
!
      if (iflag_debug.eq.1) write(*,*) 'add_data_4_check_step'
      call add_data_4_check_step                                        &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, field_ctl)
!
      end subroutine add_field_name_4_mhd
!
! -----------------------------------------------------------------------
!
      subroutine add_ctl_4_ref_temp                                     &
     &        (ref_param_T, ref_param_C, field_ctl)
!
      use t_control_array_character3
      use t_reference_scalar_param
      use m_base_field_labels
      use m_grad_field_labels
!
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      end subroutine add_ctl_4_ref_temp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_ctl_4_forces                                       &
     &         (fl_prop, ref_param_T, ref_param_C, field_ctl)
!
      use t_control_array_character3
      use t_reference_scalar_param
      use m_base_field_labels
      use m_base_force_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if (ref_param_T%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
      end if
!
      if (ref_param_C%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
      end if
!
      if(fl_prop%iflag_4_coriolis)                                      &
     &  call add_phys_name_ctl(Coriolis_force, field_ctl)
!
      if(fl_prop%iflag_FEM_gravity .eq. id_FORCE_at_node) then
        if(fl_prop%iflag_4_gravity)                                     &
     &    call add_phys_name_ctl(buoyancy, field_ctl)
        if(fl_prop%iflag_4_composit_buo)                                &
     &    call add_phys_name_ctl(composite_buoyancy, field_ctl)
      end if
!
      end subroutine add_ctl_4_forces
!
! -----------------------------------------------------------------------
!
      subroutine add_work_area_4_potentials                             &
     &         (fl_prop, cd_prop, field_ctl)
!
      use t_control_array_character3
      use m_base_field_labels
      use m_explicit_term_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!    set work fields for potentials
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(pressure_work, field_ctl)
      end if
      if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution                &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(m_potential_work, field_ctl)
      end if
      if(cd_prop%iflag_magneto_cv .eq. id_turn_ON) then
        call add_phys_name_ctl(background_B, field_ctl)
      end if
!
      end subroutine add_work_area_4_potentials
!
! -----------------------------------------------------------------------
!
      subroutine add_data_4_previous_step                               &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, field_ctl)
!
      use t_control_array_character3
      use m_explicit_term_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(fl_prop%iflag_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(previous_momentum, field_ctl)
        call add_phys_name_ctl(previous_pressure, field_ctl)
!
        call add_phys_name_ctl(sum_forces, field_ctl)
!        call add_phys_name_ctl(div_sum_forces, field_ctl)
      end if
      if      (cd_prop%iflag_Bevo_scheme .ne. id_no_evolution           &
     &    .or. cd_prop%iflag_Aevo_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(previous_induction, field_ctl)
      end if
      if(ht_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(previous_heat, field_ctl)
      end if
      if(cp_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(previous_composition, field_ctl)
      end if
!
      end subroutine add_data_4_previous_step
!
! -----------------------------------------------------------------------
!
      subroutine add_data_4_check_step                                  &
     &         (fl_prop, cd_prop,  ht_prop, cp_prop, field_ctl)
!
      use t_control_array_character3
      use m_explicit_term_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(fl_prop%iflag_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(check_momentum, field_ctl)
        call add_phys_name_ctl(check_pressure, field_ctl)
      end if
      if(cd_prop%iflag_Bevo_scheme .ne. id_no_evolution                 &
     &     .or. cd_prop%iflag_Aevo_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(check_induction, field_ctl)
        call add_phys_name_ctl(check_potential, field_ctl)
      end if
      if(ht_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(check_heat, field_ctl)
      end if
      if(cp_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(check_composition, field_ctl)
      end if
!
!      if(fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(check_momentum_2, field_ctl)
!        call add_phys_name_ctl(check_pressure_2, field_ctl)
!      end if
!      if     (cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson         &
!     &   .or. cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(check_induction_2, field_ctl)
!        call add_phys_name_ctl(check_potential_2, field_ctl)
!      end if
!      if(ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(check_heat_2, field_ctl)
!      end if
!      if(cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(check_composition_2, field_ctl)
!      end if
!
      end subroutine add_data_4_check_step
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_evo_magne(cd_prop, field_ctl)
!
      use t_physical_property
      use t_explicit_term_labels
      use m_base_field_labels
      use m_explicit_term_labels
      use add_nodal_fields_ctl
!
      type(conductive_property), intent(in) :: cd_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(check_field_list_ctl(check_induction, field_ctl))            &
     &     call add_phys_name_ctl(vector_potential, field_ctl)
      else
        if(check_field_list_ctl(check_induction, field_ctl))            &
     &     call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_evo_magne
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_4_MHD
