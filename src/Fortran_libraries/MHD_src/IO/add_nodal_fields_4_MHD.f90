!>@file   add_nodal_fields_4_MHD.f90
!!@brief  module add_nodal_fields_4_MHD
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui Sep., 2006
!
!> @brief Add missing field for MHD dynamo to field list
!!
!!@verbatim
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
      use m_phys_labels
      use t_control_parameter
      use t_read_control_arrays
      use t_physical_property
      use add_nodal_fields_ctl
      use calypso_mpi
!
      implicit  none
!
      private :: add_work_area_4_potentials, add_ctl_4_forces
      private :: add_data_4_previous_step, add_data_4_check_step
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_mhd(MHD_prop, field_ctl)
!
      use t_reference_scalar_param
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!    set work fields for potentials
!
      if (iflag_debug.eq.1) write(*,*) 'add_work_area_4_potentials'
      call add_work_area_4_potentials                                   &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop, field_ctl)
!
!    set work fields for reference temperature
!
      call calypso_mpi_barrier
      if (iflag_debug.eq.1) write(*,*) 'add_ctl_4_forces'
      call add_ctl_4_forces                                             &
     &   (MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C, &
     &    field_ctl)
!
!     set work fields for adams-bashforth
!
      call calypso_mpi_barrier
      if (iflag_debug.eq.1) write(*,*) 'add_data_4_previous_step'
      call add_data_4_previous_step                                     &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, field_ctl)
!
!     set work fields for evolution check
!
      call calypso_mpi_barrier
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
      use t_reference_scalar_param
!
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if (ref_param_T%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl(fhd_ref_temp, field_ctl)
        call add_phys_name_ctl(fhd_grad_ref_temp, field_ctl)
      end if
!
      if (ref_param_C%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl(fhd_ref_light, field_ctl)
        call add_phys_name_ctl(fhd_grad_ref_light, field_ctl)
      end if
!
      end subroutine add_ctl_4_ref_temp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_ctl_4_forces                                       &
     &         (fl_prop, ref_param_T, ref_param_C, field_ctl)
!
      use t_reference_scalar_param
!
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if (ref_param_T%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
      end if
!
      if (ref_param_C%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl(fhd_part_light, field_ctl)
      end if
!
      if (fl_prop%iflag_4_coriolis .gt. id_turn_OFF)                    &
     &              call add_phys_name_ctl(fhd_Coriolis, field_ctl)
      if (fl_prop%iflag_4_gravity .eq. id_FORCE_at_node)                &
     &              call add_phys_name_ctl(fhd_buoyancy, field_ctl)
      if (fl_prop%iflag_4_composit_buo .eq. id_FORCE_at_node)           &
     &              call add_phys_name_ctl(fhd_comp_buo, field_ctl)
      if (fl_prop%iflag_4_filter_gravity .eq. id_FORCE_at_node)         &
     &              call add_phys_name_ctl(fhd_filter_buo, field_ctl)
!
      end subroutine add_ctl_4_forces
!
! -----------------------------------------------------------------------
!
      subroutine add_work_area_4_potentials                             &
     &         (fl_prop, cd_prop, field_ctl)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!    set work fields for potentials
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_press_work, field_ctl)
      end if
      if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution                &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_m_potential_work, field_ctl)
      end if
!
      end subroutine add_work_area_4_potentials
!
! -----------------------------------------------------------------------
!
      subroutine add_data_4_previous_step                               &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, field_ctl)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(fl_prop%iflag_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(fhd_pre_mom, field_ctl)
        call add_phys_name_ctl(fhd_pre_press, field_ctl)
!
        call add_phys_name_ctl(fhd_forces, field_ctl)
        call add_phys_name_ctl(fhd_div_forces, field_ctl)
      end if
      if      (cd_prop%iflag_Bevo_scheme .ne. id_no_evolution           &
     &    .or. cd_prop%iflag_Aevo_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(fhd_pre_uxb, field_ctl)
      end if
      if(ht_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(fhd_pre_heat, field_ctl)
      end if
      if(cp_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(fhd_pre_composit, field_ctl)
      end if
!
      end subroutine add_data_4_previous_step
!
! -----------------------------------------------------------------------
!
      subroutine add_data_4_check_step                                  &
     &         (fl_prop, cd_prop,  ht_prop, cp_prop, field_ctl)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(fl_prop%iflag_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(fhd_chk_mom, field_ctl)
        call add_phys_name_ctl(fhd_chk_press, field_ctl)
      end if
      if(cd_prop%iflag_Bevo_scheme .ne. id_no_evolution                 &
     &     .or. cd_prop%iflag_Aevo_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(fhd_chk_uxb, field_ctl)
        call add_phys_name_ctl(fhd_chk_potential, field_ctl)
      end if
      if(ht_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(fhd_chk_heat, field_ctl)
      end if
      if(cp_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(fhd_chk_composit, field_ctl)
      end if
!
!      if(fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_mom_2, field_ctl)
!        call add_phys_name_ctl(fhd_chk_press_2, field_ctl)
!      end if
!      if     (cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson         &
!     &   .or. cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_uxb_2, field_ctl)
!        call add_phys_name_ctl(fhd_chk_potential_2, field_ctl)
!      end if
!      if(ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_heat_2, field_ctl)
!      end if
!      if(cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_composit_2, field_ctl)
!      end if
!
      end subroutine add_data_4_check_step
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_4_MHD
