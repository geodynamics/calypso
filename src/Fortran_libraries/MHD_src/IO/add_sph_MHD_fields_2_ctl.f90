!>@file   add_sph_MHD_fields_2_ctl.f90
!!@brief  module add_sph_MHD_fields_2_ctl
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Add fields in control list for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_field_name_4_sph_mhd                             &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, field_ctl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_sph_MHD_fields_2_ctl
!
      use m_precision
!
      use m_phys_labels
      use t_control_array_character3
      use t_physical_property
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_sph_mhd                               &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, field_ctl)
!
      use add_nodal_fields_ctl
      use m_rot_force_labels
      use m_div_force_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
!   velocity flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution                      &
     &     .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(velocity%name, field_ctl)
      end if
!   vorticity flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(vorticity%name, field_ctl)
      end if
!   magnetic field flag
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution                 &
     &     .or. fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(current_density%name, field_ctl)
      end if
!
!   gradient of temperature flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(grad_temp%name, field_ctl)
        call add_phys_name_ctl(perturbation_temp%name, field_ctl)
        call add_phys_name_ctl(grad_pert_temp%name, field_ctl)
      end if
!
!   gradient of dummy scalar flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(grad_composition%name, field_ctl)
        call add_phys_name_ctl(perturbation_composition%name,           &
    &       field_ctl)
        call add_phys_name_ctl(grad_pert_composition%name, field_ctl)
      end if
!
!
!
!   advection flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(viscous_diffusion%name, field_ctl)
        call add_phys_name_ctl(div_viscousity%name, field_ctl)
        call add_phys_name_ctl(vorticity_diffusion%name, field_ctl)
!
        call add_phys_name_ctl(inertia%name, field_ctl)
        call add_phys_name_ctl(rot_inertia%name, field_ctl)
        call add_phys_name_ctl(div_inertia%name, field_ctl)
!
!   Coriolis flag
        if(fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          call add_phys_name_ctl(Coriolis_force%name, field_ctl)
          call add_phys_name_ctl(rot_Coriolis_force%name, field_ctl)
          call add_phys_name_ctl(div_Coriolis_force%name, field_ctl)
        end if
!   Lorentz flag
        if(fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
          call add_phys_name_ctl(Lorentz_force%name, field_ctl)
          call add_phys_name_ctl(rot_Lorentz_force%name, field_ctl)
          call add_phys_name_ctl(div_Lorentz_force%name, field_ctl)
        end if
!   thermal buoyancy flag
        if(fl_prop%iflag_4_gravity .gt. id_turn_OFF) then
          call add_phys_name_ctl(buoyancy%name, field_ctl)
          call add_phys_name_ctl(rot_buoyancy%name, field_ctl)
          call add_phys_name_ctl(div_buoyancy%name, field_ctl)
        end if
!   compositional buoyancy flag
        if(fl_prop%iflag_4_composit_buo .gt. id_turn_OFF) then
          call add_phys_name_ctl(composite_buoyancy%name, field_ctl)
          call add_phys_name_ctl                                        &
     &       (div_composite_buoyancy%name, field_ctl)
          call add_phys_name_ctl                                        &
     &       (rot_composite_buoyancy%name, field_ctl)
        end if
      end if
!
!   induction flag
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(magnetic_diffusion%name, field_ctl)
!
        call add_phys_name_ctl(magnetic_induction%name, field_ctl)
        call add_phys_name_ctl(vecp_induction%name, field_ctl)
      end if
!
!   divergence of heat flux flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(thermal_diffusion%name, field_ctl)
        call add_phys_name_ctl(heat_flux%name, field_ctl)
        call add_phys_name_ctl(heat_advect%name, field_ctl)
      end if
!
!   divergence of dummy scalar flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(composition_diffusion%name, field_ctl)
        call add_phys_name_ctl(composite_flux%name, field_ctl)
        call add_phys_name_ctl(composition_advect%name, field_ctl)
      end if
!
      end subroutine add_field_name_4_sph_mhd
!
! -----------------------------------------------------------------------
!
      end module add_sph_MHD_fields_2_ctl
