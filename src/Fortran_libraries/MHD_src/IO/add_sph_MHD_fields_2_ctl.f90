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
        call add_phys_name_ctl(fhd_velo, field_ctl)
      end if
!   vorticity flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_vort, field_ctl)
      end if
!   magnetic field flag
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution                 &
     &     .or. fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_current, field_ctl)
      end if
!
!   gradient of temperature flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_grad_temp, field_ctl)
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
        call add_phys_name_ctl(fhd_grad_par_temp, field_ctl)
      end if
!
!   gradient of dummy scalar flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_grad_composit, field_ctl)
        call add_phys_name_ctl(fhd_part_light, field_ctl)
        call add_phys_name_ctl(fhd_grad_par_light, field_ctl)
      end if
!
!
!
!   advection flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_viscous, field_ctl)
        call add_phys_name_ctl(fhd_div_viscous, field_ctl)
        call add_phys_name_ctl(fhd_w_viscous, field_ctl)
!
        call add_phys_name_ctl(fhd_inertia, field_ctl)
        call add_phys_name_ctl(fhd_rot_inertia, field_ctl)
        call add_phys_name_ctl(fhd_div_inertia, field_ctl)
!
!   Coriolis flag
        if(fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_Coriolis, field_ctl)
          call add_phys_name_ctl(fhd_rot_Coriolis, field_ctl)
          call add_phys_name_ctl(fhd_div_Coriolis, field_ctl)
        end if
!   Lorentz flag
        if(fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_Lorentz, field_ctl)
          call add_phys_name_ctl(fhd_rot_Lorentz, field_ctl)
          call add_phys_name_ctl(fhd_div_Lorentz, field_ctl)
        end if
!   buoyancy flag
        if(fl_prop%iflag_4_gravity .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_buoyancy, field_ctl)
          call add_phys_name_ctl(fhd_rot_buoyancy, field_ctl)
          call add_phys_name_ctl(fhd_div_buoyancy, field_ctl)
        end if
!   compositional buoyancy flag
        if(fl_prop%iflag_4_composit_buo .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_comp_buo, field_ctl)
          call add_phys_name_ctl(fhd_div_comp_buo, field_ctl)
          call add_phys_name_ctl(fhd_rot_comp_buo, field_ctl)
        end if
!   filtered buoyancy flag
        if(fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_filter_buo, field_ctl)
          call add_phys_name_ctl(fhd_div_filter_buo, field_ctl)
          call add_phys_name_ctl(fhd_rot_filter_buo, field_ctl)
        end if
      end if
!
!   induction flag
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_mag_diffuse, field_ctl)
!
        call add_phys_name_ctl(fhd_mag_induct, field_ctl)
        call add_phys_name_ctl(fhd_vp_induct, field_ctl)
      end if
!
!   divergence of heat flux flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_thermal_diffusion, field_ctl)
        call add_phys_name_ctl(fhd_h_flux, field_ctl)
        call add_phys_name_ctl(fhd_heat_advect, field_ctl)
      end if
!
!   divergence of dummy scalar flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_c_diffuse, field_ctl)
        call add_phys_name_ctl(fhd_c_flux, field_ctl)
        call add_phys_name_ctl(fhd_composit_advect, field_ctl)
      end if
!
      end subroutine add_field_name_4_sph_mhd
!
! -----------------------------------------------------------------------
!
      end module add_sph_MHD_fields_2_ctl
