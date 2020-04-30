!>@file   add_base_field_4_sph_trns.f90
!!@brief  module add_base_field_4_sph_trns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_base_vector_4_MHD_sph_trns                       &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop,                   &
!!     &          ipol_base, iphys_base, b_trns_base, trns)
!!      subroutine add_base_scalar_4_MHD_sph_trns(ht_prop, cp_prop,     &
!!     &          ipol_base, iphys_base, b_trns_base, trns)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol_base, iphys_base
!!        type(phys_address), intent(inout) :: b_trns_base
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_base_vector_sph_trns_snap                        &
!!     &         (ipol_base, iphys_base, b_trns_base, trns)
!!      subroutine add_base_scalar_sph_trns_snap                        &
!!     &         (ipol_base, iphys_base, b_trns_base, trns)
!!        type(phys_address), intent(in) :: ipol_base, iphys_base
!!        type(phys_address), intent(inout) :: b_trns_base
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_base_field_4_sph_trns
!
      use m_precision
!
      use t_base_field_labels
      use t_addresses_sph_transform
      use t_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_base_vector_4_MHD_sph_trns                         &
     &         (fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          ipol_base, iphys_base, b_trns_base, trns)
!
      use add_field_to_sph_trans_list
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(base_field_address), intent(in) :: ipol_base, iphys_base
      type(base_field_address), intent(inout) :: b_trns_base
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   velocity flag
      if(       fl_prop%iflag_scheme .gt. id_no_evolution               &
     &     .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution          &
     &     .or. ht_prop%iflag_scheme .gt. id_no_evolution               &
     &     .or. cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_4_sph_trns_by_pol(velocity,                      &
     &      ipol_base%i_velo, iphys_base%i_velo, b_trns_base%i_velo,    &
     &      trns)
      end if
!   vorticity flag
      if(       fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_4_sph_trns_by_pol(vorticity,                     &
     &      ipol_base%i_vort, iphys_base%i_vort, b_trns_base%i_vort,    &
     &      trns)
      end if
!   magnetic field flag
      if(       cd_prop%iflag_Bevo_scheme .gt. id_no_evolution          &
     &     .or. fl_prop%iflag_4_lorentz .gt.     id_turn_OFF) then
        call add_field_4_sph_trns_by_pol(magnetic_field,                &
     &      ipol_base%i_magne, iphys_base%i_magne, b_trns_base%i_magne, &
     &      trns)
      end if
!   current density flag
      if(fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
        call add_field_4_sph_trns_by_pol(current_density,               &
     &      ipol_base%i_current, iphys_base%i_current,                  &
     &      b_trns_base%i_current, trns)
      end if
!
      end subroutine add_base_vector_4_MHD_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine add_base_scalar_4_MHD_sph_trns(ht_prop, cp_prop,       &
     &          ipol_base, iphys_base, b_trns_base, trns)
!
      use add_field_to_sph_trans_list
!
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(base_field_address), intent(in) :: ipol_base, iphys_base
      type(base_field_address), intent(inout) :: b_trns_base
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   temperature flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_4_sph_trns_by_pol(temperature,                   &
     &      ipol_base%i_temp, iphys_base%i_temp, b_trns_base%i_temp,    &
     &      trns)
      end if
!   composition flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_4_sph_trns_by_pol(composition,                   &
     &      ipol_base%i_light, iphys_base%i_light, b_trns_base%i_light, &
     &      trns)
      end if
!
      end subroutine add_base_scalar_4_MHD_sph_trns
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_base_vector_sph_trns_snap                          &
     &         (ipol_base, iphys_base, b_trns_base, trns)
!
      use add_field_to_sph_trans_list
!
      type(base_field_address), intent(in) :: ipol_base, iphys_base
      type(base_field_address), intent(inout) :: b_trns_base
      type(spherical_transform_data), intent(inout) :: trns
!
!
!      if(b_trns_base%i_velo .eq. 0) then
      call add_field_name_4_sph_trns_snap(velocity,                     &
     &    ipol_base%i_velo, iphys_base%i_velo, b_trns_base%i_velo,      &
     &    trns)
!      end if
!      if(b_trns_base%i_vort .eq. 0) then
      call add_field_name_4_sph_trns_snap(vorticity,                    &
     &    ipol_base%i_vort, iphys_base%i_vort, b_trns_base%i_vort,      &
     &    trns)
!      end if
!      if(b_trns_base%i_magne .eq. 0) then
      call add_field_name_4_sph_trns_snap(magnetic_field,               &
     &    ipol_base%i_magne, iphys_base%i_magne, b_trns_base%i_magne,   &
     &    trns)
!      end if
!      if(b_trns_base%i_current .eq. 0) then
      call add_field_name_4_sph_trns_snap(current_density,              &
     &    ipol_base%i_current, iphys_base%i_current,                    &
     &    b_trns_base%i_current, trns)
!      end if
!
      end subroutine add_base_vector_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_base_scalar_sph_trns_snap                          &
     &         (ipol_base, iphys_base, b_trns_base, trns)
!
      use add_field_to_sph_trans_list
!
      type(base_field_address), intent(in) :: ipol_base, iphys_base
      type(base_field_address), intent(inout) :: b_trns_base
      type(spherical_transform_data), intent(inout) :: trns
!
!
!      if(b_trns_base%i_temp.eq.0 .or. ipol_base%i_per_temp.gt.0) then
      call add_field_name_4_sph_trns_snap(temperature,                  &
     &    ipol_base%i_temp, iphys_base%i_temp, b_trns_base%i_temp,      &
     &    trns)
!      end if
!      if(b_trns_base%i_light .eq. 0) then
      call add_field_name_4_sph_trns_snap(composition,                  &
     &    ipol_base%i_light, iphys_base%i_light, b_trns_base%i_light,   &
     &    trns)
!      end if
!
      call add_field_name_4_sph_trns_snap(pressure,                     &
     &    ipol_base%i_press, iphys_base%i_press, b_trns_base%i_press,   &
     &    trns)
!
!
      call add_field_4_sph_trns_by_pol(perturbation_temp,               &
     &    ipol_base%i_per_temp, iphys_base%i_per_temp,                  &
     &    b_trns_base%i_per_temp, trns)
!
      call add_field_4_sph_trns_by_pol(perturbation_composition,        &
     &    ipol_base%i_per_light, iphys_base%i_per_light,                &
     &    b_trns_base%i_per_light, trns)
!
      end subroutine add_base_scalar_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_base_field_4_sph_trns
