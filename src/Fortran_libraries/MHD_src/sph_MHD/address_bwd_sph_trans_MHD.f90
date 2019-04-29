!>@file   address_bwd_sph_trans_MHD.f90
!!@brief  module address_bwd_sph_trans_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_address_vector_MHD                           &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop,                   &
!!     &          ipol, itor, iphys, b_trns, trns_back)
!!      subroutine b_trans_address_scalar_MHD                           &
!!     &         (ht_prop, cp_prop, ipol, b_trns, trns_back)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(inout) :: b_trns
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!@endverbatim
!
      module address_bwd_sph_trans_MHD
!
      use m_precision
!
      use m_phys_labels
      use m_phys_constants
      use t_phys_address
      use t_addresses_sph_transform
      use t_control_parameter
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
      subroutine b_trans_address_vector_MHD                             &
     &         (fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          ipol, itor, iphys, b_trns, trns_back)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
!   velocity flag
      if(       fl_prop%iflag_scheme .gt. id_no_evolution               &
     &     .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution          &
     &     .or. ht_prop%iflag_scheme .gt. id_no_evolution               &
     &     .or. cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_name_4_sph_trns(ipol%i_velo, fhd_velo, n_vector, &
     &      ipol%i_velo, itor%i_velo, iphys%i_velo,                     &
     &      b_trns%i_velo, trns_back)
      end if
!   vorticity flag
      if(       fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_name_4_sph_trns(ipol%i_vort, fhd_vort, n_vector, &
     &      ipol%i_vort, itor%i_vort, iphys%i_vort,                     &
     &      b_trns%i_vort, trns_back)
      end if
!   magnetic field flag
      if(       cd_prop%iflag_Bevo_scheme .gt. id_no_evolution          &
     &     .or. fl_prop%iflag_4_lorentz .gt.     id_turn_OFF) then
        call add_field_name_4_sph_trns                                  &
     &     (ipol%i_magne, fhd_magne, n_vector,                          &
     &      ipol%i_magne, itor%i_magne, iphys%i_magne,                  &
     &      b_trns%i_magne, trns_back)
      end if
!   current density flag
      if(fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
        call add_field_name_4_sph_trns                                  &
     &     (ipol%i_current, fhd_current, n_vector,                      &
     &      ipol%i_current, itor%i_current, iphys%i_current,            &
     &      b_trns%i_current, trns_back)
      end if
      trns_back%num_vector = trns_back%nfield
!
      end subroutine b_trans_address_vector_MHD
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_MHD                             &
     &         (ht_prop, cp_prop, ipol, itor, iphys, b_trns, trns_back)
!
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   temperature flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_name_4_sph_trns(ipol%i_temp, fhd_temp, n_scalar, &
     &      ipol%i_temp, itor%i_temp, iphys%i_temp,                     &
     &      b_trns%i_temp, trns_back)
      end if
!   composition flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_name_4_sph_trns                                  &
     &     (ipol%i_light, fhd_light, n_scalar,                          &
     &      ipol%i_light, itor%i_light, iphys%i_light,                  &
     &      b_trns%i_light, trns_back)
      end if
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
!
      end subroutine b_trans_address_scalar_MHD
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_MHD
