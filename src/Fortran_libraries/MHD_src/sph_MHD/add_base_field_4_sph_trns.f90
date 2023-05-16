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
!!     &         (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!!      subroutine add_base_scalar_4_MHD_sph_trns                       &
!!     &         (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol_base, iphys_base
!!        type(phys_address), intent(inout) :: b_trns_base
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_base_vector_sph_trns_snap                        &
!!     &         (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!!      subroutine add_base_scalar_sph_trns_snap                        &
!!     &         (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol_base, iphys_base
!!        type(phys_address), intent(inout) :: b_trns_base
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_base_field_4_sph_trns
!
      use m_precision
!
      use t_phys_data
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
!     &         (fl_prop, cd_prop, ht_prop, cp_prop,                    &
     &         (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!
      use add_field_to_sph_trans_list
!
!      type(fluid_property), intent(in) :: fl_prop
!      type(conductive_property), intent(in)  :: cd_prop
!      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_base, iphys_base
      type(base_field_address), intent(inout) :: b_trns_base
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   velocity flag
!      if(       fl_prop%iflag_4_inertia .or. cd_prop%iflag_4_induction &
!     &     .or. ht_prop%iflag_4_advection                              &
!     &     .or. cp_prop%iflag_4_advection) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_base%i_velo, iphys_base%i_velo, b_trns_base%i_velo,    &
     &      trns)
!      end if
!   vorticity flag
!      if(fl_prop%iflag_4_inertia) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_base%i_vort, iphys_base%i_vort, b_trns_base%i_vort,    &
     &      trns)
!      end if
!   magnetic field flag
!      if(cd_prop%iflag_4_induction .or. fl_prop%iflag_4_lorentz) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_base%i_magne, iphys_base%i_magne, b_trns_base%i_magne, &
     &      trns)
!      end if
!   current density flag
!      if(fl_prop%iflag_4_lorentz) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_base%i_current, iphys_base%i_current,                  &
     &      b_trns_base%i_current, trns)
!      end if
!
!    external magnetic field flag
!      if(cd_prop%iflag_4_induction .or. fl_prop%iflag_4_lorentz) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_base%i_back_B, iphys_base%i_back_B,                    &
     &      b_trns_base%i_back_B, trns)
!      end if
!
      end subroutine add_base_vector_4_MHD_sph_trns
!
!-----------------------------------------------------------------------
!
!      subroutine add_base_scalar_4_MHD_sph_trns(ht_prop, cp_prop,      &
      subroutine add_base_scalar_4_MHD_sph_trns                         &
     &         (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!
      use add_field_to_sph_trans_list
!
!      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_base, iphys_base
      type(base_field_address), intent(inout) :: b_trns_base
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   temperature flag
!      if(ht_prop%iflag_4_advection) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_base%i_temp, iphys_base%i_temp, b_trns_base%i_temp,    &
     &      trns)
!      end if
!   composition flag
!      if(cp_prop%iflag_4_advection) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_base%i_light, iphys_base%i_light, b_trns_base%i_light, &
     &      trns)
!      end if
!
      end subroutine add_base_scalar_4_MHD_sph_trns
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_base_vector_sph_trns_snap                          &
     &         (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_base, iphys_base
      type(base_field_address), intent(inout) :: b_trns_base
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_base_vector_4_MHD_sph_trns                               &
     &   (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!
      end subroutine add_base_vector_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_base_scalar_sph_trns_snap                          &
     &         (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_base, iphys_base
      type(base_field_address), intent(inout) :: b_trns_base
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_base%i_temp, iphys_base%i_temp, b_trns_base%i_temp,      &
     &    trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_base%i_light, iphys_base%i_light, b_trns_base%i_light,   &
     &    trns)
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_base%i_press, iphys_base%i_press, b_trns_base%i_press,   &
     &    trns)
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_base%i_per_temp, iphys_base%i_per_temp,                  &
     &    b_trns_base%i_per_temp, trns)
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_base%i_per_light, iphys_base%i_per_light,                &
     &    b_trns_base%i_per_light, trns)
!
      end subroutine add_base_scalar_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_base_field_4_sph_trns
