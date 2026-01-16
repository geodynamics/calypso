!>@file   add_field_comps_4_sph_trns.f90
!!@brief  module add_field_comps_4_sph_trns
!!
!!@author H. Matsui (UC Davis)
!!@n      and T. Kera (Tohoku University)
!!
!!@date   Programmed in 2010
!!@n      Modified in July, 2021
!
!>@brief Field components addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_vector_comps_sph_trns_snap                       &
!!     &         (d_rj, ipol_cmp, iphys_cmp, f_trns_cmp, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(field_component_address), intent(in) :: ipol_cmp
!!        type(field_component_address), intent(in) :: iphys_cmp
!!        type(field_component_address), intent(in) :: ipol_cmp
!!        type(field_component_address), intent(inout) :: f_trns_cmp
!!        type(spherical_transform_data), intent(inout) :: trns
!!
!!      subroutine add_scalar_comps_sph_trns_snap                       &
!!     &         (d_rj, ipol_cmp, iphys_cmp, b_trns_cmp, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(field_component_address), intent(in) :: ipol_cmp
!!        type(field_component_address), intent(in) :: iphys_cmp
!!        type(field_component_address), intent(inout) :: b_trns_cmp
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_sym_scl_cmps_sph_trns_snap                       &
!!     &         (d_rj, ipol_cmp, iphys_cmp, b_trns_cmp, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(field_component_address), intent(in) :: ipol_cmp
!!        type(field_component_address), intent(in) :: iphys_cmp
!!        type(field_component_address), intent(inout) :: b_trns_cmp
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_field_comps_4_sph_trns
!
      use m_precision
!
      use t_phys_data
      use t_field_product_labels
      use t_field_component_labels
      use t_addresses_sph_transform
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_comps_sph_trns_snap                         &
     &         (d_rj, ipol_cmp, iphys_cmp, f_trns_cmp, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(field_component_address), intent(in) :: ipol_cmp
      type(field_component_address), intent(in) :: iphys_cmp
      type(field_component_address), intent(inout) :: f_trns_cmp
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_velo_r, iphys_cmp%i_velo_r,                        &
     &    f_trns_cmp%i_velo_r, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_velo_t, iphys_cmp%i_velo_t,                        &
     &    f_trns_cmp%i_velo_t, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_velo_p, iphys_cmp%i_velo_p,                        &
     &    f_trns_cmp%i_velo_p, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_velo_s, iphys_cmp%i_velo_s,                        &
     &    f_trns_cmp%i_velo_s, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_velo_x, iphys_cmp%i_velo_x,                        &
     &    f_trns_cmp%i_velo_x, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_velo_y, iphys_cmp%i_velo_y,                        &
     &    f_trns_cmp%i_velo_y, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_velo_z, iphys_cmp%i_velo_z,                        &
     &    f_trns_cmp%i_velo_z, trns)
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_magne_r, iphys_cmp%i_magne_r,                      &
     &    f_trns_cmp%i_magne_r, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_magne_t, iphys_cmp%i_magne_t,                      &
     &    f_trns_cmp%i_magne_t, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_magne_p, iphys_cmp%i_magne_p,                      &
     &    f_trns_cmp%i_magne_p, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_magne_s, iphys_cmp%i_magne_s,                      &
     &    f_trns_cmp%i_magne_s, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_magne_z, iphys_cmp%i_magne_z,                      &
     &    f_trns_cmp%i_magne_z, trns)
!
      end subroutine add_vector_comps_sph_trns_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_scalar_comps_sph_trns_snap                         &
     &         (d_rj, ipol_cmp, iphys_cmp, b_trns_cmp, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(field_component_address), intent(in) :: ipol_cmp
      type(field_component_address), intent(in) :: iphys_cmp
      type(field_component_address), intent(inout) :: b_trns_cmp
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_temp_from_CMB, iphys_cmp%i_temp_from_CMB,          &
     &    b_trns_cmp%i_temp_from_CMB, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_light_from_CMB, iphys_cmp%i_light_from_CMB,        &
     &    b_trns_cmp%i_light_from_CMB, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_entropy_from_CMB, iphys_cmp%i_entropy_from_CMB,    &
     &    b_trns_cmp%i_entropy_from_CMB, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_density_from_CMB, iphys_cmp%i_density_from_CMB,    &
     &    b_trns_cmp%i_density_from_CMB, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_pressure_from_CMB, iphys_cmp%i_pressure_from_CMB,  &
     &    b_trns_cmp%i_pressure_from_CMB, trns)
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_asph_pressure, iphys_cmp%i_asph_pressure,          &
     &    b_trns_cmp%i_asph_pressure, trns)
!
      end subroutine add_scalar_comps_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_sym_scl_cmps_sph_trns_snap                         &
     &         (d_rj, ipol_cmp, iphys_cmp, b_trns_cmp, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(field_component_address), intent(in) :: ipol_cmp
      type(field_component_address), intent(in) :: iphys_cmp
      type(field_component_address), intent(inout) :: b_trns_cmp
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &   ipol_cmp%i_sym_temp_from_CMB, iphys_cmp%i_sym_temp_from_CMB,   &
     &   b_trns_cmp%i_sym_temp_from_CMB, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &   ipol_cmp%i_sym_light_from_CMB, iphys_cmp%i_sym_light_from_CMB, &
     &   b_trns_cmp%i_sym_light_from_CMB, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_sym_entropy_from_CMB,                              &
     &    iphys_cmp%i_sym_entropy_from_CMB,                             &
     &    b_trns_cmp%i_sym_entropy_from_CMB, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_sym_density_from_CMB,                              &
     &    iphys_cmp%i_sym_density_from_CMB,                             &
     &    b_trns_cmp%i_sym_density_from_CMB, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_sym_pressure_from_CMB,                             &
     &    iphys_cmp%i_sym_pressure_from_CMB,                            &
     &    b_trns_cmp%i_sym_pressure_from_CMB, trns)
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_cmp%i_sym_asph_pressure, iphys_cmp%i_sym_asph_pressure,  &
     &    b_trns_cmp%i_sym_asph_pressure, trns)
!
      end subroutine add_sym_scl_cmps_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_field_comps_4_sph_trns
