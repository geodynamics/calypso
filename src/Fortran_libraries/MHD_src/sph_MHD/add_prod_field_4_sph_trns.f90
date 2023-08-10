!>@file   add_prod_field_4_sph_trns.f90
!!@brief  module add_prod_field_4_sph_trns
!!
!!@author H. Matsui (UC Davis)
!!@n      and T. Kera (Tohoku University)
!!
!!@date   Programmed in 2010
!!@n      Modified in July, 2021
!
!>@brief Products of field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_subtracted_sph_trns_snap                         &
!!     &         (d_rj, ipol_prd, iphys_prd, b_trns_prd, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_products_address), intent(in) :: ipol_prd, iphys_prd
!!        type(phys_products_address), intent(inout) :: b_trns_prd
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_prod_vector_sph_trns_snap                        &
!!     &         (d_rj, ipol_prd, iphys_prd, f_trns_prd, trns)
!!      subroutine add_prod_scalar_sph_trns_snap                        &
!!     &         (d_rj, ipol_prd, iphys_prd, f_trns_prd, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_products_address), intent(in) :: ipol_prd, iphys_prd
!!        type(phys_products_address), intent(inout) :: f_trns_prd
!!        type(spherical_transform_data), intent(inout) :: trns
!!
!!      subroutine add_field_comps_sph_trns_snap                        &
!!     &         (d_rj, ipol_cmp, iphys_cmp, f_trns_cmp, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(field_component_address), intent(in) :: ipol_cmp
!!        type(field_component_address), intent(in) :: iphys_cmp
!!        type(field_component_address), intent(in) :: ipol_cmp
!!        type(field_component_address), intent(inout) :: f_trns_cmp
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_prod_field_4_sph_trns
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
      subroutine add_subtracted_sph_trns_snap                           &
     &         (d_rj, ipol_prd, iphys_prd, b_trns_prd, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(phys_products_address), intent(in) :: ipol_prd, iphys_prd
      type(phys_products_address), intent(inout) :: b_trns_prd
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_geostrophic, iphys_prd%i_geostrophic,              &
     &    b_trns_prd%i_geostrophic, trns)
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_truncated_B, iphys_prd%i_truncated_B,              &
     &    b_trns_prd%i_truncated_B, trns)
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_stream_pol_u, iphys_prd%i_stream_pol_u,            &
     &    b_trns_prd%i_stream_pol_u, trns)
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_stream_pol_b, iphys_prd%i_stream_pol_b,            &
     &    b_trns_prd%i_stream_pol_b, trns)
!
      end subroutine add_subtracted_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_prod_vector_sph_trns_snap                          &
     &         (d_rj, ipol_prd, iphys_prd, f_trns_prd, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(phys_products_address), intent(in) :: ipol_prd, iphys_prd
      type(phys_products_address), intent(inout) :: f_trns_prd
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_electric, iphys_prd%i_electric,                    &
     &    f_trns_prd%i_electric, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_poynting, iphys_prd%i_poynting,                    &
     &    f_trns_prd%i_poynting, trns)
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_square_v, iphys_prd%i_square_v,                    &
     &    f_trns_prd%i_square_v, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_square_w, iphys_prd%i_square_w,                    &
     &    f_trns_prd%i_square_w, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_square_b, iphys_prd%i_square_b,                    &
     &    f_trns_prd%i_square_b, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_square_a, iphys_prd%i_square_a,                    &
     &    f_trns_prd%i_square_a, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_square_j, iphys_prd%i_square_j,                    &
     &    f_trns_prd%i_square_j, trns)
!
      end subroutine add_prod_vector_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_prod_scalar_sph_trns_snap                          &
     &         (d_rj, ipol_prd, iphys_prd, f_trns_prd, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(phys_products_address), intent(in) :: ipol_prd, iphys_prd
      type(phys_products_address), intent(inout) :: f_trns_prd
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_k_heli, iphys_prd%i_k_heli,                        &
     &    f_trns_prd%i_k_heli, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_m_heli, iphys_prd%i_m_heli,                        &
     &    f_trns_prd%i_m_heli, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_c_heli, iphys_prd%i_c_heli,                        &
     &    f_trns_prd%i_c_heli, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_x_heli, iphys_prd%i_x_heli,                        &
     &    f_trns_prd%i_x_heli, trns)
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_velo_scale, iphys_prd%i_velo_scale,                &
     &    f_trns_prd%i_velo_scale, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_magne_scale, iphys_prd%i_magne_scale,              &
     &    f_trns_prd%i_magne_scale, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_temp_scale, iphys_prd%i_temp_scale,                &
     &    f_trns_prd%i_temp_scale, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_comp_scale, iphys_prd%i_comp_scale,                &
     &    f_trns_prd%i_comp_scale, trns)
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_square_t, iphys_prd%i_square_t,                    &
     &    f_trns_prd%i_square_t, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_square_c, iphys_prd%i_square_c,                    &
     &    f_trns_prd%i_square_c, trns)
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_magnetic_intensity,                                &
     &    iphys_prd%i_magnetic_intensity,                               &
     &    f_trns_prd%i_magnetic_intensity, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_declination, iphys_prd%i_declination,              &
     &    f_trns_prd%i_declination, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_inclination, iphys_prd%i_inclination,              &
     &    f_trns_prd%i_inclination, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_vgp_latitude, iphys_prd%i_vgp_latitude,            &
     &    f_trns_prd%i_vgp_latitude, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_prd%i_vgp_longigude, iphys_prd%i_vgp_longigude,          &
     &    f_trns_prd%i_vgp_longigude, trns)
!
      end subroutine add_prod_scalar_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_field_comps_sph_trns_snap                          &
     &         (d_rj, ipol_cmp, iphys_cmp, f_trns_cmp, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(field_component_address), intent(in) :: ipol_cmp, iphys_cmp
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
      end subroutine add_field_comps_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_prod_field_4_sph_trns
