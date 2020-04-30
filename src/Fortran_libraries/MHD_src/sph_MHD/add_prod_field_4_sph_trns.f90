!>@file   add_prod_field_4_sph_trns.f90
!!@brief  module add_prod_field_4_sph_trns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Products of field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_subtracted_sph_trns_snap                         &
!!     &         (ipol_prd, iphys_prd, b_trns_prd, trns)
!!        type(phys_products_address), intent(in) :: ipol_prd, iphys_prd
!!        type(phys_products_address), intent(inout) :: b_trns_prd
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_prod_vector_sph_trns_snap                        &
!!     &         (ipol_prd, iphys_prd, f_trns_prd, trns)
!!      subroutine add_prod_scalar_sph_trns_snap                        &
!!     &         (ipol_prd, iphys_prd, f_trns_prd, trns)
!!        type(phys_products_address), intent(in) :: ipol_prd, iphys_prd
!!        type(phys_products_address), intent(inout) :: f_trns_prd
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_prod_field_4_sph_trns
!
      use m_precision
!
      use t_field_product_labels
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
     &         (ipol_prd, iphys_prd, b_trns_prd, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_products_address), intent(in) :: ipol_prd, iphys_prd
      type(phys_products_address), intent(inout) :: b_trns_prd
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(rest_of_geostrophic,          &
     &    ipol_prd%i_geostrophic, iphys_prd%i_geostrophic,              &
     &    b_trns_prd%i_geostrophic, trns)
!
      call add_field_name_4_sph_trns_snap(truncated_magnetic_field,     &
     &    ipol_prd%i_truncated_B, iphys_prd%i_truncated_B,              &
     &    b_trns_prd%i_truncated_B, trns)
!
      end subroutine add_subtracted_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_prod_vector_sph_trns_snap                          &
     &         (ipol_prd, iphys_prd, f_trns_prd, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_products_address), intent(in) :: ipol_prd, iphys_prd
      type(phys_products_address), intent(inout) :: f_trns_prd
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(electric_field,               &
     &    ipol_prd%i_electric, iphys_prd%i_electric,                    &
     &    f_trns_prd%i_electric, trns)
      call add_field_name_4_sph_trns_snap(poynting_flux,                &
     &    ipol_prd%i_poynting, iphys_prd%i_poynting,                    &
     &    f_trns_prd%i_poynting, trns)
!
      call add_field_name_4_sph_trns_snap(square_velocity,              &
     &    ipol_prd%i_square_v, iphys_prd%i_square_v,                    &
     &    f_trns_prd%i_square_v, trns)
      call add_field_name_4_sph_trns_snap(square_vorticity,             &
     &    ipol_prd%i_square_w, iphys_prd%i_square_w,                    &
     &    f_trns_prd%i_square_w, trns)
      call add_field_name_4_sph_trns_snap(square_magne,                 &
     &    ipol_prd%i_square_b, iphys_prd%i_square_b,                    &
     &    f_trns_prd%i_square_b, trns)
      call add_field_name_4_sph_trns_snap(square_vector_potential,      &
     &    ipol_prd%i_square_a, iphys_prd%i_square_a,                    &
     &    f_trns_prd%i_square_a, trns)
      call add_field_name_4_sph_trns_snap(square_current,               &
     &    ipol_prd%i_square_j, iphys_prd%i_square_j,                    &
     &    f_trns_prd%i_square_j, trns)
!
      end subroutine add_prod_vector_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_prod_scalar_sph_trns_snap                          &
     &         (ipol_prd, iphys_prd, f_trns_prd, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_products_address), intent(in) :: ipol_prd, iphys_prd
      type(phys_products_address), intent(inout) :: f_trns_prd
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(kinetic_helicity,             &
     &    ipol_prd%i_k_heli, iphys_prd%i_k_heli,                        &
     &    f_trns_prd%i_k_heli, trns)
      call add_field_name_4_sph_trns_snap(magnetic_helicity,            &
     &    ipol_prd%i_m_heli, iphys_prd%i_m_heli,                        &
     &    f_trns_prd%i_m_heli, trns)
      call add_field_name_4_sph_trns_snap(current_helicity,             &
     &    ipol_prd%i_c_heli, iphys_prd%i_c_heli,                        &
     &    f_trns_prd%i_c_heli, trns)
      call add_field_name_4_sph_trns_snap(cross_helicity,               &
     &    ipol_prd%i_x_heli, iphys_prd%i_x_heli,                        &
     &    f_trns_prd%i_x_heli, trns)
!
      call add_field_name_4_sph_trns_snap(velocity_scale,               &
     &    ipol_prd%i_velo_scale, iphys_prd%i_velo_scale,                &
     &    f_trns_prd%i_velo_scale, trns)
      call add_field_name_4_sph_trns_snap(magnetic_scale,               &
     &    ipol_prd%i_magne_scale, iphys_prd%i_magne_scale,              &
     &    f_trns_prd%i_magne_scale, trns)
      call add_field_name_4_sph_trns_snap(temperature_scale,            &
     &    ipol_prd%i_temp_scale, iphys_prd%i_temp_scale,                &
     &    f_trns_prd%i_temp_scale, trns)
      call add_field_name_4_sph_trns_snap(composition_scale,            &
     &    ipol_prd%i_comp_scale, iphys_prd%i_comp_scale,                &
     &    f_trns_prd%i_comp_scale, trns)
!
      call add_field_name_4_sph_trns_snap(square_temperature,           &
     &    ipol_prd%i_square_t, iphys_prd%i_square_t,                    &
     &    f_trns_prd%i_square_t, trns)
      call add_field_name_4_sph_trns_snap(square_composition,           &
     &    ipol_prd%i_square_c, iphys_prd%i_square_c,                    &
     &    f_trns_prd%i_square_c, trns)
!
      end subroutine add_prod_scalar_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_prod_field_4_sph_trns
