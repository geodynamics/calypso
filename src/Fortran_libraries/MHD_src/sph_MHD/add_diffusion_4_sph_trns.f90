!>@file   add_diffusion_4_sph_trns.f90
!!@brief  module add_diffusion_4_sph_trns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Diffusion addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_vector_diffusion_sph_trns                        &
!!     &         (ipol_dif, iphys_dif, b_trns_dif, trns)
!!      subroutine add_scalar_diffusion_sph_trns                        &
!!     &         (ipol_dif, iphys_dif, b_trns_dif, trns)
!!        type(diffusion_address), intent(in) :: ipol_dif, iphys_dif
!!        type(diffusion_address), intent(inout) :: b_trns_dif
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_diffusion_4_sph_trns
!
      use m_precision
      use m_constants
!
      use t_diffusion_term_labels
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
      subroutine add_vector_diffusion_sph_trns                          &
     &         (ipol_dif, iphys_dif, b_trns_dif, trns)
!
      use m_diffusion_term_labels
      use add_field_to_sph_trans_list
!
      type(diffusion_address), intent(in) :: ipol_dif, iphys_dif
!
      type(diffusion_address), intent(inout) :: b_trns_dif
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(viscous_diffusion,            &
     &    ipol_dif%i_v_diffuse, iphys_dif%i_v_diffuse,                  &
     &    b_trns_dif%i_v_diffuse, trns)
      call add_field_name_4_sph_trns_snap(vorticity_diffusion,          &
     &    ipol_dif%i_w_diffuse, iphys_dif%i_w_diffuse,                  &
     &    b_trns_dif%i_w_diffuse, trns)
      call add_field_name_4_sph_trns_snap(vector_potential_diffusion,   &
     &    ipol_dif%i_vp_diffuse, iphys_dif%i_vp_diffuse,                &
     &    b_trns_dif%i_vp_diffuse, trns)
      call add_field_name_4_sph_trns_snap(magnetic_diffusion,           &
     &    ipol_dif%i_b_diffuse, iphys_dif%i_b_diffuse,                  &
     &    b_trns_dif%i_b_diffuse, trns)
!
      end subroutine add_vector_diffusion_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine add_scalar_diffusion_sph_trns                          &
     &         (ipol_dif, iphys_dif, b_trns_dif, trns)
!
      use m_diffusion_term_labels
      use add_field_to_sph_trans_list
!
      type(diffusion_address), intent(in) :: ipol_dif, iphys_dif
!
      type(diffusion_address), intent(inout) :: b_trns_dif
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(thermal_diffusion,            &
     &    ipol_dif%i_t_diffuse, iphys_dif%i_t_diffuse,                  &
     &    b_trns_dif%i_t_diffuse, trns)
      call add_field_name_4_sph_trns_snap(composition_diffusion,        &
     &    ipol_dif%i_c_diffuse, iphys_dif%i_c_diffuse,                  &
     &    b_trns_dif%i_c_diffuse, trns)
!
      end subroutine add_scalar_diffusion_sph_trns
!
!-----------------------------------------------------------------------
!
      end module add_diffusion_4_sph_trns
