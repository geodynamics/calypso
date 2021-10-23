!>@file   add_energy_flux_4_sph_trns.f90
!!@brief  module add_energy_flux_4_sph_trns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Diffusion addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_ene_flux_4_sph_trns_snap                        &
!!     &         (d_rj, ipol_efx, iphys_efx, f_trns_efx, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(energy_flux_address), intent(in) :: ipol_efx, iphys_efx
!!        type(energy_flux_address), intent(inout) :: f_trns_efx
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_energy_flux_4_sph_trns
!
      use m_precision
      use m_constants
!
      use t_phys_data
      use t_energy_flux_labels
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
      subroutine add_ene_flux_4_sph_trns_snap                          &
     &         (d_rj, ipol_efx, iphys_efx, f_trns_efx, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(energy_flux_address), intent(in) :: ipol_efx, iphys_efx
      type(energy_flux_address), intent(inout) :: f_trns_efx
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
      &    ipol_efx%i_m_advect_work, iphys_efx%i_m_advect_work,         &
      &    f_trns_efx%i_m_advect_work, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_efx%i_me_gen, iphys_efx%i_me_gen,                        &
     &    f_trns_efx%i_me_gen, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_efx%i_ujb, iphys_efx%i_ujb, f_trns_efx%i_ujb, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_efx%i_nega_ujb, iphys_efx%i_nega_ujb,                    &
     &    f_trns_efx%i_nega_ujb, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_efx%i_buo_gen, iphys_efx%i_buo_gen,                      &
     &    f_trns_efx%i_buo_gen, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_efx%i_c_buo_gen, iphys_efx%i_c_buo_gen,                  &
     &    f_trns_efx%i_c_buo_gen, trns)
!
      end subroutine add_ene_flux_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_energy_flux_4_sph_trns
