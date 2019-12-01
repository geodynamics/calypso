!>@file   address_bwd_sph_trans_snap.f90
!!@brief  module address_bwd_sph_trans_snap
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_address_vector_snap                          &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!      subroutine b_trans_address_scalar_snap                          &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!@endverbatim
!
      module address_bwd_sph_trans_snap
!
      use m_precision
!
      use m_phys_labels
      use m_phys_constants
      use t_phys_address
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
      subroutine b_trans_address_vector_snap                            &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
!      if(b_trns%i_velo .eq. 0) then
      call add_field_name_4_sph_trns_snap(fhd_velo, n_vector,           &
     &    ipol%i_velo, itor%i_velo, iphys%i_velo,                       &
     &    b_trns%i_velo, trns_back)
!      end if
!      if(b_trns%i_vort .eq. 0) then
      call add_field_name_4_sph_trns_snap(fhd_vort, n_vector,           &
     &    ipol%i_vort, itor%i_vort, iphys%i_vort,                       &
     &    b_trns%i_vort, trns_back)
!      end if
!      if(b_trns%i_magne .eq. 0) then
      call add_field_name_4_sph_trns_snap(fhd_magne, n_vector,          &
     &    ipol%i_magne, itor%i_magne, iphys%i_magne,                    &
     &    b_trns%i_magne, trns_back)
!      end if
!      if(b_trns%i_current .eq. 0) then
      call add_field_name_4_sph_trns_snap(fhd_current, n_vector,        &
     &    ipol%i_current, itor%i_current, iphys%i_current,              &
     &    b_trns%i_current, trns_back)
!      end if
!
      call add_field_name_4_sph_trns_snap(fhd_viscous, n_vector,        &
     &    ipol%i_v_diffuse, itor%i_v_diffuse, iphys%i_v_diffuse,        &
     &    b_trns%i_v_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_w_viscous, n_vector,      &
     &    ipol%i_w_diffuse, itor%i_w_diffuse, iphys%i_w_diffuse,        &
     &    b_trns%i_w_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_vecp_diffuse, n_vector,   &
     &    ipol%i_vp_diffuse, itor%i_vp_diffuse, iphys%i_vp_diffuse,     &
     &    b_trns%i_vp_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_mag_diffuse, n_vector,    &
     &    ipol%i_b_diffuse, itor%i_b_diffuse, iphys%i_b_diffuse,        &
     &    b_trns%i_b_diffuse, trns_back)
!
      call add_field_name_4_sph_trns_snap(fhd_rot_inertia, n_vector,    &
     &    ipol%i_rot_inertia, itor%i_rot_inertia, iphys%i_rot_inertia,  &
     &    b_trns%i_rot_inertia, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_rot_Coriolis, n_vector,   &
     &    ipol%i_rot_Coriolis, itor%i_rot_Coriolis,                     &
     &    iphys%i_rot_Coriolis, b_trns%i_rot_Coriolis, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_rot_Lorentz, n_vector,    &
     &    ipol%i_rot_Lorentz, itor%i_rot_Lorentz, iphys%i_rot_Lorentz,  &
     &    b_trns%i_rot_Lorentz, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_rot_buoyancy, n_vector,   &
     &    ipol%i_rot_buoyancy, itor%i_rot_buoyancy,                     &
     &    iphys%i_rot_buoyancy, b_trns%i_rot_buoyancy, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_rot_comp_buo, n_vector,   &
     &    ipol%i_rot_comp_buo, itor%i_rot_comp_buo,                     &
     &    iphys%i_rot_comp_buo, b_trns%i_rot_comp_buo, trns_back)
!
      call add_field_name_4_sph_trns_snap(fhd_buoyancy, n_vector,       &
     &    ipol%i_buoyancy, itor%i_buoyancy, iphys%i_buoyancy,           &
     &    b_trns%i_buoyancy, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_comp_buo, n_vector,       &
     &    ipol%i_comp_buo, itor%i_comp_buo, iphys%i_comp_buo,           &
     &    b_trns%i_comp_buo, trns_back)
!
      call add_field_name_4_sph_trns_snap(fhd_geostrophic, n_vector,    &
     &    ipol%i_geostrophic, itor%i_geostrophic, iphys%i_geostrophic,  &
     &    b_trns%i_geostrophic, trns_back)
!
      call add_field_name_4_sph_trns_snap(fhd_h_flux_w_sgs, n_vector,   &
     &    ipol%i_h_flux_w_sgs, itor%i_h_flux_w_sgs,                     &
     &    iphys%i_h_flux_w_sgs, b_trns%i_h_flux_w_sgs, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_c_flux_w_sgs, n_vector,   &
     &    ipol%i_c_flux_w_sgs, itor%i_c_flux_w_sgs,                     &
     &    iphys%i_c_flux_w_sgs, b_trns%i_c_flux_w_sgs, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_inertia_w_sgs, n_vector,  &
     &    ipol%i_inertia_w_sgs, itor%i_inertia_w_sgs,                   &
     &    iphys%i_inertia_w_sgs, b_trns%i_inertia_w_sgs, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_Lorentz_w_sgs, n_vector,  &
     &    ipol%i_Lorentz_w_sgs, itor%i_Lorentz_w_sgs,                   &
     &    iphys%i_Lorentz_w_sgs, b_trns%i_Lorentz_w_sgs, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_vp_induct_w_sgs, n_vector, ipol%i_vp_induct_w_sgs,        &
     &    itor%i_vp_induct_w_sgs, iphys%i_vp_induct_w_sgs,              &
     &    b_trns%i_vp_induct_w_sgs, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_mag_induct_w_sgs, n_vector, ipol%i_mag_induct_w_sgs,      &
     &    itor%i_mag_induct_w_sgs, iphys%i_mag_induct_w_sgs,            &
     &    b_trns%i_mag_induct_w_sgs, trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_SGS_rot_inertia, n_vector, ipol%i_SGS_rot_inertia,        &
     &    itor%i_SGS_rot_inertia, iphys%i_SGS_rot_inertia,              &
     &    b_trns%i_SGS_rot_inertia, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_SGS_rot_Lorentz, n_vector, ipol%i_SGS_rot_Lorentz,        &
     &    itor%i_SGS_rot_Lorentz, iphys%i_SGS_rot_Lorentz,              &
     &    b_trns%i_SGS_rot_Lorentz, trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_induction, fhd_SGS_induction, n_vector,            &
     &    ipol%i_SGS_induction, itor%i_SGS_induction,                   &
     &    iphys%i_SGS_induction, b_trns%i_SGS_induction, trns_back)
!
      call add_field_name_4_sph_trns_snap(fhd_press_grad, n_vector,     &
     &    ipol%i_press_grad, itor%i_press_grad, iphys%i_press_grad,     &
     &    b_trns%i_press_grad, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_mag_induct, n_vector,     &
     &    ipol%i_induction, itor%i_induction, iphys%i_induction,        &
     &    b_trns%i_induction, trns_back)
!
      call add_field_name_4_sph_trns_snap(fhd_grad_temp, n_vector,      &
     &    ipol%i_grad_t, itor%i_grad_t, iphys%i_grad_t,                 &
     &    b_trns%i_grad_t, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_grad_composit, n_vector,  &
     &    ipol%i_grad_composit, itor%i_grad_composit,                   &
     &    iphys%i_grad_composit, b_trns%i_grad_composit, trns_back)
!
      call add_field_name_4_sph_trns(ipol%i_grad_vx, fhd_grad_v_1,      &
     &    n_vector, ipol%i_grad_vx, itor%i_grad_vx, iphys%i_grad_vx,    &
     &    b_trns%i_grad_vx, trns_back)
      call add_field_name_4_sph_trns(ipol%i_grad_vy, fhd_grad_v_2,      &
     &     n_vector, ipol%i_grad_vy, itor%i_grad_vy, iphys%i_grad_vy,   &
     &    b_trns%i_grad_vy, trns_back)
      call add_field_name_4_sph_trns(ipol%i_grad_vy, fhd_grad_v_3,      &
     &     n_vector, ipol%i_grad_vz, itor%i_grad_vz, iphys%i_grad_vz,   &
     &    b_trns%i_grad_vz, trns_back)
!
      call add_field_name_4_sph_trns(ipol%i_grad_wx, fhd_grad_w_1,      &
     &     n_vector, ipol%i_grad_wx, itor%i_grad_wx, iphys%i_grad_wx,   &
     &    b_trns%i_grad_wx, trns_back)
      call add_field_name_4_sph_trns(ipol%i_grad_wy, fhd_grad_w_2,      &
     &     n_vector, ipol%i_grad_wy, itor%i_grad_wy, iphys%i_grad_wy,   &
     &    b_trns%i_grad_wy, trns_back)
      call add_field_name_4_sph_trns(ipol%i_grad_wz, fhd_grad_w_3,      &
     &     n_vector, ipol%i_grad_wz, itor%i_grad_wz, iphys%i_grad_wz,   &
     &    b_trns%i_grad_wz, trns_back)
!
      call add_field_name_4_sph_trns(ipol%i_grad_ax, fhd_grad_a_1,      &
     &     n_vector, ipol%i_grad_ax, itor%i_grad_ax, iphys%i_grad_ax,   &
     &    b_trns%i_grad_ax, trns_back)
      call add_field_name_4_sph_trns(ipol%i_grad_ay, fhd_grad_a_2,      &
     &     n_vector, ipol%i_grad_ay, itor%i_grad_ay, iphys%i_grad_ay,   &
     &    b_trns%i_grad_ay, trns_back)
      call add_field_name_4_sph_trns(ipol%i_grad_az, fhd_grad_a_3,      &
     &     n_vector, ipol%i_grad_az, itor%i_grad_az, iphys%i_grad_az,   &
     &    b_trns%i_grad_az, trns_back)
!
      call add_field_name_4_sph_trns(ipol%i_grad_bx, fhd_grad_b_1,      &
     &     n_vector, ipol%i_grad_bx, itor%i_grad_bx, iphys%i_grad_bx,   &
     &    b_trns%i_grad_bx, trns_back)
      call add_field_name_4_sph_trns(ipol%i_grad_by, fhd_grad_b_2,      &
     &     n_vector, ipol%i_grad_by, itor%i_grad_by, iphys%i_grad_by,   &
     &    b_trns%i_grad_by, trns_back)
      call add_field_name_4_sph_trns(ipol%i_grad_bz, fhd_grad_b_3,      &
     &     n_vector, ipol%i_grad_bz, itor%i_grad_bz, iphys%i_grad_bz,   &
     &    b_trns%i_grad_bz, trns_back)
!
      call add_field_name_4_sph_trns(ipol%i_grad_jx, fhd_grad_j_1,      &
     &     n_vector, ipol%i_grad_jx, itor%i_grad_jx, iphys%i_grad_jx,   &
     &    b_trns%i_grad_jx, trns_back)
      call add_field_name_4_sph_trns(ipol%i_grad_jy, fhd_grad_j_2,      &
     &     n_vector, ipol%i_grad_jy, itor%i_grad_jy, iphys%i_grad_jy,   &
     &    b_trns%i_grad_jy, trns_back)
      call add_field_name_4_sph_trns(ipol%i_grad_jz, fhd_grad_j_3,      &
     &     n_vector, ipol%i_grad_jz, itor%i_grad_jz, iphys%i_grad_jz,   &
     &    b_trns%i_grad_jz, trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_truncated_B, n_vector, ipol%i_truncated_B,                &
     &    itor%i_truncated_B, iphys%i_truncated_B,                      &
     &    b_trns%i_truncated_B, trns_back)
!
      trns_back%num_vector = trns_back%nfield
!
      end subroutine b_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_snap                            &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!      if(b_trns%i_temp.eq.0 .or. ipol%i_par_temp.gt.0) then
      call add_field_name_4_sph_trns_snap(fhd_temp, n_scalar,           &
     &    ipol%i_temp, itor%i_temp, iphys%i_temp,                       &
     &    b_trns%i_temp, trns_back)
!      end if
!      if(b_trns%i_light .eq. 0) then
      call add_field_name_4_sph_trns_snap(fhd_light, n_scalar,          &
     &    ipol%i_light, itor%i_light, iphys%i_light,                    &
     &    b_trns%i_light, trns_back)
!      end if
!
      call add_field_name_4_sph_trns_snap(fhd_press, n_scalar,          &
     &    ipol%i_press, itor%i_press, iphys%i_press,                    &
     &    b_trns%i_press, trns_back)
      call add_field_name_4_sph_trns_nofld(fhd_part_temp, n_scalar,     &
     &    ipol%i_par_temp, itor%i_par_temp, iphys%i_par_temp,           &
     &    b_trns%i_par_temp, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_filter_temp, n_scalar,    &
     &    ipol%i_filter_temp, itor%i_filter_temp, iphys%i_filter_temp,  &
     &    b_trns%i_filter_temp, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_thermal_diffusion, n_scalar,                              &
     &    ipol%i_t_diffuse, itor%i_t_diffuse, iphys%i_t_diffuse,        &
     &    b_trns%i_t_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(fhd_c_diffuse, n_scalar,      &
     &    ipol%i_c_diffuse, itor%i_c_diffuse, iphys%i_c_diffuse,        &
     &    b_trns%i_c_diffuse, trns_back)
!
      call add_field_name_4_sph_trns_snap(fhd_heat_advect, n_scalar,    &
     &    ipol%i_h_advect, itor%i_h_advect, iphys%i_h_advect,           &
     &    b_trns%i_h_advect, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_composit_advect, n_scalar,                                &
     &    ipol%i_c_advect, itor%i_c_advect, iphys%i_c_advect,           &
     &    b_trns%i_c_advect, trns_back)
!
      call add_field_name_4_sph_trns_snap(fhd_div_Coriolis, n_scalar,   &
     &    ipol%i_div_Coriolis, itor%i_div_Coriolis,                     &
     &    iphys%i_div_Coriolis, b_trns%i_div_Coriolis, trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_SGS_div_inertia, n_scalar,                                &
     &    ipol%i_SGS_div_inertia, itor%i_SGS_div_inertia,               &
     &    iphys%i_SGS_div_inertia, b_trns%i_SGS_div_inertia, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_SGS_div_Lorentz, n_scalar,                                &
     &    ipol%i_SGS_div_Lorentz, itor%i_SGS_div_Lorentz,               &
     &    iphys%i_SGS_div_Lorentz, b_trns%i_SGS_div_Lorentz, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_div_SGS_h_flux, n_scalar,                                 &
     &    ipol%i_SGS_div_h_flux, itor%i_SGS_div_h_flux,                 &
     &    iphys%i_SGS_div_h_flux, b_trns%i_SGS_div_h_flux, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_div_SGS_c_flux, n_scalar,                                 &
     &    ipol%i_SGS_div_c_flux, itor%i_SGS_div_c_flux,                 &
     &     iphys%i_SGS_div_c_flux, b_trns%i_SGS_div_c_flux, trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
!
      end subroutine b_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_snap
