!>@file   set_address_sph_trans_snap.f90
!!@brief  module set_address_sph_trans_snap
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_snapshot_trans(ipol, iphys, trns_snap, &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!@endverbatim
!
      module set_address_sph_trans_snap
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
      private :: b_trans_address_vector_snap
      private :: b_trans_address_scalar_snap
      private :: f_trans_address_vector_snap
      private :: f_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_snapshot_trans(ipol, iphys, trns_snap,   &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
!
!
      call b_trans_address_vector_snap(ipol, iphys,                     &
     &    trns_snap%nvector_rj_2_rtp, trns_snap%b_trns)
      call b_trans_address_scalar_snap(ipol, iphys,                     &
     &    trns_snap%nvector_rj_2_rtp, trns_snap%nscalar_rj_2_rtp,       &
     &    trns_snap%b_trns)
      trns_snap%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_snap                                  &
     &   (ipol, iphys, trns_snap%nvector_rtp_2_rj, trns_snap%f_trns)
      call f_trans_address_scalar_snap(ipol, iphys,                     &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns)
       trns_snap%ntensor_rtp_2_rj = 0
!
!
      nscltsr_rj_2_rtp                                                  &
     &      = trns_snap%nscalar_rj_2_rtp + 6*trns_snap%ntensor_rj_2_rtp
      trns_snap%ncomp_rj_2_rtp                                          &
     &      = 3*trns_snap%nvector_rj_2_rtp + nscltsr_rj_2_rtp
!
      nscltsr_rtp_2_rj                                                  &
     &      = trns_snap%nscalar_rtp_2_rj + 6*trns_snap%ntensor_rtp_2_rj
      trns_snap%ncomp_rtp_2_rj                                          &
     &      = 3*trns_snap%nvector_rtp_2_rj + nscltsr_rtp_2_rj
!
!
      ncomp_sph_trans = max(ncomp_sph_trans, trns_snap%ncomp_rtp_2_rj)
      ncomp_sph_trans = max(ncomp_sph_trans, trns_snap%ncomp_rj_2_rtp)
!
      nvector_sph_trans                                                 &
     &       = max(nvector_sph_trans, trns_snap%nvector_rj_2_rtp)
      nvector_sph_trans                                                 &
     &       = max(nvector_sph_trans, trns_snap%nvector_rtp_2_rj)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rtp_2_rj)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rj_2_rtp)
!
      end subroutine set_addresses_snapshot_trans
!
!-----------------------------------------------------------------------
!
      subroutine check_address_trans_sph_snap                           &
     &         (ipol, idpdr, itor, iphys, trns_snap)
!
      use check_address_sph_trans
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(in) :: trns_snap
!
!
      write(*,*) 'addresses of spherical transform for snapshot'
!
      call check_add_trans_sph_MHD                                      &
     &   (ipol, idpdr, itor, iphys, trns_snap%b_trns, trns_snap%f_trns, &
     &    trns_snap%ncomp_rj_2_rtp, trns_snap%nvector_rj_2_rtp,         &
     &    trns_snap%nscalar_rj_2_rtp, trns_snap%ncomp_rtp_2_rj,         &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj)
!
      end subroutine check_address_trans_sph_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_vector_snap                            &
     &         (ipol, iphys, nvector_snap_rj_2_rtp, bs_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      integer(kind = kint), intent(inout) :: nvector_snap_rj_2_rtp
      type(phys_address), intent(inout) :: bs_trns
!
!
      nvector_snap_rj_2_rtp = 0
!      if(b_trns%i_velo .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_velo, iphys%i_velo,         &
     &      nvector_snap_rj_2_rtp, bs_trns%i_velo)
!      end if
!      if(b_trns%i_vort .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_vort, iphys%i_vort,         &
     &      nvector_snap_rj_2_rtp, bs_trns%i_vort)
!      end if
!      if(b_trns%i_magne .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_magne, iphys%i_magne,       &
     &      nvector_snap_rj_2_rtp, bs_trns%i_magne)
!      end if
!      if(b_trns%i_current .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_current, iphys%i_current,   &
     &      nvector_snap_rj_2_rtp, bs_trns%i_current)
!      end if
!
      call add_vec_trans_flag_snap(ipol%i_v_diffuse, iphys%i_v_diffuse, &
     &    nvector_snap_rj_2_rtp, bs_trns%i_v_diffuse)
      call add_vec_trans_flag_snap(ipol%i_w_diffuse, iphys%i_w_diffuse, &
     &    nvector_snap_rj_2_rtp, bs_trns%i_w_diffuse)
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_vp_diffuse, iphys%i_vp_diffuse,                        &
     &    nvector_snap_rj_2_rtp, bs_trns%i_vp_diffuse)
      call add_vec_trans_flag_snap(ipol%i_b_diffuse, iphys%i_b_diffuse, &
     &    nvector_snap_rj_2_rtp, bs_trns%i_b_diffuse)
!
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_rot_inertia, iphys%i_rot_inertia,                      &
     &    nvector_snap_rj_2_rtp, bs_trns%i_rot_inertia)
      call add_vec_trans_flag_snap(ipol%i_rot_Coriolis,                 &
     &    iphys%i_rot_Coriolis, nvector_snap_rj_2_rtp,                  &
     &    bs_trns%i_rot_Coriolis)
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_rot_Lorentz, iphys%i_rot_Lorentz,                      &
     &    nvector_snap_rj_2_rtp, bs_trns%i_rot_Lorentz)
      call add_vec_trans_flag_snap(ipol%i_rot_buoyancy,                 &
     &    iphys%i_rot_buoyancy, nvector_snap_rj_2_rtp,                  &
     &    bs_trns%i_rot_buoyancy)
      call add_vec_trans_flag_snap(ipol%i_rot_comp_buo,                 &
     &    iphys%i_rot_comp_buo, nvector_snap_rj_2_rtp,                  &
     &    bs_trns%i_rot_comp_buo)
!
      call add_vec_trans_flag_snap(ipol%i_SGS_inertia,                  &
     &    iphys%i_SGS_inertia, nvector_snap_rj_2_rtp,                   &
     &    bs_trns%i_SGS_inertia)
      call add_vec_trans_flag_snap(ipol%i_SGS_Lorentz,                  &
     &    iphys%i_SGS_Lorentz, nvector_snap_rj_2_rtp,                   &
     &    bs_trns%i_SGS_Lorentz)
      call add_vec_trans_flag_snap(ipol%i_SGS_vp_induct,                &
     &    iphys%i_SGS_vp_induct, nvector_snap_rj_2_rtp,                 &
     &    bs_trns%i_SGS_vp_induct)
      call add_vec_trans_flag_snap(ipol%i_SGS_h_flux,                   &
     &    iphys%i_SGS_h_flux, nvector_snap_rj_2_rtp,                    &
     &    bs_trns%i_SGS_h_flux)
      call add_vec_trans_flag_snap(ipol%i_SGS_c_flux,                   &
     &    iphys%i_SGS_c_flux, nvector_snap_rj_2_rtp,                    &
     &    bs_trns%i_SGS_c_flux)
!
      call add_vec_trans_flag_snap(ipol%i_buoyancy,                     &
     &    iphys%i_buoyancy, nvector_snap_rj_2_rtp, bs_trns%i_buoyancy)
      call add_vec_trans_flag_snap(ipol%i_comp_buo,                     &
     &    iphys%i_comp_buo, nvector_snap_rj_2_rtp, bs_trns%i_comp_buo)
!
      call add_vec_trans_flag_snap(ipol%i_geostrophic,                  &
     &    iphys%i_geostrophic, nvector_snap_rj_2_rtp,                   &
     &    bs_trns%i_geostrophic)
!
      call add_vec_trans_flag_snap(ipol%i_h_flux_w_sgs,                 &
     &    iphys%i_h_flux_w_sgs, nvector_snap_rj_2_rtp,                  &
     &    bs_trns%i_h_flux_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_c_flux_w_sgs,                 &
     &    iphys%i_c_flux_w_sgs, nvector_snap_rj_2_rtp,                  &
     &    bs_trns%i_c_flux_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_inertia_w_sgs,                &
     &    iphys%i_inertia_w_sgs, nvector_snap_rj_2_rtp,                 &
     &    bs_trns%i_inertia_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_Lorentz_w_sgs,                &
     &    iphys%i_Lorentz_w_sgs, nvector_snap_rj_2_rtp,                 &
     &    bs_trns%i_Lorentz_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_vp_induct_w_sgs,              &
     &    iphys%i_vp_induct_w_sgs, nvector_snap_rj_2_rtp,               &
     &    bs_trns%i_vp_induct_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_mag_induct_w_sgs,             &
     &    iphys%i_mag_induct_w_sgs, nvector_snap_rj_2_rtp,              &
     &    bs_trns%i_mag_induct_w_sgs)
!
      call add_vec_trans_flag_snap(ipol%i_SGS_rot_inertia,              &
     &    iphys%i_SGS_rot_inertia, nvector_snap_rj_2_rtp,               &
     &    bs_trns%i_SGS_rot_inertia)
      call add_vec_trans_flag_snap(ipol%i_SGS_rot_Lorentz,              &
     &    iphys%i_SGS_rot_Lorentz, nvector_snap_rj_2_rtp,               &
     &    bs_trns%i_SGS_rot_Lorentz)
      call add_vec_trans_flag_snap(ipol%i_SGS_induction,                &
     &    iphys%i_SGS_induction, nvector_snap_rj_2_rtp,                 &
     &    bs_trns%i_SGS_induction)
!
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_press_grad, iphys%i_press_grad,                        &
     &    nvector_snap_rj_2_rtp, bs_trns%i_press_grad)
      call add_vec_trans_flag_snap(ipol%i_induction, iphys%i_induction, &
     &    nvector_snap_rj_2_rtp, bs_trns%i_induction)
!
      call add_vec_trans_flag_snap(ipol%i_grad_t, iphys%i_grad_t,       &
     &    nvector_snap_rj_2_rtp, bs_trns%i_grad_t)
      call add_vec_trans_flag_snap(ipol%i_grad_composit,                &
     &    iphys%i_grad_composit, nvector_snap_rj_2_rtp,                 &
     &    bs_trns%i_grad_composit)
!
      call add_vec_trans_flag_snap(ipol%i_grad_vx, iphys%i_grad_vx,     &
     &    nvector_snap_rj_2_rtp, bs_trns%i_grad_vx)
      call add_vec_trans_flag_snap(ipol%i_grad_vy, iphys%i_grad_vy,     &
     &    nvector_snap_rj_2_rtp, bs_trns%i_grad_vy)
      call add_vec_trans_flag_snap(ipol%i_grad_vz, iphys%i_grad_vz,     &
     &    nvector_snap_rj_2_rtp, bs_trns%i_grad_vz)
!
      end subroutine b_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_snap(ipol, iphys,               &
     &          nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp, bs_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      integer(kind = kint), intent(in) :: nvector_snap_rj_2_rtp
      integer(kind = kint), intent(inout) :: nscalar_snap_rj_2_rtp
      type(phys_address), intent(inout) :: bs_trns
!
!
      nscalar_snap_rj_2_rtp = 0
!      if(b_trns%i_temp.eq.0 .or. ipol%i_par_temp.gt.0) then
        call add_scl_trans_flag_snap(ipol%i_temp, iphys%i_temp,         &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                 &
     &    bs_trns%i_temp)
!      end if
!      if(b_trns%i_light .eq. 0) then
        call add_scl_trans_flag_snap(ipol%i_light, iphys%i_light,       &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                 &
     &    bs_trns%i_light)
!      end if
!
      call add_scl_trans_flag_snap(ipol%i_press, iphys%i_press,         &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                 &
     &    bs_trns%i_press)
      call add_scl_trans_flag_snap(ipol%i_par_temp, iphys%i_par_temp,   &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                 &
     &    bs_trns%i_par_temp)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_filter_temp, iphys%i_filter_temp,                      &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                 &
     &    bs_trns%i_filter_temp)
      call add_scl_trans_flag_snap(ipol%i_t_diffuse, iphys%i_t_diffuse, &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                 &
     &    bs_trns%i_t_diffuse)
      call add_scl_trans_flag_snap(ipol%i_c_diffuse, iphys%i_c_diffuse, &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                 &
     &    bs_trns%i_c_diffuse)
!
      call add_scl_trans_flag_snap(ipol%i_h_advect, iphys%i_h_advect,   &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                 &
     &    bs_trns%i_h_advect)
      call add_scl_trans_flag_snap(ipol%i_c_advect, iphys%i_c_advect,   &
     &    nvector_snap_rj_2_rtp, nscalar_snap_rj_2_rtp,                 &
     &    bs_trns%i_c_advect)
!
      call add_scl_trans_flag_snap(ipol%i_div_Coriolis,                 &
     &    iphys%i_div_Coriolis, nvector_snap_rj_2_rtp,                  &
     &    nscalar_snap_rj_2_rtp, bs_trns%i_div_Coriolis)
!
      call add_scl_trans_flag_snap(ipol%i_SGS_div_inertia,              &
     &    iphys%i_SGS_div_inertia, nvector_snap_rj_2_rtp,               &
     &    nscalar_snap_rj_2_rtp, bs_trns%i_SGS_div_inertia)
      call add_scl_trans_flag_snap(ipol%i_SGS_div_Lorentz,              &
     &    iphys%i_SGS_div_Lorentz, nvector_snap_rj_2_rtp,               &
     &    nscalar_snap_rj_2_rtp, bs_trns%i_SGS_div_Lorentz)
      call add_scl_trans_flag_snap(ipol%i_SGS_div_h_flux,               &
     &    iphys%i_SGS_div_h_flux, nvector_snap_rj_2_rtp,                &
     &    nscalar_snap_rj_2_rtp, bs_trns%i_SGS_div_h_flux)
      call add_scl_trans_flag_snap(ipol%i_SGS_div_c_flux,               &
     &    iphys%i_SGS_div_c_flux, nvector_snap_rj_2_rtp,                &
     &    nscalar_snap_rj_2_rtp, bs_trns%i_SGS_div_c_flux)
!
      end subroutine b_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_vector_snap                            &
     &         (ipol, iphys, nvector_snap_rtp_2_rj, fs_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      integer(kind = kint), intent(inout) :: nvector_snap_rtp_2_rj
      type(phys_address), intent(inout) :: fs_trns
!
      nvector_snap_rtp_2_rj = 0
      call add_vec_trans_flag_snap(ipol%i_coriolis, iphys%i_coriolis,   &
     &    nvector_snap_rtp_2_rj, fs_trns%i_coriolis)
      call add_vec_trans_flag_snap(ipol%i_electric, iphys%i_electric,   &
     &    nvector_snap_rtp_2_rj, fs_trns%i_electric)
      call add_vec_trans_flag_snap(ipol%i_poynting, iphys%i_poynting,   &
     &    nvector_snap_rtp_2_rj, fs_trns%i_poynting)
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_mag_stretch, iphys%i_mag_stretch,                      &
     &    nvector_snap_rtp_2_rj, fs_trns%i_mag_stretch)
!
      end subroutine f_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_snap(ipol, iphys,               &
     &          nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj, fs_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      integer(kind = kint), intent(in) :: nvector_snap_rtp_2_rj
      integer(kind = kint), intent(inout) :: nscalar_snap_rtp_2_rj
      type(phys_address), intent(inout) :: fs_trns
!
!
      nscalar_snap_rtp_2_rj = 0
      call add_scl_trans_flag_snap(ipol%i_me_gen, iphys%i_me_gen,       &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_me_gen)
      call add_scl_trans_flag_snap(ipol%i_ujb, iphys%i_ujb,             &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj, fs_trns%i_ujb)
      call add_scl_trans_flag_snap(ipol%i_nega_ujb, iphys%i_nega_ujb,   &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_nega_ujb)
!
      call add_scl_trans_flag_snap(ipol%i_buo_gen, iphys%i_buo_gen,     &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_buo_gen)
      call add_scl_trans_flag_snap(ipol%i_c_buo_gen, iphys%i_c_buo_gen, &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_c_buo_gen)
      call add_scl_trans_flag_snap(ipol%i_f_buo_gen, iphys%i_f_buo_gen, &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_f_buo_gen)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_reynolds_wk, iphys%i_reynolds_wk,                      &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_reynolds_wk)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_Lor_wk, iphys%i_SGS_Lor_wk,                        &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_SGS_Lor_wk)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_me_gen, iphys%i_SGS_me_gen,                        &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_SGS_me_gen)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_buo_wk, iphys%i_SGS_buo_wk,                        &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_SGS_buo_wk)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_comp_buo_wk, iphys%i_SGS_comp_buo_wk,              &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_SGS_comp_buo_wk)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_h_flux, iphys%i_Csim_SGS_h_flux,              &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_Csim_SGS_h_flux)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_c_flux, iphys%i_Csim_SGS_c_flux,              &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_Csim_SGS_c_flux)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_m_flux, iphys%i_Csim_SGS_m_flux,              &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_Csim_SGS_m_flux)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_Lorentz, iphys%i_Csim_SGS_Lorentz,            &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_Csim_SGS_Lorentz)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_induction, iphys%i_Csim_SGS_induction,        &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_Csim_SGS_induction)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_buoyancy, iphys%i_Csim_SGS_buoyancy,          &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_Csim_SGS_buoyancy)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_comp_buo, iphys%i_Csim_SGS_comp_buo,          &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_Csim_SGS_comp_buo)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_velo_scale, iphys%i_velo_scale,                        &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_velo_scale)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_magne_scale, iphys%i_magne_scale,                      &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_magne_scale)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_temp_scale, iphys%i_temp_scale,                        &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_temp_scale)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_comp_scale, iphys%i_comp_scale,                        &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    fs_trns%i_comp_scale)
!
      end subroutine f_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_snap
