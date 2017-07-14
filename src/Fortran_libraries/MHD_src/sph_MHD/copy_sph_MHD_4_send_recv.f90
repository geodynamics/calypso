!>@file   copy_sph_MHD_4_send_recv.f90
!!@brief  module copy_sph_MHD_4_send_recv
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Copy spectrum data and field data to spherical transform buffer
!!       for dynamo simulation
!!
!!@verbatim
!!      subroutine copy_mhd_spectr_to_send(ncomp_send, n_WS, WS)
!!      subroutine copy_mhd_spectr_from_recv(ncomp_recv, n_WR, WR)
!!
!!      subroutine copy_snap_spectr_to_send                            &
!!     &         (ncomp_send, n_WS, WS, v_pl_local)
!!      subroutine copy_snap_vec_spec_from_trans(ncomp_recv, n_WR, WR)
!!
!!      subroutine copy_tmp_scl_spec_from_trans(ncomp_recv, n_WR, WR)
!!@endverbatim
!
      module copy_sph_MHD_4_send_recv
!
      use m_precision
      use m_machine_parameter
      use m_sph_phys_address
      use copy_spectr_4_sph_trans
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_spectr_to_send(ncomp_send, n_WS, WS)
!
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_velo, b_trns%i_velo, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_vort, b_trns%i_vort, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_magne, b_trns%i_magne, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_current, b_trns%i_current, n_WS, WS)
!
      call sel_sph_rj_scalar_to_send                                    &
     &   (ncomp_send, ipol%i_temp, b_trns%i_temp, n_WS, WS)
      call sel_sph_rj_scalar_to_send                                    &
     &   (ncomp_send, ipol%i_light, b_trns%i_light, n_WS, WS)
!
      end subroutine copy_mhd_spectr_to_send
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_spectr_from_recv(ncomp_recv, n_WR, WR)
!
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
!   advection flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_m_advect, f_trns%i_m_advect, n_WR, WR)
!   Coriolis flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_coriolis, f_trns%i_coriolis, n_WR, WR)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_rot_Coriolis, f_trns%i_rot_Coriolis, n_WR, WR)
!   Lorentz flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_lorentz, f_trns%i_lorentz, n_WR, WR)
!
!   induction flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_vp_induct, f_trns%i_vp_induct, n_WR, WR)
!
!   heat flux flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_h_flux, f_trns%i_h_flux, n_WR, WR)
!   composition flux flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                     &
     &      ipol%i_c_flux, f_trns%i_c_flux, n_WR, WR)
!
      end  subroutine copy_mhd_spectr_from_recv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_snap_spectr_to_send                              &
     &         (ncomp_send, n_WS, WS, v_pl_local)
!
      use m_spheric_parameter
      use m_addresses_trans_sph_snap
      use m_work_pole_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pl_local(nnod_pole,ncomp_send)
!
!      Vectors
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_velo, bs_trns%i_velo, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_vort, bs_trns%i_vort, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_magne, bs_trns%i_magne, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_current, bs_trns%i_current, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_v_diffuse, bs_trns%i_v_diffuse, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_w_diffuse, bs_trns%i_w_diffuse, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_vp_diffuse, bs_trns%i_vp_diffuse, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_b_diffuse, bs_trns%i_b_diffuse, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_inertia, bs_trns%i_rot_inertia, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_Coriolis, bs_trns%i_rot_Coriolis, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_Lorentz, bs_trns%i_rot_Lorentz, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_buoyancy, bs_trns%i_rot_buoyancy, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_comp_buo, bs_trns%i_rot_comp_buo, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_press_grad, bs_trns%i_press_grad, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_induction, bs_trns%i_induction, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_grad_t, bs_trns%i_grad_t, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_grad_composit, bs_trns%i_grad_composit, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &      ipol%i_grad_vx, bs_trns%i_grad_vx, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &      ipol%i_grad_vy, bs_trns%i_grad_vy, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &      ipol%i_grad_vz, bs_trns%i_grad_vz, n_WS, WS)
!
!      Scalar fields
!
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_temp, bs_trns%i_temp, n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_light, bs_trns%i_light, n_WS, WS, v_pl_local)
!
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_press, bs_trns%i_press, n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_par_temp, bs_trns%i_par_temp, n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_filter_temp, bs_trns%i_filter_temp,                    &
     &    n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_t_diffuse, bs_trns%i_t_diffuse,                        &
     &    n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_c_diffuse, bs_trns%i_c_diffuse,                        &
     &    n_WS, WS, v_pl_local)
!
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_h_advect, bs_trns%i_h_advect,                          &
     &    n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_c_advect, bs_trns%i_c_advect,                          &
     &    n_WS, WS, v_pl_local)
!
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_div_Coriolis, bs_trns%i_div_Coriolis,                  &
     &    n_WS, WS, v_pl_local)
!
      end subroutine copy_snap_spectr_to_send
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_spec_from_trans(ncomp_recv, n_WR, WR)
!
      use m_addresses_trans_sph_snap
!
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
!      Vectors
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_coriolis, fs_trns%i_coriolis, n_WR, WR)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_electric, fs_trns%i_electric, n_WR, WR)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_poynting, fs_trns%i_poynting, n_WR, WR)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_mag_stretch, fs_trns%i_mag_stretch, n_WR, WR)
!
!      Scalars
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_me_gen, fs_trns%i_me_gen, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_ujb, fs_trns%i_ujb, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_nega_ujb, fs_trns%i_nega_ujb, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_buo_gen, fs_trns%i_buo_gen, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_c_buo_gen, fs_trns%i_c_buo_gen, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_f_buo_gen, fs_trns%i_f_buo_gen, n_WR, WR)
!
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_velo_scale, fs_trns%i_velo_scale, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_magne_scale, fs_trns%i_magne_scale, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_temp_scale, fs_trns%i_temp_scale, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_comp_scale, fs_trns%i_comp_scale, n_WR, WR)
!
      end  subroutine copy_snap_vec_spec_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_scl_spec_from_trans(ncomp_recv, n_WR, WR)
!
      use m_addresses_trans_sph_tmp
!
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
!
!
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_grad_vx, ft_trns%i_grad_vx, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_grad_vy, ft_trns%i_grad_vy, n_WR, WR)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &      ipol%i_grad_vz, ft_trns%i_grad_vz, n_WR, WR)
!
      end  subroutine copy_tmp_scl_spec_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_sph_MHD_4_send_recv
