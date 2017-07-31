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
!!      subroutine copy_mhd_spectr_to_send                              &
!!     &         (ncomp_send, b_trns, comm_rj, ipol, rj_fld, n_WS, WS)
!!        type(phys_address), intent(in) :: b_trns
!!      subroutine copy_mhd_spectr_from_recv                            &
!!     &         (ncomp_recv, f_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!!        type(phys_address), intent(in) :: f_trns
!!
!!      subroutine copy_SGS_spectr_to_send(nnod_pole, ncomp_send,       &
!!     &          bg_trns, sph_rj, comm_rj, ipol, rj_fld, n_WS, WS)
!!        type(phys_address), intent(in) :: bg_trns
!!      subroutine copy_SGS_vec_spec_from_trans                        &
!!     &         (ncomp_recv, fg_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!!        type(phys_address), intent(in) :: fg_trns
!!
!!      subroutine copy_snap_spectr_to_send(nnod_pole, ncomp_send,      &
!!     &          bs_trns, sph_rj, comm_rj, ipol, rj_fld,               &
!!     &          n_WS, WS, v_pl_local)
!!      subroutine copy_snap_vec_spec_from_trans                        &
!!     &         (ncomp_recv, fs_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!!        type(phys_address), intent(in) :: bs_trns
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine copy_tmp_scl_spec_from_trans                         &
!!     &         (ncomp_recv, ft_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(in) :: ft_trns
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module copy_sph_MHD_4_send_recv
!
      use m_precision
      use m_machine_parameter
      use copy_spectr_4_sph_trans
!
      use t_sph_trans_comm_tbl
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_spectr_to_send                                &
     &         (ncomp_send, b_trns, comm_rj, ipol, rj_fld, n_WS, WS)
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: b_trns
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_velo, b_trns%i_velo,                       &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_vort, b_trns%i_vort,                       &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_magne, b_trns%i_magne,                     &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_current, b_trns%i_current,                 &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_filter_velo, b_trns%i_filter_velo,         &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_filter_vort, b_trns%i_filter_vort,         &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_filter_magne, b_trns%i_filter_magne,       &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_filter_current, b_trns%i_filter_current,   &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_wide_fil_velo, b_trns%i_wide_fil_velo,     &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_wide_fil_vort, b_trns%i_wide_fil_vort,     &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send                                    &
     &   (ncomp_send, ipol%i_wide_fil_magne, b_trns%i_wide_fil_magne,   &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_wide_fil_current, b_trns%i_wide_fil_current,           &
     &    comm_rj, rj_fld, n_WS, WS)
!
!
      call sel_sph_rj_scalar_to_send                                    &
     &   (ncomp_send, ipol%i_temp, b_trns%i_temp,                       &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_scalar_to_send                                    &
     &   (ncomp_send, ipol%i_light, b_trns%i_light,                     &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_scalar_to_send                                    &
     &   (ncomp_send, ipol%i_filter_temp, b_trns%i_filter_temp,         &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_scalar_to_send                                    &
     &   (ncomp_send, ipol%i_filter_comp, b_trns%i_filter_comp,         &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_scalar_to_send                                    &
     &   (ncomp_send, ipol%i_wide_fil_temp, b_trns%i_wide_fil_temp,     &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_scalar_to_send                                    &
     &   (ncomp_send, ipol%i_wide_fil_comp, b_trns%i_wide_fil_comp,     &
     &    comm_rj, rj_fld, n_WS, WS)
!
!
      end subroutine copy_mhd_spectr_to_send
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_spectr_from_recv                              &
     &         (ncomp_recv, f_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: f_trns
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
!
!   advection flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_m_advect, f_trns%i_m_advect,                           &
     &    comm_rj, n_WR, WR, rj_fld)
!   Coriolis flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_coriolis, f_trns%i_coriolis,                           &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &     ipol%i_rot_Coriolis, f_trns%i_rot_Coriolis,                  &
     &    comm_rj, n_WR, WR, rj_fld)
!   Lorentz flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_lorentz, f_trns%i_lorentz,                             &
     &    comm_rj, n_WR, WR, rj_fld)
!
!   induction flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_vp_induct, f_trns%i_vp_induct,                         &
     &    comm_rj, n_WR, WR, rj_fld)
!
!   heat flux flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_h_flux, f_trns%i_h_flux,                               &
     &    comm_rj, n_WR, WR, rj_fld)
!   composition flux flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_c_flux, f_trns%i_c_flux,                               &
     &    comm_rj, n_WR, WR, rj_fld)
!
!
!
!   filtered advection flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_inertia, f_trns%i_SGS_inertia,                     &
     &    comm_rj, n_WR, WR, rj_fld)
!   filtered Lorentz flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_Lorentz, f_trns%i_SGS_Lorentz,                     &
     &    comm_rj, n_WR, WR, rj_fld)
!
!   filtered induction flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_vp_induct, f_trns%i_SGS_vp_induct,                 &
     &    comm_rj, n_WR, WR, rj_fld)
!
!   filtered heat flux flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_h_flux, f_trns%i_SGS_h_flux,                       &
     &    comm_rj, n_WR, WR, rj_fld)
!   filtered composition flux flag
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_c_flux, f_trns%i_SGS_c_flux,                       &
     &    comm_rj, n_WR, WR, rj_fld)
!
      end  subroutine copy_mhd_spectr_from_recv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_SGS_spectr_to_send(nnod_pole, ncomp_send,         &
     &          bg_trns, sph_rj, comm_rj, ipol, rj_fld, n_WS, WS)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: bg_trns
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: nnod_pole
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!      Vectors
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_inertia, bg_trns%i_SGS_inertia,                    &
     &     comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_Lorentz, bg_trns%i_SGS_Lorentz,                    &
     &     comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_vp_induct, bg_trns%i_SGS_vp_induct,                &
     &     comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_h_flux, bg_trns%i_SGS_h_flux,                      &
     &     comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_c_flux, bg_trns%i_SGS_c_flux,                      &
     &     comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_wide_SGS_inertia, bg_trns%i_wide_SGS_inertia,          &
     &     comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_wide_SGS_Lorentz, bg_trns%i_wide_SGS_Lorentz,          &
     &     comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_wide_SGS_vp_induct, bg_trns%i_wide_SGS_vp_induct,      &
     &     comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_wide_SGS_h_flux, bg_trns%i_wide_SGS_h_flux,            &
     &     comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_wide_SGS_c_flux, bg_trns%i_wide_SGS_c_flux,            &
     &     comm_rj, rj_fld, n_WS, WS)
!
      end subroutine copy_SGS_spectr_to_send
!
!-----------------------------------------------------------------------
!
      subroutine copy_SGS_vec_spec_from_trans                          &
     &         (ncomp_recv, fg_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: fg_trns
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
!
!      Vectors
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_inertia, fg_trns%i_SGS_inertia,                    &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_Lorentz, fg_trns%i_SGS_Lorentz,                    &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_vp_induct, fg_trns%i_SGS_vp_induct,                &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_h_flux, fg_trns%i_SGS_h_flux,                      &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_c_flux, fg_trns%i_SGS_c_flux,                      &
     &    comm_rj, n_WR, WR, rj_fld)
!
!      Scalars
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_Csim_SGS_buoyancy, fg_trns%i_Csim_SGS_buoyancy,        &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_Csim_SGS_comp_buo, fg_trns%i_Csim_SGS_comp_buo,        &
     &    comm_rj, n_WR, WR, rj_fld)
!
      end  subroutine copy_SGS_vec_spec_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_Csym_vec_spec_from_trans                          &
     &         (ncomp_recv, fg_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: fg_trns
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
!
!      Vectors
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_inertia, fg_trns%i_SGS_inertia,                    &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_Lorentz, fg_trns%i_SGS_Lorentz,                    &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_vp_induct, fg_trns%i_SGS_vp_induct,                &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_h_flux, fg_trns%i_SGS_h_flux,                      &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_c_flux, fg_trns%i_SGS_c_flux,                      &
     &    comm_rj, n_WR, WR, rj_fld)
!
      end  subroutine copy_Csym_vec_spec_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_spectr_to_send(nnod_pole, ncomp_send,        &
     &          bs_trns, sph_rj, comm_rj, ipol, rj_fld,                 &
     &          n_WS, WS, v_pl_local)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: bs_trns
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: nnod_pole
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pl_local(nnod_pole,ncomp_send)
!
!      Vectors
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_velo, bs_trns%i_velo, comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_vort, bs_trns%i_vort, comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_magne, bs_trns%i_magne, comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_current, bs_trns%i_current, comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_v_diffuse, bs_trns%i_v_diffuse,                        &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_w_diffuse, bs_trns%i_w_diffuse,                        &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_vp_diffuse, bs_trns%i_vp_diffuse,                      &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_b_diffuse, bs_trns%i_b_diffuse,                        &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_inertia, bs_trns%i_rot_inertia,                    &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_Coriolis, bs_trns%i_rot_Coriolis,                  &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_Lorentz, bs_trns%i_rot_Lorentz,                    &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_buoyancy, bs_trns%i_rot_buoyancy,                  &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_rot_comp_buo, bs_trns%i_rot_comp_buo,                  &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_inertia, bs_trns%i_SGS_inertia,                    &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_Lorentz, bs_trns%i_SGS_Lorentz,                    &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_vp_induct, bs_trns%i_SGS_vp_induct,                &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_h_flux, bs_trns%i_SGS_h_flux,                      &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_c_flux, bs_trns%i_SGS_c_flux,                      &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_buoyancy, bs_trns%i_buoyancy,                          &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_comp_buo, bs_trns%i_comp_buo,                          &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_geostrophic, bs_trns%i_geostrophic,                    &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_h_flux_w_sgs, bs_trns%i_h_flux_w_sgs,                  &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_c_flux_w_sgs, bs_trns%i_c_flux_w_sgs,                  &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_inertia_w_sgs, bs_trns%i_inertia_w_sgs,                &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_Lorentz_w_sgs, bs_trns%i_Lorentz_w_sgs,                &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_vp_induct_w_sgs, bs_trns%i_vp_induct_w_sgs,            &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_mag_induct_w_sgs, bs_trns%i_mag_induct_w_sgs,          &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_rot_inertia, bs_trns%i_SGS_rot_inertia,            &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_rot_Lorentz, bs_trns%i_SGS_rot_Lorentz,            &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_SGS_induction, bs_trns%i_SGS_induction,                &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_press_grad, bs_trns%i_press_grad,                      &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_induction, bs_trns%i_induction,                        &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_grad_t, bs_trns%i_grad_t, comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_grad_composit, bs_trns%i_grad_composit,                &
     &    comm_rj, rj_fld, n_WS, WS)
!
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_grad_vx, bs_trns%i_grad_vx,                            &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_grad_vy, bs_trns%i_grad_vy,                            &
     &    comm_rj, rj_fld, n_WS, WS)
      call sel_sph_rj_vector_to_send(ncomp_send,                        &
     &    ipol%i_grad_vz, bs_trns%i_grad_vz,                            &
     &    comm_rj, rj_fld, n_WS, WS)
!
!      Scalar fields
!
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_temp, bs_trns%i_temp, nnod_pole,                       &
     &    sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_light, bs_trns%i_light, nnod_pole,                     &
     &    sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
!
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_press, bs_trns%i_press, nnod_pole,                     &
     &    sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_par_temp, bs_trns%i_par_temp, nnod_pole,               &
     &    sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_filter_temp, bs_trns%i_filter_temp, nnod_pole,         &
     &    sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_t_diffuse, bs_trns%i_t_diffuse, nnod_pole,             &
     &    sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_c_diffuse, bs_trns%i_c_diffuse, nnod_pole,             &
     &    sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
!
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_h_advect, bs_trns%i_h_advect, nnod_pole,               &
     &    sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_c_advect, bs_trns%i_c_advect, nnod_pole,               &
     &    sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
!
      call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                   &
     &    ipol%i_div_Coriolis, bs_trns%i_div_Coriolis, nnod_pole,       &
     &    sph_rj, comm_rj, rj_fld, n_WS, WS, v_pl_local)
!
      end subroutine copy_snap_spectr_to_send
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_spec_from_trans                          &
     &         (ncomp_recv, fs_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: fs_trns
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
!
!      Vectors
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_coriolis, fs_trns%i_coriolis,                          &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_electric, fs_trns%i_electric,                          &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_poynting, fs_trns%i_poynting,                          &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_vector_from_recv(ncomp_recv,                      &
     &    ipol%i_mag_stretch, fs_trns%i_mag_stretch,                    &
     &    comm_rj, n_WR, WR, rj_fld)
!
!      Scalars
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_me_gen, fs_trns%i_me_gen,                              &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_ujb, fs_trns%i_ujb,                                    &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_nega_ujb, fs_trns%i_nega_ujb,                          &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_buo_gen, fs_trns%i_buo_gen,                            &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_c_buo_gen, fs_trns%i_c_buo_gen,                        &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_f_buo_gen, fs_trns%i_f_buo_gen,                        &
     &    comm_rj, n_WR, WR, rj_fld)
!
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_reynolds_wk, fs_trns%i_reynolds_wk,                    &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_Lor_wk, fs_trns%i_SGS_Lor_wk,                      &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_buo_wk, fs_trns%i_SGS_buo_wk,                      &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_comp_buo_wk, fs_trns%i_SGS_comp_buo_wk,            &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_SGS_me_gen, fs_trns%i_SGS_me_gen,                      &
     &    comm_rj, n_WR, WR, rj_fld)
!
!  Model coefficients
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_Csim_SGS_m_flux, fs_trns%i_Csim_SGS_m_flux,            &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_Csim_SGS_Lorentz, fs_trns%i_Csim_SGS_Lorentz,          &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_Csim_SGS_induction, fs_trns%i_Csim_SGS_induction,      &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_Csim_SGS_h_flux, fs_trns%i_Csim_SGS_h_flux,            &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_Csim_SGS_c_flux, fs_trns%i_Csim_SGS_c_flux,            &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_Csim_SGS_buoyancy, fs_trns%i_Csim_SGS_buoyancy,        &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_Csim_SGS_comp_buo, fs_trns%i_Csim_SGS_comp_buo,        &
     &    comm_rj, n_WR, WR, rj_fld)
!
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_velo_scale, fs_trns%i_velo_scale,                      &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_magne_scale, fs_trns%i_magne_scale,                    &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_temp_scale, fs_trns%i_temp_scale,                      &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_comp_scale, fs_trns%i_comp_scale,                      &
     &    comm_rj, n_WR, WR, rj_fld)
!
      end  subroutine copy_snap_vec_spec_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tmp_scl_spec_from_trans                           &
     &         (ncomp_recv, ft_trns, comm_rj, ipol, n_WR, WR, rj_fld)
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: ft_trns
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
!
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_grad_vx, ft_trns%i_grad_vx,                            &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_grad_vy, ft_trns%i_grad_vy,                            &
     &    comm_rj, n_WR, WR, rj_fld)
      call sel_sph_rj_scalar_from_recv(ncomp_recv,                      &
     &    ipol%i_grad_vz, ft_trns%i_grad_vz,                            &
     &    comm_rj, n_WR, WR, rj_fld)
!
      end  subroutine copy_tmp_scl_spec_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_sph_MHD_4_send_recv
