!>@file   sph_trans_w_coriols.f90
!!@brief  module sph_trans_w_coriols
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Spherical harmonics transform
!!       including Coriolis terms
!!
!!@verbatim
!!      subroutine sph_b_trans_w_coriolis                               &
!!     &         (sph, comms_sph, fl_prop, sph_bc_U, omega_sph,         &
!!     &          b_trns, f_trns, trans_p, gt_cor, trns_bwd,            &
!!     &          WK_leg, WK_FFTs_MHD, cor_rlm, SR_sig, SR_r)
!!      subroutine sph_f_trans_w_coriolis                               &
!!     &         (sph, comms_sph, trans_p, cor_rlm, f_trns,             &
!!     &          trns_fwd, WK_leg, WK_FFTs_MHD, SR_sig, SR_r)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: b_trns
!!        type(phys_address), intent(in) :: f_trns
!!        type(spherical_transform_data), intent(inout) :: trns_bwd
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine sph_b_trans_licv(sph_rlm, comm_rlm, comm_rj,         &
!!     &          fl_prop, sph_bc_U, omega_sph, leg, gt_cor,            &
!!     &          b_trns, f_trns, trns_bwd, cor_rlm, SR_sig, SR_r)
!!      subroutine sph_f_trans_licv(sph_rlm, comm_rlm, comm_rj,         &
!!     &          cor_rlm, f_trns, trns_fwd, SR_sig, SR_r)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!!        type(phys_address), intent(in) :: b_trns
!!        type(phys_address), intent(in) :: f_trns
!!        type(spherical_transform_data), intent(in) :: trns_bwd
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!   input /outpt arrays for single vector
!!      radial component:      v_rtp(i_rtp,1)
!!      elevetional component: v_rtp(i_rtp,2)
!!      azimuthal component:   v_rtp(i_rtp,3)
!!
!!      Poloidal component:          WR(3*i_rj-2)
!!      diff. of Poloidal component: WR(3*i_rj-1)
!!      Toroidal component:          WR(3*i_rj  )
!!@endverbatim
!!
!!@param ncomp_trans Number of components for transform
!!@param nvector     Number of vectors for transform
!!@param nscalar     Number of scalars for transform
!!
!!@param n_WS Number of components for send buffrt
!!@param n_WR Number of components for recieve buffer
!!@param WS Send buffer
!!@param WR Recieve buffer
!!@param fld_rtp Field data to make nonlinear terms
!!@param frc_rtp Nonlinear terms
!
      module sph_trans_w_coriols
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_elapsed_labels_SPH_TRNS
      use m_elapsed_labels_4_MHD
      use m_machine_parameter
      use spherical_SRs_N
!
      use t_solver_SR
      use t_physical_property
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_poloidal_rotation
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_params_sph_MHD
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_w_coriolis                                 &
     &         (sph, comms_sph, fl_prop, sph_bc_U, omega_sph,           &
     &          b_trns, f_trns, trans_p, gt_cor, trns_bwd,              &
     &          WK_leg, WK_FFTs_MHD, cor_rlm, SR_sig, SR_r)
!
      use cal_coriolis_rlm
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
      type(phys_address), intent(in) :: b_trns
      type(phys_address), intent(in) :: f_trns
!
      type(spherical_transform_data), intent(inout) :: trns_bwd
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iflag_debug .gt. 0) write(*,*) 'calypso_sph_comm_rj_2_rlm_N'
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+1)
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+10)
      call calypso_sph_comm_N(trns_bwd%ncomp,                           &
     &    comms_sph%comm_rj, comms_sph%comm_rlm, SR_sig, SR_r)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+1)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+10)
!
      if(iflag_debug .gt. 0) write(*,*) 's_cal_coriolis_rlm'
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+8)
      call s_cal_coriolis_rlm                                           &
     &   (trns_bwd%ncomp, sph%sph_rlm, comms_sph%comm_rlm,              &
     &    fl_prop, sph_bc_U, omega_sph, b_trns, f_trns,                 &
     &    trans_p%leg, gt_cor, SR_r%n_WR, SR_r%WR(1), cor_rlm)
      call finish_send_recv_sph(comms_sph%comm_rj, SR_sig)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+8)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+5)
      if(iflag_debug .gt. 0) write(*,*)  'sel_backward_legendre_trans', &
     &       trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar
      call sel_backward_legendre_trans                                  &
     &   (trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar,     &
     &    sph%sph_rlm, sph%sph_rtm, comms_sph%comm_rlm,                 &
     &    comms_sph%comm_rtm, trans_p%leg, trans_p%idx_trns,            &
     &    SR_r%n_WR, SR_r%n_WS, SR_r%WR(1), SR_r%WS(1), WK_leg)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+5)
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &      'calypso_sph_comm_rtm_2_rtp_N'
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+2)
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+10)
      call calypso_sph_comm_N(trns_bwd%ncomp,                           &
     &    comms_sph%comm_rtm, comms_sph%comm_rtp, SR_sig, SR_r)
      call finish_send_recv_sph(comms_sph%comm_rtm, SR_sig)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+2)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+10)
!
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+7)
      if(iflag_debug .gt. 0) write(*,*) 'back_FFT_select_from_recv',    &
     &        trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar
      call back_FFT_select_from_recv(sph%sph_rtp, comms_sph%comm_rtp,   &
     &    trns_bwd%ncomp, SR_r%n_WR, SR_r%WR(1), trns_bwd%fld_rtp,      &
     &    WK_FFTs_MHD)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+7)
!
      if(iflag_debug .gt. 0) write(*,*) 'finish_send_recv_rtm_2_rtp'
!
      end subroutine sph_b_trans_w_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_w_coriolis                                 &
     &         (sph, comms_sph, trans_p, cor_rlm, f_trns,               &
     &          trns_fwd, WK_leg, WK_FFTs_MHD, SR_sig, SR_r)
!
      use cal_coriolis_rlm
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(coriolis_rlm_data), intent(in) :: cor_rlm
      type(phys_address), intent(in) :: f_trns
!
      type(spherical_transform_data), intent(inout) :: trns_fwd
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+7)
      call fwd_FFT_select_to_send(sph%sph_rtp, comms_sph%comm_rtp,      &
     &    trns_fwd%ncomp, SR_r%n_WS, trns_fwd%fld_rtp, SR_r%WS(1),      &
     &    WK_FFTs_MHD)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+7)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+3)
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+10)
      call calypso_sph_comm_N(trns_fwd%ncomp,                           &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm, SR_sig, SR_r)
      call finish_send_recv_sph(comms_sph%comm_rtp, SR_sig)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+3)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+10)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+6)
      if(iflag_debug .gt. 0) write(*,*) 'sel_forward_legendre_trans'
      call sel_forward_legendre_trans                                   &
     &   (trns_fwd%ncomp, trns_fwd%num_vector, trns_fwd%num_scalar,     &
     &    sph%sph_rtm, sph%sph_rlm, comms_sph%comm_rtm,                 &
     &    comms_sph%comm_rlm, trans_p%leg, trans_p%idx_trns,            &
     &    SR_r%n_WR, SR_r%n_WS, SR_r%WR(1), SR_r%WS(1), WK_leg)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+6)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'copy_coriolis_terms_rlm'
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+8)
      call copy_coriolis_terms_rlm                                      &
     &   (trns_fwd%ncomp, sph%sph_rlm, comms_sph%comm_rlm,              &
     &    f_trns, cor_rlm, SR_r%n_WS, SR_r%WS(1))
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+8)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+4)
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+10)
      call calypso_sph_comm_N(trns_fwd%ncomp,                           &
     &    comms_sph%comm_rlm, comms_sph%comm_rj, SR_sig, SR_r)
      call finish_send_recv_sph(comms_sph%comm_rlm, SR_sig)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+4)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+10)
!
      end subroutine sph_f_trans_w_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_licv(sph_rlm, comm_rlm, comm_rj,           &
     &          fl_prop, sph_bc_U, omega_sph, leg, gt_cor,              &
     &          b_trns, f_trns, trns_bwd, cor_rlm, SR_sig, SR_r)
!
      use cal_coriolis_rlm
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(legendre_4_sph_trans), intent(in) :: leg
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
      type(phys_address), intent(in) :: b_trns
      type(phys_address), intent(in) :: f_trns
      type(spherical_transform_data), intent(in) :: trns_bwd
!
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+1)
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+10)
      call calypso_sph_comm_N(trns_bwd%ncomp, comm_rj, comm_rlm,        &
     &                        SR_sig, SR_r)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+1)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+10)
!
      if(iflag_debug .gt. 0) write(*,*) 's_cal_coriolis_rlm'
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+8)
      call s_cal_coriolis_rlm(trns_bwd%ncomp, sph_rlm, comm_rlm,        &
     &    fl_prop, sph_bc_U, omega_sph, b_trns, f_trns, leg, gt_cor,    &
     &    SR_r%n_WR, SR_r%WR(1), cor_rlm)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+8)
!
      call finish_send_recv_sph(comm_rj, SR_sig)
!
      end subroutine sph_b_trans_licv
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_licv(sph_rlm, comm_rlm, comm_rj,           &
     &          cor_rlm, f_trns, trns_fwd, SR_sig, SR_r)
!
      use cal_coriolis_rlm
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(coriolis_rlm_data), intent(in) :: cor_rlm
      type(phys_address), intent(in) :: f_trns
      type(spherical_transform_data), intent(in) :: trns_fwd
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+8)
      if(iflag_debug .gt. 0) write(*,*) 'copy_coriolis_terms_rlm'
      call copy_coriolis_terms_rlm(trns_fwd%ncomp, sph_rlm, comm_rlm,   &
     &    f_trns, cor_rlm, SR_r%n_WS, SR_r%WS(1))
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+8)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+4)
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+10)
      call calypso_sph_comm_N(trns_fwd%ncomp, comm_rlm, comm_rj,        &
     &                        SR_sig, SR_r)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+4)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+10)
!
      call finish_send_recv_sph(comm_rlm, SR_sig)
!
      end subroutine sph_f_trans_licv
!
! -----------------------------------------------------------------------
!
      end module sph_trans_w_coriols
