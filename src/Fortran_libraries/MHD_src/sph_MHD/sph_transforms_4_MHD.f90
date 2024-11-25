!>@file   sph_transforms_4_MHD.f90
!!@brief  module sph_transforms_4_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine sph_back_trans_4_MHD(sph, comms_sph, fl_prop,        &
!!     &          sph_bc_U, omega_sph, trans_p, gt_cor, rj_fld,         &
!!     &          b_trns, f_trns, trns_bwd, WK_leg, WK_FFTs_MHD,        &
!!     &          cor_rlm, SR_sig, SR_r)
!!        Input ::  rj_fld
!!        Output :: trns_MHD, cor_rlm
!!      subroutine sph_pole_trans_4_MHD(sph, comms_sph, trans_p, rj_fld,&
!!     &                                trns_bwd, SR_sig, SR_r)
!!        Input ::  rj_fld
!!        Output :: trns_MHD
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_address), intent(in) :: b_trns
!!        type(phys_address), intent(in) :: f_trns
!!        type(spherical_transform_data), intent(inout) :: trns_bwd
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine sph_forward_trans_4_MHD                              &
!!     &         (sph, comms_sph, fl_prop, trans_p, cor_rlm, f_trns,    &
!!     &          trns_fwd, WK_leg, WK_FFTs_MHD, rj_fld, SR_sig, SR_r)
!!        Input :: trns_fwd, cor_rlm
!!        Output ::  rj_fld
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(coriolis_rlm_data), intent(in) :: cor_rlm
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(in) :: f_trns
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine sph_transform_4_licv                                 &
!!     &         (sph_rlm, comm_rlm, comm_rj, fl_prop, sph_bc_U,        &
!!     &          omega_sph, trans_p, gt_cor, trns_MHD,                 &
!!     &          rj_fld, cor_rlm, SR_sig, SR_r)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!!
      module sph_transforms_4_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SPH_TRNS
      use m_legendre_transform_list
!
      use calypso_mpi
!
      use t_physical_property
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_params_sph_MHD
      use t_solver_SR
!
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_4_MHD(sph, comms_sph, fl_prop,          &
     &          sph_bc_U, omega_sph, trans_p, gt_cor, rj_fld,           &
     &          b_trns, f_trns, trns_bwd, WK_leg, WK_FFTs_MHD,          &
     &          cor_rlm, SR_sig, SR_r)
!
      use sph_trans_w_coriols
      use set_address_sph_trans_MHD
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
      type(phys_data), intent(in) :: rj_fld
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
      call check_calypso_sph_comm_buf_N(trns_bwd%ncomp,                 &
     &   comms_sph%comm_rj, comms_sph%comm_rlm, SR_sig, SR_r)
      call check_calypso_sph_comm_buf_N(trns_bwd%ncomp,                 &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp, SR_sig, SR_r)
!
      if(iflag_debug .gt. 0) write(*,*) 'mhd_spectr_to_sendbuf'
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+8)
      call mhd_spectr_to_sendbuf                                        &
     &   (trns_bwd, comms_sph%comm_rj, rj_fld, SR_r%n_WS, SR_r%WS(1))
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+8)
!
      if(trns_bwd%ncomp .eq. 0) return
      call sph_b_trans_w_coriolis(sph, comms_sph, fl_prop, sph_bc_U,    &
     &    omega_sph, b_trns, f_trns, trans_p, gt_cor,                   &
     &    trns_bwd, WK_leg, WK_FFTs_MHD, cor_rlm, SR_sig, SR_r)
!
      end subroutine sph_back_trans_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_4_MHD                                &
     &         (sph, comms_sph, fl_prop, trans_p, cor_rlm, f_trns,      &
     &          trns_fwd, WK_leg, WK_FFTs_MHD, rj_fld, SR_sig, SR_r)
!
      use sph_trans_w_coriols
      use set_address_sph_trans_MHD
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(coriolis_rlm_data), intent(in) :: cor_rlm
      type(phys_address), intent(in) :: f_trns
!
      type(spherical_transform_data), intent(inout) :: trns_fwd
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(work_for_FFTs), intent(inout) :: WK_FFTs_MHD
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call check_calypso_sph_comm_buf_N(trns_fwd%ncomp,                 &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm, SR_sig, SR_r)
      call check_calypso_sph_comm_buf_N(trns_fwd%ncomp,                 &
     &    comms_sph%comm_rlm, comms_sph%comm_rj, SR_sig, SR_r)
!
      if(trns_fwd%ncomp .eq. 0) return
      call sph_f_trans_w_coriolis                                       &
     &   (sph, comms_sph, trans_p, cor_rlm, f_trns, trns_fwd,           &
     &    WK_leg, WK_FFTs_MHD, SR_sig, SR_r)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+9)
      call mhd_spectr_from_recvbuf(trans_p%iflag_SPH_recv,              &
     &    trns_fwd, comms_sph%comm_rj, SR_r%n_WR, SR_r%WR(1), rj_fld)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+9)
!
      end subroutine sph_forward_trans_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_pole_trans_4_MHD(sph, comms_sph, trans_p, rj_fld,  &
     &                                trns_bwd, SR_sig, SR_r)
!
      use set_address_sph_trans_MHD
      use spherical_transforms
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_data), intent(in) :: rj_fld
      type(spherical_transform_data), intent(inout) :: trns_bwd
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call check_calypso_sph_comm_buf_N(trns_bwd%ncomp,                 &
     &    comms_sph%comm_rj, comms_sph%comm_rlm, SR_sig, SR_r)
      call check_calypso_sph_comm_buf_N(trns_bwd%ncomp,                 &
     &    comms_sph%comm_rtm, comms_sph%comm_rtp, SR_sig, SR_r)
!
      if(iflag_debug .gt. 0) write(*,*) 'mhd_spectr_to_sendbuf'
      call mhd_spectr_to_sendbuf(trns_bwd, comms_sph%comm_rj, rj_fld,   &
     &                           SR_r%n_WS, SR_r%WS(1))
!
      if(trns_bwd%ncomp .eq. 0) return
      call pole_b_transform                                             &
     &   (trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar,     &
     &    sph, comms_sph, trans_p,                                      &
     &    trns_bwd%flc_pole, trns_bwd%fld_pole, SR_sig, SR_r)
!
      end subroutine sph_pole_trans_4_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_transform_4_licv                                   &
     &         (sph_rlm, comm_rlm, comm_rj, fl_prop, sph_bc_U,          &
     &          omega_sph, trans_p, gt_cor, trns_MHD,                   &
     &          rj_fld, cor_rlm, SR_sig, SR_r)
!
      use sph_trans_w_coriols
      use set_address_sph_trans_MHD
      use spherical_SRs_N
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!
      type(phys_data), intent(inout) :: rj_fld
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if(trns_MHD%backward%ncomp .eq. 0                                 &
     &   .or. trns_MHD%forward%ncomp .eq. 0) return
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (trns_MHD%backward%ncomp, comm_rj, comm_rlm, SR_sig, SR_r)
      call check_calypso_sph_comm_buf_N                                 &
     &   (trns_MHD%forward%ncomp, comm_rlm, comm_rj, SR_sig, SR_r)
!
      call mhd_spectr_to_sendbuf                                        &
     &   (trns_MHD%backward, comm_rj, rj_fld, SR_r%n_WS, SR_r%WS(1))
!
      call sph_b_trans_licv(sph_rlm, comm_rlm, comm_rj, fl_prop,        &
     &    sph_bc_U, omega_sph, trans_p%leg, gt_cor,                     &
     &    trns_MHD%b_trns, trns_MHD%f_trns, trns_MHD%backward,          &
     &    cor_rlm, SR_sig, SR_r)
      call sph_f_trans_licv(sph_rlm, comm_rlm, comm_rj, cor_rlm,        &
     &    trns_MHD%f_trns, trns_MHD%forward, SR_sig, SR_r)
!
      call mhd_spectr_from_recvbuf(trans_p%iflag_SPH_recv,              &
     &    trns_MHD%forward, comm_rj, SR_r%n_WR, SR_r%WR(1), rj_fld)
!
      end subroutine sph_transform_4_licv
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_4_MHD
