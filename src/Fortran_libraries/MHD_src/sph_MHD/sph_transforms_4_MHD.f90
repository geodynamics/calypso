!>@file   sph_transforms_4_MHD.f90
!!@brief  module sph_transforms_4_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine sph_back_trans_4_MHD                                 &
!!     &         (sph, comms_sph, fl_prop, sph_bc_U, omega_sph,         &
!!     &          trans_p, gt_cor, ipol, rj_fld, trns_MHD,              &
!!     &          WK_sph, MHD_mul_FFTW, cor_rlm)
!!      subroutine sph_pole_trans_4_MHD(sph, comms_sph,                 &
!!     &          trans_p, ipol, rj_fld, trns_MHD)
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!      subroutine sph_forward_trans_4_MHD                              &
!!     &         (sph, comms_sph, fl_prop, trans_p, cor_rlm,            &
!!     &          ipol, trns_MHD, WK_sph, MHD_mul_FFTW, rj_fld)
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(coriolis_rlm_data), intent(in) :: cor_rlm
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine sph_transform_4_licv                                 &
!!     &         (sph_rlm, comm_rlm, comm_rj, fl_prop, sph_bc_U,        &
!!     &          omega_sph, leg, gt_cor, trns_MHD,                     &
!!     &          ipol, rj_fld, cor_rlm)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!@endverbatim
!!
      module sph_transforms_4_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_legendre_transform_list
!
      use calypso_mpi
!
      use t_physical_property
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
      use t_sph_single_FFTW
      use t_sph_transforms
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_params_sph_MHD
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
      subroutine sph_back_trans_4_MHD                                   &
     &         (sph, comms_sph, fl_prop, sph_bc_U, omega_sph,           &
     &          trans_p, gt_cor, ipol, rj_fld, trns_MHD,                &
     &          WK_sph, MHD_mul_FFTW, cor_rlm)
!
      use m_solver_SR
      use sph_trans_w_coriols
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
!
!
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
!      call start_eleps_time(51)
      if(iflag_debug .gt. 0) write(*,*) 'copy_mhd_spectr_to_send'
      call copy_mhd_spectr_to_send                                      &
     &   (trns_MHD%ncomp_rj_2_rtp, trns_MHD%b_trns, comms_sph%comm_rj,  &
     &    ipol, rj_fld, n_WS, WS)
!      call end_eleps_time(51)
!
      if(trns_MHD%ncomp_rj_2_rtp .eq. 0) return
      call sph_b_trans_w_coriolis(trns_MHD%ncomp_rj_2_rtp,              &
     &    trns_MHD%nvector_rj_2_rtp, trns_MHD%nscalar_rj_2_rtp,         &
     &    sph, comms_sph, fl_prop, sph_bc_U, omega_sph,                 &
     &    trans_p, gt_cor, n_WS, n_WR, WS(1), WR(1),                    &
     &    trns_MHD, WK_sph, MHD_mul_FFTW, cor_rlm)
!
      end subroutine sph_back_trans_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_4_MHD                                &
     &         (sph, comms_sph, fl_prop, trans_p, cor_rlm,              &
     &          ipol, trns_MHD, WK_sph, MHD_mul_FFTW, rj_fld)
!
      use m_solver_SR
      use sph_trans_w_coriols
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(coriolis_rlm_data), intent(in) :: cor_rlm
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
      type(phys_data), intent(inout) :: rj_fld
!
!
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rtp_2_rj,        &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rtp_2_rj,        &
     &    comms_sph%comm_rlm, comms_sph%comm_rj)
!
      if(trns_MHD%ncomp_rtp_2_rj .eq. 0) return
      call sph_f_trans_w_coriolis(trns_MHD%ncomp_rtp_2_rj,              &
     &    trns_MHD%nvector_rtp_2_rj, trns_MHD%nscalar_rtp_2_rj,         &
     &    sph, comms_sph, fl_prop, trans_p, cor_rlm, trns_MHD,          &
     &    n_WS, n_WR, WS(1), WR(1), WK_sph, MHD_mul_FFTW)
!
      call copy_mhd_spectr_from_recv(trns_MHD%ncomp_rtp_2_rj,           &
     &    trns_MHD%f_trns, comms_sph%comm_rj, ipol,                     &
     &    n_WR, WR(1), rj_fld)
!
      end subroutine sph_forward_trans_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_pole_trans_4_MHD(sph, comms_sph,                   &
     &          trans_p, ipol, rj_fld, trns_MHD)
!
      use m_solver_SR
      use t_sph_transforms
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
!
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
!      call start_eleps_time(51)
      if(iflag_debug .gt. 0) write(*,*) 'copy_mhd_spectr_to_send'
      call copy_mhd_spectr_to_send                                      &
     &   (trns_MHD%ncomp_rj_2_rtp, trns_MHD%b_trns, comms_sph%comm_rj,  &
     &    ipol, rj_fld, n_WS, WS)
!      call end_eleps_time(51)
!
      if(trns_MHD%ncomp_rj_2_rtp .eq. 0) return
      call pole_b_transform(trns_MHD%ncomp_rj_2_rtp,                    &
     &    trns_MHD%nvector_rj_2_rtp, trns_MHD%nscalar_rj_2_rtp,         &
     &    sph, comms_sph, trans_p, n_WS, n_WR, WS(1), WR(1),            &
     &    trns_MHD%flc_pole, trns_MHD%fld_pole)
!
      end subroutine sph_pole_trans_4_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_transform_4_licv                                   &
     &         (sph_rlm, comm_rlm, comm_rj, fl_prop, sph_bc_U,          &
     &          omega_sph, leg, gt_cor, trns_MHD,                       &
     &          ipol, rj_fld, cor_rlm)
!
      use m_solver_SR
      use sph_trans_w_coriols
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(legendre_4_sph_trans), intent(in) :: leg
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
!
!
      if(trns_MHD%ncomp_rj_2_rtp .eq. 0                                 &
     &   .or. trns_MHD%ncomp_rtp_2_rj .eq. 0) return
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (trns_MHD%ncomp_rj_2_rtp, comm_rj, comm_rlm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (trns_MHD%ncomp_rtp_2_rj, comm_rlm, comm_rj)
!
      call copy_mhd_spectr_to_send(trns_MHD%ncomp_rj_2_rtp,             &
     &    trns_MHD%b_trns, comm_rj, ipol, rj_fld, n_WS, WS(1))
!
      call sph_b_trans_licv(trns_MHD%ncomp_rj_2_rtp,                    &
     &    sph_rlm, comm_rlm, comm_rj, fl_prop, sph_bc_U, omega_sph,     &
     &    leg, gt_cor, trns_MHD, n_WR, WR(1), cor_rlm)
      call sph_f_trans_licv(trns_MHD%ncomp_rtp_2_rj,                    &
     &    sph_rlm, comm_rlm, comm_rj, fl_prop, cor_rlm, trns_MHD,       &
     &    n_WS, WS(1))
!
      call copy_mhd_spectr_from_recv                                    &
     &   (trns_MHD%ncomp_rtp_2_rj, trns_MHD%f_trns, comm_rj, ipol,      &
     &    n_WR, WR(1), rj_fld)
!
      end subroutine sph_transform_4_licv
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_4_MHD
