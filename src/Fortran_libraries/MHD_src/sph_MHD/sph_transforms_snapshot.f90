!>@file   sph_transforms_snapshot.f90
!!@brief  module sph_transforms_snapshot
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p, &
!!     &          rj_fld, trns_bwd, WK_sph)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(address_each_sph_trans), intent(inout) :: trns_bwd
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!      subroutine sph_forward_trans_snapshot_MHD                       &
!!     &         (sph, comms_sph, trans_p, trns_fwd, WK_sph, rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(address_each_sph_trans), intent(in) :: trns_fwd
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!@endverbatim
!!
      module sph_transforms_snapshot
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
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
      use t_sph_transforms
!
      use m_legendre_transform_list
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p,   &
     &          rj_fld, trns_bwd, WK_sph)
!
      use m_solver_SR
      use set_address_sph_trans_MHD
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(phys_data), intent(inout) :: rj_fld
      type(address_each_sph_trans), intent(inout) :: trns_bwd
      type(spherical_trns_works), intent(inout) :: WK_sph
!
      integer(kind = kint) :: nscalar_trans
!
!
      if(trns_bwd%ncomp .le. 0) return
!
      nscalar_trans = trns_bwd%num_scalar + 6*trns_bwd%num_tensor
      call check_calypso_sph_comm_buf_N(trns_bwd%ncomp,                 &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_bwd%ncomp,                 &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      call mhd_spectr_to_sendbuf_wpole                                  &
     &   (sph%sph_rtp%nnod_pole, sph%sph_rj, comms_sph%comm_rj,         &
     &    rj_fld, n_WS, WS(1), trns_bwd)
!
      call sph_b_trans_w_poles(trns_bwd%ncomp, trns_bwd%num_vector,     &
     &    nscalar_trans, sph, comms_sph, trans_p,                       &
     &    n_WS, n_WR, WS(1), WR(1), trns_bwd%fld_rtp,                   &
     &    trns_bwd%flc_pole, trns_bwd%fld_pole, WK_sph)
!
      end subroutine sph_back_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_snapshot_MHD                         &
     &         (sph, comms_sph, trans_p, trns_fwd, WK_sph, rj_fld)
!
      use m_solver_SR
      use set_address_sph_trans_MHD
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(address_each_sph_trans), intent(in) :: trns_fwd
!
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(trns_fwd%ncomp .eq. 0) return
!
      call check_calypso_sph_comm_buf_N(trns_fwd%ncomp,                 &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N(trns_fwd%ncomp,                 &
     &    comms_sph%comm_rlm, comms_sph%comm_rj)
!
!   transform for vectors and scalars
      call sph_forward_transforms(trns_fwd%ncomp, trns_fwd%num_vector,  &
     &    trns_fwd%num_scalar, sph, comms_sph, trans_p,                 &
     &    trns_fwd%fld_rtp, n_WS, n_WR, WS, WR, WK_sph)
!
      call mhd_spectr_from_recvbuf                                      &
     &   (trns_fwd, comms_sph%comm_rj, n_WR, WR(1), rj_fld)
!
      end subroutine sph_forward_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_snapshot
