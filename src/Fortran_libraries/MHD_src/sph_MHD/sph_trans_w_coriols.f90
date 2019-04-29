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
!!     &         (sph, comms_sph, fl_prop, sph_bc_U, omega_sph, b_trns, &
!!     &          trans_p, gt_cor, n_WS, n_WR, WS, WR, trns_bwd,        &
!!     &          WK_sph, MHD_mul_FFTW, cor_rlm)
!!      subroutine sph_f_trans_w_coriolis                               &
!!     &         (sph, comms_sph, fl_prop, trans_p, cor_rlm, f_trns,    &
!!     &          trns_fwd, n_WS, n_WR, WS, WR, WK_sph, MHD_mul_FFTW)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: b_trns
!!        type(phys_address), intent(in) :: f_trns
!!        type(address_each_sph_trans), intent(inout) :: trns_bwd
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!!
!!      subroutine sph_b_transform_SGS                                  &
!!     &         (sph, comms_sph, trans_p, n_WS, n_WR, WS, WR,          &
!!     &          trns_bwd, WK_sph, SGS_mul_FFTW)
!!      subroutine sph_f_transform_SGS(ncomp_trans, nvector, nscalar,   &
!!     &          sph, comms_sph, trans_p, trns_SGS,                    &
!!     &          n_WS, n_WR, WS, WR, WK_sph, SGS_mul_FFTW)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(address_each_sph_trans), intent(inout) :: trns_bwd
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!!
!!      subroutine sph_b_trans_licv(sph_rlm, comm_rlm, comm_rj,         &
!!     &          fl_prop, sph_bc_U, omega_sph, leg, gt_cor,            &
!!     &          b_trns, trns_bwd, n_WR, WR, cor_rlm)
!!      subroutine sph_f_trans_licv(sph_rlm, comm_rlm, comm_rj,         &
!!     &          fl_prop, cor_rlm, f_trns, trns_fwd,  n_WS, WS)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!!        type(phys_address), intent(in) :: b_trns
!!        type(phys_address), intent(in) :: f_trns
!!        type(address_each_sph_trans), intent(in) :: trns_bwd
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
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
      use MHD_FFT_selector
      use spherical_SRs_N
!
      use t_physical_property
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_poloidal_rotation
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
      use t_legendre_trans_select
      use t_sph_transforms
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
     &         (sph, comms_sph, fl_prop, sph_bc_U, omega_sph, b_trns,   &
     &          trans_p, gt_cor, n_WS, n_WR, WS, WR, trns_bwd,          &
     &          WK_sph, MHD_mul_FFTW, cor_rlm)
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
      type(phys_address), intent(in) :: b_trns
!
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
      type(address_each_sph_trans), intent(inout) :: trns_bwd
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
!
!
      if(iflag_debug .gt. 0) write(*,*) 'calypso_sph_comm_rj_2_rlm_N'
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+1)
      call calypso_sph_comm_N                                           &
     &   (trns_bwd%ncomp, comms_sph%comm_rj, comms_sph%comm_rlm)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+1)
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_coriolis_rlm'
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+8)
      call sum_coriolis_rlm                                             &
     &   (trns_bwd%ncomp, sph%sph_rlm, comms_sph%comm_rlm,              &
     &    fl_prop, sph_bc_U, omega_sph, b_trns, trans_p%leg,            &
     &    gt_cor, n_WR, WR, cor_rlm)
      call finish_send_recv_sph(comms_sph%comm_rj)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+8)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+5)
      if(iflag_debug .gt. 0) write(*,*)  'sel_backward_legendre_trans', &
     &       trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar
      call sel_backward_legendre_trans                                  &
     &   (trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar,     &
     &    sph%sph_rlm, sph%sph_rtm, comms_sph%comm_rlm,                 &
     &    comms_sph%comm_rtm, trans_p%leg, trans_p%idx_trns,            &
     &    n_WR, n_WS, WR, WS, WK_sph%WK_leg)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+5)
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &      'calypso_sph_comm_rtm_2_rtp_N'
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+2)
      call calypso_sph_comm_N                                           &
     &   (trns_bwd%ncomp, comms_sph%comm_rtm, comms_sph%comm_rtp)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+2)
!
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+7)
      if(iflag_debug .gt. 0) write(*,*) 'back_MHD_FFT_sel_from_recv',   &
     &        trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar
      call back_MHD_FFT_sel_from_recv                                   &
     &   (sph%sph_rtp, comms_sph%comm_rtp, trns_bwd%ncomp,              &
     &    n_WR, WR, trns_bwd%fld_rtp, WK_sph%WK_FFTs, MHD_mul_FFTW)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+7)
!
      if(iflag_debug .gt. 0) write(*,*) 'finish_send_recv_rtm_2_rtp'
      call finish_send_recv_sph(comms_sph%comm_rtm)
!
      end subroutine sph_b_trans_w_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_w_coriolis                                 &
     &         (sph, comms_sph, fl_prop, trans_p, cor_rlm, f_trns,      &
     &          trns_fwd, n_WS, n_WR, WS, WR, WK_sph, MHD_mul_FFTW)
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fluid_property), intent(in) :: fl_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(coriolis_rlm_data), intent(in) :: cor_rlm
      type(phys_address), intent(in) :: f_trns
!
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+7)
      call fwd_MHD_FFT_sel_to_send                                      &
     &   (sph%sph_rtp, comms_sph%comm_rtp, trns_fwd%ncomp,              &
     &    n_WS, trns_fwd, WS, WK_sph%WK_FFTs, MHD_mul_FFTW)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+7)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+3)
      call calypso_sph_comm_N                                           &
     &   (trns_fwd%ncomp, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call finish_send_recv_sph(comms_sph%comm_rtp)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+3)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+6)
      if(iflag_debug .gt. 0) write(*,*) 'sel_forward_legendre_trans'
      call sel_forward_legendre_trans                                   &
     &   (trns_fwd%ncomp, trns_fwd%num_vector, trns_fwd%num_scalar,     &
     &    sph%sph_rtm, sph%sph_rlm, comms_sph%comm_rtm,                 &
     &    comms_sph%comm_rlm, trans_p%leg, trans_p%idx_trns,            &
     &    n_WR, n_WS, WR, WS, WK_sph%WK_leg)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+6)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'copy_coriolis_terms_rlm'
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+8)
      call copy_coriolis_terms_rlm                                      &
     &   (trns_fwd%ncomp, sph%sph_rlm, comms_sph%comm_rlm, fl_prop,     &
     &    f_trns, cor_rlm, n_WS, WS)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+8)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+4)
      call calypso_sph_comm_N                                           &
     &   (trns_fwd%ncomp, comms_sph%comm_rlm, comms_sph%comm_rj)
      call finish_send_recv_sph(comms_sph%comm_rlm)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+4)
!
      end subroutine sph_f_trans_w_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_b_transform_SGS                                    &
     &         (sph, comms_sph, trans_p, n_WS, n_WR, WS, WR,            &
     &          trns_bwd, WK_sph, SGS_mul_FFTW)
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
!
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
      type(address_each_sph_trans), intent(inout) :: trns_bwd
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
!
!
      if(iflag_debug .gt. 0) write(*,*) 'calypso_sph_comm_rj_2_rlm_N'
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+1)
      call calypso_sph_comm_N                                           &
     &   (trns_bwd%ncomp, comms_sph%comm_rj, comms_sph%comm_rlm)
      call finish_send_recv_sph(comms_sph%comm_rj)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+1)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+5)
      if(iflag_debug .gt. 0) write(*,*) 'sel_backward_legendre_trans',  &
     &         trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar
      call sel_backward_legendre_trans                                  &
     &   (trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar,     &
     &    sph%sph_rlm, sph%sph_rtm, comms_sph%comm_rlm,                 &
     &    comms_sph%comm_rtm, trans_p%leg, trans_p%idx_trns,            &
     &    n_WR, n_WS, WR, WS, WK_sph%WK_leg)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+5)
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &      'calypso_sph_comm_rtm_2_rtp_N'
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+2)
      call calypso_sph_comm_N                                           &
     &   (trns_bwd%ncomp, comms_sph%comm_rtm, comms_sph%comm_rtp)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+2)
!
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+7)
      if(iflag_debug .gt. 0) write(*,*) 'back_MHD_FFT_sel_from_recv',   &
     &    trns_bwd%ncomp, trns_bwd%num_vector, trns_bwd%num_scalar
      call back_MHD_FFT_sel_from_recv                                   &
     &   (sph%sph_rtp, comms_sph%comm_rtp, trns_bwd%ncomp,              &
     &    n_WR, WR, trns_bwd%fld_rtp, WK_sph%WK_FFTs, SGS_mul_FFTW)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+7)
!
      if(iflag_debug .gt. 0) write(*,*) 'finish_send_recv_rtm_2_rtp'
      call finish_send_recv_sph(comms_sph%comm_rtm)
!
      end subroutine sph_b_transform_SGS
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_transform_SGS                                    &
     &         (sph, comms_sph, trans_p, trns_fwd,                      &
     &          n_WS, n_WR, WS, WR, WK_sph, SGS_mul_FFTW)
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real(kind = kreal), intent(inout) :: WS(n_WS), WR(n_WR)
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
!
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+7)
      call fwd_MHD_FFT_sel_to_send                                      &
     &   (sph%sph_rtp, comms_sph%comm_rtp, trns_fwd%ncomp,              &
     &    n_WS, trns_fwd, WS, WK_sph%WK_FFTs, SGS_mul_FFTW)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+7)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+3)
      call calypso_sph_comm_N                                           &
     &   (trns_fwd%ncomp, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call finish_send_recv_sph(comms_sph%comm_rtp)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+3)
!
      if(iflag_debug .gt. 0) write(*,*) 'sel_forward_legendre_trans'
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+6)
      call sel_forward_legendre_trans                                   &
     &   (trns_fwd%ncomp, trns_fwd%num_vector, trns_fwd%num_scalar,     &
     &    sph%sph_rtm, sph%sph_rlm, comms_sph%comm_rtm,                 &
     &    comms_sph%comm_rlm, trans_p%leg, trans_p%idx_trns,            &
     &    n_WR, n_WS, WR, WS, WK_sph%WK_leg)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+6)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+4)
      call calypso_sph_comm_N                                           &
     &   (trns_fwd%ncomp, comms_sph%comm_rlm, comms_sph%comm_rj)
      call finish_send_recv_sph(comms_sph%comm_rlm)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+4)
!
      end subroutine sph_f_transform_SGS
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_b_trans_licv(sph_rlm, comm_rlm, comm_rj,           &
     &          fl_prop, sph_bc_U, omega_sph, leg, gt_cor,              &
     &          b_trns, trns_bwd, n_WR, WR, cor_rlm)
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
      type(address_each_sph_trans), intent(in) :: trns_bwd
!
      integer(kind = kint), intent(in) :: n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
!
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+1)
      call calypso_sph_comm_N(trns_bwd%ncomp, comm_rj, comm_rlm)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+1)
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_coriolis_rlm'
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+8)
      call sum_coriolis_rlm(trns_bwd%ncomp, sph_rlm, comm_rlm,          &
     &    fl_prop, sph_bc_U, omega_sph, b_trns, leg, gt_cor,            &
     &    n_WR, WR, cor_rlm)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+8)
!
      call finish_send_recv_sph(comm_rj)
!
      end subroutine sph_b_trans_licv
!
! -----------------------------------------------------------------------
!
      subroutine sph_f_trans_licv(sph_rlm, comm_rlm, comm_rj,           &
     &          fl_prop, cor_rlm, f_trns, trns_fwd,  n_WS, WS)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(fluid_property), intent(in) :: fl_prop
      type(coriolis_rlm_data), intent(in) :: cor_rlm
      type(phys_address), intent(in) :: f_trns
      type(address_each_sph_trans), intent(in) :: trns_fwd
!
      integer(kind = kint), intent(in) :: n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+8)
      if(iflag_debug .gt. 0) write(*,*) 'copy_coriolis_terms_rlm'
      call copy_coriolis_terms_rlm                                      &
     &   (trns_fwd%ncomp, sph_rlm, comm_rlm, fl_prop,                   &
     &    f_trns, cor_rlm, n_WS, WS)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+8)
!
      if(iflag_SPH_time) call start_elapsed_time(ist_elapsed_SPH+4)
      call calypso_sph_comm_N(trns_fwd%ncomp, comm_rlm, comm_rj)
      if(iflag_SPH_time) call end_elapsed_time(ist_elapsed_SPH+4)
!
      call finish_send_recv_sph(comm_rlm)
!
      end subroutine sph_f_trans_licv
!
! -----------------------------------------------------------------------
!
      end module sph_trans_w_coriols
