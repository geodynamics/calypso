!>@file   init_sphrical_transform_MHD.f90
!!@brief  module init_sphrical_transform_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_sph_transform_MHD                               &
!!     &         (SPH_model, iphys, trans_p, WK, SPH_MHD, SR_sig, SR_r)
!!      subroutine init_leg_fourier_trans_MHD(sph, comms_sph,           &
!!     &          ncomp_max_trans, trans_p, WK, SR_sig, SR_r)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine init_work_4_coriolis(sph_MHD_bc, sph, trans_p, WK)
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(sph_grids), intent(in) :: sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!@endverbatim
!!
      module init_sphrical_transform_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_sph_trans_comm_tbl
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_const_wz_coriolis_rtp
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_data_sph_MHD
      use t_solver_SR
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_MHD                                 &
     &         (SPH_model, iphys, trans_p, WK, SPH_MHD, SR_sig, SR_r)
!
      use set_address_sph_trans_MHD
      use set_address_sph_trans_snap
      use check_sph_mhd_openmp_size
      use init_legendre_transform_MHD
!
      type(phys_address), intent(in) :: iphys
      type(SPH_MHD_model_data), intent(in) :: SPH_model
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!>      total number of components for spherical harmonics transform
      integer(kind = kint), save :: ncomp_max_trans = 0
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint), save :: nvector_max_trans = 0
!>      total number of svalars for spherical harmonics transform
      integer(kind = kint), save :: nscalar_max_trans = 0
!
!
      call s_check_sph_mhd_openmp_size(WK%WK_leg, SPH_MHD%sph)
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD                                  &
     &   (SPH_MHD%fld, SPH_MHD%ipol, iphys, WK%trns_MHD,                &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call set_addresses_snapshot_trans                                 &
     &   (SPH_MHD%fld, SPH_MHD%ipol, iphys, WK%trns_snap,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_ene_flux_trans                                 &
     &   (SPH_MHD%fld, SPH_MHD%ipol, iphys, WK%trns_eflux,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_diff_vect_trans                                &
     &   (SPH_MHD%fld, SPH_MHD%ipol, iphys, WK%trns_difv,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call alloc_sph_trans_address(SPH_MHD%sph, WK)
!
      call init_leg_fourier_trans_MHD(SPH_MHD%sph, SPH_MHD%comms,       &
     &    ncomp_max_trans, trans_p, WK, SR_sig, SR_r)
!
      call init_work_4_coriolis                                         &
     &   (SPH_model%sph_MHD_bc, SPH_MHD%sph, trans_p, WK)
!
      call init_leg_trans_sph_MHD                                       &
     &   (SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_model%omega_sph,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    WK%trns_MHD, WK%WK_leg, WK%WK_FFTs_MHD, trans_p,              &
     &    WK%gt_cor, WK%cor_rlm, SPH_MHD%fld, SR_sig, SR_r)
!
      end subroutine init_sph_transform_MHD
!
!-----------------------------------------------------------------------
!
      subroutine init_leg_fourier_trans_MHD(sph, comms_sph,             &
     &          ncomp_max_trans, trans_p, WK, SR_sig, SR_r)
!
      use init_sph_trans
      use init_FFT_4_MHD
      use pole_sph_transform
      use skip_comment_f
!

      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_legendre_trans'
      call initialize_legendre_trans                                    &
     &   (trans_p%nvector_legendre, ncomp_max_trans, sph, comms_sph,    &
     &    trans_p%leg, trans_p%idx_trns, SR_sig, SR_r,                  &
     &    trans_p%iflag_SPH_recv)
!
      WK%iflag_MHD_FFT = trans_p%iflag_FFT
      call init_fourier_transform_4_MHD                                 &
     &   (sph%sph_rtp, comms_sph%comm_rtp,                              &
     &    WK%trns_MHD, WK%WK_FFTs_MHD, SR_r, WK%iflag_MHD_FFT)
!
      trans_p%iflag_FFT = set_FFT_mode_4_snapshot(WK%iflag_MHD_FFT)
      call init_sph_FFT_select(my_rank, trans_p%iflag_FFT,              &
     &    sph%sph_rtp, comms_sph%comm_rtp,                              &
     &    ncomp_max_trans, ncomp_max_trans, WK%WK_FFTs)
!
      if(my_rank .eq. 0)  call write_import_table_mode(trans_p)
!
      end subroutine init_leg_fourier_trans_MHD
!
!-----------------------------------------------------------------------
!
      subroutine init_work_4_coriolis(sph_MHD_bc, sph, trans_p, WK)
!
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_grids), intent(in) :: sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_sphere_ave_coriolis'
      call alloc_sphere_ave_coriolis(sph%sph_rj, WK%ave_cor)
      if (iflag_debug.eq.1) write(*,*) 'init_sum_coriolis_rlm'
      call init_sum_coriolis_rlm                                        &
     &   (sph%sph_params%l_truncation, sph%sph_rlm,                     &
     &    sph_MHD_bc%sph_bc_U, trans_p%leg, WK%gt_cor, WK%cor_rlm)
!
      end subroutine init_work_4_coriolis
!
!-----------------------------------------------------------------------
!
      end module init_sphrical_transform_MHD
