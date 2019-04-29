!>@file   init_sph_trans.f90
!!@brief  module init_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Initialize spherical harmonics transform
!!
!!@verbatim
!!      subroutine initialize_sph_trans                                 &
!!     &         (ncomp_trans, nvector_trns, nscalar_trns,              &
!!     &          sph, comms_sph, trans_p, WK_sph)
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!      subroutine initialize_legendre_trans                            &
!!     &         (ncomp_trans, sph, comms_sph, leg, idx_trns)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(legendre_4_sph_trans), intent(inout) :: leg
!!        type(index_4_sph_trans), intent(inout) :: idx_trns
!!@endverbatim
!
      module init_sph_trans
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_transforms
!
      implicit none
!
      private :: init_legendre_rtm, set_blocks_4_leg_trans
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine initialize_sph_trans                                   &
     &         (ncomp_trans, nvector_trns, nscalar_trns,                &
     &          sph, comms_sph, trans_p, WK_sph)
!
      use init_FFT_4_sph
!
      integer(kind = kint), intent(in) :: ncomp_trans
      integer(kind = kint), intent(in) :: nvector_trns, nscalar_trns
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(spherical_trns_works), intent(inout) :: WK_sph
!
!
      iflag_FFT = iflag_FFTPACK
      if(WK_sph%WK_leg%id_legendre .eq. iflag_leg_undefined) then
        WK_sph%WK_leg%id_legendre = iflag_leg_sym_dgemm_big
      end if
!
      call initialize_legendre_trans                                    &
     &   (ncomp_trans, sph, comms_sph, trans_p%leg, trans_p%idx_trns)
      call init_fourier_transform_4_sph(ncomp_trans, sph%sph_rtp,       &
     &    comms_sph%comm_rtp, WK_sph%WK_FFTs)
      call sel_init_legendre_trans                                      &
     &   (ncomp_trans, nvector_trns, nscalar_trns,                      &
     &    sph%sph_rtm, sph%sph_rlm, trans_p%leg, trans_p%idx_trns,      &
     &    WK_sph%WK_leg)
!
      end subroutine initialize_sph_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine initialize_legendre_trans                              &
     &         (ncomp_trans, sph, comms_sph, leg, idx_trns)
!
      use m_FFT_selector
      use schmidt_poly_on_rtm_grid
      use set_legendre_matrices
      use set_params_sph_trans
!
      integer(kind = kint), intent(in) :: ncomp_trans
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(legendre_4_sph_trans), intent(inout) :: leg
      type(index_4_sph_trans), intent(inout) :: idx_trns
!
!
      call alloc_work_4_sph_trans                                       &
     &   (sph%sph_rtm%nidx_rtm, sph%sph_rlm%nidx_rlm, idx_trns)
!
      call radial_4_sph_trans                                           &
     &   (sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
      call set_mdx_rlm_rtm(sph%sph_params%l_truncation,                 &
     &    sph%sph_rtm%nidx_rtm, sph%sph_rlm%nidx_rlm,                   &
     &    sph%sph_rtm%idx_gl_1d_rtm_m, sph%sph_rlm%idx_gl_1d_rlm_j,     &
     &    idx_trns%mdx_p_rlm_rtm, idx_trns%mdx_n_rlm_rtm,               &
     &    idx_trns%maxdegree_rlm, idx_trns%lstack_rlm)
!
      call init_legendre_rtm(sph%sph_params%l_truncation,               &
     &    sph%sph_rj, sph%sph_rtm, sph%sph_rlm, idx_trns, leg)
!
      call const_conatitude_rtp(sph%sph_rtm, sph%sph_rtp, leg)
      call const_sin_theta_rtp(leg, sph%sph_rtm, sph%sph_rtp)
!
      call set_sym_legendre_stack(sph%sph_rtm%nidx_rtm(3),              &
     &    idx_trns%lstack_rlm, idx_trns%lstack_even_rlm)
!
      call set_blocks_4_leg_trans                                       &
     &   (ncomp_trans, sph, comms_sph, idx_trns)
!
      end subroutine initialize_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine init_legendre_rtm                                      &
     &         (l_truncation, sph_rj, sph_rtm, sph_rlm, idx_trns, leg)
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_spheric_rj_data
!
      use set_legendre_matrices
      use set_params_sph_trans
      use schmidt_poly_on_rtm_grid
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
!
      type(legendre_4_sph_trans), intent(inout) :: leg
!
!
      call set_gauss_points_rtm(sph_rtm%nidx_rtm(2), leg)
!
!     set Legendre polynomials
!
!
      call alloc_schmidt_normalize                                      &
     &   (sph_rlm%nidx_rlm(2), sph_rj%nidx_rj(2), leg)
      call copy_sph_normalization_2_rlm(sph_rlm, leg%g_sph_rlm)
      call copy_sph_normalization_2_rj(sph_rj, leg%g_sph_rj)
!
      call alloc_schmidt_poly_rtm                                       &
     &   (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2), leg)
      call set_lagender_4_rlm(l_truncation, sph_rtm, sph_rlm,           &
     &    idx_trns, leg%g_colat_rtm, leg%P_rtm, leg%dPdt_rtm)
!
      call alloc_trans_schmidt_rtm                                      &
     &   (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2), leg)
      call set_trans_legendre_rtm                                       &
     &   (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2),                     &
     &    leg%P_rtm, leg%dPdt_rtm, leg%P_jl, leg%dPdt_jl)
!
      call set_sin_theta_rtm(sph_rtm%nidx_rtm(2), leg%g_colat_rtm,      &
     &    leg%asin_t_rtm)
!
      call alloc_schmidt_p_rtm_pole(sph_rlm%nidx_rlm(2), leg)
      call set_lagender_pole_rlm(l_truncation, sph_rtm, sph_rlm,        &
     &    idx_trns, leg%P_pole_rtm, leg%dPdt_pole_rtm)
!
      end subroutine init_legendre_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_blocks_4_leg_trans                                 &
     &         (ncomp_trans, sph, comms_sph, idx_trns)
!
      use calypso_mpi
      use m_machine_parameter
      use m_sph_communicators
      use m_legendre_transform_list
      use init_spherical_SRs
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: ncomp_trans
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(index_4_sph_trans), intent(inout) :: idx_trns
!
      integer(kind = kint) :: lmax_block_rtm
!
!
      if(nvector_legendre .le. 0                                        &
     &     .or. nvector_legendre .gt. sph%sph_rtm%nidx_rtm(2)) then
        idx_trns%nblock_l_rtm =  1
      else
        idx_trns%nblock_l_rtm                                           &
     &         =  sph%sph_rtm%nidx_rtm(2) / nvector_legendre
      end if
      if(nvector_legendre .le. 0                                        &
     &     .or. nvector_legendre .gt. sph%sph_rlm%nidx_rlm(2)) then
        idx_trns%nblock_j_rlm =  1
      else
        idx_trns%nblock_j_rlm                                           &
     &          =  sph%sph_rlm%nidx_rlm(2) / nvector_legendre
      end if
!
      call alloc_l_rtm_block(idx_trns)
      call count_number_4_smp                                           &
     &   (idx_trns%nblock_l_rtm, ione, sph%sph_rtm%nidx_rtm(2),         &
     &    idx_trns%lstack_block_rtm, lmax_block_rtm)
!
!
      call split_rtp_comms(comms_sph%comm_rtp%nneib_domain,             &
     &    comms_sph%comm_rtp%id_domain, comms_sph%comm_rj%nneib_domain)
      call init_sph_send_recv_N(ncomp_trans, sph, comms_sph)
!
      if(my_rank .ne. 0) return
      write(*,*) 'Vector length for Legendre transform:',               &
     &          nvector_legendre
      write(*,*) 'Block number for meridinal grid: ',                   &
     &          idx_trns%nblock_l_rtm
      write(*,*) 'Block number for Legendre transform: ',               &
     &          idx_trns%nblock_j_rlm
!
      end subroutine set_blocks_4_leg_trans
!
! -----------------------------------------------------------------------
!
      end module init_sph_trans
