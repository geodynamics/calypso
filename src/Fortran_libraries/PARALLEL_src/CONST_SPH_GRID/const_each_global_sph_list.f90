!>@file   const_each_global_sph_list.f90
!!@brief  module const_each_global_sph_list
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set global spherical harmonics ID
!!
!!@verbatim
!!      subroutine const_global_rtp_grids(sph_params, sph_rtp,          &
!!     &          s3d_ranks, sph_lc1, sph_lcp, sph_dbc, stk_lc1d)
!!      subroutine const_global_rtm_grids(sph_params, sph_rtm,          &
!!     &          s3d_ranks, sph_lc1, sph_lcp, sph_dbc, stk_lc1d)
!!      subroutine const_global_rj_modes_by_rlm                         &
!!     &         (sph_rj, s3d_ranks, sph_lc1, sph_lcp, stk_lc1d, s2d_tbl)
!!      subroutine const_global_rlm_modes(sph_params, sph_rtp, sph_rj,  &
!!     &          s3d_ranks, sph_lc1, sph_lcp, stk_lc1d, s2d_tbl)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(spheric_global_rank), intent(inout) :: s3d_ranks
!!        type(sph_local_1d_param), intent(inout) :: sph_lc1
!!        type(sph_local_parameters), intent(inout) :: sph_lcp
!!        type(sph_local_default_BC), intent(inout) :: sph_dbc
!!        type(sph_1d_index_stack), intent(inout) :: stk_lc1d
!!@endverbatim
!!
!!@param ip_rank process ID
!
      module const_each_global_sph_list
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      use t_spheric_parameter
      use t_sph_local_parameter
      use t_spheric_global_ranks
      use t_sph_1d_global_index
      use t_sph_local_parameter
      use t_2d_sph_trans_table
!
      use set_global_spherical_param
      use set_indices_4_sph_tranform
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_global_rtp_grids(sph_params, sph_rtp,            &
     &          s3d_ranks, sph_lc1, sph_lcp, sph_dbc, stk_lc1d)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(spheric_global_rank), intent(inout) :: s3d_ranks
      type(sph_local_1d_param), intent(inout) :: sph_lc1
      type(sph_local_parameters), intent(inout) :: sph_lcp
      type(sph_local_default_BC), intent(inout) :: sph_dbc
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
!
      integer(kind = kint) :: ist, ied
!
!
!      write(*,*) 'set_gl_rank_3d', s3d_ranks%ndomain_rtp(1:3)
      call set_gl_rank_3d(s3d_ranks%iflag_radial_inner_domain,          &
     &    s3d_ranks%ndomain_sph, s3d_ranks%ndomain_rtp,                 &
     &    s3d_ranks%iglobal_rank_rtp)
!
!      call cal_local_nums                                              &
!     &   (s3d_ranks%ndomain_rtp(1), ione, sph_rtp%nidx_global_rtp(1),  &
!     &    sph_lc1%nidx_local_rtp_r, stk_lc1d%istack_idx_local_rtp_r)
!      write(*,*) 'cal_local_nums 1',                                   &
!     &     s3d_ranks%ndomain_rtp(1:3), sph_rtp%nidx_global_rtp(2)
      call cal_local_nums                                               &
     &   (s3d_ranks%ndomain_rtp(2), ione, sph_rtp%nidx_global_rtp(2),   &
     &    sph_lc1%nidx_local_rtp_t, stk_lc1d%istack_idx_local_rtp_t)
!      write(*,*) 'cal_local_nums 2'
      call cal_local_nums                                               &
     &   (s3d_ranks%ndomain_rtp(3), ione, sph_rtp%nidx_global_rtp(3),   &
     &    sph_lc1%nidx_local_rtp_p, stk_lc1d%istack_idx_local_rtp_p)
!
!
!      write(*,*) 'cal_local_nums_st'
      call cal_local_nums_st(s3d_ranks%ndomain_rtp(1),                  &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_dbc%nidx_local_rtp_OC, sph_dbc%ist_idx_local_rtp_OC)
!
      if (sph_params%nlayer_ICB .gt. 1) then
        ied = sph_params%nlayer_ICB - 1
!      write(*,*) 'cal_local_nums_rev'
        call cal_local_nums_rev(s3d_ranks%ndomain_rtp(1), ione, ied,    &
     &      sph_dbc%nidx_local_rtp_IC, sph_dbc%ist_idx_local_rtp_IC)
      end if
!
      if (sph_params%nlayer_CMB .lt. sph_rtp%nidx_global_rtp(1)) then
        ist = sph_params%nlayer_CMB + 1
!      write(*,*) 'cal_local_nums_rev'
        call cal_local_nums_rev(s3d_ranks%ndomain_rtp(1), ist,          &
     &      sph_rtp%nidx_global_rtp(1), sph_dbc%nidx_local_rtp_MT,      &
     &      sph_dbc%ist_idx_local_rtp_MT)
      end if
!
!      write(*,*) 'merge_num_3_local_layers'
      call merge_num_3_local_layers(s3d_ranks%ndomain_rtp(1),           &
     &    sph_dbc%nidx_local_rtp_OC, sph_dbc%nidx_local_rtp_IC,         &
     &    sph_dbc%nidx_local_rtp_MT, ione,                              &
     &    sph_lc1%nidx_local_rtp_r, stk_lc1d%istack_idx_local_rtp_r)
!
!
!      write(*,*) 'set_gl_nnod_spherical'
      call set_gl_nnod_spherical(s3d_ranks%ndomain_sph,                 &
     &    s3d_ranks%ndomain_rtp(1), s3d_ranks%ndomain_rtp(2),           &
     &    s3d_ranks%ndomain_rtp(3), s3d_ranks%iglobal_rank_rtp,         &
     &    sph_lc1%nidx_local_rtp_r, sph_lc1%nidx_local_rtp_t,           &
     &    sph_lc1%nidx_local_rtp_p, sph_lcp%nidx_local_rtp,             &
     &    sph_lcp%nnod_local_rtp)
!
      end subroutine const_global_rtp_grids
!
! -----------------------------------------------------------------------
!
      subroutine const_global_rtm_grids(sph_params, sph_rtm,            &
     &          s3d_ranks, sph_lc1, sph_lcp, sph_dbc, stk_lc1d)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(in) :: sph_rtm
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
      type(sph_local_1d_param), intent(inout) :: sph_lc1
      type(sph_local_parameters), intent(inout) :: sph_lcp
      type(sph_local_default_BC), intent(inout) :: sph_dbc
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
!
      integer(kind = kint) :: ist, ied
!
!
      call set_gl_rank_3d(s3d_ranks%iflag_radial_inner_domain,          &
     &    s3d_ranks%ndomain_sph, s3d_ranks%ndomain_rtm,                 &
     &    s3d_ranks%iglobal_rank_rtm)
!
!      call cal_local_nums                                              &
!     &   (s3d_ranks%ndomain_rtm(1), ione, sph_rtm%nidx_global_rtm(1),  &
!     &    sph_lc1%nidx_local_rtm_r, stk_lc1d%istack_idx_local_rtm_r)
      call cal_local_nums                                               &
     &   (s3d_ranks%ndomain_rtm(2), ione, sph_rtm%nidx_global_rtm(2),   &
     &    sph_lc1%nidx_local_rtm_t, stk_lc1d%istack_idx_local_rtm_t)
      call cal_local_num_rtm_m                                          &
     &   (s3d_ranks%ndomain_rtm(3), sph_params%l_truncation,            &
     &    sph_params%m_folding, sph_lc1%nidx_local_rtm_m,               &
     &    stk_lc1d%istack_idx_local_rtm_m)
!
      call cal_local_nums_st(s3d_ranks%ndomain_rtm(1),                  &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_dbc%nidx_local_rtm_OC, sph_dbc%ist_idx_local_rtm_OC)
!
      if (sph_params%nlayer_ICB .gt. 1) then
        ied = sph_params%nlayer_ICB - 1
        call cal_local_nums_rev(s3d_ranks%ndomain_rtm(1), ione, ied,    &
     &      sph_dbc%nidx_local_rtm_IC, sph_dbc%ist_idx_local_rtm_IC)
      end if
!
      if (sph_params%nlayer_CMB .lt. sph_rtm%nidx_global_rtm(1)) then
        ist = sph_params%nlayer_CMB + 1
        call cal_local_nums_rev                                         &
     &     (s3d_ranks%ndomain_rtm(1), ist, sph_rtm%nidx_global_rtm(1),  &
     &      sph_dbc%nidx_local_rtm_MT, sph_dbc%ist_idx_local_rtm_MT)
      end if
!
      call merge_num_3_local_layers(s3d_ranks%ndomain_rtm(1),           &
     &    sph_dbc%nidx_local_rtm_OC, sph_dbc%nidx_local_rtm_IC,         &
     &    sph_dbc%nidx_local_rtm_MT, ione, sph_lc1%nidx_local_rtm_r,    &
     &    stk_lc1d%istack_idx_local_rtm_r)
!
      call set_gl_nnod_spherical(s3d_ranks%ndomain_sph,                 &
     &    s3d_ranks%ndomain_rtm(1), s3d_ranks%ndomain_rtm(2),           &
     &    s3d_ranks%ndomain_rtm(3), s3d_ranks%iglobal_rank_rtm,         &
     &    sph_lc1%nidx_local_rtm_r, sph_lc1%nidx_local_rtm_t,           &
     &    sph_lc1%nidx_local_rtm_m, sph_lcp%nidx_local_rtm,             &
     &    sph_lcp%nnod_local_rtm)
!
!      call check_sph_gl_bc_param(izero, sph_lc1)
!
      end subroutine const_global_rtm_grids
!
! -----------------------------------------------------------------------
!
      subroutine const_global_rj_modes_by_rlm                           &
     &         (sph_rj, s3d_ranks, sph_lc1, sph_lcp, stk_lc1d, s2d_tbl)
!
      use set_sph_tranform_ordering
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
      type(sph_local_1d_param), intent(inout) :: sph_lc1
      type(sph_local_parameters), intent(inout) :: sph_lcp
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
      type(sph_trans_2d_table), intent(inout) :: s2d_tbl
!
!
      call set_gl_rank_2d                                               &
     &   (s3d_ranks%iflag_radial_inner_domain, s3d_ranks%ndomain_sph,   &
     &    s3d_ranks%ndomain_rj, s3d_ranks%iglobal_rank_rj)
!
      call cal_local_nums                                               &
     &   (s3d_ranks%ndomain_rj(1), ione, sph_rj%nidx_global_rj(1),      &
     &    sph_lc1%nidx_local_rj_r, stk_lc1d%istack_idx_local_rj_r)
! 
      call set_merged_index_4_sph_rj                                    &
     &   (s3d_ranks%ndomain_rtm(1), s3d_ranks%ndomain_rtm(3),           &
     &    s3d_ranks%ndomain_rj(2), sph_rj%nidx_global_rj(2),            &
     &    stk_lc1d%istack_idx_local_rlm_j, s2d_tbl%jtbl_fsph,           &
     &    sph_lc1%nidx_local_rj_j, stk_lc1d%istack_idx_local_rj_j,      &
     &    s2d_tbl%jtbl_rj)
!
      call set_gl_nnod_spheric_rj(s3d_ranks%ndomain_sph,                &
     &    s3d_ranks%ndomain_rj(1), s3d_ranks%ndomain_rj(2),             &
     &    s3d_ranks%iglobal_rank_rj, sph_lc1%nidx_local_rj_r,           &
     &    sph_lc1%nidx_local_rj_j, sph_lcp%nidx_local_rj,               &
     &    sph_lcp%nnod_local_rj)
!
      end subroutine const_global_rj_modes_by_rlm
!
! -----------------------------------------------------------------------
!
      subroutine const_global_rlm_modes(sph_params, sph_rtp, sph_rj,    &
     &          s3d_ranks, sph_lc1, sph_lcp, stk_lc1d, s2d_tbl)
!
      use set_sph_tranform_ordering
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
      type(sph_local_1d_param), intent(inout) :: sph_lc1
      type(sph_local_parameters), intent(inout) :: sph_lcp
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
      type(sph_trans_2d_table), intent(inout) :: s2d_tbl
!
!
      call set_gl_rank_2d(s3d_ranks%iflag_radial_inner_domain,          &
     &    s3d_ranks%ndomain_sph, s3d_ranks%ndomain_rlm,                 &
     &    s3d_ranks%iglobal_rank_rlm)
!
      sph_lc1%nidx_local_rlm_r(1:s3d_ranks%ndomain_rlm(1))              &
     &   = sph_lc1%nidx_local_rtm_r(1:s3d_ranks%ndomain_rlm(1))
      stk_lc1d%istack_idx_local_rlm_r(0:s3d_ranks%ndomain_rlm(1))       &
     &   = stk_lc1d%istack_idx_local_rtm_r(0:s3d_ranks%ndomain_rlm(1))
!
!
      call set_wavenumber_4_ispack_fft(sph_rtp%nidx_global_rtp(2),      &
     &    sph_rtp%nidx_global_rtp(3), sph_params%m_folding,             &
     &    s2d_tbl%mspec_4_ispack, s2d_tbl%mdx_ispack)
!
      call set_zonal_wavenum_4_legendre(s3d_ranks%ndomain_rtm(3),       &
     &    sph_params%l_truncation, sph_params%m_folding,                &
     &    sph_rtp%nidx_global_rtp(2), sph_rtp%nidx_global_rtp(3),       &
     &    s2d_tbl%jdx_fsph, s2d_tbl%mdx_4_lgd)
!
      call set_merged_index_4_sph_trans(s3d_ranks%ndomain_rtm(3),       &
     &    sph_params%l_truncation, sph_rj%nidx_global_rj(2),            &
     &    sph_rtp%nidx_global_rtp(3), sph_params%m_folding,             &
     &    stk_lc1d%istack_idx_local_rtm_m, s2d_tbl%mdx_4_lgd,           &
     &    sph_lc1%nidx_local_rlm_j, stk_lc1d%istack_idx_local_rlm_j,    &
     &    s2d_tbl%jtbl_fsph)
!
      call set_gl_nnod_spheric_rj(s3d_ranks%ndomain_sph,                &
     &    s3d_ranks%ndomain_rlm(1), s3d_ranks%ndomain_rlm(2),           &
     &    s3d_ranks%iglobal_rank_rlm, sph_lc1%nidx_local_rlm_r,         &
     &    sph_lc1%nidx_local_rlm_j, sph_lcp%nidx_local_rlm,             &
     &    sph_lcp%nnod_local_rlm)
!
      end subroutine const_global_rlm_modes
!
! -----------------------------------------------------------------------
!
      end module const_each_global_sph_list
