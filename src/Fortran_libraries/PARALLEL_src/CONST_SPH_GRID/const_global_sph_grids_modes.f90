!>@file   const_global_sph_grids_modes.f90
!!@brief  module const_global_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set global spherical harmonics ID
!!
!!@verbatim
!!      subroutine s_const_global_sph_grids_modes                       &
!!     &         (sph_params, sph_rtp, sph_rtm, sph_rj, s3d_ranks,      &
!!     &          sph_lcp, stk_lc1d, sph_gl1d)
!!      subroutine const_global_sph_FEM_grid                            &
!!     &         (sph_params, sph_rtp, sph_rj, s3d_ranks,               &
!!     &          sph_lcp, stk_lc1d, sph_gl1d)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(spheric_global_rank), intent(inout) :: s3d_ranks
!!        type(sph_local_parameters), intent(inout) :: sph_lcp
!!        type(sph_1d_index_stack), intent(inout) :: stk_lc1d
!!        type(sph_1d_global_index), intent(inout) :: sph_gl1d
!!@endverbatim
!!
!!@param ip_rank process ID
!
      module const_global_sph_grids_modes
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
      use const_each_global_sph_list
!
      implicit none
!
      type(sph_local_1d_param), save :: sph_lc1_SP
      type(sph_trans_2d_table), save :: s2d_tbl_SP
      type(sph_local_default_BC), save :: sph_dbc_SP
!
      private :: sph_lc1_SP, s2d_tbl_SP
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_global_sph_grids_modes                         &
     &         (sph_params, sph_rtp, sph_rtm, sph_rj, s3d_ranks,        &
     &          sph_lcp, stk_lc1d, sph_gl1d)
!
      use set_sph_1d_global_index
      use set_sph_1d_domain_id
      use set_sph_tranform_ordering
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
      type(sph_local_parameters), intent(inout) :: sph_lcp
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
!
      call alloc_sph_1d_global_stack(s3d_ranks, stk_lc1d)
      call alloc_sph_gl_parameter(s3d_ranks, sph_lcp)
      call alloc_sph_gl_bc_param(s3d_ranks, sph_dbc_SP)
      call alloc_sph_ranks(s3d_ranks)
      call alloc_nidx_local(s3d_ranks, sph_lc1_SP)
      call alloc_2d_sph_trans_table(sph_rtp, sph_rj, s2d_tbl_SP)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rtp_grids'
      call const_global_rtp_grids(sph_params, sph_rtp,                  &
     &    s3d_ranks, sph_lc1_SP, sph_lcp, sph_dbc_SP, stk_lc1d)
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rtm_grids'
      call const_global_rtm_grids(sph_params, sph_rtm,                  &
     &    s3d_ranks, sph_lc1_SP, sph_lcp, sph_dbc_SP, stk_lc1d)
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rlm_modes'
      call const_global_rlm_modes(sph_params, sph_rtp, sph_rj,          &
     &    s3d_ranks, sph_lc1_SP, sph_lcp, stk_lc1d, s2d_tbl_SP)
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rj_modes_by_rlm'
      call const_global_rj_modes_by_rlm                                 &
     &   (sph_rj, s3d_ranks, sph_lc1_SP, sph_lcp, stk_lc1d, s2d_tbl_SP)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_trans_table_fft_2_lgd'
      call set_trans_table_fft_2_lgd(sph_params%l_truncation,           &
     &    sph_rtp%nidx_global_rtp(2), sph_rtp%nidx_global_rtp(3),       &
     &    sph_params%m_folding, s2d_tbl_SP%mspec_4_ispack,              &
     &    s2d_tbl_SP%jdx_fsph, s2d_tbl_SP%mtbl_fft_2_lgd)
!
!
      call dealloc_nidx_local(sph_lc1_SP)
      call alloc_sph_1d_global_idx(s3d_ranks, stk_lc1d, sph_gl1d)
!
      call set_sph_1d_global_idx_rtp                                    &
     &   (sph_params%m_folding, sph_rtp%nidx_global_rtp(3),             &
     &    s2d_tbl_SP%mdx_ispack, s3d_ranks,                             &
     &    sph_dbc_SP, stk_lc1d, sph_gl1d)
      call set_sph_1d_global_idx_rtm                                    &
     &   (sph_params%m_folding, sph_rtp%nidx_global_rtp(3),             &
     &    s2d_tbl_SP%mtbl_fft_2_lgd, s2d_tbl_SP%mdx_4_lgd,              &
     &    s3d_ranks, sph_dbc_SP, stk_lc1d, sph_gl1d)
      call set_sph_1d_global_idx_rlm                                    &
     &   (sph_rj%nidx_global_rj(2), s2d_tbl_SP%jtbl_fsph, sph_gl1d)
      call set_sph_1d_global_idx_rj                                     &
     &   (sph_rj%nidx_global_rj(2), s2d_tbl_SP%jtbl_rj, sph_gl1d)
!
      call alloc_sph_1d_domain_id(sph_rtp, sph_rj, s3d_ranks)
!
      call set_sph_1d_domain_id_rtp(stk_lc1d, sph_gl1d, s3d_ranks)
      call set_sph_1d_domain_id_rj(stk_lc1d, sph_gl1d, s3d_ranks)
!
      if(iflag_debug .gt. 0) then
        write(50,*) 'idx_global_rtp_r', sph_gl1d%idx_global_rtp_r
        write(50,*) 'idx_global_rtm_r', sph_gl1d%idx_global_rtm_r
        write(50,*) 'idx_global_rlm_r', sph_gl1d%idx_global_rlm_r
!        call check_sph_1d_domain_id(sph_rtp, sph_rj, s3d_ranks)
      end if
!
      call dealloc_2d_sph_trans_table(s2d_tbl_SP)
      call dealloc_sph_gl_bc_param(sph_dbc_SP)
!
      end subroutine s_const_global_sph_grids_modes
!
! -----------------------------------------------------------------------
!
      subroutine const_global_sph_FEM_grid                              &
     &         (sph_params, sph_rtp, sph_rj, s3d_ranks,                 &
     &          sph_lcp, stk_lc1d, sph_gl1d)
!
      use set_sph_1d_global_index
      use set_sph_1d_domain_id
      use set_sph_tranform_ordering
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
      type(sph_local_parameters), intent(inout) :: sph_lcp
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
!
      call alloc_sph_1d_global_stack(s3d_ranks, stk_lc1d)
      call alloc_sph_gl_parameter(s3d_ranks, sph_lcp)
      call alloc_sph_gl_bc_param(s3d_ranks, sph_dbc_SP)
      call alloc_sph_ranks(s3d_ranks)
      call alloc_nidx_local(s3d_ranks, sph_lc1_SP)
!
      call alloc_2d_sph_trans_table(sph_rtp, sph_rj, s2d_tbl_SP)
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rtp_grids'
      call const_global_rtp_grids(sph_params, sph_rtp,                  &
     &    s3d_ranks, sph_lc1_SP, sph_lcp, sph_dbc_SP, stk_lc1d)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_trans_table_fft_2_lgd'
      call set_trans_table_fft_2_lgd(sph_params%l_truncation,           &
     &    sph_rtp%nidx_global_rtp(2), sph_rtp%nidx_global_rtp(3),       &
     &    sph_params%m_folding, s2d_tbl_SP%mspec_4_ispack,              &
     &    s2d_tbl_SP%jdx_fsph, s2d_tbl_SP%mtbl_fft_2_lgd)
!
      call dealloc_nidx_local(sph_lc1_SP)
      call alloc_sph_1d_global_idx(s3d_ranks, stk_lc1d, sph_gl1d)
!
      call set_sph_1d_global_idx_rtp(sph_params%m_folding,              &
     &    sph_rtp%nidx_global_rtp(3), s2d_tbl_SP%mdx_ispack,            &
     &    s3d_ranks, sph_dbc_SP, stk_lc1d, sph_gl1d)
!
      call alloc_sph_1d_domain_id(sph_rtp, sph_rj, s3d_ranks)
!
      call set_sph_1d_domain_id_rtp(stk_lc1d, sph_gl1d, s3d_ranks)
!
      if(iflag_debug .gt. 0) then
        write(50,*) 'idx_global_rtp_r', sph_gl1d%idx_global_rtp_r
      end if
!
      call dealloc_2d_sph_trans_table(s2d_tbl_SP)
      call dealloc_sph_gl_bc_param(sph_dbc_SP)
!
      end subroutine const_global_sph_FEM_grid
!
! -----------------------------------------------------------------------
!
      end module const_global_sph_grids_modes
