!>@file   set_local_sphere_by_global.f90
!!@brief  module set_local_sphere_by_global
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Copy number of global spherical harmonics indices
!!        to local data
!!
!!
!!@verbatim
!!      subroutine copy_gl_2_local_rj_param                             &
!!     &         (ip_rank, s3d_ranks, sph_lcp, stk_lc1d, sph_rj)
!!        type(sph_local_parameters), intent(in) :: sph_lcp
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!      subroutine copy_gl_2_local_rlm_param                            &
!!     &         (ip_rank, s3d_ranks, sph_lcp, stk_lc1d, sph_rlm)
!!        type(sph_local_parameters), intent(in) :: sph_lcp
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!      subroutine copy_gl_2_local_rtm_param                            &
!!     &         (ip_rank, s3d_ranks, sph_lcp, stk_lc1d, sph_rtm)
!!        type(sph_local_parameters), intent(in) :: sph_lcp
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!      subroutine copy_gl_2_local_rtp_param                            &
!!     &         (ip_rank, s3d_ranks, sph_lcp, stk_lc1d, sph_rtp)
!!        type(sph_local_parameters), intent(in) :: sph_lcp
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!@endverbatim
!
      module set_local_sphere_by_global
!
      use m_precision
      use t_spheric_parameter
      use t_spheric_global_ranks
      use t_sph_1d_global_index
      use t_sph_local_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_gl_2_local_rj_param                               &
     &         (ip_rank, s3d_ranks, sph_lcp, stk_lc1d, sph_rj)
!
      integer(kind = kint), intent(in) :: ip_rank
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_local_parameters), intent(in) :: sph_lcp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      integer(kind = kint) :: i1, i2
!
!
      sph_rj%irank_sph_rj(1:2) = s3d_ranks%iglobal_rank_rj(1:2,ip_rank)
!
      sph_rj%nnod_rj =  sph_lcp%nnod_local_rj(ip_rank+1)
!
      sph_rj%nidx_rj(1:2) =  sph_lcp%nidx_local_rj(ip_rank+1,1:2)
!
      i1 = sph_rj%irank_sph_rj(1) + 1
      i2 = sph_rj%irank_sph_rj(2) + 1
      sph_rj%ist_rj(1) =  stk_lc1d%istack_idx_local_rj_r(i1-1) + 1
      sph_rj%ist_rj(2) =  stk_lc1d%istack_idx_local_rj_j(i2-1) + 1
      sph_rj%ied_rj(1) =  stk_lc1d%istack_idx_local_rj_r(i1)
      sph_rj%ied_rj(2) =  stk_lc1d%istack_idx_local_rj_j(i2)
!
      end subroutine copy_gl_2_local_rj_param
!
! -----------------------------------------------------------------------
!
      subroutine copy_gl_2_local_rlm_param                              &
     &         (ip_rank, s3d_ranks, sph_lcp, stk_lc1d, sph_rlm)
!
      integer(kind = kint), intent(in) :: ip_rank
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_local_parameters), intent(in) :: sph_lcp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
      integer(kind = kint) :: i1, i2
!
!
      sph_rlm%irank_sph_rlm(1:2)                                        &
     &              = s3d_ranks%iglobal_rank_rlm(1:2,ip_rank)
!
      sph_rlm%nnod_rlm = sph_lcp%nnod_local_rlm(ip_rank+1)
!
      sph_rlm%nidx_rlm(1:2) = sph_lcp%nidx_local_rlm(ip_rank+1,1:2)
!
      i1 = sph_rlm%irank_sph_rlm(1) + 1
      i2 = sph_rlm%irank_sph_rlm(2) + 1
      sph_rlm%ist_rlm(1) = stk_lc1d%istack_idx_local_rlm_r(i1-1) + 1
      sph_rlm%ist_rlm(2) = stk_lc1d%istack_idx_local_rlm_j(i2-1) + 1
      sph_rlm%ied_rlm(1) = stk_lc1d%istack_idx_local_rlm_r(i1)
      sph_rlm%ied_rlm(2) = stk_lc1d%istack_idx_local_rlm_j(i2)
!
      end subroutine copy_gl_2_local_rlm_param
!
! -----------------------------------------------------------------------
!
      subroutine copy_gl_2_local_rtm_param                              &
     &         (ip_rank, s3d_ranks, sph_lcp, stk_lc1d, sph_rtm)
!
      integer(kind = kint), intent(in) :: ip_rank
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_local_parameters), intent(in) :: sph_lcp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
      integer(kind = kint) :: i1, i2, i3
!
!
      sph_rtm%irank_sph_rtm(1:3)                                        &
     &                 = s3d_ranks%iglobal_rank_rtm(1:3,ip_rank)
!
      sph_rtm%nnod_rtm = sph_lcp%nnod_local_rtm(ip_rank+1)
!
      sph_rtm%nidx_rtm(1:3) = sph_lcp%nidx_local_rtm(ip_rank+1,1:3)
!
      i1 = sph_rtm%irank_sph_rtm(1) + 1
      i2 = sph_rtm%irank_sph_rtm(2) + 1
      i3 = sph_rtm%irank_sph_rtm(3) + 1
      sph_rtm%ist_rtm(1) = stk_lc1d%istack_idx_local_rtm_r(i1-1) + 1
      sph_rtm%ist_rtm(2) = stk_lc1d%istack_idx_local_rtm_t(i2-1) + 1
      sph_rtm%ist_rtm(3) = stk_lc1d%istack_idx_local_rtm_m(i3-1) + 1
      sph_rtm%ied_rtm(1) = stk_lc1d%istack_idx_local_rtm_r(i1)
      sph_rtm%ied_rtm(2) = stk_lc1d%istack_idx_local_rtm_t(i2)
      sph_rtm%ied_rtm(3) = stk_lc1d%istack_idx_local_rtm_m(i3)
!
      end subroutine copy_gl_2_local_rtm_param
!
! -----------------------------------------------------------------------
!
      subroutine copy_gl_2_local_rtp_param                              &
     &         (ip_rank, s3d_ranks, sph_lcp, stk_lc1d, sph_rtp)
!
      integer(kind = kint), intent(in) :: ip_rank
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_local_parameters), intent(in) :: sph_lcp
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      integer(kind = kint) :: i1, i2, i3
!
!
      sph_rtp%irank_sph_rtp(1:3)                                        &
     &      = s3d_ranks%iglobal_rank_rtp(1:3,ip_rank)
!
      sph_rtp%nnod_rtp = sph_lcp%nnod_local_rtp(ip_rank+1)
!
      sph_rtp%nidx_rtp(1:3) = sph_lcp%nidx_local_rtp(ip_rank+1,1:3)
!
      i1 = sph_rtp%irank_sph_rtp(1) + 1
      i2 = sph_rtp%irank_sph_rtp(2) + 1
      i3 = sph_rtp%irank_sph_rtp(3) + 1
      sph_rtp%ist_rtp(1) = stk_lc1d%istack_idx_local_rtp_r(i1-1) + 1
      sph_rtp%ist_rtp(2) = stk_lc1d%istack_idx_local_rtp_t(i2-1) + 1
      sph_rtp%ist_rtp(3) = stk_lc1d%istack_idx_local_rtp_p(i3-1) + 1
      sph_rtp%ied_rtp(1) = stk_lc1d%istack_idx_local_rtp_r(i1)
      sph_rtp%ied_rtp(2) = stk_lc1d%istack_idx_local_rtp_t(i2)
      sph_rtp%ied_rtp(3) = stk_lc1d%istack_idx_local_rtp_p(i3)
!
      end subroutine copy_gl_2_local_rtp_param
!
! -----------------------------------------------------------------------
!
      end module set_local_sphere_by_global
