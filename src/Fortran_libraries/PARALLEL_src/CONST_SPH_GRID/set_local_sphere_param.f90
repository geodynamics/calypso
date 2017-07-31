!>@file   set_local_sphere_param.f90
!!@brief  module set_local_sphere_param
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Copy number of global spherical harmonics indices
!!        to local data
!!
!!
!!@verbatim
!!      subroutine set_global_sph_rtp_id(s3d_ranks, stk_lc1d, sph_rtp)
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!      subroutine set_global_sph_rj_id(s3d_ranks, stk_lc1d, sph_rj)
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_rj_grid), intent(inout) ::  sph_rj
!!      subroutine set_global_sph_4_rtm(s3d_ranks, stk_lc1d, sph_rtm)
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!      subroutine set_global_sph_4_rlm(s3d_ranks, stk_lc1d, sph_rlm)
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!@endverbatim
!
      module set_local_sphere_param
!
      use m_precision
!
      use t_spheric_global_ranks
      use t_sph_1d_global_index
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_rtp_id(s3d_ranks, stk_lc1d, sph_rtp)
!
      use t_spheric_rtp_data
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      integer(kind = kint) :: inod, k, l, m
      integer(kind = kint) :: k_gl, l_gl, m_gl
      integer(kind = kint) :: ndom_r, ndom_t, nsize_r, nsize_t
!
!
      ndom_r = s3d_ranks%ndomain_rtp(1)
      ndom_t = s3d_ranks%ndomain_rtp(2)
      nsize_r = stk_lc1d%istack_idx_local_rtp_r(ndom_r)
      nsize_t = stk_lc1d%istack_idx_local_rtp_t(ndom_t)
!
      inod = 0
      do m = 1, sph_rtp%nidx_rtp(3)
        m_gl = sph_rtp%idx_gl_1d_rtp_p(m,1)
        do l = 1, sph_rtp%nidx_rtp(2)
          l_gl = sph_rtp%idx_gl_1d_rtp_t(l)
          do k = 1, sph_rtp%nidx_rtp(1)
            k_gl = sph_rtp%idx_gl_1d_rtp_r(k)
!
            inod = inod + 1
            sph_rtp%idx_global_rtp(inod,1) = k_gl
            sph_rtp%idx_global_rtp(inod,2) = l_gl
            sph_rtp%idx_global_rtp(inod,3) = m_gl
          end do
        end do
      end do
!
      end subroutine set_global_sph_rtp_id
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_rj_id(s3d_ranks, stk_lc1d, sph_rj)
!
      use t_spheric_rj_data
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_rj_grid), intent(inout) ::  sph_rj
!
      integer(kind = kint) :: j, k, inod
      integer(kind = kint) :: ndom_r, nsize_r
!
      ndom_r =  s3d_ranks%ndomain_rj(1)
      nsize_r = stk_lc1d%istack_idx_local_rj_r(ndom_r)
!
      inod = 0
      do k = 1, sph_rj%nidx_rj(1)
        do j = 1, sph_rj%nidx_rj(2)
          inod = inod + 1
          sph_rj%idx_global_rj(inod,1) = sph_rj%idx_gl_1d_rj_r(k)
          sph_rj%idx_global_rj(inod,2) = sph_rj%idx_gl_1d_rj_j(j,1)
        end do
      end do
!
      if(sph_rj%inod_rj_center .eq. 0) return
      sph_rj%idx_global_rj(sph_rj%nnod_rj,1) = 0
      sph_rj%idx_global_rj(sph_rj%nnod_rj,2) = 0
!
      end subroutine set_global_sph_rj_id
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_4_rtm(s3d_ranks, stk_lc1d, sph_rtm)
!
      use t_spheric_rtm_data
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
      integer(kind = kint) :: inod, k, l, m
      integer(kind = kint) :: ndom_r, ndom_t, nsize_r, nsize_t
!
      ndom_r = s3d_ranks%ndomain_rtm(1)
      ndom_t = s3d_ranks%ndomain_rtm(2)
      nsize_r = stk_lc1d%istack_idx_local_rtm_r(ndom_r)
      nsize_t = stk_lc1d%istack_idx_local_rtm_t(ndom_t)
!
!
      inod = 0
      do m = 1, sph_rtm%nidx_rtm(3)
        do k = 1, sph_rtm%nidx_rtm(1)
          do l = 1, sph_rtm%nidx_rtm(2)
            inod = inod + 1
            sph_rtm%idx_global_rtm(inod,1) = sph_rtm%idx_gl_1d_rtm_r(k)
            sph_rtm%idx_global_rtm(inod,2) = sph_rtm%idx_gl_1d_rtm_t(l)
            sph_rtm%idx_global_rtm(inod,3)                              &
     &           = sph_rtm%idx_gl_1d_rtm_m(m,1)
          end do
        end do
      end do
!
      end subroutine set_global_sph_4_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_4_rlm(s3d_ranks, stk_lc1d, sph_rlm)
!
      use t_spheric_rlm_data
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
      integer(kind = kint) :: j, k, inod
      integer(kind = kint) :: ndom_r, nsize_r
!
      ndom_r = s3d_ranks%ndomain_rlm(1)
      nsize_r = stk_lc1d%istack_idx_local_rlm_r(ndom_r)
!
      inod = 0
      do k = 1, sph_rlm%nidx_rlm(1)
        do j = 1, sph_rlm%nidx_rlm(2)
          inod = inod + 1
          sph_rlm%idx_global_rlm(inod,1) = sph_rlm%idx_gl_1d_rlm_r(k)
          sph_rlm%idx_global_rlm(inod,2) = sph_rlm%idx_gl_1d_rlm_j(j,1)
        end do
      end do
!
      end subroutine set_global_sph_4_rlm
!
! -----------------------------------------------------------------------!
      end module set_local_sphere_param
