!
!      module set_sph_1d_domain_id
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine set_sph_1d_domain_id_rtp                             &
!!     &         (stk_lc1d, sph_gl1d, s3d_ranks)
!!      subroutine set_sph_1d_domain_id_rj                              &
!!     &         (stk_lc1d, sph_gl1d, s3d_ranks)
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_1d_global_index), intent(in) :: sph_gl1d
!!        type(spheric_global_rank), intent(inout) :: s3d_ranks
!
      module set_sph_1d_domain_id
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
      subroutine set_sph_1d_domain_id_rtp                               &
     &         (stk_lc1d, sph_gl1d, s3d_ranks)
!
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
!
      integer(kind = kint) :: ip_rank, ip, ist, ied, i, idx
!
!
      do ip = 1, s3d_ranks%ndomain_rtp(1)
        ip_rank = ip - 1
        ist = stk_lc1d%istack_idx_local_rtp_r(ip-1) + 1
        ied = stk_lc1d%istack_idx_local_rtp_r(ip)
        do i = ist, ied
          idx = sph_gl1d%idx_global_rtp_r(i)
          s3d_ranks%id_domain_rtp_r(idx) = ip_rank
        end do
      end do
!
      do ip = 1, s3d_ranks%ndomain_rtp(2)
        ip_rank = ip - 1
        ist = stk_lc1d%istack_idx_local_rtp_t(ip-1) + 1
        ied = stk_lc1d%istack_idx_local_rtp_t(ip)
        do i = ist, ied
          idx = sph_gl1d%idx_global_rtp_t(i)
          s3d_ranks%id_domain_rtp_t(idx) = ip_rank
        end do
      end do
!
      do ip = 1, s3d_ranks%ndomain_rtp(3)
        ip_rank = ip - 1
        ist = stk_lc1d%istack_idx_local_rtp_p(ip-1) + 1
        ied = stk_lc1d%istack_idx_local_rtp_p(ip)
        do i = ist, ied
          idx = sph_gl1d%idx_global_rtp_p(i,1)
          s3d_ranks%id_domain_rtp_p(idx) = ip_rank
        end do
      end do
!
      end subroutine set_sph_1d_domain_id_rtp
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_1d_domain_id_rj                                &
     &         (stk_lc1d, sph_gl1d, s3d_ranks)
!
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
!
      integer(kind = kint) :: ip_rank, ip, ist, ied, i, idx
!
!
      do ip = 1, s3d_ranks%ndomain_rj(1)
        ip_rank = ip - 1
        ist = stk_lc1d%istack_idx_local_rj_r(ip-1) + 1
        ied = stk_lc1d%istack_idx_local_rj_r(ip)
        do i = ist, ied
          idx = sph_gl1d%idx_global_rj_r(i)
          s3d_ranks%id_domain_rj_r(idx) = ip_rank
        end do
      end do
!
      do ip = 1, s3d_ranks%ndomain_rj(2)
        ip_rank = ip - 1
        ist = stk_lc1d%istack_idx_local_rj_j(ip-1) + 1
        ied = stk_lc1d%istack_idx_local_rj_j(ip)
        do i = ist, ied
          idx = sph_gl1d%idx_global_rj_j(i,1)
          s3d_ranks%id_domain_rj_j(idx) = ip_rank
        end do
      end do
!
      end subroutine set_sph_1d_domain_id_rj
!
! -----------------------------------------------------------------------
!
      end module set_sph_1d_domain_id
