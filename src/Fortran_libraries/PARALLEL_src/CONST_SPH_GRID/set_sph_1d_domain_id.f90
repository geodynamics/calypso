!
!      module set_sph_1d_domain_id
!
!     Written by H. Matsui on July, 2007
!
!      subroutine set_sph_1d_domain_id_rtp
!      subroutine set_sph_1d_domain_id_rj
!
      module set_sph_1d_domain_id
!
      use m_precision
!
      use m_spheric_parameter
      use m_sph_1d_global_index
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_1d_domain_id_rtp
!
      use m_spheric_global_ranks
!
      integer(kind = kint) :: ip_rank, ip, ist, ied, i, idx
!
!
      do ip = 1, ndomain_rtp(1)
        ip_rank = ip - 1
        ist = istack_idx_local_rtp_r(ip-1) + 1
        ied = istack_idx_local_rtp_r(ip)
        do i = ist, ied
          idx = idx_global_rtp_r(i)
          id_domain_rtp_r(idx) = ip_rank
        end do
      end do
!
      do ip = 1, ndomain_rtp(2)
        ip_rank = ip - 1
        ist = istack_idx_local_rtp_t(ip-1) + 1
        ied = istack_idx_local_rtp_t(ip)
        do i = ist, ied
          idx = idx_global_rtp_t(i)
          id_domain_rtp_t(idx) = ip_rank
        end do
      end do
!
      do ip = 1, ndomain_rtp(3)
        ip_rank = ip - 1
        ist = istack_idx_local_rtp_p(ip-1) + 1
        ied = istack_idx_local_rtp_p(ip)
        do i = ist, ied
          idx = idx_global_rtp_p(i,1)
          id_domain_rtp_p(idx) = ip_rank
        end do
      end do
!
      end subroutine set_sph_1d_domain_id_rtp
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_1d_domain_id_rj
!
      use m_spheric_global_ranks
!
      integer(kind = kint) :: ip_rank, ip, ist, ied, i, idx
!
!
      do ip = 1, ndomain_rj(1)
        ip_rank = ip - 1
        ist = istack_idx_local_rj_r(ip-1) + 1
        ied = istack_idx_local_rj_r(ip)
        do i = ist, ied
          idx = idx_global_rj_r(i)
          id_domain_rj_r(idx) = ip_rank
        end do
      end do
!
      do ip = 1, ndomain_rj(2)
        ip_rank = ip - 1
        ist = istack_idx_local_rj_j(ip-1) + 1
        ied = istack_idx_local_rj_j(ip)
        do i = ist, ied
          idx = idx_global_rj_j(i,1)
          id_domain_rj_j(idx) = ip_rank
        end do
      end do
!
      end subroutine set_sph_1d_domain_id_rj
!
! -----------------------------------------------------------------------
!
      end module set_sph_1d_domain_id
