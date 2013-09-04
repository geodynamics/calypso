!
!      module set_comm_table_rtm_rlm
!
!     Written by H. Matsui on July, 2007
!
!      subroutine allocate_ncomm
!      subroutine deallocate_ncomm
!
!      subroutine count_comm_table_4_rlm(ip_rank)
!      subroutine set_comm_table_4_rlm(ip_rank)
!
!      subroutine count_comm_table_4_rtm
!      subroutine set_comm_table_4_rtm
!
!      subroutine count_num_domain_rtm_rlm(nneib_domain)
!      subroutine set_comm_stack_rtm_rlm(ip_rank, nneib_domain,         &
!     &          id_domain, istack_sr, ntot_item_sr)
!
      module set_comm_table_rtm_rlm
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: ncomm(:)
      private :: ncomm
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ncomm
!
      use m_spheric_parameter
!
      allocate( ncomm(0:ndomain_sph) )
      ncomm = 0
!
      end subroutine allocate_ncomm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ncomm
!
      deallocate( ncomm )
!
      end subroutine deallocate_ncomm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rlm(ip_rank)
!
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ip_rank
!
      integer(kind = kint) :: ip, id_tgt_rank, kp, jp, inod
      integer(kind = kint) :: idx1, idx2, kp_rj, jp_rj
!
!
      do ip = 1, ndomain_sph
        id_tgt_rank = mod( (ip_rank+ip),ndomain_sph )
        kp = iglobal_rank_rj(1,id_tgt_rank)
        jp = iglobal_rank_rj(2,id_tgt_rank)
!
        do inod = 1, nnod_rlm
          idx1 = idx_global_rlm(inod,1)
          idx2 = idx_global_rlm(inod,2)
          kp_rj = id_domain_rj_r(idx1)
          jp_rj = id_domain_rj_j(idx2)
!
          if (kp.eq.kp_rj .and. jp.eq.jp_rj) then
            ncomm(id_tgt_rank) = ncomm(id_tgt_rank) + 1
          end if
        end do
!
      end do
!
      end subroutine count_comm_table_4_rlm
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rlm(ip_rank)
!
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ip_rank
!
      integer(kind = kint) :: ip, id_tgt_rank, kp, jp, inod, icou
      integer(kind = kint) :: idx1, idx2, kp_rj, jp_rj
!
      icou = 0
      do ip = 1, ndomain_sph
        id_tgt_rank = mod( (ip_rank+ip),ndomain_sph )
        kp = iglobal_rank_rj(1,id_tgt_rank)
        jp = iglobal_rank_rj(2,id_tgt_rank)
!
        do inod = 1, nnod_rlm
          idx1 = idx_global_rlm(inod,1)
          idx2 = idx_global_rlm(inod,2)
          kp_rj = id_domain_rj_r(idx1)
          jp_rj = id_domain_rj_j(idx2)
!
          if (kp.eq.kp_rj .and. jp.eq.jp_rj) then
            icou = icou + 1
            item_sr_rlm(icou) = inod
          end if
        end do
!
      end do
!
      end subroutine set_comm_table_4_rlm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rtm(ip_rank)
!
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ip_rank
!
      integer(kind = kint) :: ip, id_tgt_rank, kp, lp, mp, inod
      integer(kind = kint) :: idx1, idx2, idx3, kp_rtp, lp_rtp, mp_rtp
!
!
      ncomm = 0
      do ip = 1, ndomain_sph
        id_tgt_rank = mod( (ip_rank+ip),ndomain_sph )
        kp = iglobal_rank_rtp(1,id_tgt_rank)
        lp = iglobal_rank_rtp(2,id_tgt_rank)
        mp = iglobal_rank_rtp(3,id_tgt_rank)
!
        do inod = 1, nnod_rtm
          idx1 = idx_global_rtm(inod,1)
          idx2 = idx_global_rtm(inod,2)
          idx3 = idx_global_rtm(inod,3)
!
          if(idx1.ge.1 .and. idx1.le.nidx_global_rtm(1)) then
            kp_rtp = id_domain_rtp_r(idx1)
          else
            kp_rtp = -1
          end if
!
          if(idx2.ge.1 .and. idx2.le.nidx_global_rtm(2)) then
            lp_rtp = id_domain_rtp_t(idx2)
          else
            lp_rtp = -1
          end if
!
          if(idx3.ge.1 .and. idx3.le.nidx_global_rtm(3)) then
            mp_rtp = id_domain_rtp_p(idx3)
          else
            mp_rtp = -1
          end if
!
!          write(100,*) inod, idx1, idx2, idx3, &
!    &        size(id_domain_rtp_r), &
!    &        size(id_domain_rtp_t), &
!    &        size(id_domain_rtp_p)
!
          if (kp.eq.kp_rtp .and. lp.eq.lp_rtp .and. mp.eq.mp_rtp) then
            ncomm(id_tgt_rank) = ncomm(id_tgt_rank) + 1
          end if
        end do
!
      end do
!
      end subroutine count_comm_table_4_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rtm(ip_rank)
!
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ip_rank
!
      integer(kind = kint) :: ip, id_tgt_rank, kp, lp, mp, inod, icou
      integer(kind = kint) :: idx1, idx2, idx3, kp_rtp, lp_rtp, mp_rtp
!
!
      icou = 0
      do ip = 1, ndomain_sph
        id_tgt_rank = mod( (ip_rank+ip),ndomain_sph )
        kp = iglobal_rank_rtp(1,id_tgt_rank)
        lp = iglobal_rank_rtp(2,id_tgt_rank)
        mp = iglobal_rank_rtp(3,id_tgt_rank)
!
        do inod = 1, nnod_rtm
          idx1 = idx_global_rtm(inod,1)
          idx2 = idx_global_rtm(inod,2)
          idx3 = idx_global_rtm(inod,3)
!
          if(idx1.ge.1 .and. idx1.le.nidx_global_rtm(1)) then
            kp_rtp = id_domain_rtp_r(idx1)
          else
            kp_rtp = -1
          end if
!
          if(idx2.ge.1 .and. idx2.le.nidx_global_rtm(2)) then
            lp_rtp = id_domain_rtp_t(idx2)
          else
            lp_rtp = -1
          end if
!
          if(idx3.ge.1 .and. idx3.le.nidx_global_rtm(3)) then
            mp_rtp = id_domain_rtp_p(idx3)
          else
            mp_rtp = -1
          end if
!
          if (kp.eq.kp_rtp .and. lp.eq.lp_rtp .and. mp.eq.mp_rtp) then
            icou = icou + 1
            item_sr_rtm(icou) = inod
          end if
        end do
!
      end do
!
!
      end subroutine set_comm_table_4_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_num_domain_rtm_rlm(nneib_domain)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(inout) :: nneib_domain
      integer(kind = kint) :: ip_rank
!
!
      nneib_domain = 0
      do ip_rank = 0, ndomain_sph-1
        if (ncomm(ip_rank).gt.0)  nneib_domain = nneib_domain + 1
      end do
!
      end subroutine count_num_domain_rtm_rlm
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_stack_rtm_rlm(ip_rank, nneib_domain,          &
     &          id_domain, istack_sr, ntot_item_sr)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: nneib_domain
      integer(kind = kint), intent(inout) :: ntot_item_sr
      integer(kind = kint), intent(inout) :: id_domain(nneib_domain)
      integer(kind = kint), intent(inout) :: istack_sr(0:nneib_domain)
!
      integer(kind = kint) :: icou, ip, id_tgt_rank
!
!
      istack_sr(0) = 0
      icou = 0
      do ip = 1, ndomain_sph
        id_tgt_rank = mod( (ip_rank+ip),ndomain_sph )
        if (ncomm(id_tgt_rank) .gt. 0) then
          icou = icou + 1
          id_domain(icou) = id_tgt_rank
          istack_sr(icou) = istack_sr(icou-1) + ncomm(id_tgt_rank)
        end if
      end do
      ntot_item_sr = istack_sr(nneib_domain)
!
      end subroutine set_comm_stack_rtm_rlm
!
! -----------------------------------------------------------------------
!
      end module set_comm_table_rtm_rlm
      