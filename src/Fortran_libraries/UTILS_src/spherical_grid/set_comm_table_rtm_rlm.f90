!>@file   set_comm_table_rtm_rlm.f90
!!@brief  module set_comm_table_rtm_rlm
!!
!!@author  H. Matsui
!!@date Programmed on July, 2007
!
!
!> @brief Construct communication table for rlm and rtm grid
!!
!!@verbatim
!!      subroutine allocate_ncomm
!!      subroutine deallocate_ncomm
!!
!!      subroutine count_comm_table_4_rlm
!!      subroutine set_comm_table_4_rlm
!!
!!      subroutine count_comm_table_4_rtm
!!      subroutine set_comm_table_4_rtm
!!
!!      subroutine count_num_domain_rtm_rlm(nneib_domain)
!!      subroutine set_comm_stack_rtm_rlm(ip_rank,                      &
!!     &          nneib_domain, id_domain, istack_sr, ntot_item_sr)
!!@endverbatim
!
      module set_comm_table_rtm_rlm
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: ncomm(:)
      integer(kind = kint), allocatable :: ineib_tgt(:)
      private :: ncomm, ineib_tgt
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
      allocate( ineib_tgt(0:ndomain_sph) )
      ncomm = 0
      ineib_tgt = -1
!
      end subroutine allocate_ncomm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ncomm
!
      deallocate(ncomm, ineib_tgt)
!
      end subroutine deallocate_ncomm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rlm
!
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_trans_comm_table
      use set_global_spherical_param
!
      integer(kind = kint) :: id_tgt_rank, inod
      integer(kind = kint) :: idx1, idx2, kp_rj, jp_rj
!
!
!
      ncomm = 0
      do inod = 1, nnod_rlm
        idx1 = idx_global_rlm(inod,1)
        idx2 = idx_global_rlm(inod,2)
        kp_rj = id_domain_rj_r(idx1)
        jp_rj = id_domain_rj_j(idx2)
!
        id_tgt_rank = set_rank_by_1b_rj_rank(ndomain_rj, kp_rj, jp_rj)
        ncomm(id_tgt_rank) = ncomm(id_tgt_rank) + 1
      end do
!
      end subroutine count_comm_table_4_rlm
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rlm
!
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_trans_comm_table
      use set_global_spherical_param
!
      integer(kind = kint) :: ip, irank_tgt, inod
      integer(kind = kint) :: idx1, idx2, kp_rj, jp_rj
!
      do ip = 1, nneib_domain_rlm
        ncomm(ip) = istack_sr_rlm(ip-1)
      end do
!
      do inod = 1, nnod_rlm
        idx1 = idx_global_rlm(inod,1)
        idx2 = idx_global_rlm(inod,2)
        kp_rj = id_domain_rj_r(idx1)
        jp_rj = id_domain_rj_j(idx2)
!
        irank_tgt = set_rank_by_1b_rj_rank(ndomain_rj, kp_rj, jp_rj)
        ip = ineib_tgt(irank_tgt)
!
        ncomm(ip) = ncomm(ip) + 1
        item_sr_rlm(ncomm(ip)) = inod
      end do
!
      end subroutine set_comm_table_4_rlm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rtm
!
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_trans_comm_table
      use set_global_spherical_param
!
      integer(kind = kint) :: id_tgt_rank, inod
      integer(kind = kint) :: idx1, idx2, idx3, kp_rtp, lp_rtp, mp_rtp
!
!
      ncomm = 0
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
        if(kp_rtp.lt.0 .or. lp_rtp.lt.0 .or. mp_rtp.lt.0) cycle
!
        id_tgt_rank = set_rank_by_1b_sph_rank(ndomain_rtp,              &
     &               kp_rtp, lp_rtp, mp_rtp)
        ncomm(id_tgt_rank) = ncomm(id_tgt_rank) + 1
      end do
!
      end subroutine count_comm_table_4_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rtm
!
      use m_spheric_parameter
      use m_spheric_global_ranks
      use m_sph_trans_comm_table
      use set_global_spherical_param
!
      integer(kind = kint) :: ip, irank_tgt, inod
      integer(kind = kint) :: idx1, idx2, idx3, kp_rtp, lp_rtp, mp_rtp
!
!
      do ip = 1, nneib_domain_rtm
        ncomm(ip) = istack_sr_rtm(ip-1)
      end do
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
        if(kp_rtp.lt.0 .or. lp_rtp.lt.0 .or. mp_rtp.lt.0) cycle
!
        irank_tgt = set_rank_by_1b_sph_rank(ndomain_rtp,                &
     &             kp_rtp, lp_rtp, mp_rtp)
        ip = ineib_tgt(irank_tgt)
!
        ncomm(ip) = ncomm(ip) + 1
        item_sr_rtm(ncomm(ip)) = inod
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
      subroutine set_comm_stack_rtm_rlm(ip_rank,                        &
     &          nneib_domain, id_domain, istack_sr, ntot_item_sr)
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
      icou =       0
      ineib_tgt = -1
      do ip = 1, ndomain_sph
        id_tgt_rank = mod( (ip_rank+ip),ndomain_sph )
        if (ncomm(id_tgt_rank) .gt. 0) then
          icou = icou + 1
          ineib_tgt(id_tgt_rank) = icou
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
      