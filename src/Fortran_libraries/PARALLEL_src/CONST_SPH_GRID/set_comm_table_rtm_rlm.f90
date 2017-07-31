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
!!      subroutine allocate_ncomm(ndomain_sph)
!!      subroutine deallocate_ncomm
!!
!!      subroutine count_comm_table_4_rlm                               &
!!     &         (s3d_ranks, nnod_rlm, idx_global_rlm)
!!      subroutine set_comm_table_4_rlm                                 &
!!     &         (s3d_ranks, nnod_rlm, idx_global_rlm,                  &
!!     &          nneib_domain_rlm, ntot_item_sr_rlm, istack_sr_rlm,    &
!!     &          item_sr_rlm)
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!
!!      subroutine count_comm_table_4_rtm                               &
!!     &         (s3d_ranks, nnod_rtm, nidx_global_rtm, idx_global_rtm)
!!      subroutine set_comm_table_4_rtm                                 &
!!     &         (s3d_ranks, nnod_rtm, nidx_global_rtm, idx_global_rtm, &
!!     &          nneib_domain_rtm, ntot_item_sr_rtm, istack_sr_rtm,    &
!!     &          item_sr_rtm)
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!
!!      subroutine count_num_domain_rtm_rlm(ndomain_sph, nneib_domain)
!!      subroutine set_comm_stack_rtm_rlm(ip_rank, ndomain_sph,         &
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
      subroutine allocate_ncomm(ndomain_sph)
!
      integer(kind = kint), intent(in) :: ndomain_sph
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
      subroutine count_comm_table_4_rlm                                 &
     &         (s3d_ranks, nnod_rlm, idx_global_rlm)
!
      use t_spheric_global_ranks
      use set_global_spherical_param
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: idx_global_rlm(nnod_rlm,2)
!
      integer(kind = kint) :: irank_tgt, inod
      integer(kind = kint) :: idx1, idx2, kp_rj, jp_rj
!
!
!
      ncomm = 0
      do inod = 1, nnod_rlm
        idx1 = idx_global_rlm(inod,1)
        idx2 = idx_global_rlm(inod,2)
        kp_rj = s3d_ranks%id_domain_rj_r(idx1)
        jp_rj = s3d_ranks%id_domain_rj_j(idx2)
!
        irank_tgt = set_rank_by_1b_rj_rank                              &
     &            (s3d_ranks%iflag_radial_inner_domain,                 &
     &             s3d_ranks%ndomain_rj, kp_rj, jp_rj)
        ncomm(irank_tgt) = ncomm(irank_tgt) + 1
      end do
!
      end subroutine count_comm_table_4_rlm
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rlm                                   &
     &         (s3d_ranks, nnod_rlm, idx_global_rlm,                    &
     &          nneib_domain_rlm, ntot_item_sr_rlm, istack_sr_rlm,      &
     &          item_sr_rlm)
!
      use t_spheric_global_ranks
      use set_global_spherical_param
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: idx_global_rlm(nnod_rlm,2)
!
      integer(kind = kint), intent(in) :: nneib_domain_rlm
      integer(kind = kint), intent(in) :: ntot_item_sr_rlm
      integer(kind = kint), intent(in)                                  &
     &              :: istack_sr_rlm(0:nneib_domain_rlm)
!
      integer(kind = kint), intent(inout)                               &
     &              :: item_sr_rlm(ntot_item_sr_rlm)
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
        kp_rj = s3d_ranks%id_domain_rj_r(idx1)
        jp_rj = s3d_ranks%id_domain_rj_j(idx2)
!
        irank_tgt = set_rank_by_1b_rj_rank                              &
     &            (s3d_ranks%iflag_radial_inner_domain,                 &
     &             s3d_ranks%ndomain_rj, kp_rj, jp_rj)
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
      subroutine count_comm_table_4_rtm                                 &
     &         (s3d_ranks, nnod_rtm, nidx_global_rtm, idx_global_rtm)
!
      use t_spheric_global_ranks
      use set_global_spherical_param
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_global_rtm(3)
      integer(kind = kint), intent(in) :: idx_global_rtm(nnod_rtm,3)
!
      integer(kind = kint) :: irank_tgt, inod
      integer(kind = kint) :: idx1, idx2, idx3, kp_rtp, lp_rtp, mp_rtp
!
!
      ncomm = 0
      do inod = 1, nnod_rtm
        idx1 = idx_global_rtm(inod,1)
        idx2 = idx_global_rtm(inod,2)
        idx3 = idx_global_rtm(inod,3)
!
        if(idx1.ge.1 .and. idx1 .le. nidx_global_rtm(1)) then
          kp_rtp = s3d_ranks%id_domain_rtp_r(idx1)
        else
          kp_rtp = -1
        end if
!
        if(idx2.ge.1 .and. idx2 .le. nidx_global_rtm(2)) then
          lp_rtp = s3d_ranks%id_domain_rtp_t(idx2)
        else
          lp_rtp = -1
        end if
!
        if(idx3.ge.1 .and. idx3 .le. nidx_global_rtm(3)) then
          mp_rtp = s3d_ranks%id_domain_rtp_p(idx3)
        else
          mp_rtp = -1
        end if
        if(kp_rtp.lt.0 .or. lp_rtp.lt.0 .or. mp_rtp.lt.0) cycle
!
        irank_tgt = set_rank_by_1b_sph_rank                             &
     &            (s3d_ranks%iflag_radial_inner_domain,                 &
     &             s3d_ranks%ndomain_rtp, kp_rtp, lp_rtp, mp_rtp)
        ncomm(irank_tgt) = ncomm(irank_tgt) + 1
      end do
!
      end subroutine count_comm_table_4_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rtm                                   &
     &         (s3d_ranks, nnod_rtm, nidx_global_rtm, idx_global_rtm,   &
     &          nneib_domain_rtm, ntot_item_sr_rtm, istack_sr_rtm,      &
     &          item_sr_rtm)
!
      use t_spheric_global_ranks
      use set_global_spherical_param
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_global_rtm(3)
      integer(kind = kint), intent(in) :: idx_global_rtm(nnod_rtm,3)
!
      integer(kind = kint), intent(in) :: nneib_domain_rtm
      integer(kind = kint), intent(in) :: ntot_item_sr_rtm
      integer(kind = kint), intent(in)                                  &
     &              :: istack_sr_rtm(0:nneib_domain_rtm)
!
      integer(kind = kint), intent(inout)                               &
     &              :: item_sr_rtm(ntot_item_sr_rtm)
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
        if(idx1.ge.1 .and. idx1 .le. nidx_global_rtm(1)) then
          kp_rtp = s3d_ranks%id_domain_rtp_r(idx1)
        else
          kp_rtp = -1
        end if
!
        if(idx2.ge.1 .and. idx2 .le. nidx_global_rtm(2)) then
          lp_rtp = s3d_ranks%id_domain_rtp_t(idx2)
        else
          lp_rtp = -1
        end if
!
        if(idx3.ge.1 .and. idx3 .le. nidx_global_rtm(3)) then
          mp_rtp = s3d_ranks%id_domain_rtp_p(idx3)
        else
          mp_rtp = -1
        end if
        if(kp_rtp.lt.0 .or. lp_rtp.lt.0 .or. mp_rtp.lt.0) cycle
!
        irank_tgt = set_rank_by_1b_sph_rank                             &
     &            (s3d_ranks%iflag_radial_inner_domain,                 &
     &             s3d_ranks%ndomain_rtp, kp_rtp, lp_rtp, mp_rtp)
        ip = ineib_tgt(irank_tgt)
!
        ncomm(ip) = ncomm(ip) + 1
        item_sr_rtm(ncomm(ip)) = inod
      end do
!
      end subroutine set_comm_table_4_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_num_domain_rtm_rlm(ndomain_sph, nneib_domain)
!
      integer(kind = kint), intent(in) :: ndomain_sph
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
      subroutine set_comm_stack_rtm_rlm(ip_rank, ndomain_sph,           &
     &          nneib_domain, id_domain, istack_sr, ntot_item_sr)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: nneib_domain
      integer(kind = kint), intent(inout) :: ntot_item_sr
      integer(kind = kint), intent(inout) :: id_domain(nneib_domain)
      integer(kind = kint), intent(inout) :: istack_sr(0:nneib_domain)
!
      integer(kind = kint) :: icou, ip, irank_tgt
!
!
      istack_sr(0) = 0
      icou =       0
      ineib_tgt = -1
      do ip = 1, ndomain_sph
        irank_tgt = mod( (ip_rank+ip),ndomain_sph )
        if (ncomm(irank_tgt) .gt. 0) then
          icou = icou + 1
          ineib_tgt(irank_tgt) = icou
          id_domain(icou) = irank_tgt
          istack_sr(icou) = istack_sr(icou-1) + ncomm(irank_tgt)
        end if
      end do
      ntot_item_sr = istack_sr(nneib_domain)
!
      end subroutine set_comm_stack_rtm_rlm
!
! -----------------------------------------------------------------------
!
      end module set_comm_table_rtm_rlm
      