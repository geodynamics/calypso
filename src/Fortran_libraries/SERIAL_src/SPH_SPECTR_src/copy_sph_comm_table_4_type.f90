!>@file   copy_sph_comm_table_4_type.f90
!!@brief  module copy_sph_comm_table_4_type
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Copy communication table for spherical transform 
!!        from structures
!!
!!@verbatim
!!      subroutine copy_neib_rtp_from_type(rtp_comm)
!!      subroutine copy_neib_rtm_from_type(rtm_comm)
!!      subroutine copy_neib_rlm_from_type(rlm_comm)
!!      subroutine copy_neib_rj_from_type(rj_comm)
!!
!!      subroutine copy_comm_rtp_from_type(rtp_comm, nnod_rtp)
!!      subroutine copy_comm_rtm_from_type(rtm_comm, nnod_rtm)
!!      subroutine copy_comm_rlm_from_type(rlm_comm, nnod_rlm)
!!      subroutine copy_comm_rj_from_type(rj_comm, nnod_rj)
!!
!!      subroutine copy_comm_rtp_to_type(sph_rtp, rtp_comm)
!!      subroutine copy_comm_rtp_num_to_type(rtp_comm)
!!      subroutine copy_comm_rtp_item_to_type(sph_rtp, rtp_comm)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(inout) :: rtp_comm
!!      subroutine copy_comm_rtm_to_type(sph_rtm, rtm_comm)
!!      subroutine copy_comm_rtm_num_to_type(rtm_comm)
!!      subroutine copy_comm_rtm_item_to_type(sph_rtm, rtm_comm)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: rtp_comm
!!      subroutine copy_comm_rlm_to_type(sph_rlm, rlm_comm)
!!      subroutine copy_comm_rlm_num_to_type(rlm_comm)
!!      subroutine copy_comm_rlm_item_to_type(sph_rlm, rlm_comm)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: rlm_comm
!!      subroutine copy_comm_rj_to_type(sph_rj, rj_comm)
!!      subroutine copy_comm_rj_num_to_type(rj_comm)
!!      subroutine copy_comm_rj_item_to_type(sph_rj, rj_comm)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_comm_tbl), intent(inout) :: rj_comm
!!@endverbatim
!
      module copy_sph_comm_table_4_type
!
      use m_precision
!
      use m_constants
      use m_sph_trans_comm_table
      use t_sph_trans_comm_tbl
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_neib_rtp_from_type(rtp_comm)
!
      type(sph_comm_tbl), intent(in) :: rtp_comm
!
!
      nneib_domain_rtp = rtp_comm%nneib_domain
      call allocate_sph_comm_stack_rtp
!
      id_domain_rtp(1:nneib_domain_rtp)                                 &
     &      = rtp_comm%id_domain(1:nneib_domain_rtp)
      istack_sr_rtp(0:nneib_domain_rtp)                                 &
     &      = rtp_comm%istack_sr(0:nneib_domain_rtp)
!
      end subroutine copy_neib_rtp_from_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_neib_rtm_from_type(rtm_comm)
!
      type(sph_comm_tbl), intent(in) :: rtm_comm
!
!
      nneib_domain_rtm = rtm_comm%nneib_domain
      call allocate_sph_comm_stack_rtm
!
      id_domain_rtm(1:nneib_domain_rtm)                                 &
     &      = rtm_comm%id_domain(1:nneib_domain_rtm)
      istack_sr_rtm(0:nneib_domain_rtm)                                 &
     &      = rtm_comm%istack_sr(0:nneib_domain_rtm)
!
      end subroutine copy_neib_rtm_from_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_neib_rlm_from_type(rlm_comm)
!
      type(sph_comm_tbl), intent(in) :: rlm_comm
!
!
      nneib_domain_rlm = rlm_comm%nneib_domain
      call allocate_sph_comm_stack_rlm
!
      id_domain_rlm(1:nneib_domain_rlm)                                 &
     &      = rlm_comm%id_domain(1:nneib_domain_rlm)
      istack_sr_rlm(0:nneib_domain_rlm)                                 &
     &      = rlm_comm%istack_sr(0:nneib_domain_rlm)
!
      end subroutine copy_neib_rlm_from_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_neib_rj_from_type(rj_comm)
!
      type(sph_comm_tbl), intent(in) :: rj_comm
!
!
      nneib_domain_rj = rj_comm%nneib_domain
      call allocate_sph_comm_stack_rj
!
      id_domain_rj(1:nneib_domain_rj)                                   &
     &      = rj_comm%id_domain(1:nneib_domain_rj)
      istack_sr_rj(0:nneib_domain_rj)                                   &
     &      = rj_comm%istack_sr(0:nneib_domain_rj)
!
      end subroutine copy_neib_rj_from_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtp_from_type(rtp_comm, nnod_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      type(sph_comm_tbl), intent(in) :: rtp_comm
!
!
      call copy_neib_rtp_from_type(rtp_comm)
!
      ntot_item_sr_rtp = rtp_comm%ntot_item_sr
      call allocate_sph_comm_item_rtp(nnod_rtp)
!
      item_sr_rtp(1:ntot_item_sr_rtp)                                   &
     &      = rtp_comm%item_sr(1:ntot_item_sr_rtp)
      irev_sr_rtp(1:nnod_rtp) = rtp_comm%irev_sr(1:nnod_rtp)
      iflag_self_rtp = rtp_comm%iflag_self
!
!      call dealloc_type_sph_comm_item(rtp_comm)
!
      end subroutine copy_comm_rtp_from_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtm_from_type(rtm_comm, nnod_rtm)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      type(sph_comm_tbl), intent(in) :: rtm_comm
!
!
      call copy_neib_rtm_from_type(rtm_comm)
!
      ntot_item_sr_rtm = rtm_comm%ntot_item_sr
      call allocate_sph_comm_item_rtm(nnod_rtm)
!
      item_sr_rtm(1:ntot_item_sr_rtm)                                   &
     &      = rtm_comm%item_sr(1:ntot_item_sr_rtm)
      irev_sr_rtm(1:nnod_rtm) = rtm_comm%irev_sr(1:nnod_rtm)
      iflag_self_rtm = rtm_comm%iflag_self
!
!      call dealloc_type_sph_comm_item(rtm_comm)
!
      end subroutine copy_comm_rtm_from_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rlm_from_type(rlm_comm, nnod_rlm)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      type(sph_comm_tbl), intent(in) :: rlm_comm
!
!
      call copy_neib_rlm_from_type(rlm_comm)
!
      ntot_item_sr_rlm = rlm_comm%ntot_item_sr
      call allocate_sph_comm_item_rlm(nnod_rlm)
!
      item_sr_rlm(1:ntot_item_sr_rlm)                                   &
     &      = rlm_comm%item_sr(1:ntot_item_sr_rlm)
      irev_sr_rlm(1:nnod_rlm) = rlm_comm%irev_sr(1:nnod_rlm)
      iflag_self_rlm = rlm_comm%iflag_self
!
!      call dealloc_type_sph_comm_item(rlm_comm)
!
      end subroutine copy_comm_rlm_from_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rj_from_type(rj_comm, nnod_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj
      type(sph_comm_tbl), intent(in) :: rj_comm
!
!
      call copy_neib_rj_from_type(rj_comm)
!
      ntot_item_sr_rj = rj_comm%ntot_item_sr
      call allocate_sph_comm_item_rj(nnod_rj)
!
      item_sr_rj(1:ntot_item_sr_rj)                                     &
     &      = rj_comm%item_sr(1:ntot_item_sr_rj)
      irev_sr_rj(1:nnod_rj) = rj_comm%irev_sr(1:nnod_rj)
      iflag_self_rj = rj_comm%iflag_self
!
!      call dealloc_type_sph_comm_item(rj_comm)
!
      end subroutine copy_comm_rj_from_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtp_to_type(sph_rtp, rtp_comm)
!
      use t_spheric_parameter
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(inout) :: rtp_comm
!
!
      call copy_comm_rtp_num_to_type(rtp_comm)
      call copy_comm_rtp_item_to_type(sph_rtp, rtp_comm)
!
      end subroutine copy_comm_rtp_to_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtm_to_type(sph_rtm, rtm_comm)
!
      use t_spheric_parameter
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: rtm_comm
!
!
      call copy_comm_rtm_num_to_type(rtm_comm)
      call copy_comm_rtm_item_to_type(sph_rtm, rtm_comm)
!
      end subroutine copy_comm_rtm_to_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rlm_to_type(sph_rlm, rlm_comm)
!
      use t_spheric_parameter
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: rlm_comm
!
!
      call copy_comm_rlm_num_to_type(rlm_comm)
      call copy_comm_rlm_item_to_type(sph_rlm, rlm_comm)
!
      end subroutine copy_comm_rlm_to_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rj_to_type(sph_rj, rj_comm)
!
      use t_spheric_parameter
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: rj_comm
!
!
      call copy_comm_rj_num_to_type(rj_comm)
      call copy_comm_rj_item_to_type(sph_rj, rj_comm)
!
      end subroutine copy_comm_rj_to_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtp_num_to_type(rtp_comm)
!
      use t_spheric_parameter
!
      type(sph_comm_tbl), intent(inout) :: rtp_comm
!
!
      rtp_comm%nneib_domain = nneib_domain_rtp
      call alloc_type_sph_comm_stack(rtp_comm)
!
      rtp_comm%id_domain(1:nneib_domain_rtp)                            &
     &      = id_domain_rtp(1:nneib_domain_rtp)
      rtp_comm%istack_sr(0:nneib_domain_rtp)                            &
     &      = istack_sr_rtp(0:nneib_domain_rtp)
!
      end subroutine copy_comm_rtp_num_to_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtm_num_to_type(rtm_comm)
!
      use t_spheric_parameter
!
      type(sph_comm_tbl), intent(inout) :: rtm_comm
!
!
      rtm_comm%nneib_domain = nneib_domain_rtm
      call alloc_type_sph_comm_stack(rtm_comm)
!
      rtm_comm%id_domain(1:nneib_domain_rtm)                            &
     &      = id_domain_rtm(1:nneib_domain_rtm)
      rtm_comm%istack_sr(0:nneib_domain_rtm)                            &
     &      = istack_sr_rtm(0:nneib_domain_rtm)
!
      end subroutine copy_comm_rtm_num_to_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rlm_num_to_type(rlm_comm)
!
      use t_spheric_parameter
!
      type(sph_comm_tbl), intent(inout) :: rlm_comm
!
!
      rlm_comm%nneib_domain = nneib_domain_rlm
      call alloc_type_sph_comm_stack(rlm_comm)
!
      rlm_comm%id_domain(1:nneib_domain_rlm)                            &
     &      = id_domain_rlm(1:nneib_domain_rlm)
      rlm_comm%istack_sr(0:nneib_domain_rlm)                            &
     &      = istack_sr_rlm(0:nneib_domain_rlm)
!
      end subroutine copy_comm_rlm_num_to_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rj_num_to_type(rj_comm)
!
      use t_spheric_parameter
!
      type(sph_comm_tbl), intent(inout) :: rj_comm
!
!
      rj_comm%nneib_domain = nneib_domain_rj
      call alloc_type_sph_comm_stack(rj_comm)
!
      rj_comm%id_domain(1:nneib_domain_rj)                              &
     &      = id_domain_rj(1:nneib_domain_rj)
      rj_comm%istack_sr(0:nneib_domain_rj)                              &
     &      = istack_sr_rj(0:nneib_domain_rj)
!
      end subroutine copy_comm_rj_num_to_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtp_item_to_type(sph_rtp, rtp_comm)
!
      use t_spheric_parameter
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(inout) :: rtp_comm
!
!
      rtp_comm%ntot_item_sr = ntot_item_sr_rtp
      call alloc_type_sph_comm_item(sph_rtp%nnod_rtp, rtp_comm)
!
      rtp_comm%item_sr(1:ntot_item_sr_rtp)                              &
     &      = item_sr_rtp(1:ntot_item_sr_rtp)
!
      call set_reverse_sph_comm_tbl_t(sph_rtp%nnod_rtp, rtp_comm)
!
      rtp_comm%iflag_self = iflag_self_rtp
!
      end subroutine copy_comm_rtp_item_to_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtm_item_to_type(sph_rtm, rtm_comm)
!
      use t_spheric_parameter
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: rtm_comm
!
!
      rtm_comm%ntot_item_sr = ntot_item_sr_rtm
      call alloc_type_sph_comm_item(sph_rtm%nnod_rtm, rtm_comm)
!
      rtm_comm%item_sr(1:ntot_item_sr_rtm)                              &
     &      = item_sr_rtm(1:ntot_item_sr_rtm)
!
      call set_reverse_sph_comm_tbl_t(sph_rtm%nnod_rtm, rtm_comm)
!
      rtm_comm%iflag_self = iflag_self_rtm
!
      end subroutine copy_comm_rtm_item_to_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rlm_item_to_type(sph_rlm, rlm_comm)
!
      use t_spheric_parameter
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: rlm_comm
!
!
      rlm_comm%ntot_item_sr = ntot_item_sr_rlm
      call alloc_type_sph_comm_item(sph_rlm%nnod_rlm, rlm_comm)
!
      rlm_comm%item_sr(1:ntot_item_sr_rlm)                              &
     &      = item_sr_rlm(1:ntot_item_sr_rlm)
!
      call set_reverse_sph_comm_tbl_t(sph_rlm%nnod_rlm, rlm_comm)
!
      rlm_comm%iflag_self = iflag_self_rlm
!
      end subroutine copy_comm_rlm_item_to_type
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rj_item_to_type(sph_rj, rj_comm)
!
      use t_spheric_parameter
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_comm_tbl), intent(inout) :: rj_comm
!
!
      rj_comm%ntot_item_sr = ntot_item_sr_rj
      call alloc_type_sph_comm_item(sph_rj%nnod_rj, rj_comm)
!
      rj_comm%item_sr(1:ntot_item_sr_rj)                                &
     &      = item_sr_rj(1:ntot_item_sr_rj)
!
      call set_reverse_sph_comm_tbl_t(sph_rj%nnod_rj, rj_comm)
!
      rj_comm%iflag_self = iflag_self_rj
!
      end subroutine copy_comm_rj_item_to_type
!
! -----------------------------------------------------------------------
!
      end module copy_sph_comm_table_4_type
