!
!      module const_comm_table_sph
!
!     Written by H. Matsui on July, 2007
!
!      subroutine const_comm_table_4_rlm(ip_rank, nnod_rlm)
!      subroutine const_comm_table_4_rtm(ip_rank, nnod_rtm)
!
!      subroutine const_comm_table_4_rj(ip_rank, nnod_rj)
!      subroutine const_comm_table_4_rtp(ip_rank, nnod_rtp)
!
      module const_comm_table_sph
!
      use m_precision
!
      use m_sph_trans_comm_table
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rlm(ip_rank, nnod_rlm)
!
      use set_comm_table_rtm_rlm
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: nnod_rlm
!
!
      call allocate_ncomm
!
      call count_comm_table_4_rlm(ip_rank)
!
      call count_num_domain_rtm_rlm(nneib_domain_rlm)
!
      call allocate_sph_comm_stack_rlm
!
      call set_comm_stack_rtm_rlm(ip_rank, nneib_domain_rlm,            &
     &    id_domain_rlm, istack_sr_rlm, ntot_item_sr_rlm)
!
      call deallocate_ncomm
      call allocate_sph_comm_item_rlm(nnod_rlm)
!
      call set_comm_table_4_rlm(ip_rank)
!
!      call allocate_idx_gl_rlm_out
!      call set_global_id_4_comm_rlm
!
      end subroutine const_comm_table_4_rlm
!
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rtm(ip_rank, nnod_rtm)
!
      use set_comm_table_rtm_rlm
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: nnod_rtm
!
!      write(*,*) 'allocate_ncomm'
      call allocate_ncomm
!
!      write(*,*) 'count_comm_table_4_rtm'
      call count_comm_table_4_rtm(ip_rank)
!
!      write(*,*) 'count_num_domain_rtm_rlm'
      call count_num_domain_rtm_rlm(nneib_domain_rtm)
!
!      write(*,*) 'allocate_sph_comm_stack_rtm'
      call allocate_sph_comm_stack_rtm
!
!      write(*,*) 'set_comm_stack_rtm_rlm'
      call set_comm_stack_rtm_rlm(ip_rank, nneib_domain_rtm,            &
     &    id_domain_rtm, istack_sr_rtm, ntot_item_sr_rtm)
!
      call deallocate_ncomm
      call allocate_sph_comm_item_rtm(nnod_rtm)
!
!      write(*,*) 'set_comm_table_4_rtm'
      call set_comm_table_4_rtm(ip_rank)
!
!      call allocate_idx_gl_rtm_out
!      write(*,*) 'set_global_id_4_comm_rtm'
!      call set_global_id_4_comm_rtm
!
      end subroutine const_comm_table_4_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rj(ip_rank, nnod_rj)
!
      use set_comm_table_rtp_rj
!
      integer(kind = kint), intent(in) :: ip_rank, nnod_rj
      integer(kind = kint) :: icou
!
!
      call allocate_domain_sr_tmp
!
      nneib_domain_rj = 0
      call count_comm_table_4_rj(ip_rank)
!
      call allocate_sph_comm_stack_rj
!
      call set_comm_stack_rtp_rj(nneib_domain_rj, id_domain_rj,         &
     &    istack_sr_rj, ntot_item_sr_rj)
!
      call deallocate_domain_sr_tmp
      call allocate_sph_comm_item_rj(nnod_rj)
!
      icou = 0
      call set_comm_table_4_rj(ip_rank, icou)
!
      end subroutine const_comm_table_4_rj
!
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rtp(ip_rank, nnod_rtp)
!
      use set_comm_table_rtp_rj
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: nnod_rtp
!
      integer(kind = kint) :: icou
!
!
      call allocate_domain_sr_tmp
!
      nneib_domain_rtp = 0
      call count_comm_table_4_rtp(ip_rank)
!
      call allocate_sph_comm_stack_rtp
!
      call set_comm_stack_rtp_rj(nneib_domain_rtp, id_domain_rtp,       &
     &    istack_sr_rtp, ntot_item_sr_rtp)
!      write(*,*) 'nneib_domain_rtp', nneib_domain_rtp
!      write(*,*) 'id_domain_rtp', id_domain_rtp
!      write(*,*) 'ntot_item_sr_rtp', ntot_item_sr_rtp
!      write(*,*) 'istack_sr_rtp', istack_sr_rtp
!
      call deallocate_domain_sr_tmp
      call allocate_sph_comm_item_rtp(nnod_rtp)
!
      icou = 0
      call set_comm_table_4_rtp(ip_rank, icou)
!
      end subroutine const_comm_table_4_rtp
!
! -----------------------------------------------------------------------
!
      end module const_comm_table_sph
