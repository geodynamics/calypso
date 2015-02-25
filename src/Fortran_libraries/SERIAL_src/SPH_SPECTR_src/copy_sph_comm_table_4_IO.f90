!>@file   copy_sph_comm_table_4_IO.f90
!!@brief  module copy_sph_comm_table_4_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007 
!
!>@brief  Copy communication table for spherical hermonica transform
!!        between IO data
!!
!!@verbatim
!!      subroutine copy_comm_rtp_from_IO(nnod_rtp)
!!      subroutine copy_comm_rtm_from_IO(nnod_rtm)
!!      subroutine copy_comm_rlm_from_IO(nnod_rlm)
!!      subroutine copy_comm_rj_from_IO(nnod_rj)
!!
!!      subroutine copy_comm_rtp_to_IO(my_rank)
!!      subroutine copy_comm_rtm_to_IO(my_rank)
!!      subroutine copy_comm_rlm_to_IO(my_rank)
!!      subroutine copy_comm_rj_to_IO(my_rank)
!!@endverbatim
!!
!!@n @param my_rank   running process ID
!!@n @param nnod_rtp
!!      number of data points for @f$ f(r,\theta,\phi) @f$
!!@n @param nnod_rtm
!!      number of data points for @f$ f(r,\theta,m) @f$
!!@n @param nnod_rlm 
!!      number of data points for @f$ f(r,l,m) @f$
!!@n @param nnod_rj  
!!      number of data points for @f$ f(r,j) @f$
!!
!
      module copy_sph_comm_table_4_IO
!
      use m_precision
!
      use m_constants
      use m_sph_trans_comm_table
      use m_comm_data_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtp_from_IO(nnod_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp
!
!
      nneib_domain_rtp = num_neib_domain_IO
      ntot_item_sr_rtp = ntot_import_IO
!
      call allocate_sph_comm_stack_rtp
      call allocate_sph_comm_item_rtp(nnod_rtp)
!
      id_domain_rtp(1:nneib_domain_rtp)                                 &
     &      = id_neib_domain_IO(1:nneib_domain_rtp)
      istack_sr_rtp(0:nneib_domain_rtp)                                 &
     &      = istack_import_IO(0:nneib_domain_rtp)
!
      item_sr_rtp(1:ntot_item_sr_rtp)                                   &
     &      = item_import_IO(1:ntot_item_sr_rtp)
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine copy_comm_rtp_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtm_from_IO(nnod_rtm)
!
      integer(kind = kint), intent(in) :: nnod_rtm
!
!
      nneib_domain_rtm = num_neib_domain_IO
      ntot_item_sr_rtm = ntot_import_IO
!
      call allocate_sph_comm_stack_rtm
      call allocate_sph_comm_item_rtm(nnod_rtm)
!
      id_domain_rtm(1:nneib_domain_rtm)                                 &
     &      = id_neib_domain_IO(1:nneib_domain_rtm)
      istack_sr_rtm(0:nneib_domain_rtm)                                 &
     &      = istack_import_IO(0:nneib_domain_rtm)
!
      item_sr_rtm(1:ntot_item_sr_rtm)                                   &
     &      = item_import_IO(1:ntot_item_sr_rtm)
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine copy_comm_rtm_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rlm_from_IO(nnod_rlm)
!
      integer(kind = kint), intent(in) :: nnod_rlm
!
!
      nneib_domain_rlm = num_neib_domain_IO
      ntot_item_sr_rlm = ntot_import_IO
!
      call allocate_sph_comm_stack_rlm
      call allocate_sph_comm_item_rlm(nnod_rlm)
!
      id_domain_rlm(1:nneib_domain_rlm)                                 &
     &      = id_neib_domain_IO(1:nneib_domain_rlm)
      istack_sr_rlm(0:nneib_domain_rlm)                                 &
     &      = istack_import_IO(0:nneib_domain_rlm)
!
      item_sr_rlm(1:ntot_item_sr_rlm)                                   &
     &      = item_import_IO(1:ntot_item_sr_rlm)
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine copy_comm_rlm_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rj_from_IO(nnod_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj
!
!
      nneib_domain_rj = num_neib_domain_IO
      ntot_item_sr_rj = ntot_import_IO
!
      call allocate_sph_comm_stack_rj
      call allocate_sph_comm_item_rj(nnod_rj)
!
      id_domain_rj(1:nneib_domain_rj)                                   &
     &      = id_neib_domain_IO(1:nneib_domain_rj)
      istack_sr_rj(0:nneib_domain_rj)                                   &
     &      = istack_import_IO(0:nneib_domain_rj)
!
      item_sr_rj(1:ntot_item_sr_rj)                                     &
     &      = item_import_IO(1:ntot_item_sr_rj)
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine copy_comm_rj_from_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtp_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      my_rank_IO = my_rank
      num_neib_domain_IO = nneib_domain_rtp
      ntot_import_IO =     ntot_item_sr_rtp
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_import_item_IO
!
      id_neib_domain_IO(1:nneib_domain_rtp)                             &
     &      = id_domain_rtp(1:nneib_domain_rtp)
      istack_import_IO(0:nneib_domain_rtp)                              &
     &      = istack_sr_rtp(0:nneib_domain_rtp)
!
      item_import_IO(1:ntot_item_sr_rtp)                                &
     &      = item_sr_rtp(1:ntot_item_sr_rtp)
!
      call deallocate_sph_comm_item_rtp
!
      end subroutine copy_comm_rtp_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtm_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      my_rank_IO = my_rank
      num_neib_domain_IO = nneib_domain_rtm
      ntot_import_IO =     ntot_item_sr_rtm
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_import_item_IO
!
      id_neib_domain_IO(1:nneib_domain_rtm)                             &
     &      = id_domain_rtm(1:nneib_domain_rtm)
      istack_import_IO(0:nneib_domain_rtm)                              &
     &      = istack_sr_rtm(0:nneib_domain_rtm)
!
      item_import_IO(1:ntot_item_sr_rtm)                                &
     &      = item_sr_rtm(1:ntot_item_sr_rtm)
!
      call deallocate_sph_comm_item_rtm
!
      end subroutine copy_comm_rtm_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rlm_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      my_rank_IO = my_rank
      num_neib_domain_IO = nneib_domain_rlm
      ntot_import_IO =     ntot_item_sr_rlm
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_import_item_IO
!
      id_neib_domain_IO(1:nneib_domain_rlm)                             &
     &      = id_domain_rlm(1:nneib_domain_rlm)
      istack_import_IO(0:nneib_domain_rlm)                              &
     &      = istack_sr_rlm(0:nneib_domain_rlm)
!
      item_import_IO(1:ntot_item_sr_rlm)                                &
     &      = item_sr_rlm(1:ntot_item_sr_rlm)
!
      call deallocate_sph_comm_item_rlm
!
      end subroutine copy_comm_rlm_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rj_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      my_rank_IO = my_rank
      num_neib_domain_IO = nneib_domain_rj
      ntot_import_IO =     ntot_item_sr_rj
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_import_item_IO
!
      id_neib_domain_IO(1:nneib_domain_rj)                              &
     &      = id_domain_rj(1:nneib_domain_rj)
      istack_import_IO(0:nneib_domain_rj)                               &
     &      = istack_sr_rj(0:nneib_domain_rj)
!
      item_import_IO(1:ntot_item_sr_rj)                                 &
     &      = item_sr_rj(1:ntot_item_sr_rj)
!
      call deallocate_sph_comm_item_rj
!
      end subroutine copy_comm_rj_to_IO
!
! -----------------------------------------------------------------------
!
      end module copy_sph_comm_table_4_IO
