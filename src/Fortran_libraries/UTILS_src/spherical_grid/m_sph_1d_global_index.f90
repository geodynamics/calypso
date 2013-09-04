!
!      module m_sph_1d_global_index
!
!     Written by H. Matsui on July, 2007
!
!      subroutine allocate_sph_1d_global_stack
!      subroutine allocate_sph_1d_global_idx
!
!      subroutine deallocate_sph_1d_global_stack
!      subroutine deallocate_sph_1d_global_idx
!
!      subroutine check_spheric_global_stack(ip_rank)
!
!
      module m_sph_1d_global_index
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint), allocatable :: istack_idx_local_rtp_r(:)
      integer(kind = kint), allocatable :: istack_idx_local_rtp_t(:)
      integer(kind = kint), allocatable :: istack_idx_local_rtp_p(:)
      integer(kind = kint), allocatable :: istack_idx_local_rtm_r(:)
      integer(kind = kint), allocatable :: istack_idx_local_rtm_t(:)
      integer(kind = kint), allocatable :: istack_idx_local_rtm_m(:)
      integer(kind = kint), allocatable :: istack_idx_local_rlm_r(:)
      integer(kind = kint), allocatable :: istack_idx_local_rlm_j(:)
      integer(kind = kint), allocatable :: istack_idx_local_rj_r(:)
      integer(kind = kint), allocatable :: istack_idx_local_rj_j(:)
!
!
      integer(kind = kint), allocatable :: idx_global_rtp_r(:)
      integer(kind = kint), allocatable :: idx_global_rtp_t(:)
      integer(kind = kint), allocatable :: idx_global_rtp_p(:,:)
      integer(kind = kint), allocatable :: idx_global_rtm_r(:)
      integer(kind = kint), allocatable :: idx_global_rtm_t(:)
      integer(kind = kint), allocatable :: idx_global_rtm_m(:,:)
      integer(kind = kint), allocatable :: idx_global_rlm_r(:)
      integer(kind = kint), allocatable :: idx_global_rlm_j(:,:)
      integer(kind = kint), allocatable :: idx_global_rj_r(:)
      integer(kind = kint), allocatable :: idx_global_rj_j(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_global_stack
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
!
      num = ndomain_rtp(1)
      allocate(istack_idx_local_rtp_r(0:num))
      num = ndomain_rtp(2)
      allocate(istack_idx_local_rtp_t(0:num))
      num = ndomain_rtp(3)
      allocate(istack_idx_local_rtp_p(0:num))
!
      num = ndomain_rtm(1)
      allocate(istack_idx_local_rtm_r(0:num))
      num = ndomain_rtm(2)
      allocate(istack_idx_local_rtm_t(0:num))
      num = ndomain_rtm(3)
      allocate(istack_idx_local_rtm_m(0:num))
!
      num = ndomain_rlm(1)
      allocate(istack_idx_local_rlm_r(0:num))
      num = ndomain_rlm(2)
      allocate(istack_idx_local_rlm_j(0:num))
!
      num = ndomain_rj(1)
      allocate(istack_idx_local_rj_r(0:num))
      num = ndomain_rj(2)
      allocate(istack_idx_local_rj_j(0:num))
!
      istack_idx_local_rtp_r = 0
      istack_idx_local_rtp_t = 0
      istack_idx_local_rtp_p = 0
      istack_idx_local_rtm_r = 0
      istack_idx_local_rtm_t = 0
      istack_idx_local_rtm_m = 0
      istack_idx_local_rlm_r = 0
      istack_idx_local_rlm_j = 0
      istack_idx_local_rj_r =  0
      istack_idx_local_rj_j =  0
!
      end subroutine allocate_sph_1d_global_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_global_idx
!
      use m_spheric_parameter
!
      integer(kind = kint) :: n1, n2, n3
      integer(kind = kint) :: ist_1, ist_2, ist_3
      integer(kind = kint) :: ied_1, ied_2, ied_3
!
!
      n1 = ndomain_rtp(1)
      n2 = ndomain_rtp(2)
      n3 = ndomain_rtp(3)
      ist_1 = istack_idx_local_rtp_r(0) + 1
      ied_1 = istack_idx_local_rtp_r(n1)
      ist_2 = istack_idx_local_rtp_t(0) + 1
      ied_2 = istack_idx_local_rtp_t(n2)
      ist_3 = istack_idx_local_rtp_p(0) + 1
      ied_3 = istack_idx_local_rtp_p(n3)
!
      allocate( idx_global_rtp_r(ist_1:ied_1) )
      allocate( idx_global_rtp_t(ist_2:ied_2) )
      allocate( idx_global_rtp_p(ist_3:ied_3,2) )
!
      idx_global_rtp_r = 0
      idx_global_rtp_t = 0
      idx_global_rtp_p = 0
!
!
      n1 = ndomain_rtm(1)
      n2 = ndomain_rtm(2)
      n3 = ndomain_rtm(3)
      ist_1 = istack_idx_local_rtm_r(0) + 1
      ied_1 = istack_idx_local_rtm_r(n1)
      ist_2 = istack_idx_local_rtm_t(0) + 1
      ied_2 = istack_idx_local_rtm_t(n2)
      ist_3 = istack_idx_local_rtm_m(0) + 1
      ied_3 = istack_idx_local_rtm_m(n3)
!
      allocate( idx_global_rtm_r(ist_1:ied_1) )
      allocate( idx_global_rtm_t(ist_2:ied_2) )
      allocate( idx_global_rtm_m(ist_3:ied_3,2) )
!
      idx_global_rtm_r = 0
      idx_global_rtm_t = 0
      idx_global_rtm_m = 0
!
!
      n1 = ndomain_rlm(1)
      n2 = ndomain_rlm(2)
      ist_1 = istack_idx_local_rlm_r(0) + 1
      ied_1 = istack_idx_local_rlm_r(n1)
      ist_2 = istack_idx_local_rlm_j(0) + 1
      ied_2 = istack_idx_local_rlm_j(n2)
!
      allocate( idx_global_rlm_r(ist_1:ied_1) )
      allocate( idx_global_rlm_j(ist_2:ied_2,3) )
!
      idx_global_rlm_r = 0
      idx_global_rlm_j = 0
!
!
      n1 = ndomain_rj(1)
      n2 = ndomain_rj(2)
      ist_1 = istack_idx_local_rj_r(0) + 1
      ied_1 = istack_idx_local_rj_r(n1)
      ist_2 = istack_idx_local_rj_j(0) + 1
      ied_2 = istack_idx_local_rj_j(n2)
!
      allocate( idx_global_rj_r(ist_1:ied_1) )
      allocate( idx_global_rj_j(ist_2:ied_2,3) )
!
      idx_global_rj_r = 0
      idx_global_rj_j = 0
!
      end subroutine allocate_sph_1d_global_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_global_stack
!
!
      deallocate(istack_idx_local_rtp_r)
      deallocate(istack_idx_local_rtp_t)
      deallocate(istack_idx_local_rtp_p)
!
      deallocate(istack_idx_local_rtm_r)
      deallocate(istack_idx_local_rtm_t)
      deallocate(istack_idx_local_rtm_m)
!
      deallocate(istack_idx_local_rlm_r)
      deallocate(istack_idx_local_rlm_j)
!
      deallocate(istack_idx_local_rj_r)
      deallocate(istack_idx_local_rj_j)
!
      end subroutine deallocate_sph_1d_global_stack
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_global_idx
!
!
      deallocate( idx_global_rtp_r, idx_global_rtp_t, idx_global_rtp_p)
      deallocate( idx_global_rtm_r, idx_global_rtm_t, idx_global_rtm_m)
      deallocate( idx_global_rlm_r, idx_global_rlm_j)
      deallocate( idx_global_rj_r, idx_global_rj_j)
!
      end subroutine deallocate_sph_1d_global_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_spheric_global_stack(my_rank)
!
      use m_spheric_parameter
!
!
      integer(kind = kint), intent(in) :: my_rank
!
      write(my_rank+50,*) 'istack_idx_local_rtp_r',                     &
     &     istack_idx_local_rtp_r
      write(my_rank+50,*) 'istack_idx_local_rtp_t',                     &
     &     istack_idx_local_rtp_t
      write(my_rank+50,*) 'istack_idx_local_rtp_p',                     &
     &     istack_idx_local_rtp_p
!
      write(my_rank+50,*) 'istack_idx_local_rtm_r',                     &
     &     istack_idx_local_rtm_r
      write(my_rank+50,*) 'istack_idx_local_rtm_t',                     &
     &     istack_idx_local_rtm_t
      write(my_rank+50,*) 'istack_idx_local_rtm_m',                     &
     &     istack_idx_local_rtm_m
!
      write(my_rank+50,*) 'istack_idx_local_rlm_r',                     &
     &     istack_idx_local_rlm_r
      write(my_rank+50,*) 'istack_idx_local_rlm_j',                     &
     &     istack_idx_local_rlm_j
!
      write(my_rank+50,*) 'istack_idx_local_rj_r',                      &
     &     istack_idx_local_rj_r
      write(my_rank+50,*) 'istack_idx_local_rj_j',                      &
     &     istack_idx_local_rj_j
!
      end subroutine check_spheric_global_stack
!
! -----------------------------------------------------------------------
!
      end module m_sph_1d_global_index
