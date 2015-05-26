!
!      module set_local_index_table_sph
!
      module set_local_index_table_sph
!
!     Written by H. Matsui on July, 2007
!
!      subroutine set_local_idx_table_rtp
!      subroutine set_local_idx_table_rtm
!      subroutine set_local_idx_table_rlm
!      subroutine set_local_idx_table_rj
!
!      subroutine deallocate_rtp_1d_local_idx
!      subroutine deallocate_rj_1d_local_idx
!      subroutine deallocate_rtm_1d_local_idx
!      subroutine deallocate_rlm_1d_local_idx
!
      use m_precision
!
      use m_spheric_parameter
!
      implicit none
!
      integer(kind = kint), allocatable :: idx_local_rtp_r(:)
      integer(kind = kint), allocatable :: idx_local_rtp_t(:)
      integer(kind = kint), allocatable :: idx_local_rtp_p(:)
!
      integer(kind = kint), allocatable :: idx_local_rj_r(:)
      integer(kind = kint), allocatable :: idx_local_rj_j(:)
!
      integer(kind = kint), allocatable :: idx_local_rtm_r(:)
      integer(kind = kint), allocatable :: idx_local_rtm_t(:)
      integer(kind = kint), allocatable :: idx_local_rtm_m(:)
!
      integer(kind = kint), allocatable :: idx_local_rlm_r(:)
      integer(kind = kint), allocatable :: idx_local_rlm_j(:)
!
      private :: allocate_rtm_1d_local_idx, allocate_rlm_1d_local_idx
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_rtp_1d_local_idx
!
      integer(kind = kint) :: n1, n2, n3
!
      n1 = nidx_global_rtp(1)
      n2 = nidx_global_rtp(2)
      n3 = nidx_global_rtp(3)
      allocate( idx_local_rtp_r(n1) )
      allocate( idx_local_rtp_t(n2) )
      allocate( idx_local_rtp_p(n3) )
!
      idx_local_rtp_r = 0
      idx_local_rtp_t = 0
      idx_local_rtp_p = 0
!
      end subroutine allocate_rtp_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine allocate_rj_1d_local_idx
!
      integer(kind = kint) :: n1, n2
!
      n1 = nidx_global_rj(1)
      n2 = nidx_global_rj(2)
      allocate( idx_local_rj_r(n1) )
      allocate( idx_local_rj_j(0:n2) )
!
      idx_local_rj_r = 0
      idx_local_rj_j = 0
!
      end subroutine allocate_rj_1d_local_idx
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtm_1d_local_idx
!
      integer(kind = kint) :: n1, n2, n3
!
      n1 = nidx_global_rtm(1)
      n2 = nidx_global_rtm(2)
      n3 = nidx_global_rtm(3)
      allocate( idx_local_rtm_r(n1) )
      allocate( idx_local_rtm_t(n2) )
      allocate( idx_local_rtm_m(n3) )
!
      idx_local_rtm_r = 0
      idx_local_rtm_t = 0
      idx_local_rtm_m = 0
!
      end subroutine allocate_rtm_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine allocate_rlm_1d_local_idx
!
      integer(kind = kint) :: n1, n2
!
      n1 = nidx_global_rlm(1)
      n2 = nidx_global_rlm(2)
      allocate( idx_local_rlm_r(n1) )
      allocate( idx_local_rlm_j(0:n2) )
!
      idx_local_rlm_r = 0
      idx_local_rlm_j = 0
!
      end subroutine allocate_rlm_1d_local_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_local_idx_table_rtp
!
      integer(kind = kint) :: k, l, m, kk, ll, mm
!
!
      idx_local_rtp_r = 0
      idx_local_rtp_t = 0
      idx_local_rtp_p = 0
!
      do k = 1, nidx_rtp(1)
        kk = idx_gl_1d_rtp_r(k)
        idx_local_rtp_r(kk) = k
      end do
!
      do l = 1, nidx_rtp(2)
        ll = idx_gl_1d_rtp_t(l)
        idx_local_rtp_t(ll) = l
      end do
!
      do m = 1, nidx_rtp(3)
        mm = idx_gl_1d_rtp_p(m,1)
        idx_local_rtp_p(mm) = m
      end do
!
      end subroutine set_local_idx_table_rtp
!
! ----------------------------------------------------------------------
!
      subroutine set_local_idx_table_rtm
!
      integer(kind = kint) :: k, l, m, kk, ll, mm
!
!
      call allocate_rtm_1d_local_idx
!
      do k = 1, nidx_rtm(1)
        kk = idx_gl_1d_rtm_r(k)
        idx_local_rtm_r(kk) = k
      end do
!
      do l = 1, nidx_rtm(2)
        ll = idx_gl_1d_rtm_t(l)
        idx_local_rtm_t(ll) = l
      end do
!
      do m = 1, nidx_rtm(3)
        mm = idx_gl_1d_rtm_m(m,1)
        idx_local_rtm_m(mm) = m
      end do
!
      end subroutine set_local_idx_table_rtm
!
! ----------------------------------------------------------------------
!
      subroutine set_local_idx_table_rlm
!
      integer(kind = kint) :: k, j, kk, jj
!
!
      call allocate_rlm_1d_local_idx
!
      do k = 1, nidx_rlm(1)
        kk = idx_gl_1d_rlm_r(k)
        idx_local_rlm_r(kk) = k
      end do
!
      do j = 1, nidx_rtm(2)
        jj = idx_gl_1d_rlm_j(j,1)
        idx_local_rlm_j(jj) = j
      end do
!
      end subroutine set_local_idx_table_rlm
!
! ----------------------------------------------------------------------
!
      subroutine set_local_idx_table_rj
!
      integer(kind = kint) :: k, j, kk, jj
!
!
      idx_local_rj_r = 0
      idx_local_rj_j = 0
!
      do k = 1, nidx_rj(1)
        kk = idx_gl_1d_rj_r(k)
        idx_local_rj_r(kk) = k
      end do
!
      do j = 1, nidx_rj(2)
        jj = idx_gl_1d_rj_j(j,1)
        idx_local_rj_j(jj) = j
      end do
!
      end subroutine set_local_idx_table_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_rtp_1d_local_idx
!
      deallocate( idx_local_rtp_r, idx_local_rtp_t, idx_local_rtp_p )
!
      end subroutine deallocate_rtp_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine deallocate_rj_1d_local_idx
!
      deallocate( idx_local_rj_r, idx_local_rj_j )
!
      end subroutine deallocate_rj_1d_local_idx
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtm_1d_local_idx
!
      deallocate( idx_local_rtm_r, idx_local_rtm_t, idx_local_rtm_m )
!
      end subroutine deallocate_rtm_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine deallocate_rlm_1d_local_idx
!
      deallocate( idx_local_rlm_r, idx_local_rlm_j )
!
      end subroutine deallocate_rlm_1d_local_idx
!
! -----------------------------------------------------------------------!
      end module set_local_index_table_sph
