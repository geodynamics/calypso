!
!      module t_sph_local_index
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine alloc_rtp_1d_local_idx(sph_rtp, sph_lcx)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!      subroutine alloc_rj_1d_local_idx(sph_rj, sph_lcx)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!      subroutine set_local_idx_table_rtp(sph_rtp, sph_lcx)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!      subroutine set_local_idx_table_rtm(sph_rtm, sph_lcx)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!      subroutine set_local_idx_table_rlm(sph_rlm, sph_lcx)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!      subroutine set_local_idx_table_rj(sph_rj, sph_lcx)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!      subroutine dealloc_rtp_1d_local_idx(sph_lcx)
!!      subroutine dealloc_rj_1d_local_idx(sph_lcx)
!!      subroutine dealloc_rtm_1d_local_idx(sph_lcx)
!!      subroutine dealloc_rlm_1d_local_idx(sph_lcx)
!
      module t_sph_local_index
!
      use m_precision
      use t_spheric_parameter
!
      implicit none
!
!
      type sph_local_1d_index
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
      end type sph_local_1d_index
!
      private :: alloc_rtm_1d_local_idx, alloc_rlm_1d_local_idx
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_rtp_1d_local_idx(sph_rtp, sph_lcx)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint) :: n1, n2, n3
!
      n1 = sph_rtp%nidx_global_rtp(1)
      n2 = sph_rtp%nidx_global_rtp(2)
      n3 = sph_rtp%nidx_global_rtp(3)
      allocate( sph_lcx%idx_local_rtp_r(n1) )
      allocate( sph_lcx%idx_local_rtp_t(n2) )
      allocate( sph_lcx%idx_local_rtp_p(n3) )
!
      sph_lcx%idx_local_rtp_r = 0
      sph_lcx%idx_local_rtp_t = 0
      sph_lcx%idx_local_rtp_p = 0
!
      end subroutine alloc_rtp_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine alloc_rj_1d_local_idx(sph_rj, sph_lcx)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint) :: n1, n2
!
      n1 = sph_rj%nidx_global_rj(1)
      n2 = sph_rj%nidx_global_rj(2)
      allocate( sph_lcx%idx_local_rj_r(n1) )
      allocate( sph_lcx%idx_local_rj_j(0:n2) )
!
      sph_lcx%idx_local_rj_r = 0
      sph_lcx%idx_local_rj_j = 0
!
      end subroutine alloc_rj_1d_local_idx
!
! -----------------------------------------------------------------------
!
      subroutine alloc_rtm_1d_local_idx(sph_rtm, sph_lcx)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint) :: n1, n2, n3
!
      n1 = sph_rtm%nidx_global_rtm(1)
      n2 = sph_rtm%nidx_global_rtm(2)
      n3 = sph_rtm%nidx_global_rtm(3)
      allocate( sph_lcx%idx_local_rtm_r(n1) )
      allocate( sph_lcx%idx_local_rtm_t(n2) )
      allocate( sph_lcx%idx_local_rtm_m(n3) )
!
      sph_lcx%idx_local_rtm_r = 0
      sph_lcx%idx_local_rtm_t = 0
      sph_lcx%idx_local_rtm_m = 0
!
      end subroutine alloc_rtm_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine alloc_rlm_1d_local_idx(sph_rlm, sph_lcx)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint) :: n1, n2
!
!
      n1 = sph_rlm%nidx_global_rlm(1)
      n2 = sph_rlm%nidx_global_rlm(2)
      allocate( sph_lcx%idx_local_rlm_r(n1) )
      allocate( sph_lcx%idx_local_rlm_j(0:n2) )
!
      sph_lcx%idx_local_rlm_r = 0
      sph_lcx%idx_local_rlm_j = 0
!
      end subroutine alloc_rlm_1d_local_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_local_idx_table_rtp(sph_rtp, sph_lcx)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint) :: k, l, m, kk, ll, mm
!
!
      sph_lcx%idx_local_rtp_r = 0
      sph_lcx%idx_local_rtp_t = 0
      sph_lcx%idx_local_rtp_p = 0
!
      do k = 1, sph_rtp%nidx_rtp(1)
        kk = sph_rtp%idx_gl_1d_rtp_r(k)
        sph_lcx%idx_local_rtp_r(kk) = k
      end do
!
      do l = 1, sph_rtp%nidx_rtp(2)
        ll = sph_rtp%idx_gl_1d_rtp_t(l)
        sph_lcx%idx_local_rtp_t(ll) = l
      end do
!
      do m = 1, sph_rtp%nidx_rtp(3)
        mm = sph_rtp%idx_gl_1d_rtp_p(m,1)
        sph_lcx%idx_local_rtp_p(mm) = m
      end do
!
      end subroutine set_local_idx_table_rtp
!
! ----------------------------------------------------------------------
!
      subroutine set_local_idx_table_rtm(sph_rtm, sph_lcx)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint) :: k, l, m, kk, ll, mm
!
!
      call alloc_rtm_1d_local_idx(sph_rtm, sph_lcx)
!
      do k = 1, sph_rtm%nidx_rtm(1)
        kk = sph_rtm%idx_gl_1d_rtm_r(k)
        sph_lcx%idx_local_rtm_r(kk) = k
      end do
!
      do l = 1, sph_rtm%nidx_rtm(2)
        ll = sph_rtm%idx_gl_1d_rtm_t(l)
        sph_lcx%idx_local_rtm_t(ll) = l
      end do
!
      do m = 1, sph_rtm%nidx_rtm(3)
        mm = sph_rtm%idx_gl_1d_rtm_m(m,1)
        sph_lcx%idx_local_rtm_m(mm) = m
      end do
!
      end subroutine set_local_idx_table_rtm
!
! ----------------------------------------------------------------------
!
      subroutine set_local_idx_table_rlm(sph_rlm, sph_lcx)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint) :: k, j, kk, jj
!
!
      call alloc_rlm_1d_local_idx(sph_rlm, sph_lcx)
!
      do k = 1, sph_rlm%nidx_rlm(1)
        kk = sph_rlm%idx_gl_1d_rlm_r(k)
        sph_lcx%idx_local_rlm_r(kk) = k
      end do
!
      do j = 1, sph_rlm%nidx_rlm(2)
        jj = sph_rlm%idx_gl_1d_rlm_j(j,1)
        sph_lcx%idx_local_rlm_j(jj) = j
      end do
!
      end subroutine set_local_idx_table_rlm
!
! ----------------------------------------------------------------------
!
      subroutine set_local_idx_table_rj(sph_rj, sph_lcx)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      integer(kind = kint) :: k, j, kk, jj
!
!
      sph_lcx%idx_local_rj_r = 0
      sph_lcx%idx_local_rj_j = 0
!
      do k = 1, sph_rj%nidx_rj(1)
        kk = sph_rj%idx_gl_1d_rj_r(k)
        sph_lcx%idx_local_rj_r(kk) = k
      end do
!
      do j = 1, sph_rj%nidx_rj(2)
        jj = sph_rj%idx_gl_1d_rj_j(j,1)
        sph_lcx%idx_local_rj_j(jj) = j
      end do
!
      end subroutine set_local_idx_table_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_rtp_1d_local_idx(sph_lcx)
!
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      deallocate(sph_lcx%idx_local_rtp_r, sph_lcx%idx_local_rtp_t)
      deallocate(sph_lcx%idx_local_rtp_p)
!
      end subroutine dealloc_rtp_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine dealloc_rj_1d_local_idx(sph_lcx)
!
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      deallocate(sph_lcx%idx_local_rj_r, sph_lcx%idx_local_rj_j)
!
      end subroutine dealloc_rj_1d_local_idx
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_rtm_1d_local_idx(sph_lcx)
!
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      deallocate(sph_lcx%idx_local_rtm_r, sph_lcx%idx_local_rtm_t)
      deallocate(sph_lcx%idx_local_rtm_m)
!
      end subroutine dealloc_rtm_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine dealloc_rlm_1d_local_idx(sph_lcx)
!
      type(sph_local_1d_index), intent(inout) :: sph_lcx
!
      deallocate(sph_lcx%idx_local_rlm_r, sph_lcx%idx_local_rlm_j)
!
      end subroutine dealloc_rlm_1d_local_idx
!
! -----------------------------------------------------------------------!
      end module t_sph_local_index
