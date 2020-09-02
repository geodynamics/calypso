!
!      module t_sph_local_index
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine init_local_idx_table_rtp(sph_rtp, sph_lcx_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_local_1d_index_rtp), intent(inout) :: sph_lcx_rtp
!!      subroutine init_local_idx_table_rtm(sph_rtm, sph_lcx_rtm)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_local_1d_index_rtm), intent(inout) :: sph_lcx_rtm
!!      subroutine init_local_idx_table_rlm(sph_rlm, sph_lcx_rlm)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_local_1d_index_rlm), intent(inout) :: sph_lcx_rlm
!!      subroutine init_local_idx_table_rj(sph_rj, sph_lcx_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_local_1d_index_rj), intent(inout) :: sph_lcx_rj
!!
!!      subroutine dealloc_rtp_1d_local_idx(sph_lcx_rtp)
!!       type(sph_local_1d_index_rtp), intent(inout) :: sph_lcx_rtp
!!      subroutine dealloc_rj_1d_local_idx(sph_lcx_rj)
!!        type(sph_local_1d_index_rj), intent(inout) :: sph_lcx_rj
!!      subroutine dealloc_rtm_1d_local_idx(sph_lcx_rtm)
!!        type(sph_local_1d_index_rtm), intent(inout) :: sph_lcx_rtm
!!      subroutine dealloc_rlm_1d_local_idx(sph_lcx_rlm)
!!        type(sph_local_1d_index_rlm), intent(inout) :: sph_lcx_rlm
!
      module t_sph_local_index
!
      use m_precision
      use t_spheric_parameter
!
      implicit none
!
!
      type sph_local_1d_index_rj
        integer(kind = kint), allocatable :: idx_local_rj_r(:)
        integer(kind = kint), allocatable :: idx_local_rj_j(:)
      end type sph_local_1d_index_rj
!
      type sph_local_1d_index_rtp
        integer(kind = kint), allocatable :: idx_local_rtp_r(:)
        integer(kind = kint), allocatable :: idx_local_rtp_t(:)
        integer(kind = kint), allocatable :: idx_local_rtp_p(:)
      end type sph_local_1d_index_rtp
!
      type sph_local_1d_index_rtm
        integer(kind = kint), allocatable :: idx_local_rtm_r(:)
        integer(kind = kint), allocatable :: idx_local_rtm_t(:)
        integer(kind = kint), allocatable :: idx_local_rtm_m(:)
      end type sph_local_1d_index_rtm
!
      type sph_local_1d_index_rlm
        integer(kind = kint), allocatable :: idx_local_rlm_r(:)
        integer(kind = kint), allocatable :: idx_local_rlm_j(:)
      end type sph_local_1d_index_rlm
!
      private :: alloc_rj_1d_local_idx, alloc_rtp_1d_local_idx
      private :: alloc_rtm_1d_local_idx, alloc_rlm_1d_local_idx
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_rtp_1d_local_idx(sph_rtp, sph_lcx_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_local_1d_index_rtp), intent(inout) :: sph_lcx_rtp
!
      integer(kind = kint) :: n1, n2, n3
!
      n1 = sph_rtp%nidx_global_rtp(1)
      n2 = sph_rtp%nidx_global_rtp(2)
      n3 = sph_rtp%nidx_global_rtp(3)
      allocate( sph_lcx_rtp%idx_local_rtp_r(n1) )
      allocate( sph_lcx_rtp%idx_local_rtp_t(n2) )
      allocate( sph_lcx_rtp%idx_local_rtp_p(n3) )
!
      sph_lcx_rtp%idx_local_rtp_r = 0
      sph_lcx_rtp%idx_local_rtp_t = 0
      sph_lcx_rtp%idx_local_rtp_p = 0
!
      end subroutine alloc_rtp_1d_local_idx
!
! -----------------------------------------------------------------------
!
      subroutine alloc_rj_1d_local_idx(sph_rj, sph_lcx_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_local_1d_index_rj), intent(inout) :: sph_lcx_rj
!
      integer(kind = kint) :: n1, n2
!
      n1 = sph_rj%nidx_global_rj(1)
      n2 = sph_rj%nidx_global_rj(2)
      allocate( sph_lcx_rj%idx_local_rj_r(n1) )
      allocate( sph_lcx_rj%idx_local_rj_j(0:n2) )
!
      sph_lcx_rj%idx_local_rj_r = 0
      sph_lcx_rj%idx_local_rj_j = 0
!
      end subroutine alloc_rj_1d_local_idx
!
! -----------------------------------------------------------------------
!
      subroutine alloc_rtm_1d_local_idx(sph_rtm, sph_lcx_rtm)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_local_1d_index_rtm), intent(inout) :: sph_lcx_rtm
!
      integer(kind = kint) :: n1, n2, n3
!
      n1 = sph_rtm%nidx_global_rtm(1)
      n2 = sph_rtm%nidx_global_rtm(2)
      n3 = sph_rtm%nidx_global_rtm(3)
      allocate( sph_lcx_rtm%idx_local_rtm_r(n1) )
      allocate( sph_lcx_rtm%idx_local_rtm_t(n2) )
      allocate( sph_lcx_rtm%idx_local_rtm_m(n3) )
!
      sph_lcx_rtm%idx_local_rtm_r = 0
      sph_lcx_rtm%idx_local_rtm_t = 0
      sph_lcx_rtm%idx_local_rtm_m = 0
!
      end subroutine alloc_rtm_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine alloc_rlm_1d_local_idx(sph_rlm, sph_lcx_rlm)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_local_1d_index_rlm), intent(inout) :: sph_lcx_rlm
!
      integer(kind = kint) :: n1, n2
!
!
      n1 = sph_rlm%nidx_global_rlm(1)
      n2 = sph_rlm%nidx_global_rlm(2)
      allocate( sph_lcx_rlm%idx_local_rlm_r(n1) )
      allocate( sph_lcx_rlm%idx_local_rlm_j(0:n2) )
!
      sph_lcx_rlm%idx_local_rlm_r = 0
      sph_lcx_rlm%idx_local_rlm_j = 0
!
      end subroutine alloc_rlm_1d_local_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_local_idx_table_rtp(sph_rtp, sph_lcx_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_local_1d_index_rtp), intent(inout) :: sph_lcx_rtp
!
      integer(kind = kint) :: k, l, m, kk, ll, mm
!
!
      call alloc_rtp_1d_local_idx(sph_rtp, sph_lcx_rtp)
!
      do k = 1, sph_rtp%nidx_rtp(1)
        kk = sph_rtp%idx_gl_1d_rtp_r(k)
        sph_lcx_rtp%idx_local_rtp_r(kk) = k
      end do
!
      do l = 1, sph_rtp%nidx_rtp(2)
        ll = sph_rtp%idx_gl_1d_rtp_t(l)
        sph_lcx_rtp%idx_local_rtp_t(ll) = l
      end do
!
      do m = 1, sph_rtp%nidx_rtp(3)
        mm = sph_rtp%idx_gl_1d_rtp_p(m,1)
        sph_lcx_rtp%idx_local_rtp_p(mm) = m
      end do
!
      end subroutine init_local_idx_table_rtp
!
! ----------------------------------------------------------------------
!
      subroutine init_local_idx_table_rtm(sph_rtm, sph_lcx_rtm)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_local_1d_index_rtm), intent(inout) :: sph_lcx_rtm
!
      integer(kind = kint) :: k, l, m, kk, ll, mm
!
!
      call alloc_rtm_1d_local_idx(sph_rtm, sph_lcx_rtm)
!
      do k = 1, sph_rtm%nidx_rtm(1)
        kk = sph_rtm%idx_gl_1d_rtm_r(k)
        sph_lcx_rtm%idx_local_rtm_r(kk) = k
      end do
!
      do l = 1, sph_rtm%nidx_rtm(2)
        ll = sph_rtm%idx_gl_1d_rtm_t(l)
        sph_lcx_rtm%idx_local_rtm_t(ll) = l
      end do
!
      do m = 1, sph_rtm%nidx_rtm(3)
        mm = sph_rtm%idx_gl_1d_rtm_m(m,1)
        sph_lcx_rtm%idx_local_rtm_m(mm) = m
      end do
!
      end subroutine init_local_idx_table_rtm
!
! ----------------------------------------------------------------------
!
      subroutine init_local_idx_table_rlm(sph_rlm, sph_lcx_rlm)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_local_1d_index_rlm), intent(inout) :: sph_lcx_rlm
!
      integer(kind = kint) :: k, j, kk, jj
!
!
      call alloc_rlm_1d_local_idx(sph_rlm, sph_lcx_rlm)
!
      do k = 1, sph_rlm%nidx_rlm(1)
        kk = sph_rlm%idx_gl_1d_rlm_r(k)
        sph_lcx_rlm%idx_local_rlm_r(kk) = k
      end do
!
      do j = 1, sph_rlm%nidx_rlm(2)
        jj = sph_rlm%idx_gl_1d_rlm_j(j,1)
        sph_lcx_rlm%idx_local_rlm_j(jj) = j
      end do
!
      end subroutine init_local_idx_table_rlm
!
! ----------------------------------------------------------------------
!
      subroutine init_local_idx_table_rj(sph_rj, sph_lcx_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_local_1d_index_rj), intent(inout) :: sph_lcx_rj
!
      integer(kind = kint) :: k, j, kk, jj
!
!
      call alloc_rj_1d_local_idx(sph_rj, sph_lcx_rj)
!
      do k = 1, sph_rj%nidx_rj(1)
        kk = sph_rj%idx_gl_1d_rj_r(k)
        sph_lcx_rj%idx_local_rj_r(kk) = k
      end do
!
      do j = 1, sph_rj%nidx_rj(2)
        jj = sph_rj%idx_gl_1d_rj_j(j,1)
        sph_lcx_rj%idx_local_rj_j(jj) = j
      end do
!
      end subroutine init_local_idx_table_rj
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_rtp_1d_local_idx(sph_lcx_rtp)
!
      type(sph_local_1d_index_rtp), intent(inout) :: sph_lcx_rtp
!
      deallocate(sph_lcx_rtp%idx_local_rtp_r)
      deallocate(sph_lcx_rtp%idx_local_rtp_t)
      deallocate(sph_lcx_rtp%idx_local_rtp_p)
!
      end subroutine dealloc_rtp_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine dealloc_rj_1d_local_idx(sph_lcx_rj)
!
      type(sph_local_1d_index_rj), intent(inout) :: sph_lcx_rj
!
      deallocate(sph_lcx_rj%idx_local_rj_r)
      deallocate(sph_lcx_rj%idx_local_rj_j)
!
      end subroutine dealloc_rj_1d_local_idx
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_rtm_1d_local_idx(sph_lcx_rtm)
!
      type(sph_local_1d_index_rtm), intent(inout) :: sph_lcx_rtm
!
      deallocate(sph_lcx_rtm%idx_local_rtm_r)
      deallocate(sph_lcx_rtm%idx_local_rtm_t)
      deallocate(sph_lcx_rtm%idx_local_rtm_m)
!
      end subroutine dealloc_rtm_1d_local_idx
!
! -----------------------------------------------------------------------!
      subroutine dealloc_rlm_1d_local_idx(sph_lcx_rlm)
!
      type(sph_local_1d_index_rlm), intent(inout) :: sph_lcx_rlm
!
      deallocate(sph_lcx_rlm%idx_local_rlm_r)
      deallocate(sph_lcx_rlm%idx_local_rlm_j)
!
      end subroutine dealloc_rlm_1d_local_idx
!
! -----------------------------------------------------------------------!
      end module t_sph_local_index
