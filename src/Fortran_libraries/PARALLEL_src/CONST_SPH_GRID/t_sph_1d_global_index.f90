!>@file   t_sph_1d_global_index.f90
!!@brief  module t_sph_1d_global_index
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2007
!
!>@brief  Global addresses for soherical harmonics indices
!!
!!@verbatim
!!      subroutine alloc_sph_1d_global_stack(s3d_ranks, stk_lc1d)
!!        type(sph_1d_index_stack), intent(inout) :: stk_lc1d
!!      subroutine alloc_sph_1d_global_idx                              &
!!     &         (s3d_ranks, stk_lc1d, sph_gl1d)
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_1d_global_index), intent(inout) :: sph_gl1d
!!
!!      subroutine dealloc_sph_1d_global_stack(stk_lc1d)
!!        type(sph_1d_index_stack), intent(inout) :: stk_lc1d
!!      subroutine dealloc_sph_1d_global_idx(sph_gl1d)
!!        type(sph_1d_global_index), intent(inout) :: sph_gl1d
!!
!!      subroutine check_spheric_global_stack(ip_rank, stk_lc1d)
!!@endverbatim
!
!
      module t_sph_1d_global_index
!
      use m_precision
      use t_control_1D_layering
      use t_spheric_global_ranks
!
      implicit none
!
!
      type sph_1d_index_stack
!>        Global radial address for f(r,t,p)
        integer(kind = kint), allocatable :: istack_idx_local_rtp_r(:)
!>        Global radial address for f(r,t,p)
        integer(kind = kint), allocatable :: istack_idx_local_rtp_t(:)
!>        Global zonal grid address for f(r,t,p)
        integer(kind = kint), allocatable :: istack_idx_local_rtp_p(:)
!>        Global radial address for f(r,t,m)
        integer(kind = kint), allocatable :: istack_idx_local_rtm_r(:)
!>        Global meridional grid address for f(r,t,m)
        integer(kind = kint), allocatable :: istack_idx_local_rtm_t(:)
!>        Global zonal mode address for f(r,t,m)
        integer(kind = kint), allocatable :: istack_idx_local_rtm_m(:)
!>        Global radial address for f(r,l,m)
        integer(kind = kint), allocatable :: istack_idx_local_rlm_r(:)
!>        Global spherical harmonics mode address for f(r,l,m)
        integer(kind = kint), allocatable :: istack_idx_local_rlm_j(:)
!>        Global radial address for f(r,j)
        integer(kind = kint), allocatable :: istack_idx_local_rj_r(:)
!>        Global spherical harmonics mode address for f(r,j)
        integer(kind = kint), allocatable :: istack_idx_local_rj_j(:)
      end type sph_1d_index_stack
!
!
      type sph_1d_global_index
!>        number of radial address for f(r,t,p)
        integer(kind = kint) :: num_gl_rtp_r
!>        number of radial address for f(r,t,p)
        integer(kind = kint) :: num_gl_rtp_t
!>        number of zonal grid address for f(r,t,p)
        integer(kind = kint) :: num_gl_rtp_p
!>        Global radial address for f(r,t,p)
        integer(kind = kint), allocatable :: idx_global_rtp_r(:)
!>        Global radial address for f(r,t,p)
        integer(kind = kint), allocatable :: idx_global_rtp_t(:)
!>        Global zonal grid address for f(r,t,p)
        integer(kind = kint), allocatable :: idx_global_rtp_p(:,:)
!
!>        number of radial address for f(r,t,m)
        integer(kind = kint) :: num_gl_rtm_r
!>        number of meridional grid address for f(r,t,m)
        integer(kind = kint) :: num_gl_rtm_t
!>        number of zonal mode address for f(r,t,m)
        integer(kind = kint) :: num_gl_rtm_m
!>        Global radial address for f(r,t,m)
        integer(kind = kint), allocatable :: idx_global_rtm_r(:)
!>        Global meridional grid address for f(r,t,m)
        integer(kind = kint), allocatable :: idx_global_rtm_t(:)
!>        Global zonal mode address for f(r,t,m)
        integer(kind = kint), allocatable :: idx_global_rtm_m(:,:)
!
!>        number of radial address for f(r,l,m)
        integer(kind = kint) :: num_gl_rlm_r
!>        number of spherical harmonics mode address for f(r,l,m)
        integer(kind = kint) :: num_gl_rlm_j
!>        Global radial address for f(r,l,m)
        integer(kind = kint), allocatable :: idx_global_rlm_r(:)
!>        Global spherical harmonics mode address for f(r,l,m)
        integer(kind = kint), allocatable :: idx_global_rlm_j(:,:)
!
!>        number of Global radial address for f(r,j)
        integer(kind = kint) :: nun_gl_rj_r
!>        number of spherical harmonics mode address for f(r,j)
        integer(kind = kint) :: num_gl_rj_j
!>        Global radial address for f(r,j)
        integer(kind = kint), allocatable :: idx_global_rj_r(:)
!>        Global spherical harmonics mode address for f(r,j)
        integer(kind = kint), allocatable :: idx_global_rj_j(:,:)
      end type sph_1d_global_index
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_1d_global_stack(s3d_ranks, stk_lc1d)
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
      integer(kind = kint) :: num
!
!
      num = s3d_ranks%ndomain_rtp(1)
      allocate(stk_lc1d%istack_idx_local_rtp_r(0:num))
      num = s3d_ranks%ndomain_rtp(2)
      allocate(stk_lc1d%istack_idx_local_rtp_t(0:num))
      num = s3d_ranks%ndomain_rtp(3)
      allocate(stk_lc1d%istack_idx_local_rtp_p(0:num))
!
      num = s3d_ranks%ndomain_rtm(1)
      allocate(stk_lc1d%istack_idx_local_rtm_r(0:num))
      num = s3d_ranks%ndomain_rtm(2)
      allocate(stk_lc1d%istack_idx_local_rtm_t(0:num))
      num = s3d_ranks%ndomain_rtm(3)
      allocate(stk_lc1d%istack_idx_local_rtm_m(0:num))
!
      num = s3d_ranks%ndomain_rlm(1)
      allocate(stk_lc1d%istack_idx_local_rlm_r(0:num))
      num = s3d_ranks%ndomain_rlm(2)
      allocate(stk_lc1d%istack_idx_local_rlm_j(0:num))
!
      num = s3d_ranks%ndomain_rj(1)
      allocate(stk_lc1d%istack_idx_local_rj_r(0:num))
      num = s3d_ranks%ndomain_rj(2)
      allocate(stk_lc1d%istack_idx_local_rj_j(0:num))
!
      stk_lc1d%istack_idx_local_rtp_r = 0
      stk_lc1d%istack_idx_local_rtp_t = 0
      stk_lc1d%istack_idx_local_rtp_p = 0
      stk_lc1d%istack_idx_local_rtm_r = 0
      stk_lc1d%istack_idx_local_rtm_t = 0
      stk_lc1d%istack_idx_local_rtm_m = 0
      stk_lc1d%istack_idx_local_rlm_r = 0
      stk_lc1d%istack_idx_local_rlm_j = 0
      stk_lc1d%istack_idx_local_rj_r =  0
      stk_lc1d%istack_idx_local_rj_j =  0
!
      end subroutine alloc_sph_1d_global_stack
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_1d_global_idx                                &
     &         (s3d_ranks, stk_lc1d, sph_gl1d)
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
      integer(kind = kint) :: n1, n2, n3
!
!
      n1 = s3d_ranks%ndomain_rtp(1)
      n2 = s3d_ranks%ndomain_rtp(2)
      n3 = s3d_ranks%ndomain_rtp(3)
      sph_gl1d%num_gl_rtp_r = stk_lc1d%istack_idx_local_rtp_r(n1)
      sph_gl1d%num_gl_rtp_t = stk_lc1d%istack_idx_local_rtp_t(n2)
      sph_gl1d%num_gl_rtp_p = stk_lc1d%istack_idx_local_rtp_p(n3)
!
      allocate( sph_gl1d%idx_global_rtp_r(1:sph_gl1d%num_gl_rtp_r) )
      allocate( sph_gl1d%idx_global_rtp_t(1:sph_gl1d%num_gl_rtp_t) )
      allocate( sph_gl1d%idx_global_rtp_p(1:sph_gl1d%num_gl_rtp_p,2) )
!
      sph_gl1d%idx_global_rtp_r = 0
      sph_gl1d%idx_global_rtp_t = 0
      sph_gl1d%idx_global_rtp_p = 0
!
!
      n1 = s3d_ranks%ndomain_rtm(1)
      n2 = s3d_ranks%ndomain_rtm(2)
      n3 = s3d_ranks%ndomain_rtm(3)
      sph_gl1d%num_gl_rtm_r = stk_lc1d%istack_idx_local_rtm_r(n1)
      sph_gl1d%num_gl_rtm_t = stk_lc1d%istack_idx_local_rtm_t(n2)
      sph_gl1d%num_gl_rtm_m = stk_lc1d%istack_idx_local_rtm_m(n3)
!
      allocate( sph_gl1d%idx_global_rtm_r(1:sph_gl1d%num_gl_rtm_r) )
      allocate( sph_gl1d%idx_global_rtm_t(1:sph_gl1d%num_gl_rtm_t) )
      allocate( sph_gl1d%idx_global_rtm_m(0:sph_gl1d%num_gl_rtm_m,2) )
!
      sph_gl1d%idx_global_rtm_r = 0
      sph_gl1d%idx_global_rtm_t = 0
      sph_gl1d%idx_global_rtm_m = 0
!
!
      n1 = s3d_ranks%ndomain_rlm(1)
      n2 = s3d_ranks%ndomain_rlm(2)
      sph_gl1d%num_gl_rlm_r = stk_lc1d%istack_idx_local_rlm_r(n1)
      sph_gl1d%num_gl_rlm_j = stk_lc1d%istack_idx_local_rlm_j(n2)
!
      allocate( sph_gl1d%idx_global_rlm_r(1:sph_gl1d%num_gl_rlm_r) )
      allocate( sph_gl1d%idx_global_rlm_j(0:sph_gl1d%num_gl_rlm_j,3) )
!
      sph_gl1d%idx_global_rlm_r = 0
      sph_gl1d%idx_global_rlm_j = 0
!
!
      n1 = s3d_ranks%ndomain_rj(1)
      n2 = s3d_ranks%ndomain_rj(2)
      sph_gl1d%nun_gl_rj_r = stk_lc1d%istack_idx_local_rj_r(n1)
      sph_gl1d%num_gl_rj_j = stk_lc1d%istack_idx_local_rj_j(n2)
!
      allocate( sph_gl1d%idx_global_rj_r(1:sph_gl1d%nun_gl_rj_r) )
      allocate( sph_gl1d%idx_global_rj_j(0:sph_gl1d%num_gl_rj_j,3) )
!
      sph_gl1d%idx_global_rj_r = 0
      sph_gl1d%idx_global_rj_j = 0
!
      end subroutine alloc_sph_1d_global_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_1d_global_stack(stk_lc1d)
!
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
!
!
      deallocate(stk_lc1d%istack_idx_local_rtp_r)
      deallocate(stk_lc1d%istack_idx_local_rtp_t)
      deallocate(stk_lc1d%istack_idx_local_rtp_p)
!
      deallocate(stk_lc1d%istack_idx_local_rtm_r)
      deallocate(stk_lc1d%istack_idx_local_rtm_t)
      deallocate(stk_lc1d%istack_idx_local_rtm_m)
!
      deallocate(stk_lc1d%istack_idx_local_rlm_r)
      deallocate(stk_lc1d%istack_idx_local_rlm_j)
!
      deallocate(stk_lc1d%istack_idx_local_rj_r)
      deallocate(stk_lc1d%istack_idx_local_rj_j)
!
      end subroutine dealloc_sph_1d_global_stack
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_1d_global_idx(sph_gl1d)
!
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
!
      deallocate(sph_gl1d%idx_global_rtp_r, sph_gl1d%idx_global_rtp_t)
      deallocate(sph_gl1d%idx_global_rtp_p)
      deallocate(sph_gl1d%idx_global_rtm_r, sph_gl1d%idx_global_rtm_t)
      deallocate(sph_gl1d%idx_global_rtm_m)
      deallocate(sph_gl1d%idx_global_rlm_r, sph_gl1d%idx_global_rlm_j)
      deallocate(sph_gl1d%idx_global_rj_r, sph_gl1d%idx_global_rj_j)
!
      end subroutine dealloc_sph_1d_global_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_spheric_global_stack(id_rank, stk_lc1d)
!
      integer, intent(in) :: id_rank
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
!
      write(id_rank+50,*) 'istack_idx_local_rtp_r',                     &
     &     stk_lc1d%istack_idx_local_rtp_r
      write(id_rank+50,*) 'istack_idx_local_rtp_t',                     &
     &     stk_lc1d%istack_idx_local_rtp_t
      write(id_rank+50,*) 'istack_idx_local_rtp_p',                     &
     &     stk_lc1d%istack_idx_local_rtp_p
!
      write(id_rank+50,*) 'istack_idx_local_rtm_r',                     &
     &     stk_lc1d%istack_idx_local_rtm_r
      write(id_rank+50,*) 'istack_idx_local_rtm_t',                     &
     &     stk_lc1d%istack_idx_local_rtm_t
      write(id_rank+50,*) 'istack_idx_local_rtm_m',                     &
     &     stk_lc1d%istack_idx_local_rtm_m
!
      write(id_rank+50,*) 'istack_idx_local_rlm_r',                     &
     &     stk_lc1d%istack_idx_local_rlm_r
      write(id_rank+50,*) 'istack_idx_local_rlm_j',                     &
     &     stk_lc1d%istack_idx_local_rlm_j
!
      write(id_rank+50,*) 'istack_idx_local_rj_r',                      &
     &     stk_lc1d%istack_idx_local_rj_r
      write(id_rank+50,*) 'istack_idx_local_rj_j',                      &
     &     stk_lc1d%istack_idx_local_rj_j
!
      end subroutine check_spheric_global_stack
!
! -----------------------------------------------------------------------
!
      end module t_sph_1d_global_index
