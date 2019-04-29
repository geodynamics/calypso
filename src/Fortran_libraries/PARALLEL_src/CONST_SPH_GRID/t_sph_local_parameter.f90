!t_sph_local_parameter.f90
!      module t_sph_local_parameter
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine alloc_sph_gl_parameter(s3d_ranks, sph_lcp)
!!      subroutine alloc_nidx_local(s3d_ranks, sph_lc1)
!!      subroutine alloc_sph_gl_bc_param(s3d_ranks, sph_dbc)
!!        type(spheric_global_rank), intent(in) :: s3d_ranks
!!        type(sph_local_parameters), intent(inout) :: sph_lcp
!!        type(sph_local_1d_param), intent(inout) :: sph_lc1
!!        type(sph_local_default_BC), intent(inout) :: sph_dbc
!!
!!      subroutine dealloc_sph_gl_parameter(sph_lcp)
!!      subroutine dealloc_nidx_local(sph_lc1)
!!      subroutine dealloc_sph_gl_bc_param(sph_dbc)
!!
!!      subroutine check_spheric_global_param                           &
!!     &         (id_rank, s3d_ranks, sph_lcp)
!!      subroutine check_nidx_local(ip_rank, sph_lc1)
!!      subroutine check_sph_gl_bc_param(id_rank, sph_dbc)
!
!
      module t_sph_local_parameter
!
      use m_precision
      use t_spheric_global_ranks
!
      implicit none
!
      type sph_local_parameters
        integer(kind = kint), allocatable :: nnod_local_rtp(:)
        integer(kind = kint), allocatable :: nnod_local_rtm(:)
        integer(kind = kint), allocatable :: nnod_local_rlm(:)
        integer(kind = kint), allocatable :: nnod_local_rj(:)
!
        integer(kind = kint), allocatable :: nidx_local_rtp(:,:)
        integer(kind = kint), allocatable :: nidx_local_rtm(:,:)
        integer(kind = kint), allocatable :: nidx_local_rlm(:,:)
        integer(kind = kint), allocatable :: nidx_local_rj(:,:)
      end type sph_local_parameters
!
!
      type sph_local_1d_param
        integer(kind = kint), allocatable :: nidx_local_rtp_r(:)
        integer(kind = kint), allocatable :: nidx_local_rtp_t(:)
        integer(kind = kint), allocatable :: nidx_local_rtp_p(:)
!
        integer(kind = kint), allocatable :: nidx_local_rtm_r(:)
        integer(kind = kint), allocatable :: nidx_local_rtm_t(:)
        integer(kind = kint), allocatable :: nidx_local_rtm_m(:)
!
        integer(kind = kint), allocatable :: nidx_local_rlm_r(:)
        integer(kind = kint), allocatable :: nidx_local_rlm_j(:)
!
        integer(kind = kint), allocatable :: nidx_local_rj_r(:)
        integer(kind = kint), allocatable :: nidx_local_rj_j(:)
      end type sph_local_1d_param
!
!
      type sph_local_default_BC
        integer(kind = kint), allocatable :: nidx_local_rtp_IC(:)
        integer(kind = kint), allocatable :: nidx_local_rtm_IC(:)
        integer(kind = kint), allocatable :: nidx_local_rtp_OC(:)
        integer(kind = kint), allocatable :: nidx_local_rtm_OC(:)
        integer(kind = kint), allocatable :: nidx_local_rtp_MT(:)
        integer(kind = kint), allocatable :: nidx_local_rtm_MT(:)
!
        integer(kind = kint), allocatable :: ist_idx_local_rtp_IC(:)
        integer(kind = kint), allocatable :: ist_idx_local_rtm_IC(:)
        integer(kind = kint), allocatable :: ist_idx_local_rtp_OC(:)
        integer(kind = kint), allocatable :: ist_idx_local_rtm_OC(:)
        integer(kind = kint), allocatable :: ist_idx_local_rtp_MT(:)
        integer(kind = kint), allocatable :: ist_idx_local_rtm_MT(:)
      end type sph_local_default_BC
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_gl_parameter(s3d_ranks, sph_lcp)
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_local_parameters), intent(inout) :: sph_lcp
!
!
      allocate(sph_lcp%nnod_local_rtp(s3d_ranks%ndomain_sph))
      allocate(sph_lcp%nnod_local_rtm(s3d_ranks%ndomain_sph))
      allocate(sph_lcp%nnod_local_rlm(s3d_ranks%ndomain_sph))
      allocate(sph_lcp%nnod_local_rj(s3d_ranks%ndomain_sph))
!
      allocate(sph_lcp%nidx_local_rtp(s3d_ranks%ndomain_sph,3))
      allocate(sph_lcp%nidx_local_rtm(s3d_ranks%ndomain_sph,3))
      allocate(sph_lcp%nidx_local_rlm(s3d_ranks%ndomain_sph,2))
      allocate(sph_lcp%nidx_local_rj(s3d_ranks%ndomain_sph,2))
!
      sph_lcp%nnod_local_rtp = 0
      sph_lcp%nnod_local_rtm = 0
      sph_lcp%nnod_local_rlm = 0
      sph_lcp%nnod_local_rj =  0
!
      sph_lcp%nidx_local_rtp = 0
      sph_lcp%nidx_local_rtm = 0
      sph_lcp%nidx_local_rlm = 0
      sph_lcp%nidx_local_rj =  0
!
      end subroutine alloc_sph_gl_parameter
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nidx_local(s3d_ranks, sph_lc1)
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_local_1d_param), intent(inout) :: sph_lc1
!
      integer(kind = kint) :: num
!
!
      num = s3d_ranks%ndomain_rtp(1)
      allocate(sph_lc1%nidx_local_rtp_r(num))
      num = s3d_ranks%ndomain_rtp(2)
      allocate(sph_lc1%nidx_local_rtp_t(num))
      num = s3d_ranks%ndomain_rtp(3)
      allocate(sph_lc1%nidx_local_rtp_p(num))
!
      num = s3d_ranks%ndomain_rtm(1)
      allocate(sph_lc1%nidx_local_rtm_r(num))
      num = s3d_ranks%ndomain_rtm(2)
      allocate(sph_lc1%nidx_local_rtm_t(num))
      num = s3d_ranks%ndomain_rtm(3)
      allocate(sph_lc1%nidx_local_rtm_m(num))
!
      num = s3d_ranks%ndomain_rlm(1)
      allocate(sph_lc1%nidx_local_rlm_r(num))
      num = s3d_ranks%ndomain_rlm(2)
      allocate(sph_lc1%nidx_local_rlm_j(num))
!
      num = s3d_ranks%ndomain_rj(1)
      allocate(sph_lc1%nidx_local_rj_r(num))
      num = s3d_ranks%ndomain_rj(2)
      allocate(sph_lc1%nidx_local_rj_j(num))
!
      sph_lc1%nidx_local_rtp_r = 0
      sph_lc1%nidx_local_rtp_t = 0
      sph_lc1%nidx_local_rtp_p = 0
      sph_lc1%nidx_local_rtm_r = 0
      sph_lc1%nidx_local_rtm_t = 0
      sph_lc1%nidx_local_rtm_m = 0
      sph_lc1%nidx_local_rlm_r = 0
      sph_lc1%nidx_local_rlm_j = 0
      sph_lc1%nidx_local_rj_r =  0
      sph_lc1%nidx_local_rj_j =  0
!
      end subroutine alloc_nidx_local
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_gl_bc_param(s3d_ranks, sph_dbc)
!
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_local_default_BC), intent(inout) :: sph_dbc
!
      integer(kind = kint) :: num
!
!
      num = s3d_ranks%ndomain_rtp(1)
      allocate(sph_dbc%nidx_local_rtp_IC(num))
      allocate(sph_dbc%nidx_local_rtp_OC(num))
      allocate(sph_dbc%nidx_local_rtp_MT(num))
      allocate(sph_dbc%ist_idx_local_rtp_IC(num))
      allocate(sph_dbc%ist_idx_local_rtp_OC(num))
      allocate(sph_dbc%ist_idx_local_rtp_MT(num))
!
      num = s3d_ranks%ndomain_rtm(1)
      allocate(sph_dbc%nidx_local_rtm_IC(num))
      allocate(sph_dbc%nidx_local_rtm_OC(num))
      allocate(sph_dbc%nidx_local_rtm_MT(num))
      allocate(sph_dbc%ist_idx_local_rtm_IC(num))
      allocate(sph_dbc%ist_idx_local_rtm_OC(num))
      allocate(sph_dbc%ist_idx_local_rtm_MT(num))
!
      sph_dbc%nidx_local_rtp_IC =  0
      sph_dbc%nidx_local_rtm_IC =  0
      sph_dbc%nidx_local_rtp_OC =  0
      sph_dbc%nidx_local_rtm_OC =  0
      sph_dbc%nidx_local_rtp_MT =  0
      sph_dbc%nidx_local_rtm_MT =  0
!
      sph_dbc%ist_idx_local_rtp_IC =  0
      sph_dbc%ist_idx_local_rtm_IC =  0
      sph_dbc%ist_idx_local_rtp_OC =  0
      sph_dbc%ist_idx_local_rtm_OC =  0
      sph_dbc%ist_idx_local_rtp_MT =  0
      sph_dbc%ist_idx_local_rtm_MT =  0
!
      end subroutine alloc_sph_gl_bc_param
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_gl_parameter(sph_lcp)
!
      type(sph_local_parameters), intent(inout) :: sph_lcp
!
!
      deallocate(sph_lcp%nnod_local_rtp, sph_lcp%nnod_local_rtm)
      deallocate(sph_lcp%nnod_local_rlm, sph_lcp%nnod_local_rj)
!
      deallocate(sph_lcp%nidx_local_rtp, sph_lcp%nidx_local_rtm)
      deallocate(sph_lcp%nidx_local_rlm, sph_lcp%nidx_local_rj)
!
      end subroutine dealloc_sph_gl_parameter
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_nidx_local(sph_lc1)
!
      type(sph_local_1d_param), intent(inout) :: sph_lc1
!
!
      deallocate(sph_lc1%nidx_local_rtp_r, sph_lc1%nidx_local_rtp_t)
      deallocate(sph_lc1%nidx_local_rtp_p)
      deallocate(sph_lc1%nidx_local_rtm_r, sph_lc1%nidx_local_rtm_t)
      deallocate(sph_lc1%nidx_local_rtm_m)
      deallocate(sph_lc1%nidx_local_rlm_r, sph_lc1%nidx_local_rlm_j)
      deallocate(sph_lc1%nidx_local_rj_r, sph_lc1%nidx_local_rj_j)
!
      end subroutine dealloc_nidx_local
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_gl_bc_param(sph_dbc)
!
      type(sph_local_default_BC), intent(inout) :: sph_dbc
!
!
      deallocate(sph_dbc%nidx_local_rtp_IC)
      deallocate(sph_dbc%nidx_local_rtp_OC)
      deallocate(sph_dbc%nidx_local_rtp_MT)
      deallocate(sph_dbc%ist_idx_local_rtp_IC)
      deallocate(sph_dbc%ist_idx_local_rtp_OC)
      deallocate(sph_dbc%ist_idx_local_rtp_MT)
!
      deallocate(sph_dbc%nidx_local_rtm_IC)
      deallocate(sph_dbc%nidx_local_rtm_OC)
      deallocate(sph_dbc%nidx_local_rtm_MT)
      deallocate(sph_dbc%ist_idx_local_rtm_IC)
      deallocate(sph_dbc%ist_idx_local_rtm_OC)
      deallocate(sph_dbc%ist_idx_local_rtm_MT)
!
      end subroutine dealloc_sph_gl_bc_param
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_spheric_global_param                             &
     &         (id_rank, s3d_ranks, sph_lcp)
!
      integer, intent(in) :: id_rank
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_local_parameters), intent(in) :: sph_lcp
!
      integer(kind = kint) :: i
!
!
      write(id_rank+50,*) 'ndomain_rtp ', s3d_ranks%ndomain_rtp(1:3)
      write(id_rank+50,*) 'ndomain_rtm ', s3d_ranks%ndomain_rtm(1:3)
      write(id_rank+50,*) 'ndomain_rlm ', s3d_ranks%ndomain_rlm(1:2)
      write(id_rank+50,*) 'ndomain_rj ',  s3d_ranks%ndomain_rj(1:2)
      write(id_rank+50,*) 'ndomain_sph ',  s3d_ranks%ndomain_sph
!
      write(id_rank+50,*)                                               &
     &   'i, nnod_local_rtp, nidx_local_rtp'
      do i = 1, s3d_ranks%ndomain_sph
        write(id_rank+50,*) i, sph_lcp%nnod_local_rtp(i),               &
     &                         sph_lcp%nidx_local_rtp(i,1:3)
      end do
!
      write(id_rank+50,*)                                               &
     &   'i, nnod_local_rtm, nidx_local_rtm'
      do i = 1, s3d_ranks%ndomain_sph
        write(id_rank+50,*) i, sph_lcp%nnod_local_rtm(i),               &
     &                         sph_lcp%nidx_local_rtm(i,1:3)
      end do
!
      write(id_rank+50,*)                                               &
     &   'i, nnod_local_rlm, nidx_local_rlm'
      do i = 1, s3d_ranks%ndomain_sph
        write(id_rank+50,*) i, sph_lcp%nnod_local_rlm(i),               &
     &                         sph_lcp% nidx_local_rlm(i,1:2)
      end do
!
      write(id_rank+50,*)                                               &
     &   'i, nnod_local_rj, nidx_local_rj'
      do i = 1, s3d_ranks%ndomain_sph
        write(id_rank+50,*) i, sph_lcp%nnod_local_rj(i),                &
     &                         sph_lcp%nidx_local_rj(i,1:2)
      end do
!
      end subroutine check_spheric_global_param
!
! -----------------------------------------------------------------------
!
      subroutine check_nidx_local(ip_rank, sph_lc1)
!
      integer(kind = kint), intent(in) :: ip_rank
      type(sph_local_1d_param), intent(inout) :: sph_lc1
!
!
      write(ip_rank+50,*) 'nidx_local_rtp_r', sph_lc1%nidx_local_rtp_r
      write(ip_rank+50,*) 'nidx_local_rtp_t', sph_lc1%nidx_local_rtp_t
      write(ip_rank+50,*) 'nidx_local_rtp_p', sph_lc1%nidx_local_rtp_p
!
      write(ip_rank+50,*) 'nidx_local_rtm_r', sph_lc1%nidx_local_rtm_r
      write(ip_rank+50,*) 'nidx_local_rtm_t', sph_lc1%nidx_local_rtm_t
      write(ip_rank+50,*) 'nidx_local_rtm_m', sph_lc1%nidx_local_rtm_m
!
      write(ip_rank+50,*) 'nidx_local_rlm_r', sph_lc1%nidx_local_rlm_r
      write(ip_rank+50,*) 'nidx_local_rlm_j', sph_lc1%nidx_local_rlm_j
!
      write(ip_rank+50,*) 'nidx_local_rj_r', sph_lc1%nidx_local_rj_r
      write(ip_rank+50,*) 'nidx_local_rj_j', sph_lc1%nidx_local_rj_j
!
      end subroutine check_nidx_local
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_gl_bc_param(id_rank, sph_dbc)
!
      integer, intent(in) :: id_rank
      type(sph_local_default_BC), intent(in) :: sph_dbc
!
!
      write(id_rank+50,*) 'nidx_local_rtm_OC',                          &
     &                   sph_dbc%nidx_local_rtm_OC
      write(id_rank+50,*) 'nidx_local_rtm_IC',                          &
     &                   sph_dbc%nidx_local_rtm_IC
      write(id_rank+50,*) 'nidx_local_rtm_MT',                          &
     &                   sph_dbc%nidx_local_rtm_MT
      write(id_rank+50,*) 'ist_idx_local_rtm_OC',                       &
     &                   sph_dbc%ist_idx_local_rtm_OC
      write(id_rank+50,*) 'ist_idx_local_rtm_IC',                       &
     &                   sph_dbc%ist_idx_local_rtm_IC
      write(id_rank+50,*) 'ist_idx_local_rtm_MT',                       &
     &                   sph_dbc%ist_idx_local_rtm_MT
!
      end subroutine check_sph_gl_bc_param
!
! -----------------------------------------------------------------------
!
      end module t_sph_local_parameter
