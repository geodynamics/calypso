!>@file   t_spheric_global_ranks.f90
!!@brief  module t_spheric_global_ranks
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2007
!
!>@brief  Global subdomain informatikn for spherical shell
!!
!!@verbatim
!!      subroutine alloc_radius_1d_gl(nri_gl, s3d_radius)
!!      subroutine dealloc_radius_1d_gl(s3d_radius)
!!        type(spheric_global_radius), intent(inout) :: s3d_radius
!!
!!      subroutine alloc_sph_ranks(s3d_ranks)
!!      subroutine alloc_sph_1d_domain_id(sph_rtp, sph_rj, s3d_ranks)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!      subroutine dealloc_sph_ranks(s3d_ranks)
!!      subroutine dealloc_sph_1d_domain_id(s3d_ranks)
!!
!!      subroutine check_sph_domains                                    &
!!     &         (nprocs_check, s3d_ranks, ierr, e_message)
!!      subroutine check_sph_ranks(my_rank, nprocs_check)
!!      subroutine check_sph_1d_domain_id(nprocs_check)
!!@endverbatim
!
      module t_spheric_global_ranks
!
      use m_precision
!
      implicit none
!
!
      type spheric_global_radius
!>        Number of radial points
        integer(kind = kint) :: nri_global
!>        global radius data @f$ r(k) @f$
        real(kind = kreal), allocatable :: radius_1d_gl(:)
      end type spheric_global_radius
!
!
      type spheric_global_rank
!>        flag for radial inner decomposition
        integer(kind = kint) :: iflag_radial_inner_domain = 0
!
!>        number of subdomains
        integer(kind = kint) :: ndomain_sph
!
!>        number of 1d subdomains for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: ndomain_rtp(3)
!>        number of 1d subdomains for @f$ f(r,l,m) @f$
        integer(kind = kint) :: ndomain_rlm(2)
!>        number of 1d subdomains for @f$ f(r,j) @f$
        integer(kind = kint) :: ndomain_rj(2)
!>        number of 1d subdomains for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: ndomain_rtm(3)
!
        integer(kind = kint), allocatable :: iglobal_rank_rtp(:,:)
        integer(kind = kint), allocatable :: iglobal_rank_rtm(:,:)
        integer(kind = kint), allocatable :: iglobal_rank_rlm(:,:)
        integer(kind = kint), allocatable :: iglobal_rank_rj(:,:)
!
        integer(kind = kint), allocatable :: id_domain_rtp_r(:)
        integer(kind = kint), allocatable :: id_domain_rtp_t(:)
        integer(kind = kint), allocatable :: id_domain_rtp_p(:)
        integer(kind = kint), allocatable :: id_domain_rj_r(:)
        integer(kind = kint), allocatable :: id_domain_rj_j(:)
      end type spheric_global_rank
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_radius_1d_gl(nri_gl, s3d_radius)
!
      integer(kind = kint), intent(in) :: nri_gl
      type(spheric_global_radius), intent(inout) :: s3d_radius
!
!
      s3d_radius%nri_global = nri_gl
      allocate(s3d_radius%radius_1d_gl(s3d_radius%nri_global))
      if(s3d_radius%nri_global .gt. 0) s3d_radius%radius_1d_gl = 0.0d0
!
      end subroutine alloc_radius_1d_gl
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_radius_1d_gl(s3d_radius)
!
      type(spheric_global_radius), intent(inout) :: s3d_radius
!
      deallocate(s3d_radius%radius_1d_gl)
!
      end subroutine dealloc_radius_1d_gl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_sph_ranks(s3d_ranks)
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
!
!
      allocate(s3d_ranks%iglobal_rank_rtp(3,0:s3d_ranks%ndomain_sph))
      allocate(s3d_ranks%iglobal_rank_rtm(3,0:s3d_ranks%ndomain_sph))
      allocate(s3d_ranks%iglobal_rank_rlm(2,0:s3d_ranks%ndomain_sph))
      allocate(s3d_ranks%iglobal_rank_rj(2,0:s3d_ranks%ndomain_sph))
!
      s3d_ranks%iglobal_rank_rtp = 0
      s3d_ranks%iglobal_rank_rtm = 0
      s3d_ranks%iglobal_rank_rlm = 0
      s3d_ranks%iglobal_rank_rj =  0
!
      end subroutine alloc_sph_ranks
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_1d_domain_id(sph_rtp, sph_rj, s3d_ranks)
!
      use t_spheric_parameter
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
!
      integer(kind = kint) :: n1, n2, n3
!
!
      n1 = sph_rtp%nidx_global_rtp(1)
      n2 = sph_rtp%nidx_global_rtp(2)
      n3 = sph_rtp%nidx_global_rtp(3)
      allocate( s3d_ranks%id_domain_rtp_r(n1) )
      allocate( s3d_ranks%id_domain_rtp_t(n2) )
      allocate( s3d_ranks%id_domain_rtp_p(n3) )
!
      s3d_ranks%id_domain_rtp_r = -1
      s3d_ranks%id_domain_rtp_t = -1
      s3d_ranks%id_domain_rtp_p = -1
!
!
!
      n1 = sph_rj%nidx_global_rj(1)
      n2 = sph_rj%nidx_global_rj(2)
      allocate( s3d_ranks%id_domain_rj_r(n1) )
      allocate( s3d_ranks%id_domain_rj_j(0:n2) )
!
      s3d_ranks%id_domain_rj_r = -1
      s3d_ranks%id_domain_rj_j = -1
!
      end subroutine alloc_sph_1d_domain_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_ranks(s3d_ranks)
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
!
!
      deallocate(s3d_ranks%iglobal_rank_rtp)
      deallocate(s3d_ranks%iglobal_rank_rtm)
      deallocate(s3d_ranks%iglobal_rank_rlm)
      deallocate(s3d_ranks%iglobal_rank_rj)
!
      end subroutine dealloc_sph_ranks
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_1d_domain_id(s3d_ranks)
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
!
!
      deallocate( s3d_ranks%id_domain_rtp_r, s3d_ranks%id_domain_rtp_t)
      deallocate( s3d_ranks%id_domain_rtp_p )
      deallocate( s3d_ranks%id_domain_rj_r, s3d_ranks%id_domain_rj_j)
!
      end subroutine dealloc_sph_1d_domain_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_sph_domains                                      &
     &         (nprocs_check, s3d_ranks, ierr, e_message)
!
      use m_error_IDs
!
      integer(kind = kint), intent(in) :: nprocs_check
!
      integer(kind = kint), intent(inout) :: ierr
      character(len = kchara), intent(inout) :: e_message
      type(spheric_global_rank), intent(inout) :: s3d_ranks
!
      integer(kind = kint) :: np
!
!
      ierr = 0
      s3d_ranks%ndomain_sph                                             &
     &     = s3d_ranks%ndomain_rj(1) * s3d_ranks%ndomain_rj(2)
      if (s3d_ranks%ndomain_sph .ne. nprocs_check) then
        write(e_message,'(a)') 'check num of domain spectr file(r,j)'
        ierr = ierr_mesh
        return
      end if
!
      np =  s3d_ranks%ndomain_rtp(1)                                    &
     &    * s3d_ranks%ndomain_rtp(2)                                    &
     &    * s3d_ranks%ndomain_rtp(3)
      if (s3d_ranks%ndomain_sph .ne. np) then
        write(e_message,'(a)') 'check num of domain for (r,t,p)'
        ierr = ierr_mesh
        return
      end if
      np =  s3d_ranks%ndomain_rtm(1)                                    &
     &    * s3d_ranks%ndomain_rtm(2)                                    &
     &    * s3d_ranks%ndomain_rtm(3)
      if (s3d_ranks%ndomain_sph .ne. np) then
        write(e_message,'(a)') 'check num of domain for (r,t,m)'
        ierr = ierr_mesh
        return
      end if
      np = s3d_ranks%ndomain_rlm(1) * s3d_ranks%ndomain_rlm(2)
      if (s3d_ranks%ndomain_sph .ne. np) then
        write(e_message,'(a)') 'check num of domain for (r,l,m)'
        ierr = ierr_mesh
        return
      end if
!
      if(s3d_ranks%ndomain_rtm(1) .ne. s3d_ranks%ndomain_rtp(1)) then
        write(e_message,'(a,a1,a)')                                     &
     &            'Set same number of radial subdomains', char(10),     &
     &            'for Legendre transform and spherical grids'
        ierr = ierr_mesh
        return
      end if
!
      end subroutine check_sph_domains
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_ranks(my_rank, s3d_ranks)
!
      integer(kind = kint), intent(in) :: my_rank
      type(spheric_global_rank), intent(in) :: s3d_ranks
!
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'i, iglobal_rank_rtp'
      do i = 0, s3d_ranks%ndomain_sph-1
        write(my_rank+50,*) i, s3d_ranks%iglobal_rank_rtp(1:3,i)
      end do
!
      write(my_rank+50,*) 'i, iglobal_rank_rtm'
      do i = 0, s3d_ranks%ndomain_sph-1
        write(my_rank+50,*) i, s3d_ranks%iglobal_rank_rtm(1:3,i)
      end do
!
      write(my_rank+50,*) 'i, iglobal_rank_rlm'
      do i = 0, s3d_ranks%ndomain_sph-1
        write(my_rank+50,*) i, s3d_ranks%iglobal_rank_rlm(1:2,i)
      end do
!
      write(my_rank+50,*) 'i, iglobal_rank_rj'
      do i = 0, s3d_ranks%ndomain_sph-1
        write(my_rank+50,*) i, s3d_ranks%iglobal_rank_rj(1:2,i)
      end do
!
      end subroutine check_sph_ranks
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_1d_domain_id(sph_rtp, sph_rj, s3d_ranks)
!
      use t_spheric_parameter
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
      type(spheric_global_rank), intent(in) :: s3d_ranks
!
!
      write(50,*) 'id_domain_rtp_r'
      write(50,'(5i16)')                                                &
     &    s3d_ranks%id_domain_rtp_r(1:sph_rtp%nidx_global_rtp(1))
!
      write(50,*) 'id_domain_rtp_t'
      write(50,'(5i16)')                                                &
     &    s3d_ranks%id_domain_rtp_t(1:sph_rtp%nidx_global_rtp(2))
!
      write(50,*) 'id_domain_rtp_p'
      write(50,'(5i16)')                                                &
     &    s3d_ranks%id_domain_rtp_p(1:sph_rtp%nidx_global_rtp(3))
!
!
      write(50,*) 'id_domain_rj_r'
      write(50,'(5i16)')                                                &
     &    s3d_ranks%id_domain_rj_r(1:sph_rj%nidx_global_rj(1))
!
      write(50,*) 'id_domain_rj_j'
      write(50,'(5i16)') s3d_ranks%id_domain_rj_j(0)
      write(50,'(5i16)')                                                &
     &    s3d_ranks%id_domain_rj_j(1:sph_rj%nidx_global_rj(2))
!
      end subroutine check_sph_1d_domain_id
!
! -----------------------------------------------------------------------
!
      end module t_spheric_global_ranks
