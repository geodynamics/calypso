!
!      module set_global_spherical_param
!
!     Written by H. Matsui on July, 2007
!
!      subroutine set_global_sph_resolution
!        required resoplution:  nidx_global_rtp(1)
!                               nidx_global_rtp(2)
!        required resolution:  l_truncation
!
!      subroutine set_gl_nnod_spherical(nproc,                          &
!     &          ndomain_1, ndomain_2, ndomain_3, id_gl_rank,           &
!     &          nidx_local_1, nidx_local_2, nidx_local_3,              &
!     &          nidx_local, nnod_local)
!      subroutine set_gl_nnod_spheric_rj(nproc, ndomain_1, ndomain_2,   &
!     &          id_gl_rank, nidx_local_1, nidx_local_2,                &
!     &          nidx_local, nnod_local)
!
!      subroutine set_rank_by_1b_sph_rank(ndomain_3d,                   &
!     &          ip_r, ip_t, ip_p, ip_rank)
!      subroutine set_gl_rank_3d(nproc, ndomain_3d, id_gl_rank)
!      subroutine set_gl_rank_2d(nproc, ndomain_2d, id_gl_rank)
!
!      subroutine check_spheric_global_numnod(my_rank)
!
      module set_global_spherical_param
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), private :: nnod_global_rtp
      integer(kind = kint), private :: nnod_global_rtm
      integer(kind = kint), private :: nnod_global_rlm
      integer(kind = kint), private :: nnod_global_rj
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_resolution
!
      use m_spheric_parameter
!
!
      nidx_global_rtp(3) = 2*nidx_global_rtp(2)
!
      nidx_global_rtm(1) = nidx_global_rtp(1)
      nidx_global_rtm(2) = nidx_global_rtp(2)
      nidx_global_rtm(3) = 2*(l_truncation + 1)
!
      nidx_global_rlm(1) = nidx_global_rtp(1)
      nidx_global_rlm(2) = l_truncation * (l_truncation + 2)
!
      nidx_global_rj(1) = nidx_global_rtp(1)
      nidx_global_rj(2) = l_truncation * (l_truncation + 2)
!
      nnod_global_rtp                                                   &
     &  = nidx_global_rtp(1) * nidx_global_rtp(2) * nidx_global_rtp(3)
      nnod_global_rtm                                                   &
     &  = nidx_global_rtm(1) * nidx_global_rtm(2) * nidx_global_rtm(3)
      nnod_global_rlm                                                   &
     &  = nidx_global_rlm(1) * (nidx_global_rlm(2) + 1)
      nnod_global_rj                                                    &
     &  = nidx_global_rj(1) * (nidx_global_rj(2) + 1)
!
      end subroutine set_global_sph_resolution
!
! -----------------------------------------------------------------------
!
      subroutine set_gl_nnod_spherical(nproc,                           &
     &          ndomain_1, ndomain_2, ndomain_3, id_gl_rank,            &
     &          nidx_local_1, nidx_local_2, nidx_local_3,               &
     &          nidx_local, nnod_local)
!
      integer(kind = kint), intent(in) :: nproc
      integer(kind = kint), intent(in) :: ndomain_1, ndomain_2
      integer(kind = kint), intent(in) :: ndomain_3
      integer(kind = kint), intent(in) :: id_gl_rank(3,0:(nproc-1))
      integer(kind = kint), intent(in) :: nidx_local_1(ndomain_1)
      integer(kind = kint), intent(in) :: nidx_local_2(ndomain_2)
      integer(kind = kint), intent(in) :: nidx_local_3(ndomain_3)
!
      integer(kind = kint), intent(inout) :: nidx_local(nproc,3)
      integer(kind = kint), intent(inout) :: nnod_local(nproc)
!
      integer(kind = kint) :: i1, i2, i3, i, ip_rank
!
!
      do i = 1, nproc
        ip_rank = i - 1
        i1 = id_gl_rank(1,ip_rank) + 1
        i2 = id_gl_rank(2,ip_rank) + 1
        i3 = id_gl_rank(3,ip_rank) + 1
        nidx_local(i,1) = nidx_local_1(i1)
        nidx_local(i,2) = nidx_local_2(i2)
        nidx_local(i,3) = nidx_local_3(i3)
        nnod_local(i) =  nidx_local(i,1)                                &
     &                 * nidx_local(i,2)                                &
     &                 * nidx_local(i,3)
      end do
!
      end subroutine set_gl_nnod_spherical
!
! -----------------------------------------------------------------------
!
      subroutine set_gl_nnod_spheric_rj(nproc, ndomain_1, ndomain_2,    &
     &          id_gl_rank, nidx_local_1, nidx_local_2,                 &
     &          nidx_local, nnod_local)
!
      integer(kind = kint), intent(in) :: nproc
      integer(kind = kint), intent(in) :: ndomain_1, ndomain_2
      integer(kind = kint), intent(in) :: id_gl_rank(2,0:(nproc-1))
      integer(kind = kint), intent(in) :: nidx_local_1(ndomain_1)
      integer(kind = kint), intent(in) :: nidx_local_2(ndomain_2)
!
      integer(kind = kint), intent(inout) :: nidx_local(nproc,2)
      integer(kind = kint), intent(inout) :: nnod_local(nproc)
!
      integer(kind = kint) :: i1, i2, i, ip_rank
!
!
      do i = 1, nproc
        ip_rank = i - 1
        i1 = id_gl_rank(1,ip_rank) + 1
        i2 = id_gl_rank(2,ip_rank) + 1
        nidx_local(i,1) = nidx_local_1(i1)
        nidx_local(i,2) = nidx_local_2(i2)
        nnod_local(i) =  nidx_local(i,1)                                &
     &                 * nidx_local(i,2)
      end do
!
      end subroutine set_gl_nnod_spheric_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rank_by_1b_sph_rank(ndomain_3d,                    &
     &          ip_r, ip_t, ip_p, ip_rank)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, ip_p
      integer(kind = kint), intent(in) :: ndomain_3d(3)
      integer(kind = kint), intent(inout) :: ip_rank
!
!
      ip_rank =  ip_r*ndomain_3d(2)*ndomain_3d(3)                       &
     &         + ip_t*ndomain_3d(3)                                     &
     &         + ip_p
!
      end subroutine set_rank_by_1b_sph_rank
!
! -----------------------------------------------------------------------
!
      subroutine set_gl_rank_3d(nproc, ndomain_3d, id_gl_rank)
!
      integer(kind = kint), intent(in) :: nproc
      integer(kind = kint), intent(in) :: ndomain_3d(3)
      integer(kind = kint), intent(inout) :: id_gl_rank(3,0:(nproc-1))
      integer(kind = kint) :: ip_rank
      integer(kind = kint) :: itmp
!
!
      do ip_rank = 0, nproc-1
        id_gl_rank(3,ip_rank) = mod(ip_rank,ndomain_3d(3))
        itmp = (ip_rank-id_gl_rank(3,ip_rank)) / ndomain_3d(3)
        id_gl_rank(2,ip_rank) = mod(itmp,ndomain_3d(2))
        id_gl_rank(1,ip_rank) = (itmp-id_gl_rank(2,ip_rank))            &
     &                         / ndomain_3d(2)
      end do
!
      end subroutine set_gl_rank_3d
!
! -----------------------------------------------------------------------
!
      subroutine set_gl_rank_2d(nproc, ndomain_2d, id_gl_rank)
!
      integer(kind = kint), intent(in) :: nproc
      integer(kind = kint), intent(in) :: ndomain_2d(2)
      integer(kind = kint), intent(inout) :: id_gl_rank(2,0:(nproc-1))
      integer(kind = kint) :: ip_rank
!
!
      do ip_rank = 0, nproc-1
        id_gl_rank(2,ip_rank) = mod(ip_rank,ndomain_2d(2))
        id_gl_rank(1,ip_rank) = (ip_rank-id_gl_rank(2,ip_rank))         &
     &                         / ndomain_2d(2)
      end do
!
      end subroutine set_gl_rank_2d
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_spheric_global_numnod(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(my_rank+50,*) 'nnod_global_rtp ', nnod_global_rtp
      write(my_rank+50,*) 'nnod_global_rtm ', nnod_global_rtm
      write(my_rank+50,*) 'nnod_global_rlm ', nnod_global_rlm
      write(my_rank+50,*) 'nnod_global_rj ',  nnod_global_rj
!
      end subroutine check_spheric_global_numnod
!
! -----------------------------------------------------------------------
!
      end module set_global_spherical_param
