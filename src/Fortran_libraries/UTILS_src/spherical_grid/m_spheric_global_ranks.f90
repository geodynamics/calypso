!
!      module m_spheric_global_ranks
!
!     Written by H. Matsui on July, 2007
!
!      subroutine allocate_sph_ranks
!      subroutine allocate_sph_1d_domain_id
!
!      subroutine deallocate_sph_ranks
!      subroutine deallocate_sph_1d_domain_id
!
!      subroutine check_sph_ranks(my_rank)
!      subroutine check_sph_1d_domain_id
!
      module m_spheric_global_ranks
!
      use m_precision
!
      implicit none
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
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_ranks
!
      use m_spheric_parameter
!
      allocate(iglobal_rank_rtp(3,0:ndomain_sph))
      allocate(iglobal_rank_rtm(3,0:ndomain_sph))
      allocate(iglobal_rank_rlm(2,0:ndomain_sph))
      allocate(iglobal_rank_rj(2,0:ndomain_sph))
!
      iglobal_rank_rtp = 0
      iglobal_rank_rtm = 0
      iglobal_rank_rlm = 0
      iglobal_rank_rj =  0
!
      end subroutine allocate_sph_ranks
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_domain_id
!
      use m_spheric_parameter
!
      integer(kind = kint) :: n1, n2, n3
!
      n1 = nidx_global_rtp(1)
      n2 = nidx_global_rtp(2)
      n3 = nidx_global_rtp(3)
      allocate( id_domain_rtp_r(n1) )
      allocate( id_domain_rtp_t(n2) )
      allocate( id_domain_rtp_p(n3) )
!
      id_domain_rtp_r = -1
      id_domain_rtp_t = -1
      id_domain_rtp_p = -1
!
!
!
      n1 = nidx_global_rj(1)
      n2 = nidx_global_rj(2)
      allocate( id_domain_rj_r(n1) )
      allocate( id_domain_rj_j(0:n2) )
!
      id_domain_rj_r = -1
      id_domain_rj_j = -1
!
      end subroutine allocate_sph_1d_domain_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_ranks
!
      deallocate(iglobal_rank_rtp, iglobal_rank_rtm)
      deallocate(iglobal_rank_rlm, iglobal_rank_rj)
!
      end subroutine deallocate_sph_ranks
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_domain_id
!
      deallocate( id_domain_rtp_r, id_domain_rtp_t, id_domain_rtp_p )
      deallocate( id_domain_rj_r, id_domain_rj_j )
!
      end subroutine deallocate_sph_1d_domain_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_sph_ranks(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'i, iglobal_rank_rtp'
      do i = 0, ndomain_sph-1
        write(my_rank+50,*) i, iglobal_rank_rtp(1:3,i)
      end do
!
      write(my_rank+50,*) 'i, iglobal_rank_rtm'
      do i = 0, ndomain_sph-1
        write(my_rank+50,*) i, iglobal_rank_rtm(1:3,i)
      end do
!
      write(my_rank+50,*) 'i, iglobal_rank_rlm'
      do i = 0, ndomain_sph-1
        write(my_rank+50,*) i, iglobal_rank_rlm(1:2,i)
      end do
!
      write(my_rank+50,*) 'i, iglobal_rank_rj'
      do i = 0, ndomain_sph-1
        write(my_rank+50,*) i, iglobal_rank_rj(1:2,i)
      end do
!
      end subroutine check_sph_ranks
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_1d_domain_id
!
      use m_spheric_parameter
!
      write(50,*) 'id_domain_rtp_r'
      write(50,'(5i16)') id_domain_rtp_r(1:nidx_global_rtp(1))
!
      write(50,*) 'id_domain_rtp_t'
      write(50,'(5i16)') id_domain_rtp_t(1:nidx_global_rtp(2))
!
      write(50,*) 'id_domain_rtp_p'
      write(50,'(5i16)') id_domain_rtp_p(1:nidx_global_rtp(3))
!
!
      write(50,*) 'id_domain_rj_r'
      write(50,'(5i16)') id_domain_rj_r(1:nidx_global_rj(1))
!
      write(50,*) 'id_domain_rj_j'
      write(50,'(5i16)') id_domain_rj_j(0)
      write(50,'(5i16)') id_domain_rj_j(1:nidx_global_rj(2))
!
      end subroutine check_sph_1d_domain_id
!
! -----------------------------------------------------------------------
!
      end module m_spheric_global_ranks
