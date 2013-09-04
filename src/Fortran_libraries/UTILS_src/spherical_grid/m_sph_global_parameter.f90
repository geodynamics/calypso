!m_sph_global_parameter.f90
!      module m_sph_global_parameter
!
!     Written by H. Matsui on July, 2007
!
!      subroutine allocate_sph_gl_parameter
!      subroutine allocate_sph_gl_bc_param
!
!      subroutine deallocate_sph_gl_parameter
!      subroutine deallocate_sph_gl_bc_param
!
!      subroutine check_spheric_global_param(my_rank)
!      subroutine check_sph_gl_bc_param(my_rank)
!
!
      module m_sph_global_parameter
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: nnod_local_rtp(:)
      integer(kind = kint), allocatable :: nnod_local_rtm(:)
      integer(kind = kint), allocatable :: nnod_local_rlm(:)
      integer(kind = kint), allocatable :: nnod_local_rj(:)
!
      integer(kind = kint), allocatable :: nidx_local_rtp(:,:)
      integer(kind = kint), allocatable :: nidx_local_rtm(:,:)
      integer(kind = kint), allocatable :: nidx_local_rlm(:,:)
      integer(kind = kint), allocatable :: nidx_local_rj(:,:)
!
!
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
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_gl_parameter
!
      use m_spheric_parameter
!
!
      allocate(nnod_local_rtp(ndomain_sph))
      allocate(nnod_local_rtm(ndomain_sph))
      allocate(nnod_local_rlm(ndomain_sph))
      allocate(nnod_local_rj(ndomain_sph))
!
      allocate(nidx_local_rtp(ndomain_sph,3))
      allocate(nidx_local_rtm(ndomain_sph,3))
      allocate(nidx_local_rlm(ndomain_sph,2))
      allocate(nidx_local_rj(ndomain_sph,2))
!
      nnod_local_rtp = 0
      nnod_local_rtm = 0
      nnod_local_rlm = 0
      nnod_local_rj =  0
!
      nidx_local_rtp = 0
      nidx_local_rtm = 0
      nidx_local_rlm = 0
      nidx_local_rj =  0
!
      end subroutine allocate_sph_gl_parameter
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_gl_bc_param
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
!
      num = ndomain_rtp(1)
      allocate(nidx_local_rtp_IC(num))
      allocate(nidx_local_rtp_OC(num))
      allocate(nidx_local_rtp_MT(num))
      allocate(ist_idx_local_rtp_IC(num))
      allocate(ist_idx_local_rtp_OC(num))
      allocate(ist_idx_local_rtp_MT(num))
!
      num = ndomain_rtm(1)
      allocate(nidx_local_rtm_IC(num))
      allocate(nidx_local_rtm_OC(num))
      allocate(nidx_local_rtm_MT(num))
      allocate(ist_idx_local_rtm_IC(num))
      allocate(ist_idx_local_rtm_OC(num))
      allocate(ist_idx_local_rtm_MT(num))
!
      nidx_local_rtp_IC =  0
      nidx_local_rtm_IC =  0
      nidx_local_rtp_OC =  0
      nidx_local_rtm_OC =  0
      nidx_local_rtp_MT =  0
      nidx_local_rtm_MT =  0
!
      ist_idx_local_rtp_IC =  0
      ist_idx_local_rtm_IC =  0
      ist_idx_local_rtp_OC =  0
      ist_idx_local_rtm_OC =  0
      ist_idx_local_rtp_MT =  0
      ist_idx_local_rtm_MT =  0
!
      end subroutine allocate_sph_gl_bc_param
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_gl_parameter
!
!
      deallocate(nnod_local_rtp, nnod_local_rtm)
      deallocate(nnod_local_rlm, nnod_local_rj)
!
      deallocate(nidx_local_rtp, nidx_local_rtm)
      deallocate(nidx_local_rlm, nidx_local_rj)
!
      end subroutine deallocate_sph_gl_parameter
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_gl_bc_param
!
!
      deallocate(nidx_local_rtp_IC)
      deallocate(nidx_local_rtp_OC)
      deallocate(nidx_local_rtp_MT)
      deallocate(ist_idx_local_rtp_IC)
      deallocate(ist_idx_local_rtp_OC)
      deallocate(ist_idx_local_rtp_MT)
!
      deallocate(nidx_local_rtm_IC)
      deallocate(nidx_local_rtm_OC)
      deallocate(nidx_local_rtm_MT)
      deallocate(ist_idx_local_rtm_IC)
      deallocate(ist_idx_local_rtm_OC)
      deallocate(ist_idx_local_rtm_MT)
!
      end subroutine deallocate_sph_gl_bc_param
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_spheric_global_param(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'ndomain_rtp ', ndomain_rtp(1:3)
      write(my_rank+50,*) 'ndomain_rtm ', ndomain_rtm(1:3)
      write(my_rank+50,*) 'ndomain_rlm ', ndomain_rlm(1:2)
      write(my_rank+50,*) 'ndomain_rj ',  ndomain_rj(1:2)
      write(my_rank+50,*) 'ndomain_sph ',  ndomain_sph
!
      write(my_rank+50,*)                                               &
     &   'i, nnod_local_rtp, nidx_local_rtp'
      do i = 1, ndomain_sph
        write(my_rank+50,*) i, nnod_local_rtp(i), nidx_local_rtp(i,1:3)
      end do
!
      write(my_rank+50,*)                                               &
     &   'i, nnod_local_rtm, nidx_local_rtm'
      do i = 1, ndomain_sph
        write(my_rank+50,*) i, nnod_local_rtm(i), nidx_local_rtm(i,1:3)
      end do
!
      write(my_rank+50,*)                                               &
     &   'i, nnod_local_rlm, nidx_local_rlm'
      do i = 1, ndomain_sph
        write(my_rank+50,*) i, nnod_local_rlm(i), nidx_local_rlm(i,1:2)
      end do
!
      write(my_rank+50,*)                                               &
     &   'i, nnod_local_rj, nidx_local_rj'
      do i = 1, ndomain_sph
        write(my_rank+50,*) i, nnod_local_rj(i), nidx_local_rj(i,1:2)
      end do
!
      end subroutine check_spheric_global_param
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_gl_bc_param(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(my_rank+50,*) 'nidx_local_rtm_OC', nidx_local_rtm_OC
      write(my_rank+50,*) 'nidx_local_rtm_IC', nidx_local_rtm_IC
      write(my_rank+50,*) 'nidx_local_rtm_MT', nidx_local_rtm_MT
      write(my_rank+50,*) 'ist_idx_local_rtm_OC', ist_idx_local_rtm_OC
      write(my_rank+50,*) 'ist_idx_local_rtm_IC', ist_idx_local_rtm_IC
      write(my_rank+50,*) 'ist_idx_local_rtm_MT', ist_idx_local_rtm_MT
!
      end subroutine check_sph_gl_bc_param
!
! -----------------------------------------------------------------------
!
      end module m_sph_global_parameter
