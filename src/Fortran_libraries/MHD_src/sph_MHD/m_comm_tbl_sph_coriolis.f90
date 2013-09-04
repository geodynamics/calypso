!>@file   m_comm_tbl_sph_coriolis.f90
!!@brief  module m_comm_tbl_sph_coriolis
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief  Communication table for spectr data
!!        to evaluate Coriolils term
!!
!!@verbatim
!!      subroutine alloc_comm_neib_sph_coriolis
!!      subroutine alloc_comm_item_sph_coriolis
!!      subroutine dealloc_comm_item_sph_coriolis
!!
!!      subroutine alloc_comm_work_sph_coriolis(nri)
!!      subroutine dealloc_comm_work_sph_coriolis
!!
!!      subroutine check_comm_tbl_sph_cor(my_rank)
!!@endverbatim
!
      module m_comm_tbl_sph_coriolis
!
      use m_precision
!
      implicit none
!
!
!>     number of neighboring domain to send
      integer(kind = kint) :: nneib_send_cor
!>     total number of export mode
      integer(kind = kint) :: ntot_send_cor
!>     neighboring pe id to export
      integer(kind = kint), allocatable :: ip_send_cor(:)
!>     export mode end point for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: istack_send_cor(:)
!>     local id for export mode                     (i-th)
      integer(kind = kint), allocatable :: idx_send_cor(:)
!
!>     number of neighboring domain to receive
      integer(kind = kint) :: nneib_recv_cor
!>     total number of import mode 
      integer(kind = kint) :: ntot_recv_cor
!>     neighboring pe id to import
      integer(kind = kint), allocatable :: ip_recv_cor(:)
!>     import mode end point for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: istack_recv_cor(:)
!>     local id for import mode                     (i-th)
      integer(kind = kint), allocatable :: idx_recv_cor(:)
!
!  work area
!
!>      integer work area to send
      integer(kind = kint), allocatable :: isend_sph_cor(:)
!>      integer work area to receive
      integer(kind = kint), allocatable :: irecv_sph_cor(:)
!>      real work area to send
      real(kind = kreal), allocatable :: send_sph_cor(:)
!>      real work area to receive
      real(kind = kreal), allocatable :: recv_sph_cor(:)
!
!>      status flag for send
      integer, allocatable :: sta1_cor(:,:)
!>      status flag for receive
      integer, allocatable :: sta2_cor(:,:)
!>      request flag for send
      integer, allocatable :: req1_cor(:  )
!>      request flag for receive
      integer, allocatable :: req2_cor(:  )
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_comm_neib_sph_coriolis
!
!
      allocate( ip_send_cor(nneib_send_cor) )
      allocate( ip_recv_cor(nneib_recv_cor) )
      allocate( istack_send_cor(0:nneib_send_cor) )
      allocate( istack_recv_cor(0:nneib_recv_cor) )
!
      if(nneib_send_cor .gt. 0) ip_send_cor = 0
      if(nneib_recv_cor .gt. 0) ip_recv_cor = 0
      istack_send_cor = 0
      istack_recv_cor = 0
!
      end subroutine alloc_comm_neib_sph_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine alloc_comm_item_sph_coriolis
!
!
      allocate( idx_send_cor(ntot_send_cor) )
      allocate( idx_recv_cor(ntot_recv_cor) )
      if(ntot_send_cor .gt. 0) idx_send_cor = 0
      if(ntot_recv_cor .gt. 0) idx_recv_cor = 0
!
      end subroutine alloc_comm_item_sph_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_comm_item_sph_coriolis
!
!
      deallocate( idx_send_cor, idx_recv_cor )
      deallocate( ip_send_cor, ip_recv_cor )
      deallocate( istack_send_cor, istack_recv_cor )
!
      end subroutine dealloc_comm_item_sph_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_comm_work_sph_coriolis(nri)
!
      use m_parallel_var_dof
!
      integer(kind = kint), intent(in) :: nri
!
!
      allocate( isend_sph_cor(ntot_send_cor) )
      allocate( irecv_sph_cor(ntot_recv_cor) )
      allocate( send_sph_cor(6*ntot_send_cor*nri) )
      allocate( recv_sph_cor(6*ntot_recv_cor*nri) )
      isend_sph_cor = 0
      irecv_sph_cor = 0
      send_sph_cor = 0.0d0
      recv_sph_cor = 0.0d0
!
      allocate( req1_cor(nneib_send_cor) )
      allocate( req2_cor(nneib_recv_cor) )
      allocate( sta1_cor(MPI_STATUS_SIZE,nneib_send_cor) )
      allocate( sta2_cor(MPI_STATUS_SIZE,nneib_recv_cor) )
!
      end subroutine alloc_comm_work_sph_coriolis
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_comm_work_sph_coriolis
!
!
      deallocate( isend_sph_cor, irecv_sph_cor )
      deallocate( send_sph_cor, recv_sph_cor )
      deallocate( req1_cor, req2_cor )
      deallocate( sta1_cor, sta2_cor )
!
      end subroutine dealloc_comm_work_sph_coriolis
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_comm_tbl_sph_cor(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i
!
!
      write(50+my_rank,*) 'nneib_send_cor', nneib_send_cor
      write(50+my_rank,*) 'ip_send_cor', ip_send_cor
      write(50+my_rank,*) 'istack_send_cor',                     &
     &                     istack_send_cor
      do i = 1, ntot_send_cor
        write(50+my_rank,*) i, idx_send_cor(i)
      end do
!
      write(50+my_rank,*) 'nneib_recv_cor', nneib_recv_cor
      write(50+my_rank,*) 'ip_recv_cor',  ip_recv_cor
      write(50+my_rank,*) 'istack_recv_cor',                    &
     &                     istack_recv_cor
      do i = 1, ntot_recv_cor
        write(50+my_rank,*) i, idx_recv_cor(i)
      end do
!
      end subroutine check_comm_tbl_sph_cor
!
! -----------------------------------------------------------------------
!
      end module m_comm_tbl_sph_coriolis
