!m_next_nod_id_nod_w_hang.f90
!      module m_next_nod_id_nod_w_hang
!
!> @brief Neighbouring node list for each node
!
!      Written by H.Matsui on Oct., 2006
!
!      subroutine allocate_num_next_nod_w_hang(numnod)
!      subroutine allocate_inod_next_nod_w_hang
!
!      subroutine deallocate_next_nod_w_hang
!
!      subroutine check_next_node_id_nod_hang(my_rank, numnod)
!
      module m_next_nod_id_nod_w_hang
!
      use m_precision
!
      implicit none
!
      integer (kind=kint) :: ntot_next_node_hanged
!<   total number of neighbouring node list for each node
      integer (kind=kint) :: nmin_next_node_hanged
!<   minimum number of neighbouring node for each node
      integer (kind=kint) :: nmax_next_node_hanged
!<   maximum number of neighbouring node for each node
      integer (kind=kint), allocatable :: nnod_next_node_hanged(:)
!<   number of neighbouring node list for each node
      integer (kind=kint), allocatable :: istack_next_node_hanged(:)
!<   end number of neighbouring node list for each node
!
      integer (kind=kint), allocatable :: inod_next_node_hanged(:)
!<   local node ID of neighbouring node for each node
      integer (kind=kint), allocatable :: iweight_next_hanged(:)
!<   Weighting count for neighbouring node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_num_next_nod_w_hang(numnod)
!
      integer(kind= kint), intent(in) :: numnod
!
      allocate( nnod_next_node_hanged(numnod) )
      allocate( istack_next_node_hanged(0:numnod) )
      nmin_next_node_hanged = 0
      nmax_next_node_hanged = 0
      nnod_next_node_hanged = 0
      istack_next_node_hanged = 0
!
      end subroutine allocate_num_next_nod_w_hang
!
!-----------------------------------------------------------------------
!
      subroutine allocate_inod_next_nod_w_hang
!
      allocate( inod_next_node_hanged(ntot_next_node_hanged) )
      allocate( iweight_next_hanged(ntot_next_node_hanged) )
      inod_next_node_hanged = 0
      iweight_next_hanged = 0
!
      end subroutine allocate_inod_next_nod_w_hang
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_next_nod_w_hang
!
      deallocate( nnod_next_node_hanged )
      deallocate( istack_next_node_hanged )
!
      deallocate( inod_next_node_hanged )
      deallocate( iweight_next_hanged )
!
      end subroutine deallocate_next_nod_w_hang
!
!-----------------------------------------------------------------------
!
      subroutine check_next_node_id_nod_hang(my_rank, numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      integer(kind = kint) :: inod, ist, ied
!
      do inod = 1, numnod
        ist = istack_next_node_hanged(inod-1) + 1
        ied = istack_next_node_hanged(inod)
        write(50+my_rank,*)                                             &
     &                 'next node ID for node inod_next_node_hanged ',  &
     &                  inod, ist, ied, nnod_next_node_hanged(inod)
        write(50+my_rank,'(8i10)') inod_next_node_hanged(ist:ied)
        write(50+my_rank,*) 'iweight_next_hanged'
        write(50+my_rank,'(8i10)') iweight_next_hanged(ist:ied)
      end do
!
      end subroutine check_next_node_id_nod_hang
!
!-----------------------------------------------------------------------
!
      end module m_next_nod_id_nod_w_hang
