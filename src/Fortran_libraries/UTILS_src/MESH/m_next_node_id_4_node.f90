!m_next_node_id_4_node.f90
!      module m_next_node_id_4_node
!
!> @brief Neighbouring node list for each node
!
!      Written by H.Matsui on Oct., 2006
!
!      subroutine allocate_num_next_node(numnod)
!      subroutine allocate_inod_next_node
!
!      subroutine deallocate_inod_next_node
!
!      subroutine check_next_node_id_4_node(my_rank, numnod)
!
      module m_next_node_id_4_node
!
      use m_precision
!
      implicit none
!
      integer (kind=kint) :: ntot_next_nod_4_node
!<   total number of neighbouring node list for each node
      integer (kind=kint) :: nmin_next_nod_4_node
!<   minimum number of neighbouring node for each node
      integer (kind=kint) :: nmax_next_nod_4_node
!<   maximum number of neighbouring node for each node
      integer (kind=kint), allocatable :: nnod_next_4_node(:)
!<   number of neighbouring node list for each node
      integer (kind=kint), allocatable :: inod_next_stack_4_node(:)
!<   end number of neighbouring node list for each node
!
      integer (kind=kint), allocatable :: inod_next_4_node(:)
!<   local node ID of neighbouring node for each node
      integer (kind=kint), allocatable :: iweight_next_4_node(:)
!<   Weighting count for neighbouring node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_num_next_node(numnod)
!
      integer(kind= kint), intent(in) :: numnod
!
      allocate( nnod_next_4_node(numnod) )
      allocate( inod_next_stack_4_node(0:numnod) )
      nmin_next_nod_4_node = 0
      nmax_next_nod_4_node = 0
      nnod_next_4_node = 0
      inod_next_stack_4_node = 0
!
      end subroutine allocate_num_next_node
!
!-----------------------------------------------------------------------
!
      subroutine allocate_inod_next_node
!
      allocate( inod_next_4_node(ntot_next_nod_4_node) )
      allocate( iweight_next_4_node(ntot_next_nod_4_node) )
      inod_next_4_node = 0
      iweight_next_4_node = 0
!
      end subroutine allocate_inod_next_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_inod_next_node
!
      deallocate( nnod_next_4_node )
      deallocate( inod_next_stack_4_node )
!
      deallocate( inod_next_4_node )
      deallocate( iweight_next_4_node )
!
      end subroutine deallocate_inod_next_node
!
!-----------------------------------------------------------------------
!
      subroutine check_next_node_id_4_node(my_rank, numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      integer(kind = kint) :: inod, inum, ist, ied
!
      do inod = 1, numnod
        ist = inod_next_stack_4_node(inod-1) + 1
        ied = inod_next_stack_4_node(inod)
        write(50+my_rank,*) 'next node ID for node inod_next_4_node ',  &
     &                     inod, ist, ied, nnod_next_4_node(inod)
        write(50+my_rank,'(8i10)') inod_next_4_node(ist:ied)
        write(50+my_rank,*) 'iweight_next_4_node'
        write(50+my_rank,'(8i10)') iweight_next_4_node(ist:ied)
      end do
!
      end subroutine check_next_node_id_4_node
!
!-----------------------------------------------------------------------
!
      end module m_next_node_id_4_node
