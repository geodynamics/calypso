!
!      module m_element_id_4_node
!
!> @brief Belonged element list for each node
!
!      Written by H.Matsui on Oct., 2006
!
!      subroutine allocate_numele_belonged(numnod)
!      subroutine allocate_iele_belonged
!
!      subroutine deallocate_iele_belonged
!
!      subroutine check_element_id_4_node(my_rank, numnod)
!
      module m_element_id_4_node
!
      use m_precision
!
      implicit none
!
      integer (kind=kint) :: ntot_ele_4_node
!<   total number of belonged element list for each node
      integer (kind=kint) :: nmin_ele_4_node
!<   minimum number of belonged element for each node
      integer (kind=kint) :: nmax_ele_4_node
!<   maximum number of belonged element for each node
      integer (kind=kint), allocatable :: nele_4_node(:)
!<   number of belonged element for each node
      integer (kind=kint), allocatable :: iele_stack_4_node(:)
!<   end number of belonged element list for each node
!
      integer (kind=kint), allocatable :: iele_4_node(:)
!<   local element ID of belonged element for each node
      integer (kind=kint), allocatable :: iconn_4_node(:)
!<   node ID in belonged element for each node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_numele_belonged(numnod)
!
      integer(kind= kint), intent(in) :: numnod
!
      allocate( nele_4_node(numnod) )
      allocate( iele_stack_4_node(0:numnod) )
      nele_4_node = 0
      iele_stack_4_node = 0
!
      end subroutine allocate_numele_belonged
!
!-----------------------------------------------------------------------
!
      subroutine allocate_iele_belonged
!
      allocate( iele_4_node(ntot_ele_4_node) )
      allocate( iconn_4_node(ntot_ele_4_node) )
      iele_4_node = 0
      iconn_4_node = 0
!
      end subroutine allocate_iele_belonged
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_iele_belonged
!
      deallocate( nele_4_node )
      deallocate( iele_stack_4_node )
!
      deallocate( iele_4_node )
      deallocate( iconn_4_node )
!
      end subroutine deallocate_iele_belonged
!
!-----------------------------------------------------------------------
!
      subroutine check_element_id_4_node(my_rank, numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      integer(kind = kint) :: inod, inum, ist, ied
!
      do inod = 1, numnod
        ist = iele_stack_4_node(inod-1) + 1
        ied = iele_stack_4_node(inod)
        write(50+my_rank,*) 'element and local index for node ',        &
     &                     inod, ist, ied, nele_4_node(inod)
        do inum = ist, ied
          write(50+my_rank,*) iele_4_node(inum), iconn_4_node(inum)
        end do
      end do
!
      end subroutine check_element_id_4_node
!
!-----------------------------------------------------------------------
!
      end module m_element_id_4_node
