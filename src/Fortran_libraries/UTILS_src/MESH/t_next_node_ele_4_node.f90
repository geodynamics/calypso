!>@file  t_next_node_ele_4_node.f90
!!       module t_next_node_ele_4_node
!!
!!@author H. Matsui
!!@date   Programmed on Nov., 2008
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Neighbouring node and element list for each node
!!
!!@verbatim
!!      subroutine alloc_numele_belonged(numnod, neib_ele)
!!        integer(kind= kint), intent(in) :: numnod
!!        type(element_around_node), intent(inout) :: neib_ele
!!      subroutine alloc_iele_belonged(neib_ele)
!!        type(element_around_node), intent(inout) :: neib_ele
!!      subroutine alloc_num_next_node(numnod, neib_nod)
!!        integer(kind= kint), intent(in) :: numnod
!!        type(next_nod_id_4_nod), intent(inout) :: neib_nod
!!      subroutine alloc_inod_next_node(neib_nod)
!!        type(next_nod_id_4_nod), intent(inout) :: neib_nod
!!
!!      subroutine dealloc_iele_belonged(neib_ele)
!!        type(element_around_node), intent(inout) :: neib_ele
!!      subroutine dealloc_inod_next_node(neib_nod)
!!        type(next_nod_id_4_nod), intent(inout) :: neib_nod
!!
!!      subroutine check_ele_id_4_node_type(my_rank, numnod, neib_ele)
!!        integer(kind = kint), intent(in) :: my_rank, numnod
!!        type(element_around_node), intent(in) :: neib_ele
!!      subroutine check_next_node_id_4_node(my_rank, numnod, neib_nod)
!!        integer(kind = kint), intent(in) :: my_rank, numnod
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!@endverbatim
!
      module t_next_node_ele_4_node
!
      use m_precision
!
      implicit none
!
!>   Structure of belonged element list for each node
      type element_around_node
!>   total number of belonged element for each node
        integer (kind=kint) :: ntot
!>   minimum number of belonged element for each node
        integer (kind=kint) :: nmin
!>   maximum number of belonged element list for each node
        integer (kind=kint) :: nmax
!>   number of belonged element for each node
        integer (kind=kint), allocatable :: nele_4_node(:)
!>   end number of belonged element list for each node
        integer (kind=kint), allocatable :: istack_4_node(:)
!
!>   local element ID of belonged element for each node
        integer (kind=kint), allocatable :: iele_4_node(:)
!>   node ID in belonged element for each node
        integer (kind=kint), allocatable :: iconn_4_node(:)
      end type element_around_node
!
!
!>   Structure of neighbouring node list for each node
      type next_nod_id_4_nod
!>   total number of neighbouring node list for each node
        integer (kind=kint) :: ntot
!>   minimum number of neighbouring node for each node
        integer (kind=kint) :: nmin
!>   maximum number of neighbouring node for each node
        integer (kind=kint) :: nmax
!>   number of neighbouring node list for each node
        integer (kind=kint), allocatable :: nnod_next(:)
!>   end number of neighbouring node list for each node
        integer (kind=kint), allocatable :: istack_next(:)
!
!>   local node ID of neighbouring node for each node
        integer (kind=kint), allocatable :: inod_next(:)
!>   Weighting count for neighbouring node
        integer (kind=kint), allocatable :: iweight_next(:)
      end type next_nod_id_4_nod
!
!>   Structure of neighbouring node and element list for each node
      type next_nod_ele_table
!>   Structure of belonged element list for each node
        type(element_around_node) :: neib_ele
!>   Structure of neighbouring node list for each node
        type(next_nod_id_4_nod) ::   neib_nod
      end type next_nod_ele_table
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_numele_belonged(numnod, neib_ele)
!
      integer(kind= kint), intent(in) :: numnod
      type(element_around_node), intent(inout) :: neib_ele
!
      allocate( neib_ele%nele_4_node(numnod) )
      allocate( neib_ele%istack_4_node(0:numnod) )
!
      if( numnod .gt. 0) neib_ele%nele_4_node = 0
      neib_ele%istack_4_node = 0
!
      end subroutine alloc_numele_belonged
!
!-----------------------------------------------------------------------
!
      subroutine alloc_iele_belonged(neib_ele)
!
      type(element_around_node), intent(inout) :: neib_ele
!
      allocate( neib_ele%iele_4_node(neib_ele%ntot) )
      allocate( neib_ele%iconn_4_node(neib_ele%ntot) )
!
      if (neib_ele%ntot .gt. 0) then
        neib_ele%iele_4_node =  0
        neib_ele%iconn_4_node = 0
      end if
!
      end subroutine alloc_iele_belonged
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_num_next_node(numnod, neib_nod)
!
      integer(kind= kint), intent(in) :: numnod
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
!
      allocate( neib_nod%nnod_next(numnod) )
      allocate( neib_nod%istack_next(0:numnod) )
!
      neib_nod%nmin =        0
      neib_nod%nmax =        0
      if(numnod .gt. 0) neib_nod%nnod_next =   0
      neib_nod%istack_next = 0
!
      end subroutine alloc_num_next_node
!
!-----------------------------------------------------------------------
!
      subroutine alloc_inod_next_node(neib_nod)
!
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
!
      allocate( neib_nod%inod_next(neib_nod%ntot) )
      allocate( neib_nod%iweight_next(neib_nod%ntot) )
!
      if(neib_nod%ntot .gt. 0) then
        neib_nod%inod_next = 0
        neib_nod%iweight_next = 0
      end if
!
      end subroutine alloc_inod_next_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_iele_belonged(neib_ele)
!
      type(element_around_node), intent(inout) :: neib_ele
!
      deallocate( neib_ele%nele_4_node )
      deallocate( neib_ele%istack_4_node )
!
      deallocate( neib_ele%iele_4_node )
      deallocate( neib_ele%iconn_4_node )
!
      end subroutine dealloc_iele_belonged
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_inod_next_node(neib_nod)
!
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
!
      deallocate( neib_nod%nnod_next )
      deallocate( neib_nod%istack_next )
!
      deallocate( neib_nod%inod_next )
      deallocate( neib_nod%iweight_next )
!
      end subroutine dealloc_inod_next_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_ele_id_4_node_type(my_rank, numnod, neib_ele)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      type(element_around_node), intent(in) :: neib_ele
!
      integer(kind = kint) :: inod, inum, ist, ied
!
      do inod = 1, numnod
        ist = neib_ele%istack_4_node(inod-1) + 1
        ied = neib_ele%istack_4_node(inod)
        write(50+my_rank,*) 'element and local index for node ',        &
     &                     inod, ist, ied, neib_ele%nele_4_node(inod)
        do inum = ist, ied
          write(50+my_rank,*)                                           &
     &        neib_ele%iele_4_node(inum), neib_ele%iconn_4_node(inum)
        end do
      end do
!
      end subroutine check_ele_id_4_node_type
!
!-----------------------------------------------------------------------
!
      subroutine check_next_node_id_4_node(my_rank, numnod, neib_nod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      integer(kind = kint) :: inod, ist, ied
!
      do inod = 1, numnod
        ist = neib_nod%istack_next(inod-1) + 1
        ied = neib_nod%istack_next(inod)
        write(50+my_rank,*) 'next node ID for node nnod_next ',         &
     &                     inod, ist, ied, neib_nod%nnod_next(inod)
        write(50+my_rank,'(8i16)') neib_nod%nnod_next(ist:ied)
        write(50+my_rank,*) 'iweight_next'
        write(50+my_rank,'(8i16)') neib_nod%iweight_next(ist:ied)
      end do
!
      end subroutine check_next_node_id_4_node
!
!-----------------------------------------------------------------------
!
      end module t_next_node_ele_4_node
 