!
!      module m_near_node_id_4_node
!
!      Written by H. Matsui on Aug., 2006
!
!     subroutine allocate_num_4_near_nod(numnod)
!     subroutine allocate_num_4_near_nod_w(numnod)
!
!     subroutine allocate_near_node
!     subroutine allocate_near_node_w
!
!     subroutine deallocate_num_4_near_nod
!     subroutine deallocate_num_4_near_nod_w
!
!     subroutine deallocate_near_node
!     subroutine deallocate_near_node_w
!
!      subroutine check_near_nod_4_node(my_rank,numnod)
!      subroutine check_near_nod_4_nod_sorted(my_rank,numnod)
!
!
      module m_near_node_id_4_node
!
      use m_precision
!
      implicit none
!
!     element informations surrounded nodes for each node
!
      integer(kind = kint) :: ntot_nod_near_nod
      integer(kind = kint) :: nmax_nod_near_nod, nmin_nod_near_nod
      integer(kind = kint), allocatable :: nnod_near_nod(:)
      integer(kind = kint), allocatable :: inod_stack_near_nod(:)
      integer(kind = kint), allocatable :: inod_near_nod(:)
      integer(kind = kint), allocatable :: idist_from_center(:)
      integer(kind = kint), allocatable :: iweight_node(:)
!
!     element informations surrounded nodes for each node
!
      integer(kind = kint) :: ntot_nod_near_nod_w
      integer(kind = kint) :: nmax_nod_near_nod_w, nmin_nod_near_nod_w
      integer(kind = kint), allocatable :: nnod_near_nod_w(:)
      integer(kind = kint), allocatable :: inod_stack_near_nod_w(:)
      integer(kind = kint), allocatable :: inod_near_nod_w(:)
      integer(kind = kint), allocatable :: idist_from_center_w(:)
      integer(kind = kint), allocatable :: iweight_node_w(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_nod(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate(nnod_near_nod(numnod))
      allocate(inod_stack_near_nod(0:numnod))
!
      nmax_nod_near_nod = 0
      nmin_nod_near_nod = 0
      nnod_near_nod = 0
      inod_stack_near_nod = 0
!
      end subroutine allocate_num_4_near_nod
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_nod_w(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate(nnod_near_nod_w(numnod))
      allocate(inod_stack_near_nod_w(0:numnod))
!
      nmax_nod_near_nod_w = 0
      nmin_nod_near_nod_w = 0
      nnod_near_nod_w = 0
      inod_stack_near_nod_w = 0
!
      end subroutine allocate_num_4_near_nod_w
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_node
!
      allocate(inod_near_nod(ntot_nod_near_nod))
      allocate(idist_from_center(ntot_nod_near_nod))
      allocate(iweight_node(ntot_nod_near_nod))
      inod_near_nod = 0
      idist_from_center = 0
      iweight_node = 0
!
      end subroutine allocate_near_node
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_node_w
!
      allocate(inod_near_nod_w(ntot_nod_near_nod_w))
      allocate(idist_from_center_w(ntot_nod_near_nod_w))
      allocate(iweight_node_w(ntot_nod_near_nod_w))
      inod_near_nod_w = 0
      idist_from_center_w = 0
      iweight_node_w = 0
!
      end subroutine allocate_near_node_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_node
!
      deallocate(nnod_near_nod)
      deallocate(inod_stack_near_nod)
!
      end subroutine deallocate_num_4_near_node
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_nod_w
!
      deallocate(nnod_near_nod_w)
      deallocate(inod_stack_near_nod_w)
!
      end subroutine deallocate_num_4_near_nod_w
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_node
!
      deallocate(inod_near_nod)
      deallocate(idist_from_center)
      deallocate(iweight_node)
!
      end subroutine deallocate_near_node
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_node_w
!
      deallocate(inod_near_nod_w)
      deallocate(idist_from_center_w)
      deallocate(iweight_node_w)
!
      end subroutine deallocate_near_node_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_near_nod_4_node(my_rank,numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      integer(kind = kint) :: inod, inum, ist, ied
!
      write(50+my_rank,*) 'max and min. of near node ID for node ',     &
     &                    nmax_nod_near_nod, nmin_nod_near_nod
      do inod = 1, numnod
        ist = inod_stack_near_nod(inod-1) + 1
        ied = inod_stack_near_nod(inod)
        write(50+my_rank,*) 'near node ID for node nnod_near_nod',      &
     &                     inod, ist, ied, nnod_near_nod(inod)
        write(50+my_rank,'(8i10)') inod_near_nod(ist:ied)
        write(50+my_rank,*) 'iweight_node '
        write(50+my_rank,'(8i10)') iweight_node(ist:ied)
        write(50+my_rank,*) 'idist_from_center '
        write(50+my_rank,'(8i10)') idist_from_center(ist:ied)
      end do
!
      end subroutine check_near_nod_4_node
!
! -----------------------------------------------------------------------
!
      subroutine check_near_nod_4_nod_sorted(my_rank,numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      integer(kind = kint) :: inod, inum, ist, ied
!
      write(50+my_rank,*) 'max and min. of near node ID for node ',     &
     &                    nmax_nod_near_nod, nmin_nod_near_nod
      do inod = 1, numnod
        ist = inod_stack_near_nod(inod-1) + 1
        ied = inod_stack_near_nod(inod)
        write(50+my_rank,*) 'near node ID for node (sorted)',           &
     &                     inod, ist, ied, nnod_near_nod(inod)
        write(50+my_rank,'(8i10)') inod_near_nod(ist:ied)
      end do
!
      end subroutine check_near_nod_4_nod_sorted
!
! -----------------------------------------------------------------------
!
      end module m_near_node_id_4_node
