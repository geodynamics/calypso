!
!      module m_near_element_id_4_node
!
!      Written by H. Matsui on Aug., 2006
!
!     subroutine allocate_num_4_near_ele(numnod)
!     subroutine allocate_num_4_near_ele_w(numnod)
!
!     subroutine allocate_near_element
!     subroutine allocate_near_element_w
!
!     subroutine deallocate_num_4_near_ele
!     subroutine deallocate_num_4_near_ele_w
!
!     subroutine deallocate_near_element
!     subroutine deallocate_near_element_w
!
!      subroutine check_near_ele_4_node(my_rank,numnod)
!
      module m_near_element_id_4_node
!
      use m_precision
!
      implicit none
!
!     element informations surrounded elements for each node
!
      integer(kind = kint) :: ntot_ele_near_nod
      integer(kind = kint) :: nmax_ele_near_nod, nmin_ele_near_nod
      integer(kind = kint), allocatable :: nele_near_nod(:)
      integer(kind = kint), allocatable :: iele_stack_near_nod(:)
      integer(kind = kint), allocatable :: iele_near_nod(:)
!
!     element informations surrounded elements for each node
!
      integer(kind = kint) :: ntot_ele_near_nod_w
      integer(kind = kint) :: nmax_ele_near_nod_w, nmin_ele_near_nod_w
      integer(kind = kint), allocatable :: nele_near_nod_w(:)
      integer(kind = kint), allocatable :: iele_stack_near_nod_w(:)
      integer(kind = kint), allocatable :: iele_near_nod_w(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_ele(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate(nele_near_nod(numnod))
      allocate(iele_stack_near_nod(0:numnod))
!
      nmax_ele_near_nod = 0
      nmin_ele_near_nod = 0
      nele_near_nod = 0
      iele_stack_near_nod = 0
!
      end subroutine allocate_num_4_near_ele
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_4_near_ele_w(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate(nele_near_nod_w(numnod))
      allocate(iele_stack_near_nod_w(0:numnod))
!
      nmax_ele_near_nod_w = 0
      nmin_ele_near_nod_w = 0
      nele_near_nod_w = 0
      iele_stack_near_nod_w = 0
!
      end subroutine allocate_num_4_near_ele_w
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_element
!
      allocate(iele_near_nod(ntot_ele_near_nod))
      iele_near_nod = 0
!
      end subroutine allocate_near_element
!
! -----------------------------------------------------------------------
!
      subroutine allocate_near_element_w
!
      allocate(iele_near_nod_w(ntot_ele_near_nod_w))
      iele_near_nod_w = 0
!
      end subroutine allocate_near_element_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_ele
!
      deallocate(nele_near_nod)
      deallocate(iele_stack_near_nod)
!
      end subroutine deallocate_num_4_near_ele
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_4_near_ele_w
!
      deallocate(nele_near_nod_w)
      deallocate(iele_stack_near_nod_w)
!
      end subroutine deallocate_num_4_near_ele_w
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_element
!
      deallocate(iele_near_nod)
!
      end subroutine deallocate_near_element
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_element_w
!
      deallocate(iele_near_nod_w)
!
      end subroutine deallocate_near_element_w
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_near_ele_4_node(my_rank,numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'max and min. of near element for node ',     &
     &                    nmax_ele_near_nod, nmin_ele_near_nod
      do inod = 1, numnod
        ist = iele_stack_near_nod(inod-1) + 1
        ied = iele_stack_near_nod(inod)
        write(50+my_rank,*) 'near element ID for node ',                &
     &                     inod, ist, ied, nele_near_nod(inod)
        write(50+my_rank,'(8i10)') iele_near_nod(ist:ied)
      end do
!
      end subroutine check_near_ele_4_node
!
! -----------------------------------------------------------------------
!
      end module m_near_element_id_4_node
