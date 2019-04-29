!>@file   t_sum_hash.f90
!!@brief  module t_sum_hash
!!
!!@author H. Matsui
!!@date Programmed in May, 2009
!
!>@brief Hash table using sum of local node ID
!!
!!@verbatim
!!      subroutine alloc_sum_hash                                       &
!!     &         (numnod, nele, num_4_ele, nnod_4_edge, h_tbl)
!!      subroutine clear_sum_hash(h_tbl)
!!      subroutine dealloc_sum_hash(h_tbl)
!!@endverbatim
!
      module t_sum_hash
!
      use m_precision
!
      implicit none
!
!>      Structure for hash table
      type sum_hash_tbl
!>        Total number of ID
        integer(kind = kint_gl) :: ntot_id
!>        Maximum address of sum of node ID
        integer(kind = kint_gl) :: iend_hash
!>        Number of items at each level
        integer(kind = kint_gl), allocatable :: num_hash(:)
!>        Stack of items at each level
        integer(kind = kint_gl), allocatable :: istack_hash(:)
!
!>        Total number of list
        integer(kind = kint_gl) :: ntot_list
!>        item addresses
        integer(kind = kint), allocatable :: id_hash(:,:)
!>        item flag
        integer(kind = kint_gl), allocatable :: iflag_hash(:)
      end type sum_hash_tbl
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_sum_hash                                         &
     &         (numnod, nele, num_4_ele, nnod_4_edge, h_tbl)
!
      integer(kind = kint), intent(in) :: numnod, nele, num_4_ele
      integer(kind = kint), intent(in) :: nnod_4_edge
      type(sum_hash_tbl), intent(inout) :: h_tbl
!
!
      h_tbl%ntot_id =   int(numnod*nnod_4_edge, KIND(h_tbl%ntot_id))
      h_tbl%iend_hash = h_tbl%ntot_id
      allocate(h_tbl%num_hash(h_tbl%ntot_id))
      allocate(h_tbl%istack_hash(0:h_tbl%ntot_id))
!
      h_tbl%ntot_list = int(num_4_ele*nele, KIND(h_tbl%iend_hash))
      allocate(h_tbl%id_hash(h_tbl%ntot_list,2) )
      allocate(h_tbl%iflag_hash(h_tbl%ntot_list) )
!
!
      call clear_sum_hash(h_tbl)
!
      end subroutine alloc_sum_hash
!
!------------------------------------------------------------------
!
      subroutine clear_sum_hash(h_tbl)
!
      type(sum_hash_tbl), intent(inout) :: h_tbl
!
!
      h_tbl%num_hash = 0
      h_tbl%istack_hash = 0
      h_tbl%id_hash = 0
      h_tbl%iflag_hash = 0
!
      end subroutine clear_sum_hash
!
!------------------------------------------------------------------
!
      subroutine dealloc_sum_hash(h_tbl)
!
      type(sum_hash_tbl), intent(inout) :: h_tbl
!
!
      deallocate(h_tbl%id_hash, h_tbl%iflag_hash)
      deallocate(h_tbl%num_hash, h_tbl%istack_hash)
!
      end subroutine dealloc_sum_hash
!
!------------------------------------------------------------------
!
      end module t_sum_hash
