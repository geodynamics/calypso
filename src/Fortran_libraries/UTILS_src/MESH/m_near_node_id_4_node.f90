!
!      module m_near_node_id_4_node
!
!      Written by H. Matsui on Aug., 2006
!
!      subroutine deallocate_near_node
!
!      subroutine check_near_nod_4_node(my_rank,numnod)
!      subroutine check_near_nod_4_nod_sorted(my_rank,numnod)
!
!
      module m_near_node_id_4_node
!
      use m_precision
      use t_near_mesh_id_4_node
!
      implicit none
!
!> structure of surrounded node for each node
      type(near_mesh), save :: near_node1_tbl
!> structure of surrounded node for each node
      type(near_mesh), save :: near_node1_wide
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_near_node
!
!
      call dealloc_near_node(near_node1_tbl)
      call dealloc_num_4_near_node(near_node1_tbl)
!
      end subroutine deallocate_near_node
!
! -----------------------------------------------------------------------
!
      subroutine check_near_nod_4_node(my_rank,numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
!
      write(50+my_rank,*) 'max and min. of near node ID for node '
      call check_near_nodes_list(my_rank, numnod, near_node1_tbl)
!
      end subroutine check_near_nod_4_node
!
! -----------------------------------------------------------------------
!
      subroutine check_near_nod_4_nod_sorted(my_rank,numnod)
!
      integer(kind = kint), intent(in) :: my_rank, numnod
!
      write(50+my_rank,*) 'max and min. of near node ID for node '
      call check_near_elements(my_rank, numnod, near_node1_tbl)
!
      end subroutine check_near_nod_4_nod_sorted
!
! -----------------------------------------------------------------------
!
      end module m_near_node_id_4_node
