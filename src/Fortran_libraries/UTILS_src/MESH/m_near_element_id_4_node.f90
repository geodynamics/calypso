!
!      module m_near_element_id_4_node
!
!      Written by H. Matsui on Aug., 2006
!
!      subroutine deallocate_near_element
!
      module m_near_element_id_4_node
!
      use m_precision
      use t_near_mesh_id_4_node
!
      implicit none
!
!> structure of surrounded element for each node
        type(near_mesh) :: near_ele1_tbl
!> structure of surrounded element for each node
        type(near_mesh) :: near_ele1_wide
!
!
!> structure of surrounded surface for each node
        type(near_mesh), save :: near_surf1_tbl
!> structure of surrounded surface for each node
        type(near_mesh), save :: near_surf1_wide
!
!
!> structure of surrounded edge for each node
        type(near_mesh), save :: near_edge_tbl
!> structure of surrounded edge for each node
        type(near_mesh), save :: near_edge_wide
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_near_element
!
!
      call dealloc_num_4_near_node(near_ele1_tbl)
      call dealloc_near_node(near_ele1_tbl)
!
      end subroutine deallocate_near_element
!
!-----------------------------------------------------------------------
!
      end module m_near_element_id_4_node
