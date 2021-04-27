!>@file   load_element_mesh_data.f90
!!@brief  module load_element_mesh_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy FEM mesh data from IO structure
!!
!!@verbatim
!!      subroutine set_surface_mesh_from_IO(ele, surf, surf_mesh_IO)
!!      subroutine set_edge_mesh_from_IO(ele, surf, edge, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(element_data), intent(inout) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) :: edge
!!        type(communication_table), intent(inout) :: ele_comm
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine set_surface_mesh_to_IO(ele, surf, surf_mesh_IO)
!!      subroutine set_edge_mesh_to_IO                                  &
!!     &         (ele, surf, edge, edge_mesh_IO)
!!@endverbatim
!
      module load_element_mesh_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_mesh_data
      use t_read_mesh_data
      use t_file_IO_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_surface_mesh_from_IO(ele, surf, surf_mesh_IO)
!
      use set_surface_data_4_IO
      use set_surface_data
!
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
!
      call copy_surf_connect_from_IO                                    &
     &   (surf_mesh_IO%ele, surf_mesh_IO%sfed, surf, ele%numele)
      call dealloc_surface_mesh_IO(surf_mesh_IO)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                    'set_surf_rotation_flag after load'
      call set_surf_rotation_flag(ele%numele, surf%numsurf,             &
     &    ele%nnod_4_ele, surf%nnod_4_surf, ele%ie, surf%ie_surf,       &
     &    surf%isf_4_ele, surf%isf_rot_ele)
!
      end subroutine set_surface_mesh_from_IO
!
!  ---------------------------------------------------------------------
!
      subroutine set_edge_mesh_from_IO(ele, surf, edge, edge_mesh_IO)
!
      use set_edge_data_4_IO
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(edge_data), intent(inout) :: edge
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      call copy_edge_connect_from_IO                                    &
     &   (edge_mesh_IO%ele, edge_mesh_IO%sfed,                          &
     &    edge, ele%numele, surf%numsurf)
      call dealloc_edge_mesh_IO(edge_mesh_IO)
!
      end subroutine set_edge_mesh_from_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_surface_mesh_to_IO(ele, surf, surf_mesh_IO)
!
      use set_surface_data_4_IO
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      call empty_comm_table(surf_mesh_IO%comm)
      call copy_surf_connect_to_IO(surf, ele%numele,                    &
     &    surf_mesh_IO%ele, surf_mesh_IO%sfed)
!
!      call copy_surf_geometry_to_IO                                    &
!     &   (surf, surf_mesh_IO%node, surf_mesh_IO%sfed)
!
      end subroutine set_surface_mesh_to_IO
!
!  ---------------------------------------------------------------------
!
      subroutine set_edge_mesh_to_IO                                    &
     &         (ele, surf, edge, edge_mesh_IO)
!
      use set_edge_data_4_IO
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      call empty_comm_table(edge_mesh_IO%comm)
      call copy_edge_connect_to_IO(edge, ele%numele, surf%numsurf,      &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed)
!
!      call copy_edge_geometry_to_IO                                    &
!     &   (edge, edge_mesh_IO%node, edge_mesh_IO%sfed)
!
      end subroutine set_edge_mesh_to_IO
!
!  ---------------------------------------------------------------------
!
      end module load_element_mesh_data
