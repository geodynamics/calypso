!>@file   load_element_mesh_data.f90
!!@brief  module load_element_mesh_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy FEM mesh data from IO structure
!!
!!@verbatim
!!      subroutine load_element_surface_edge                            &
!!     &         (mesh_file, mesh, ele_mesh, ele_mesh_IO)
!!      subroutine output_element_surface_edge                          &
!!     &         (mesh_file, mesh, ele_mesh, ele_mesh_IO)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!
!!      subroutine set_ele_comm_tbl_from_IO                             &
!!     &         (ele, ele_comm, ele_mesh_IO)
!!      subroutine set_surface_mesh_from_IO                             &
!!     &         (ele, surf, surf_comm, surf_mesh_IO)
!!      subroutine set_edge_mesh_from_IO                                &
!!     &         (ele, surf, edge, edge_comm, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(element_data), intent(inout) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) :: edge
!!        type(communication_table), intent(inout) :: ele_comm
!!        type(communication_table), intent(inout) :: surf_comm
!!        type(communication_table), intent(inout) :: edge_comm
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine set_ele_comm_tbl_to_IO(ele, ele_comm, ele_mesh_IO)
!!      subroutine set_surface_mesh_to_IO                               &
!!     &         (ele, surf, surf_comm, surf_mesh_IO)
!!      subroutine set_edge_mesh_to_IO                                  &
!!     &         (ele, surf, edge, edge_comm, edge_mesh_IO)
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine load_element_surface_edge                              &
     &         (mesh_file, mesh, ele_mesh, ele_mesh_IO)
!
      use element_mesh_IO_select
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(inout) :: ele_mesh
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      call sel_read_ele_mesh                                            &
     &   (mesh_file, my_rank_IO, ele_mesh_IO, ierr)
      call set_ele_comm_tbl_from_IO                                     &
     &   (mesh%ele, ele_mesh%ele_comm, ele_mesh_IO)
!
!
      call sel_read_surf_mesh                                           &
     &   (mesh_file, my_rank_IO, ele_mesh_IO, ierr)
      call set_surface_mesh_from_IO                                     &
     &   (mesh%ele, ele_mesh%surf, ele_mesh%surf_comm, ele_mesh_IO)
!
!
      call sel_read_edge_mesh                                           &
     &   (mesh_file, my_rank_IO, ele_mesh_IO, ierr)
      call set_edge_mesh_from_IO(mesh%ele, ele_mesh%surf,               &
     &    ele_mesh%edge, ele_mesh%edge_comm, ele_mesh_IO)
!
      end subroutine load_element_surface_edge
!
!  ---------------------------------------------------------------------
!
      subroutine output_element_surface_edge                            &
     &         (mesh_file, mesh, ele_mesh, ele_mesh_IO)
!
      use element_mesh_IO_select
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      call set_ele_comm_tbl_to_IO                                       &
     &   (mesh%ele, ele_mesh%ele_comm, ele_mesh_IO)
      call sel_write_ele_mesh_file                                      &
     &   (mesh_file, my_rank_IO, ele_mesh_IO)
!
!
      call set_surface_mesh_to_IO                                       &
     &   (mesh%ele, ele_mesh%surf, ele_mesh%surf_comm, ele_mesh_IO)
      call sel_write_surf_mesh_file                                     &
     &   (mesh_file, my_rank_IO, ele_mesh_IO)
!
!
      call set_edge_mesh_to_IO(mesh%ele, ele_mesh%surf, ele_mesh%edge,  &
     &   ele_mesh%edge_comm, ele_mesh_IO)
      call sel_write_edge_mesh_file                                     &
     &   (mesh_file, my_rank_IO, ele_mesh_IO)
!
      end subroutine output_element_surface_edge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_comm_tbl_from_IO                               &
     &         (ele, ele_comm, ele_mesh_IO)
!
      use set_surface_data_4_IO
!
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(inout) :: ele_comm
!
!
!       Subsittuiton of const_ele_comm_table
      call copy_comm_tbl_type(ele_mesh_IO%comm, ele_comm)
!
!       Subsittuiton of set_center_of_element
!          and s_int_volume_of_domain
!      call copy_ele_geometry_from_IO                                   &
!     &   (ele_mesh_IO%node, ele_mesh_IO%sfed, ele)
!
!      call position_2_sph( ele%numele, ele%x_ele,                      &
!     &    ele%r_ele, ele%theta_ele,   ele%phi_ele,                     &
!     &    ele%ar_ele, ele%s_ele, ele%as_ele)
!
      end subroutine set_ele_comm_tbl_from_IO
!
!  ---------------------------------------------------------------------
!
      subroutine set_surface_mesh_from_IO                               &
     &         (ele, surf, surf_comm, surf_mesh_IO)
!
      use set_surface_data_4_IO
      use set_surface_data
!
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
      type(communication_table), intent(inout) :: surf_comm
!
!
!       Subsittuiton of const_surf_comm_table
      call copy_comm_tbl_type(surf_mesh_IO%comm, surf_comm)
!
!       Subsittuiton of construct_surface_data 
!            and const_global_surface_id
      call copy_surf_connect_from_IO                                    &
     &   (surf_mesh_IO%ele, surf_mesh_IO%sfed, surf, ele%numele)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                    'set_surf_rotation_flag after load'
      call set_surf_rotation_flag(ele%numele, surf%numsurf,             &
     &    ele%nnod_4_ele, surf%nnod_4_surf, ele%ie, surf%ie_surf,       &
     &    surf%isf_4_ele, surf%isf_rot_ele)
!
!       Subsittuiton of set_center_of_surface
!          and int_normal_4_all_surface
!      call copy_surf_geometry_from_IO                                  &
!     &   (surf_mesh_IO%node, surf_mesh_IO%sfed, surf)
!
!      call position_2_sph(surf%numsurf, surf%x_surf,                   &
!     &    surf%r_surf, surf%theta_surf, surf%phi_surf,                 &
!     &    surf%ar_surf, surf%s_surf, surf%as_surf)
!
      end subroutine set_surface_mesh_from_IO
!
!  ---------------------------------------------------------------------
!
      subroutine set_edge_mesh_from_IO                                  &
     &         (ele, surf, edge, edge_comm, edge_mesh_IO)
!
      use set_edge_data_4_IO
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(edge_data), intent(inout) :: edge
      type(communication_table), intent(inout) :: edge_comm
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
!       Subsittuiton of const_surf_comm_table
      call copy_comm_tbl_type(edge_mesh_IO%comm, edge_comm)
!
!       Subsittuiton of construct_edge_data
!            and const_global_edge_id
      call copy_edge_connect_from_IO                                    &
     &   (edge_mesh_IO%ele, edge_mesh_IO%sfed,                          &
     &    edge, ele%numele, surf%numsurf)
!
!       Subsittuiton of set_center_of_edge
!          and s_int_edge_vector
!      call copy_edge_geometry_from_IO                                  &
!     &   (edge_mesh_IO%node, edge_mesh_IO%sfed, edge)
!
!      call position_2_sph(edge%numedge, edge%x_edge,                   &
!     &    edge%r_edge, edge%theta_edge, edge%phi_edge,                 &
!     &    edge%ar_edge, edge%s_edge, edge%as_edge)
!
      end subroutine set_edge_mesh_from_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_comm_tbl_to_IO(ele, ele_comm, ele_mesh_IO)
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: ele_comm
!
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_geometry_to_IO'
      call copy_comm_tbl_type(ele_comm, ele_mesh_IO%comm)
!
!      call copy_ele_geometry_to_IO                                     &
!     &   (ele, ele_mesh_IO%node, ele_mesh_IO%sfed)
!
      end subroutine set_ele_comm_tbl_to_IO
!
!  ---------------------------------------------------------------------
!
      subroutine set_surface_mesh_to_IO                                 &
     &         (ele, surf, surf_comm, surf_mesh_IO)
!
      use set_surface_data_4_IO
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(communication_table), intent(in) :: surf_comm
!
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      call copy_comm_tbl_type(surf_comm, surf_mesh_IO%comm)
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
     &         (ele, surf, edge, edge_comm, edge_mesh_IO)
!
      use set_edge_data_4_IO
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: edge_comm
!
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      call copy_comm_tbl_type(edge_comm, edge_mesh_IO%comm)
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
