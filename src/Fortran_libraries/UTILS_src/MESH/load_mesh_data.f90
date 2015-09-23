!
!      module load_mesh_data
!
!     Written by H. Matsui on July, 2007
!
!      subroutine input_mesh(my_rank)
!      subroutine output_mesh(my_rank)
!
      module load_mesh_data
!
      use m_precision
      use m_machine_parameter
!
      use mesh_IO_select
!
      implicit none
!
      private :: set_mesh_to_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh(my_rank)
!
      use m_geometry_data
!
      integer(kind = kint), intent(in) :: my_rank
!
!       set mesh informations
!
      call sel_read_mesh(my_rank)
      call set_mesh_data
!
      call allocate_ele_geometry_type(ele1)
!
      end subroutine input_mesh
!
! -----------------------------------------------------------------------
!
      subroutine output_mesh(my_rank)
!
      use m_geometry_data
!
      integer(kind = kint), intent(in) :: my_rank
!
!       save mesh information
!
      call set_mesh_to_IO(my_rank)
      call sel_write_mesh_file(my_rank)
!
      end subroutine output_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_mesh_data
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
!
      use set_node_data_4_IO
      use set_element_data_4_IO
      use set_nnod_4_ele_by_type
      use set_comm_table_4_IO
      use set_group_types_4_IO
!
!
      call copy_comm_tbl_type_from_IO(nod_comm)
!
      call copy_node_geometry_from_IO(node1)
      call copy_ele_connect_from_IO(ele1)
!
      call set_3D_nnod_4_sfed_by_ele                                   &
     &   (ele1%nnod_4_ele, surf1%nnod_4_surf, edge1%nnod_4_edge)
!
      call allocate_sph_node_geometry(node1)
!
      call set_nod_grp_type_from_IO(nod_grp1)
      call set_ele_grp_type_from_IO(ele_grp1)
      call set_surf_grp_type_from_IO(sf_grp1)
!
      end subroutine set_mesh_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_to_IO(my_rank)
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
!
      use set_node_data_4_IO
      use set_element_data_4_IO
      use set_comm_table_4_IO
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_comm_tbl_type_to_IO(my_rank, nod_comm)
      call copy_node_geometry_to_IO(node1)
      call copy_ele_connect_to_IO(ele1)
!
      call set_node_grp_type_to_IO(nod_grp1)
      call set_ele_grp_type_to_IO(ele_grp1)
      call set_surface_grp_type_to_IO(sf_grp1)
!
!
      call deallocate_ele_connect_type(ele1)
      call deallocate_node_geometry_type(node1)
!
      call deallocate_grp_type(nod_grp1)
      call deallocate_grp_type(ele_grp1)
      call deallocate_sf_grp_type(sf_grp1)
!
      end subroutine set_mesh_to_IO
!
!   --------------------------------------------------------------------
!
      end module load_mesh_data
