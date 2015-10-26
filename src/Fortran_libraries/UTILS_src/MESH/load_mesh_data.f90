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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine input_mesh(my_rank)
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
!
      use set_nnod_4_ele_by_type
      use set_mesh_types
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!       set mesh informations
      call sel_read_mesh(my_rank)
!
      call set_mesh_geometry_data(nod_comm, node1, ele1)
      call set_grp_data_from_IO(nod_grp1, ele_grp1, sf_grp1)
!
      call set_3D_nnod_4_sfed_by_ele                                   &
     &   (ele1%nnod_4_ele, surf1%nnod_4_surf, edge1%nnod_4_edge)
!
      call allocate_ele_geometry_type(ele1)
!
      end subroutine input_mesh
!
! -----------------------------------------------------------------------
!
      subroutine output_mesh(my_rank)
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
!
      use set_mesh_types
      use set_group_types_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_mesh_data_to_IO(my_rank, nod_comm, node1, ele1)
      call set_grp_data_to_IO(nod_grp1, ele_grp1, sf_grp1)
!
!       save mesh information
      call sel_write_mesh_file(my_rank)
!
      call deallocate_ele_connect_type(ele1)
      call deallocate_node_geometry_type(node1)
!
      call deallocate_grp_type(nod_grp1)
      call deallocate_grp_type(ele_grp1)
      call deallocate_sf_grp_type(sf_grp1)
!
      end subroutine output_mesh
!
! -----------------------------------------------------------------------
!
      end module load_mesh_data
