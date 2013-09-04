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
      private :: set_mesh_data, set_mesh_to_IO
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
      call allocate_element_geometry
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
      use set_nod_comm_tbl_4_IO
      use set_node_geometry_4_IO
      use set_element_connect_4_IO
      use set_group_data_4_IO
!
!
      call copy_node_comm_tbl_from_IO
!
      call copy_node_geometry_from_IO
      call copy_element_connect_from_IO
!
      call copy_group_data_from_IO
!
      end subroutine set_mesh_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_to_IO(my_rank)
!
      use m_geometry_data
      use set_nod_comm_tbl_4_IO
      use set_node_geometry_4_IO
      use set_element_connect_4_IO
      use set_group_data_4_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call copy_node_comm_tbl_to_IO(my_rank)
      call copy_node_geometry_to_IO
      call copy_element_connect_to_IO
      call copy_group_data_to_IO
      call deallocate_node_geometry
!
      end subroutine set_mesh_to_IO
!
!   --------------------------------------------------------------------
!
      end module load_mesh_data
