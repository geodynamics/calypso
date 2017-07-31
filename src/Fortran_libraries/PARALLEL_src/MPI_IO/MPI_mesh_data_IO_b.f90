!>@file   MPI_mesh_data_IO_b.f90
!!@brief  module MPI_mesh_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for gzipped binary mesh data IO
!!
!!@verbatim
!!      subroutine mpi_write_geometry_data_b(IO_param, mesh_IO)
!!      subroutine mpi_write_mesh_groups_b(IO_param, mesh_group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine mpi_read_geometry_data_b(IO_param, mesh_IO)
!!      subroutine mpi_read_mesh_groups_b(IO_param, mesh_group_IO)
!!      subroutine mpi_read_num_node_ele_b(IO_param, mesh_IO)
!!      subroutine mpi_read_num_node_ele(IO_param, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!@endverbatim
!
      module MPI_mesh_data_IO_b
!
      use m_precision
      use m_constants
!
      use t_calypso_mpi_IO_param
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
!
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      implicit  none
!
      private :: mpi_write_geometry_info_b, mpi_write_element_info_b
      private :: mpi_read_number_of_node_b, mpi_read_geometry_info_b
      private :: mpi_read_number_of_element_b, mpi_read_element_info_b
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mpi_write_geometry_data_b(IO_param, mesh_IO)
!
      use MPI_domain_data_IO_b
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_write_domain_info_b(IO_param, mesh_IO%nod_comm)
!
      call mpi_write_geometry_info_b(IO_param, mesh_IO%node)
      call mpi_write_element_info_b(IO_param, mesh_IO%ele)
!
      call mpi_write_import_data_b(IO_param, mesh_IO%nod_comm)
      call mpi_write_export_data_b(IO_param, mesh_IO%nod_comm)
!
      end subroutine mpi_write_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_mesh_groups_b(IO_param, mesh_group_IO)
!
      use MPI_groups_IO_b
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   write node group
      call mpi_write_grp_data_b(IO_param, mesh_group_IO%nod_grp)
!  write element group
      call mpi_write_grp_data_b(IO_param, mesh_group_IO%ele_grp)
!  write surface group
      call mpi_write_surf_grp_data_b(IO_param, mesh_group_IO%surf_grp)
!
      end subroutine mpi_write_mesh_groups_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_data_b(IO_param, mesh_IO)
!
      use m_error_IDs
      use MPI_domain_data_IO_b
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_read_domain_info_b(IO_param, mesh_IO%nod_comm)
!
      call mpi_read_number_of_node_b(IO_param, mesh_IO%node)
      call mpi_read_geometry_info_b(IO_param, mesh_IO%node)
!
!  ----  read element data -------
!
      call mpi_read_number_of_element_b(IO_param, mesh_IO%ele)
      call mpi_read_element_info_b(IO_param, mesh_IO%ele)
!
! ----  import & export 
!
      call mpi_read_import_data_b(IO_param, mesh_IO%nod_comm)
      call mpi_read_export_data_b(IO_param, mesh_IO%nod_comm)
!
      end subroutine mpi_read_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_mesh_groups_b(IO_param, mesh_group_IO)
!
      use MPI_groups_IO_b
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call mpi_read_group_data_b(IO_param, mesh_group_IO%nod_grp)
!  read element group
      call mpi_read_group_data_b(IO_param, mesh_group_IO%ele_grp)
!  read surface group
      call mpi_read_surf_grp_data_b(IO_param, mesh_group_IO%surf_grp)
!
      end subroutine mpi_read_mesh_groups_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_num_node_ele_b(IO_param, mesh_IO)
!
      use m_error_IDs
      use MPI_domain_data_IO_b
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_read_domain_info_b(IO_param, mesh_IO%nod_comm)
!
      call mpi_read_number_of_node_b(IO_param, mesh_IO%node)
      call mpi_read_geometry_info_b(IO_param, mesh_IO%node)
!
!  ----  read element data -------
!
      call mpi_read_number_of_element_b(IO_param, mesh_IO%ele)
!
      end subroutine mpi_read_num_node_ele_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_num_node_ele(IO_param, mesh_IO)
!
      use m_error_IDs
      use MPI_domain_data_IO_b
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_read_domain_info_b(IO_param, mesh_IO%nod_comm)
!
      call mpi_read_number_of_node_b(IO_param, mesh_IO%node)
!
      end subroutine mpi_read_num_node_ele
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_geometry_info_b(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call mpi_write_one_integer_b(IO_param, nod_IO%numnod)
      call mpi_write_one_integer_b(IO_param, nod_IO%internal_node)
!
      call set_istack_4_parallell_data(nod_IO%numnod, IO_param)
!
      call mpi_write_int8_vector_b                                      &
     &   (IO_param, nod_IO%numnod, nod_IO%inod_global)
      call mpi_write_2d_vector_b                                        &
     &   (IO_param, nod_IO%numnod, ithree, nod_IO%xx)
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine mpi_write_geometry_info_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_element_info_b(IO_param, ele_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: num
!
!
      call mpi_write_one_integer_b(IO_param, ele_IO%numele)
!
      call set_istack_4_parallell_data(ele_IO%numele, IO_param)
      call mpi_write_int_vector_b                                       &
     &   (IO_param, ele_IO%numele, ele_IO%elmtyp)
      call mpi_write_int8_vector_b                                      &
     &   (IO_param, ele_IO%numele, ele_IO%iele_global)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call mul_istack_4_parallell_vect(ele_IO%nnod_4_ele, IO_param)
      call mpi_write_int_vector_b(IO_param, num, ele_IO%ie)
!
      call deallocate_ele_connect_type(ele_IO)
!
      end subroutine mpi_write_element_info_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_number_of_node_b(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call mpi_read_one_integer_b(IO_param, nod_IO%numnod)
      call mpi_read_one_integer_b(IO_param, nod_IO%internal_node)
!
      end subroutine mpi_read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_info_b(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call alloc_node_geometry_base(nod_IO)
!
      call mpi_read_int8_vector_b                                       &
     &   (IO_param, nod_IO%numnod, nod_IO%inod_global)
      call mpi_read_2d_vector_b                                         &
     &   (IO_param, nod_IO%numnod, ithree, nod_IO%xx)
!
      end subroutine mpi_read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_number_of_element_b(IO_param, ele_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
!
      call mpi_read_one_integer_b                                       &
     &   (IO_param, ele_IO%numele)
!
      end subroutine mpi_read_number_of_element_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_element_info_b(IO_param, ele_IO)
!
      use set_nnod_4_ele_by_type
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(element_data), intent(inout) :: ele_IO
!
      integer (kind = kint) :: num, i
!
!
      call alloc_element_types(ele_IO)
!
      call mpi_read_int_vector_b                                        &
     &   (IO_param, ele_IO%numele, ele_IO%elmtyp)
!
      ele_IO%nnod_4_ele = 0
      do i = 1, ele_IO%numele
        call s_set_nnod_4_ele_by_type                                   &
     &     (ele_IO%elmtyp(i), ele_IO%nodelm(i))
        ele_IO%nnod_4_ele = max(ele_IO%nnod_4_ele,ele_IO%nodelm(i))
      end do
!
      call alloc_ele_connectivity(ele_IO)
!
      call mpi_read_int8_vector_b                                       &
     &   (IO_param, ele_IO%numele, ele_IO%iele_global)
!
      num = ele_IO%numele * ele_IO%nnod_4_ele
      call mpi_read_int_vector_b(IO_param, num, ele_IO%ie)
!
      end subroutine mpi_read_element_info_b
!
!------------------------------------------------------------------
!
      end module MPI_mesh_data_IO_b
