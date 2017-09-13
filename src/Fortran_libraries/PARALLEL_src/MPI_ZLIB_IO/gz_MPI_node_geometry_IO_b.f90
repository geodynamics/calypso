!>@file   gz_MPI_node_geometry_IO_b.f90
!!@brief  module gz_MPI_node_geometry_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on  Aug., 2006
!
!>@brief  routines for ASCII data IO for mesh geometry
!!
!!@verbatim
!!      subroutine gz_mpi_write_geometry_info_b(IO_param, nod_IO)
!!      subroutine gz_mpi_write_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!!      subroutine gz_mpi_write_vect_in_ele_b(IO_param, nod_IO, sfed_IO)
!!
!!      subroutine gz_mpi_read_number_of_node_b(IO_param, nod_IO)
!!      subroutine gz_mpi_read_geometry_info_b(IO_param, nod_IO)
!!      subroutine gz_mpi_read_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!!      subroutine gz_mpi_read_vect_in_ele_b(IO_param, nod_IO, sfed_IO)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module gz_MPI_node_geometry_IO_b
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
!
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_datum_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geometry_info_b(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call gz_mpi_write_one_integer_b(IO_param, nod_IO%numnod)
      call gz_mpi_write_one_integer_b(IO_param, nod_IO%internal_node)
!
      call gz_mpi_write_int8_vector_b                                   &
     &   (IO_param, nod_IO%numnod, nod_IO%inod_global)
      call gz_mpi_write_2d_vector_b                                     &
     &   (IO_param, nod_IO%numnod, n_vector, nod_IO%xx)
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine gz_mpi_write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_write_one_integer_b(IO_param, nod_IO%numnod)
      call gz_mpi_write_one_integer_b(IO_param, nod_IO%internal_node)
      call gz_mpi_write_1d_vector_b                                     &
     &   (IO_param, nod_IO%numnod, sfed_IO%ele_scalar)
!
      call dealloc_ele_scalar_IO(sfed_IO)
!
      end subroutine gz_mpi_write_scl_in_ele_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_vect_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_write_one_integer_b(IO_param, nod_IO%numnod)
      call gz_mpi_write_one_integer_b(IO_param, nod_IO%internal_node)
      call gz_mpi_write_2d_vector_b                                     &
     &   (IO_param, nod_IO%numnod, n_vector, sfed_IO%ele_vector)
!
      call dealloc_ele_vector_IO(sfed_IO)
!
      end subroutine gz_mpi_write_vect_in_ele_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_number_of_node_b(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call gz_mpi_read_one_integer_b(IO_param, nod_IO%numnod)
      call gz_mpi_read_one_integer_b(IO_param, nod_IO%internal_node)
!
      end subroutine gz_mpi_read_number_of_node_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geometry_info_b(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call alloc_node_geometry_base(nod_IO)
!
      call gz_mpi_read_int8_vector_b                                    &
     &   (IO_param, nod_IO%numnod, nod_IO%inod_global)
      call gz_mpi_read_2d_vector_b                                      &
     &   (IO_param, nod_IO%numnod, n_vector, nod_IO%xx)
!
      end subroutine gz_mpi_read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_read_number_of_node_b(IO_param, nod_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      call gz_mpi_read_1d_vector_b                                      &
     &   (IO_param, nod_IO%numnod, sfed_IO%ele_scalar)
!
      end subroutine gz_mpi_read_scl_in_ele_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_vect_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_read_number_of_node_b(IO_param, nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      call gz_mpi_read_2d_vector_b                                      &
     &   (IO_param, nod_IO%numnod, n_vector, sfed_IO%ele_vector)
!
      end subroutine gz_mpi_read_vect_in_ele_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_node_geometry_IO_b
