!>@file   MPI_node_geometry_IO.f90
!!@brief  module MPI_node_geometry_IO
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on  Aug., 2006
!
!>@brief  routines for ASCII data IO for mesh geometry
!!
!!@verbatim
!!      subroutine mpi_write_geometry_info(IO_param, nod_IO)
!!      subroutine mpi_write_scl_in_ele(IO_param, nod_IO, sfed_IO)
!!      subroutine mpi_write_vect_in_ele(IO_param, nod_IO, sfed_IO)
!!
!!      subroutine mpi_read_number_of_node(IO_param, nod_IO)
!!      subroutine mpi_read_geometry_info(IO_param, nod_IO)
!!      subroutine mpi_read_scl_in_ele(IO_param, nod_IO, sfed_IO)
!!      subroutine mpi_read_vect_in_ele(IO_param, nod_IO, sfed_IO)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module MPI_node_geometry_IO
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
      use MPI_vectors_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_write_geometry_info(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call mpi_write_num_of_data(IO_param, nod_IO%internal_node)
!
      call mpi_write_node_position(IO_param,                            &
     &    nod_IO%numnod, ithree, nod_IO%inod_global, nod_IO%xx)
!
      call dealloc_node_geometry_base(nod_IO)
!
      end subroutine mpi_write_geometry_info
!
!------------------------------------------------------------------
!
      subroutine mpi_write_scl_in_ele(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call mpi_write_num_of_data(IO_param, nod_IO%internal_node)
      call mpi_write_scalar                                             &
     &   (IO_param, nod_IO%numnod, sfed_IO%ele_scalar)
!
      call dealloc_ele_scalar_IO(sfed_IO)
!
      end subroutine mpi_write_scl_in_ele
!
!------------------------------------------------------------------
!
      subroutine mpi_write_vect_in_ele(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call mpi_write_num_of_data(IO_param, nod_IO%internal_node)
      call mpi_write_vector                                             &
     &   (IO_param, nod_IO%numnod, n_vector, sfed_IO%ele_vector)
!
      call dealloc_ele_vector_IO(sfed_IO)
!
      end subroutine mpi_write_vect_in_ele
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_number_of_node(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call mpi_read_num_of_data(IO_param, nod_IO%internal_node)
      call mpi_read_num_of_data(IO_param, nod_IO%numnod)
!
      end subroutine mpi_read_number_of_node
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_info(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call alloc_node_geometry_base(nod_IO)
!
      call mpi_read_node_position(IO_param,                             &
     &    nod_IO%numnod, n_vector, nod_IO%inod_global, nod_IO%xx)
!
      end subroutine mpi_read_geometry_info
!
!------------------------------------------------------------------
!
      subroutine mpi_read_scl_in_ele(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call mpi_read_number_of_node(IO_param, nod_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      call mpi_read_scalar(IO_param, nod_IO%numnod, sfed_IO%ele_scalar)
!
      end subroutine mpi_read_scl_in_ele
!
!------------------------------------------------------------------
!
      subroutine mpi_read_vect_in_ele(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call mpi_read_number_of_node(IO_param, nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      call mpi_read_vector                                              &
     &   (IO_param, nod_IO%numnod, n_vector, sfed_IO%ele_vector)
!
      end subroutine mpi_read_vect_in_ele
!
!------------------------------------------------------------------
!
      end module MPI_node_geometry_IO
