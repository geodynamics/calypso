!>@file   gz_MPI_node_geometry_IO.f90
!!@brief  module gz_MPI_node_geometry_IO
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on  Aug., 2006
!
!>@brief  routines for ASCII data IO for mesh geometry
!!
!!@verbatim
!!      subroutine gz_mpi_write_geometry_info(IO_param, nod_IO)
!!      subroutine gz_mpi_write_scl_in_ele(IO_param, nod_IO, sfed_IO)
!!      subroutine gz_mpi_write_vect_in_ele(IO_param, nod_IO, sfed_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine gz_mpi_read_number_of_node(IO_param, nod_IO)
!!      subroutine gz_mpi_read_geometry_info(IO_param, nod_IO)
!!      subroutine gz_mpi_read_scl_in_ele(IO_param, nod_IO, sfed_IO)
!!      subroutine gz_mpi_read_vect_in_ele(IO_param, nod_IO, sfed_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module gz_MPI_node_geometry_IO
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
      use gz_MPI_position_IO
      use gz_MPI_vectors_IO
      use gz_MPI_integer_list_IO
!
      use transfer_to_long_integers
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geometry_info(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
!
!
      call gz_mpi_write_num_of_data(IO_param, nod_IO%internal_node)
      call gz_mpi_write_num_of_data(IO_param, nod_IO%numnod)
!
      call gz_mpi_write_node_position                                   &
     &   (IO_param, cast_long(nod_IO%numnod), ithree,                   &
     &    nod_IO%inod_global, nod_IO%xx)
!
      end subroutine gz_mpi_write_geometry_info
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_scl_in_ele(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call gz_mpi_write_num_of_data(IO_param, nod_IO%internal_node)
      call gz_mpi_write_num_of_data(IO_param, nod_IO%numnod)
      call gz_mpi_write_scalar                                          &
     &   (IO_param, cast_long(nod_IO%numnod), sfed_IO%ele_scalar)
!
      end subroutine gz_mpi_write_scl_in_ele
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_vect_in_ele(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call gz_mpi_write_num_of_data(IO_param, nod_IO%internal_node)
      call gz_mpi_write_num_of_data(IO_param, nod_IO%numnod)
      call gz_mpi_write_vector(IO_param,                                &
     &    cast_long(nod_IO%numnod), n_vector, sfed_IO%ele_vector)
!
      end subroutine gz_mpi_write_vect_in_ele
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_number_of_node(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call gz_mpi_read_num_of_data(IO_param, nod_IO%internal_node)
      call gz_mpi_read_num_of_data(IO_param, nod_IO%numnod)
!
      end subroutine gz_mpi_read_number_of_node
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geometry_info(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
!
!
      call alloc_node_geometry_base(nod_IO)
!
      call gz_mpi_read_node_position                                    &
     &   (IO_param, cast_long(nod_IO%numnod), n_vector,                 &
     &    nod_IO%inod_global, nod_IO%xx)
!
      end subroutine gz_mpi_read_geometry_info
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_scl_in_ele(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_read_number_of_node(IO_param, nod_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      call gz_mpi_read_scalar                                           &
     &   (IO_param, cast_long(nod_IO%numnod), sfed_IO%ele_scalar)
!
      end subroutine gz_mpi_read_scl_in_ele
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_vect_in_ele(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_read_number_of_node(IO_param, nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      call gz_mpi_read_vector(IO_param,                                 &
     &    cast_long(nod_IO%numnod), n_vector, sfed_IO%ele_vector)
!
      end subroutine gz_mpi_read_vect_in_ele
!
!------------------------------------------------------------------
!
      end module gz_MPI_node_geometry_IO
