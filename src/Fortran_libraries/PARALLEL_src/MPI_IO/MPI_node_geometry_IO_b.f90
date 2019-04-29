!>@file   MPI_node_geometry_IO_b.f90
!!@brief  module MPI_node_geometry_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on  Aug., 2006
!
!>@brief  routines for ASCII data IO for mesh geometry
!!
!!@verbatim
!!      subroutine mpi_write_geometry_info_b(IO_param, nod_IO)
!!      subroutine mpi_write_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!!      subroutine mpi_write_vect_in_ele_b(IO_param, nod_IO, sfed_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine mpi_read_number_of_node_b(IO_param, nod_IO)
!!      subroutine mpi_read_geometry_info_b(IO_param, nod_IO)
!!      subroutine mpi_read_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!!      subroutine mpi_read_vect_in_ele_b(IO_param, nod_IO, sfed_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module MPI_node_geometry_IO_b
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
      use MPI_binary_data_IO
      use MPI_binary_head_IO
!
      implicit none
!
      private :: mpi_write_num_node_b
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_write_num_node_b(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
!
!
      call mpi_write_one_integer_b(IO_param, nod_IO%numnod)
      call mpi_write_one_integer_b(IO_param, nod_IO%internal_node)
!
      end subroutine mpi_write_num_node_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_geometry_info_b(IO_param, nod_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
!
      integer(kind = kint_gl) ::  num64
!
!
      call mpi_write_num_node_b(IO_param, nod_IO)
!
      num64 = nod_IO%numnod
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int8_vector_b                                      &
     &   (IO_param, num64, nod_IO%inod_global)
      call mpi_write_2d_vector_b                                        &
     &   (IO_param, num64, ithree, nod_IO%xx)
!
      end subroutine mpi_write_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_write_num_node_b(IO_param, nod_IO)
!
      num64 = nod_IO%numnod
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_1d_vector_b                                        &
     &   (IO_param, num64, sfed_IO%ele_scalar)
!
      end subroutine mpi_write_scl_in_ele_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_vect_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_write_num_node_b(IO_param, nod_IO)
!
      num64 = nod_IO%numnod
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_2d_vector_b                                        &
     &   (IO_param, num64, n_vector, sfed_IO%ele_vector)
!
      end subroutine mpi_write_vect_in_ele_b
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
      integer(kind = kint_gl) ::  num64
!
!
      call alloc_node_geometry_base(nod_IO)
!
      num64 = nod_IO%numnod
      call mpi_read_int8_vector_b                                       &
     &   (IO_param, num64, nod_IO%inod_global)
      call mpi_read_2d_vector_b                                         &
     &   (IO_param, num64, ithree, nod_IO%xx)
!
      end subroutine mpi_read_geometry_info_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_scl_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_read_number_of_node_b(IO_param, nod_IO)
      call alloc_ele_scalar_IO(nod_IO, sfed_IO)
!
      num64 = nod_IO%numnod
      call mpi_write_1d_vector_b                                        &
     &   (IO_param, num64, sfed_IO%ele_scalar)
!
      end subroutine mpi_read_scl_in_ele_b
!
!------------------------------------------------------------------
!
      subroutine mpi_read_vect_in_ele_b(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call mpi_read_number_of_node_b(IO_param, nod_IO)
      call alloc_ele_vector_IO(nod_IO, sfed_IO)
!
      num64 = nod_IO%numnod
      call mpi_read_2d_vector_b                                         &
     &   (IO_param, num64, n_vector, sfed_IO%ele_vector)
!
      end subroutine mpi_read_vect_in_ele_b
!
!------------------------------------------------------------------
!
      end module MPI_node_geometry_IO_b
