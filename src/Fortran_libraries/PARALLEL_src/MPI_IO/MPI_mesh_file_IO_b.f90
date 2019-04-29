!>@file   MPI_mesh_file_IO_b.f90
!!@brief  module MPI_mesh_file_IO_b
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine mpi_read_mesh_file_b                                 &
!!     &         (num_pe, id_rank, file_name, fem_IO)
!!        type(mesh_data), intent(inout) :: fem_IO
!!      subroutine mpi_read_mesh_geometry_b(num_pe, id_rank, mesh_IO)
!!      subroutine mpi_read_node_size_b                                 &
!!     &         (num_pe, id_rank, file_name, mesh_IO)
!!      subroutine mpi_read_geometry_size_b                             &
!!     &         (num_pe, id_rank, file_name, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine mpi_write_mesh_file_b                                &
!!     &         (num_pe, id_rank, file_name, fem_IO)
!!        type(mesh_data), intent(in) :: fem_IO
!!@endverbatim
!
      module MPI_mesh_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_calypso_mpi_IO
      use t_mesh_data
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
!
      implicit none
!
      type(calypso_MPI_IO_params), private, save :: IO_param
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_file_b                                   &
     &         (num_pe, id_rank, file_name, fem_IO)
!
      use MPI_mesh_data_IO_b
      use MPI_groups_IO_b
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_data), intent(inout) :: fem_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(file_name)
!
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call mpi_read_geometry_data_b(IO_param, fem_IO%mesh)
      call mpi_read_mesh_groups_b(IO_param, fem_IO%group)
!
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_geometry_b                               &
     &         (num_pe, id_rank, file_name, mesh_IO)
!
      use MPI_mesh_data_IO_b
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(file_name)
!
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
      call mpi_read_geometry_data_b(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
       subroutine mpi_read_node_size_b                                  &
      &         (num_pe, id_rank, file_name, mesh_IO)
!
       use MPI_domain_data_IO_b
       use MPI_mesh_data_IO_b
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(file_name)
!
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
      call mpi_read_num_node_ele(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_node_size_b
!
!------------------------------------------------------------------
!
       subroutine mpi_read_geometry_size_b                              &
      &         (num_pe, id_rank, file_name, mesh_IO)
!
       use MPI_domain_data_IO_b
       use MPI_mesh_data_IO_b
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(file_name)
!
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
      call mpi_read_num_node_ele_b(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_mesh_file_b                                  &
     &         (num_pe, id_rank, file_name, fem_IO)
!
      use m_machine_parameter
      use MPI_mesh_data_IO_b
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_data), intent(in) :: fem_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped binary merged mesh file: ', trim(file_name)
!
      call open_write_mpi_file_b                                        &
     &   (file_name, num_pe, id_rank, IO_param)
      call mpi_write_geometry_data_b(IO_param, fem_IO%mesh)
      call mpi_write_mesh_groups_b(IO_param, fem_IO%group)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module MPI_mesh_file_IO_b
