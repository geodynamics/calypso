!>@file  MPI_mesh_file_IO.f90
!!      module MPI_mesh_file_IO
!!
!!@author  H. Matsui
!!@date      Programmed in Aug., 2016
!!
!>@brief ASCII mesh file IO
!!
!!@verbatim
!!      subroutine mpi_read_mesh_file                                   &
!!     &         (nprocs_in, my_rank_IO, file_name, fem_IO)
!!        type(mesh_data), intent(inout) :: fem_IO
!!      subroutine mpi_read_mesh_geometry                               &
!!     &         (nprocs_in, my_rank_IO, file_name, mesh_IO)
!!      subroutine mpi_read_node_size                                   &
!!     &         (nprocs_in, my_rank_IO, file_name, mesh_IO)
!!      subroutine mpi_read_geometry_size                               &
!!     &         (nprocs_in, my_rank_IO, file_name, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine mpi_write_mesh_file                                  &
!!     &         (nprocs_in, my_rank_IO, file_name, fem_IO)
!!        type(mesh_data), intent(inout) :: fem_IO
!!@endverbatim
!
      module MPI_mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
      use MPI_mesh_data_IO
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_file                                     &
     &         (nprocs_in, my_rank_IO, file_name, fem_IO)
!
      use mesh_data_IO
      use groups_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_data), intent(inout) :: fem_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call mpi_read_geometry_data(IO_param, fem_IO%mesh)
      call mpi_read_mesh_groups(IO_param, fem_IO%group)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_geometry                                 &
     &         (nprocs_in, my_rank_IO, file_name, mesh_IO)
!
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
      call mpi_read_geometry_data(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
!
      end subroutine mpi_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_node_size                                     &
     &         (nprocs_in, my_rank_IO, file_name, mesh_IO)
!
      use mesh_data_IO
      use node_geometry_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
      call mpi_read_num_node(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
!
      end subroutine mpi_read_node_size
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_size                                 &
     &         (nprocs_in, my_rank_IO, file_name, mesh_IO)
!
      use mesh_data_IO
      use element_connect_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
      call mpi_read_num_node_ele(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_mesh_file                                    &
     &         (nprocs_in, my_rank_IO, file_name, fem_IO)
!
      use m_machine_parameter
      use m_fem_mesh_labels
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      character(len=kchara), intent(in) :: file_name
      type(mesh_data), intent(inout) :: fem_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Write ascii mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call mpi_write_geometry_data(IO_param, fem_IO%mesh)
      call mpi_write_mesh_groups(IO_param, fem_IO%group)
!
      call close_mpi_file(IO_param)
!
      end subroutine mpi_write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module MPI_mesh_file_IO
