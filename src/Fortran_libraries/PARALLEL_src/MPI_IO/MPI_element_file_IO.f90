!>@file  MPI_element_file_IO.f90
!!      module MPI_element_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine mpi_input_element_file                               &
!!     &         (nprocs_in, my_rank_IO, file_name, ele_mesh_IO, ierr)
!!      subroutine mpi_input_surface_file                               &
!!     &         (nprocs_in, my_rank_IO, file_name, surf_mesh_IO, ierr)
!!      subroutine mpi_input_edge_file                                  &
!!     &         (nprocs_in, my_rank_IO, file_name, edge_mesh_IO, ierr)
!!
!!      subroutine mpi_output_element_file                              &
!!     &         (nprocs_in, my_rank_IO, ele_mesh_IO)
!!      subroutine mpi_output_surface_file                              &
!!     &         (nprocs_in, my_rank_IO, file_name, surf_mesh_IO)
!!      subroutine mpi_output_edge_file                                 &
!!     &         (nprocs_in, my_rank_IO, file_name, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!@endverbatim
!!
!!@param my_rank_IO  MPI rank
!
      module MPI_element_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_calypso_mpi_IO_param
      use t_read_mesh_data
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
      character(len=kchara), private :: file_name
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_input_element_file                                 &
     &         (nprocs_in, my_rank_IO, file_name, ele_mesh_IO, ierr)
!
      use MPI_element_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Read ascii element comm file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call mpi_read_element_comm_table(IO_param, ele_mesh_IO%comm)
!      call mpi_read_element_geometry(IO_param,                         &
!     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_input_element_file
!
!------------------------------------------------------------------
!
      subroutine mpi_input_surface_file                                 &
     &         (nprocs_in, my_rank_IO, file_name, surf_mesh_IO, ierr)
!
      use MPI_surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Read ascii surface mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call mpi_read_surface_connection(IO_param, surf_mesh_IO%comm,     &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed)
!      call mpi_read_surface_geometry(IO_param,                         &
!     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_input_surface_file
!
!------------------------------------------------------------------
!
      subroutine mpi_input_edge_file                                    &
     &         (nprocs_in, my_rank_IO, file_name, edge_mesh_IO, ierr)
!
      use MPI_edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Read ascii edge mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call mpi_read_edge_connection(IO_param, edge_mesh_IO%comm,        &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed)
!      call mpi_read_edge_geometry(IO_param,                            &
!     &    edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_input_edge_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_output_element_file                                &
     &         (nprocs_in, my_rank_IO, file_name, ele_mesh_IO)
!
      use MPI_element_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii element comm file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call mpi_write_element_comm_table(IO_param, ele_mesh_IO%comm)
!      call mpi_write_element_geometry(IO_param,                        &
!     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_output_element_file
!
!------------------------------------------------------------------
!
      subroutine mpi_output_surface_file                                &
     &         (nprocs_in, my_rank_IO, file_name, surf_mesh_IO)
!
      use MPI_surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii surface mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call mpi_write_surface_connection(IO_param, surf_mesh_IO%comm,    &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed)
!      call mpi_write_surface_geometry(IO_param,                        &
!     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_output_surface_file
!
!------------------------------------------------------------------
!
      subroutine mpi_output_edge_file                                   &
     &         (nprocs_in, my_rank_IO, file_name, edge_mesh_IO)
!
      use MPI_edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii edge mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call mpi_write_edge_connection(IO_param, edge_mesh_IO%comm,       &
     &   edge_mesh_IO%ele, edge_mesh_IO%sfed)
!      call mpi_write_edge_geometry(IO_param,                           &
!     &   edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_output_edge_file
!
!------------------------------------------------------------------
!
      end module MPI_element_file_IO
