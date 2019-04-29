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
!!     &         (num_pe, id_rank, file_name, ele_mesh_IO, ierr)
!!      subroutine mpi_input_surface_file                               &
!!     &         (num_pe, id_rank, file_name, surf_mesh_IO, ierr)
!!      subroutine mpi_input_edge_file                                  &
!!     &         (num_pe, id_rank, file_name, edge_mesh_IO, ierr)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine mpi_output_element_file                              &
!!     &         (num_pe, id_rank, ele_mesh_IO)
!!      subroutine mpi_output_surface_file                              &
!!     &         (num_pe, id_rank, file_name, surf_mesh_IO)
!!      subroutine mpi_output_edge_file                                 &
!!     &         (num_pe, id_rank, file_name, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
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
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_input_element_file                                 &
     &         (num_pe, id_rank, file_name, ele_mesh_IO, ierr)
!
      use MPI_element_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read merged ascii element comm file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
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
     &         (num_pe, id_rank, file_name, surf_mesh_IO, ierr)
!
      use MPI_surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read merged ascii surface mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
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
     &         (num_pe, id_rank, file_name, edge_mesh_IO, ierr)
!
      use MPI_edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read merged ascii edge mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
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
     &         (num_pe, id_rank, file_name, ele_mesh_IO)
!
      use MPI_element_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write merged ascii element comm file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, num_pe, id_rank, IO_param)
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
     &         (num_pe, id_rank, file_name, surf_mesh_IO)
!
      use MPI_surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write merged ascii surface mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, num_pe, id_rank, IO_param)
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
     &         (num_pe, id_rank, file_name, edge_mesh_IO)
!
      use MPI_edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write merged ascii edge mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, num_pe, id_rank, IO_param)
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
