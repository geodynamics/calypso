!>@file  gz_MPI_element_file_IO.f90
!!      module gz_MPI_element_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine gz_mpi_input_element_file                            &
!!     &         (num_pe, id_rank, file_name, ele_mesh_IO, ierr)
!!      subroutine gz_mpi_input_surface_file                            &
!!     &         (num_pe, id_rank, file_name, surf_mesh_IO, ierr)
!!      subroutine gz_mpi_input_edge_file                               &
!!     &         (num_pe, id_rank, file_name, edge_mesh_IO, ierr)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine gz_mpi_output_element_file                           &
!!     &         (num_pe, id_rank, ele_mesh_IO)
!!      subroutine gz_mpi_output_surface_file                           &
!!     &         (num_pe, id_rank, file_name, surf_mesh_IO)
!!      subroutine gz_mpi_output_edge_file                              &
!!     &         (num_pe, id_rank, file_name, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(in) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module gz_MPI_element_file_IO
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
      subroutine gz_mpi_input_element_file                              &
     &         (num_pe, id_rank, file_name, ele_mesh_IO, ierr)
!
      use gz_MPI_element_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped merged ascii element comm file: ',                &
     &   trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_ele_comm_table(IO_param, ele_mesh_IO%comm)
!      call gz_mpi_read_ele_geometry(IO_param,                          &
!     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_input_element_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_input_surface_file                              &
     &         (num_pe, id_rank, file_name, surf_mesh_IO, ierr)
!
      use gz_MPI_surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped merged ascii surface mesh file: ',                &
     &   trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_surf_connect(IO_param, surf_mesh_IO%comm,        &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed)
!      call gz_mpi_read_surf_geometry(IO_param,                         &
!     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_input_surface_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_input_edge_file                                 &
     &         (num_pe, id_rank, file_name, edge_mesh_IO, ierr)
!
      use gz_MPI_edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped merged ascii edge mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_edge_connect(IO_param, edge_mesh_IO%comm,        &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed)
!      call gz_mpi_read_edge_geometry(IO_param,                         &
!     &    edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_input_edge_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_output_element_file                             &
     &         (num_pe, id_rank, file_name, ele_mesh_IO)
!
      use gz_MPI_element_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped merged ascii element comm file: ',               &
     &   trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_write_ele_comm_table(IO_param, ele_mesh_IO%comm)
!      call gz_mpi_write_ele_geometry(IO_param,                         &
!     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_output_element_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_output_surface_file                             &
     &         (num_pe, id_rank, file_name, surf_mesh_IO)
!
      use gz_MPI_surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped merged ascii surface mesh file: ',               &
     &   trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_write_surf_connect(IO_param, surf_mesh_IO%comm,       &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed)
!      call gz_mpi_write_surf_geometry(IO_param,                        &
!     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_output_surface_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_output_edge_file                                &
     &         (num_pe, id_rank, file_name, edge_mesh_IO)
!
      use gz_MPI_edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped merged ascii edge mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_write_edge_connect(IO_param, edge_mesh_IO%comm,       &
     &   edge_mesh_IO%ele, edge_mesh_IO%sfed)
!      call gz_mpi_write_edge_geometry(IO_param,                        &
!     &   edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_output_edge_file
!
!------------------------------------------------------------------
!
      end module gz_MPI_element_file_IO
