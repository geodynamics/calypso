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
!!     &         (num_pe, id_rank, file_name, ele_mesh_IO)
!!      subroutine gz_mpi_input_surface_file                            &
!!     &         (num_pe, id_rank, file_name, surf_mesh_IO)
!!      subroutine gz_mpi_input_edge_file                               &
!!     &         (num_pe, id_rank, file_name, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine gz_mpi_output_element_file(file_name, ele_mesh_IO)
!!      subroutine gz_mpi_output_surface_file(file_name, surf_mesh_IO)
!!      subroutine gz_mpi_output_edge_file(file_name, edge_mesh_IO)
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
      private :: gz_mpi_read_ele_geometry, gz_mpi_write_ele_geometry
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_input_element_file                              &
     &         (num_pe, id_rank, file_name, ele_mesh_IO)
!
      use gz_MPI_comm_table_IO
      use m_fem_mesh_labels
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped merged ascii element comm file: ',                &
     &   trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_para()))
      call gz_mpi_read_comm_table(IO_param, ele_mesh_IO%comm)
!      call gz_mpi_read_ele_geometry(IO_param,                          &
!     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_input_element_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_input_surface_file                              &
     &         (num_pe, id_rank, file_name, surf_mesh_IO)
!
      use gz_MPI_surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
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
     &         (num_pe, id_rank, file_name, edge_mesh_IO)
!
      use gz_MPI_edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
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
      subroutine gz_mpi_output_element_file(file_name, ele_mesh_IO)
!
      use gz_MPI_comm_table_IO
      use m_fem_mesh_labels
!
      character(len=kchara), intent(in) :: file_name
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped merged ascii element comm file: ',               &
     &   trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ecomm_para()), hd_ecomm_para())
      call gz_mpi_write_comm_table(IO_param, ele_mesh_IO%comm)
!      call gz_mpi_write_ele_geometry(IO_param,                         &
!     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_output_element_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_output_surface_file(file_name, surf_mesh_IO)
!
      use gz_MPI_surface_data_IO
!
      character(len=kchara), intent(in) :: file_name
      type(surf_edge_IO_file), intent(in) :: surf_mesh_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped merged ascii surface mesh file: ',               &
     &   trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param)
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
      subroutine gz_mpi_output_edge_file(file_name, edge_mesh_IO)
!
      use gz_MPI_edge_data_IO
!
      character(len=kchara), intent(in) :: file_name
      type(surf_edge_IO_file), intent(in) :: edge_mesh_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped merged ascii edge mesh file: ', trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param)
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
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_ele_geometry(IO_param, nod_IO, sfed_IO)
!
      use m_fem_mesh_labels
      use m_fem_surface_labels
      use gz_MPI_node_geometry_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_point()))
      call gz_mpi_read_geometry_info(IO_param, nod_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_vol()))
      call gz_mpi_read_scl_in_ele(IO_param, nod_IO, sfed_IO)
!
      end subroutine gz_mpi_read_ele_geometry
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_ele_geometry(IO_param, nod_IO, sfed_IO)
!
      use m_fem_mesh_labels
      use m_fem_surface_labels
      use gz_MPI_node_geometry_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ecomm_point()), hd_ecomm_point())
      call gz_mpi_write_geometry_info(IO_param, nod_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ecomm_vol()), hd_ecomm_vol())
      call gz_mpi_write_scl_in_ele(IO_param, nod_IO, sfed_IO)
!
      end subroutine gz_mpi_write_ele_geometry
!
!------------------------------------------------------------------
!
      end module gz_MPI_element_file_IO
