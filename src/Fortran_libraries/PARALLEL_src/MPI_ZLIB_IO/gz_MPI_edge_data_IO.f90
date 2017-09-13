!>@file  gz_MPI_edge_data_IO.f90
!!      module gz_MPI_edge_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief data IO orutines for edge
!!
!!@verbatim
!!      subroutine gz_mpi_read_edge_connect                             &
!!     &         (IO_param, comm_IO, ele_IO, sfed_IO)
!!      subroutine gz_mpi_write_edge_connect                            &
!!     &         (IO_param, comm_IO, ele_IO, sfed_IO)
!!
!!      subroutine gz_mpi_read_edge_geometry(IO_param, nod_IO, sfed_IO)
!!      subroutine gz_mpi_write_edge_geometry(IO_param, nod_IO, sfed_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
!
      module gz_MPI_edge_data_IO
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
      use m_fem_surface_labels
!
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use gz_MPI_node_geometry_IO
      use gz_MPI_element_connect_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_edge_connect                               &
     &         (IO_param, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_skip_header(IO_param, len(hd_edge_para()))
      call gz_mpi_skip_header(IO_param, len(hd_fem_para()))
      call gz_mpi_read_domain_info(IO_param, comm_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_edge_connect()))
      call gz_mpi_read_num_element(IO_param, ele_IO)
      call gz_mpi_read_element_info(IO_param, ele_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_edge_on_surf()))
      call gz_mpi_read_surface_4_element(IO_param, sfed_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_edge_on_ele()))
      call gz_mpi_read_edge_4_element(IO_param, sfed_IO)
!
!
!
      call gz_mpi_skip_header(IO_param, len(hd_edge_import()))
      call gz_mpi_read_import_data(IO_param, comm_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_edge_export()))
      call gz_mpi_read_export_data(IO_param, comm_IO)
!
      end subroutine gz_mpi_read_edge_connect
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_edge_connect                              &
     &         (IO_param, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_para()), hd_edge_para())
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call gz_mpi_write_domain_info(IO_param, comm_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_connect()), hd_edge_connect())
      call gz_mpi_write_element_info(IO_param, ele_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_on_surf()), hd_edge_on_surf())
      call gz_mpi_write_surface_4_element(IO_param, sfed_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_on_ele()), hd_edge_on_ele())
      call gz_mpi_write_edge_4_element(IO_param, sfed_IO)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_import()), hd_edge_import())
      call gz_mpi_write_import_data(IO_param, comm_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_export()), hd_edge_export())
      call gz_mpi_write_export_data(IO_param, comm_IO)
!
      end subroutine gz_mpi_write_edge_connect
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_edge_geometry(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_skip_header(IO_param, len(hd_edge_point()))
      call gz_mpi_read_number_of_node(IO_param, nod_IO)
      call gz_mpi_read_geometry_info(IO_param, nod_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_edge_dir()))
      call gz_mpi_read_vect_in_ele(IO_param, nod_IO, sfed_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_edge_length()))
      call gz_mpi_read_scl_in_ele(IO_param, nod_IO, sfed_IO)
!
      end subroutine gz_mpi_read_edge_geometry
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_edge_geometry(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_point()), hd_edge_point())
      call gz_mpi_write_geometry_info(IO_param, nod_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_dir()), hd_edge_dir())
      call gz_mpi_write_vect_in_ele(IO_param, nod_IO, sfed_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_length()), hd_edge_length())
      call gz_mpi_write_scl_in_ele(IO_param, nod_IO, sfed_IO)
!
      end subroutine gz_mpi_write_edge_geometry
!
!------------------------------------------------------------------
!
      end module gz_MPI_edge_data_IO
