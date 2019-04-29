!>@file  gz_MPI_element_data_IO.f90
!!      module gz_MPI_element_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine gz_mpi_read_ele_comm_table(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine gz_mpi_write_ele_comm_table(IO_param, comm_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine gz_mpi_read_ele_geometry(IO_param, nod_IO, sfed_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine gz_mpi_write_ele_geometry(IO_param, nod_IO, sfed_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!@endverbatim
!
      module gz_MPI_element_data_IO
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
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
      subroutine gz_mpi_read_ele_comm_table(IO_param, comm_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(inout) :: comm_IO
!
!
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_para()))
      call gz_mpi_skip_header(IO_param, len(hd_fem_para()))
      call gz_mpi_read_domain_info(IO_param, comm_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_import()))
      call gz_mpi_read_import_data(IO_param, comm_IO)
!
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_export()))
      call gz_mpi_read_export_data(IO_param, comm_IO)
!
      end subroutine gz_mpi_read_ele_comm_table
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_ele_comm_table(IO_param, comm_IO)
!
      use m_fem_mesh_labels
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(communication_table), intent(in) :: comm_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ecomm_para()), hd_ecomm_para())
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call gz_mpi_write_domain_info(IO_param, comm_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ecomm_import()), hd_ecomm_import())
      call gz_mpi_write_import_data(IO_param, comm_IO)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ecomm_export()), hd_ecomm_export())
      call gz_mpi_write_export_data(IO_param, comm_IO)
!
      end subroutine gz_mpi_write_ele_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_ele_geometry(IO_param, nod_IO, sfed_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call gz_mpi_skip_header(IO_param, len(hd_ecomm_point()))
      call gz_mpi_read_number_of_node(IO_param, nod_IO)
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
      end module gz_MPI_element_data_IO
