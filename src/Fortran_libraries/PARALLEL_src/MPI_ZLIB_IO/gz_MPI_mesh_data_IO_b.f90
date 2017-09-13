!>@file   gz_MPI_mesh_data_IO_b.f90
!!@brief  module gz_MPI_mesh_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for gzipped binary mesh data IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_geometry_data_b(IO_param, mesh_IO)
!!      subroutine gz_mpi_write_mesh_groups_b(IO_param, mesh_group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine gz_mpi_read_num_node_b(IO_param, mesh_IO)
!!      subroutine gz_mpi_read_num_node_ele_b(IO_param, mesh_IO)
!!      subroutine gz_mpi_read_geometry_data_b(IO_param, mesh_IO)
!!      subroutine gz_mpi_read_mesh_groups_b(IO_param, mesh_group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!@endverbatim
!
      module gz_MPI_mesh_data_IO_b
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_calypso_mpi_IO_param
!
      use gz_MPI_domain_data_IO_b
      use gz_MPI_node_geometry_IO_b
      use gz_MPI_element_connect_IO_b
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geometry_data_b(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_write_domain_info_b(IO_param, mesh_IO%nod_comm)
!
      call gz_mpi_write_geometry_info_b(IO_param, mesh_IO%node)
      call gz_mpi_write_element_info_b(IO_param, mesh_IO%ele)
!
      call gz_mpi_write_import_data_b(IO_param, mesh_IO%nod_comm)
      call gz_mpi_write_export_data_b(IO_param, mesh_IO%nod_comm)
!
      end subroutine gz_mpi_write_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_mesh_groups_b(IO_param, mesh_group_IO)
!
      use gz_MPI_groups_IO_b
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!   write node group
      call gz_mpi_write_grp_data_b                                      &
     &   (IO_param, mesh_group_IO%nod_grp)
!  write element group
      call gz_mpi_write_grp_data_b                                      &
     &   (IO_param, mesh_group_IO%ele_grp)
!  write surface group
      call gz_mpi_write_surf_grp_data_b                                 &
     &   (IO_param, mesh_group_IO%surf_grp)
!
      end subroutine gz_mpi_write_mesh_groups_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_num_node_b(IO_param, mesh_IO)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_read_domain_info_b(IO_param, mesh_IO%nod_comm)
!
      call gz_mpi_read_number_of_node_b(IO_param, mesh_IO%node)
!
      end subroutine gz_mpi_read_num_node_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_num_node_ele_b(IO_param, mesh_IO)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_read_num_node_b(IO_param, mesh_IO)
      call gz_mpi_read_geometry_info_b(IO_param, mesh_IO%node)
!
!  ----  read element data -------
!
      call gz_mpi_read_num_element_b(IO_param, mesh_IO%ele)
!
      end subroutine gz_mpi_read_num_node_ele_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geometry_data_b(IO_param, mesh_IO)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_read_num_node_ele_b(IO_param, mesh_IO)
      call gz_mpi_read_ele_info_b(IO_param, mesh_IO%ele)
!
! ----  import & export 
!
      call gz_mpi_read_import_data_b(IO_param, mesh_IO%nod_comm)
      call gz_mpi_read_export_data_b(IO_param, mesh_IO%nod_comm)
!
      end subroutine gz_mpi_read_geometry_data_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_mesh_groups_b(IO_param, mesh_group_IO)
!
      use gz_MPI_groups_IO_b
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call gz_mpi_read_group_data_b                                     &
     &   (IO_param, mesh_group_IO%nod_grp)
!  read element group
      call gz_mpi_read_group_data_b                                     &
     &   (IO_param, mesh_group_IO%ele_grp)
!  read surface group
      call gz_mpi_read_surf_grp_data_b                                  &
     &   (IO_param, mesh_group_IO%surf_grp)
!
      end subroutine gz_mpi_read_mesh_groups_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_mesh_data_IO_b
