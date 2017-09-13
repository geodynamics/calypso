!>@file   gz_MPI_mesh_data_IO.f90
!!@brief  module gz_MPI_mesh_data_IO
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for gzipped binary mesh data IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_geometry_data(IO_param, mesh_IO)
!!      subroutine gz_mpi_write_mesh_groups(IO_param, mesh_group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!
!!      subroutine gz_mpi_read_num_node(IO_param, mesh_IO)
!!      subroutine gz_mpi_read_num_node_ele(IO_param, mesh_IO)
!!      subroutine gz_mpi_read_geometry_data(IO_param, mesh_IO)
!!      subroutine gz_mpi_read_mesh_groups(IO_param, mesh_group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!@endverbatim
!
      module gz_MPI_mesh_data_IO
!
      use m_precision
      use m_constants
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_calypso_mpi_IO_param
      use m_fem_mesh_labels
!
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
      use gz_MPI_node_geometry_IO
      use gz_MPI_element_connect_IO
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_geometry_data(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call gz_mpi_write_domain_info(IO_param, mesh_IO%nod_comm)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_node()), hd_fem_node())
      call gz_mpi_write_geometry_info(IO_param, mesh_IO%node)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_elem()), hd_fem_elem())
      call gz_mpi_write_element_info(IO_param, mesh_IO%ele)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_import()), hd_fem_import())
      call gz_mpi_write_import_data(IO_param, mesh_IO%nod_comm)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_export()), hd_fem_export())
      call gz_mpi_write_export_data(IO_param, mesh_IO%nod_comm)
!
      end subroutine gz_mpi_write_geometry_data
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_mesh_groups(IO_param, mesh_group_IO)
!
      use gz_MPI_groups_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!   write node group
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_nodgrp()), hd_fem_nodgrp())
      call gz_mpi_write_grp_data                                        &
     &   (IO_param, mesh_group_IO%nod_grp)
!  write element group
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_elegrp()), hd_fem_elegrp())
      call gz_mpi_write_grp_data                                        &
     &   (IO_param, mesh_group_IO%ele_grp)
!  write surface group
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_sfgrp()), hd_fem_sfgrp())
      call gz_mpi_write_surf_grp_data                                   &
     &   (IO_param, mesh_group_IO%surf_grp)
!
      end subroutine gz_mpi_write_mesh_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_num_node(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_para()))
      call gz_mpi_read_domain_info(IO_param, mesh_IO%nod_comm)
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_node()))
      call gz_mpi_read_number_of_node(IO_param, mesh_IO%node)
!
      end subroutine gz_mpi_read_num_node
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_num_node_ele(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_read_num_node(IO_param, mesh_IO)
      call gz_mpi_read_geometry_info(IO_param, mesh_IO%node)
!
!  ----  read element data -------
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_elem()))
      call gz_mpi_read_num_element(IO_param, mesh_IO%ele)
!
      end subroutine gz_mpi_read_num_node_ele
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geometry_data(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call gz_mpi_read_num_node_ele(IO_param, mesh_IO)
!
      call gz_mpi_read_element_info(IO_param, mesh_IO%ele)
!
! ----  import & export
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_import()))
      call gz_mpi_read_import_data(IO_param, mesh_IO%nod_comm)
!
      call gz_mpi_skip_header(IO_param, len(hd_fem_export()))
      call gz_mpi_read_export_data(IO_param, mesh_IO%nod_comm)
!
      end subroutine gz_mpi_read_geometry_data
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_mesh_groups(IO_param, mesh_group_IO)
!
      use gz_MPI_groups_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call gz_mpi_skip_header(IO_param, len(hd_fem_nodgrp()))
      call gz_mpi_read_group_data                                       &
     &   (IO_param, mesh_group_IO%nod_grp)
!  read element group
      call gz_mpi_skip_header(IO_param, len(hd_fem_elegrp()))
      call gz_mpi_read_group_data                                       &
     &   (IO_param, mesh_group_IO%ele_grp)
!  read surface group
      call gz_mpi_skip_header(IO_param, len(hd_fem_sfgrp()))
      call gz_mpi_read_surf_grp_data                                    &
     &   (IO_param, mesh_group_IO%surf_grp)
!
      end subroutine gz_mpi_read_mesh_groups
!
!------------------------------------------------------------------
!
      end module gz_MPI_mesh_data_IO
