!>@file   MPI_mesh_data_IO.f90
!!@brief  module MPI_mesh_data_IO
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Sep., 2006
!
!>@brief  Routines for gzipped binary mesh data IO
!!
!!@verbatim
!!      subroutine mpi_write_geometry_data(IO_param, mesh_IO)
!!      subroutine mpi_write_mesh_groups(IO_param, mesh_group_IO)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   mesh_group_IO
!!
!!      subroutine mpi_read_num_node(IO_param, mesh_IO)
!!      subroutine mpi_read_num_node_ele(IO_param, mesh_IO)
!!      subroutine mpi_read_geometry_data(IO_param, mesh_IO)
!!      subroutine mpi_read_mesh_groups(IO_param, mesh_group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   mesh_group_IO
!!@endverbatim
!
      module MPI_mesh_data_IO
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
      use MPI_ascii_data_IO
      use MPI_domain_data_IO
      use MPI_node_geometry_IO
      use MPI_element_connect_IO
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mpi_write_geometry_data(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(in) :: mesh_IO
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call mpi_write_domain_info(IO_param, mesh_IO%nod_comm)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_node()), hd_fem_node())
      call mpi_write_geometry_info(IO_param, mesh_IO%node)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_elem()), hd_fem_elem())
      call mpi_write_element_info(IO_param, mesh_IO%ele)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_import()), hd_fem_import())
      call mpi_write_import_data(IO_param, mesh_IO%nod_comm)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_export()), hd_fem_export())
      call mpi_write_export_data(IO_param, mesh_IO%nod_comm)
!
      end subroutine mpi_write_geometry_data
!
!------------------------------------------------------------------
!
      subroutine mpi_write_mesh_groups(IO_param, mesh_group_IO)
!
      use MPI_groups_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(in) ::   mesh_group_IO
!
!   write node group
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_nodgrp()), hd_fem_nodgrp())
      call mpi_write_grp_data                                           &
     &   (IO_param, mesh_group_IO%nod_grp)
!  write element group
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_elegrp()), hd_fem_elegrp())
      call mpi_write_grp_data                                           &
     &   (IO_param, mesh_group_IO%ele_grp)
!  write surface group
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_sfgrp()), hd_fem_sfgrp())
      call mpi_write_surf_grp_data                                      &
     &   (IO_param, mesh_group_IO%surf_grp)
!
      end subroutine mpi_write_mesh_groups
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_num_node(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_skip_read(IO_param, len(hd_fem_para()))
      call mpi_read_domain_info(IO_param, mesh_IO%nod_comm)
!
      call mpi_skip_read(IO_param, len(hd_fem_node()))
      call mpi_read_number_of_node(IO_param, mesh_IO%node)
!
      end subroutine mpi_read_num_node
!
!------------------------------------------------------------------
!
      subroutine mpi_read_num_node_ele(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_read_num_node(IO_param, mesh_IO)
      call mpi_read_geometry_info(IO_param, mesh_IO%node)
!
!  ----  read element data -------
!
      call mpi_skip_read(IO_param, len(hd_fem_elem()))
      call mpi_read_num_element(IO_param, mesh_IO%ele)
!
      end subroutine mpi_read_num_node_ele
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_data(IO_param, mesh_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      call mpi_read_num_node_ele(IO_param, mesh_IO)
      call mpi_read_element_info(IO_param, mesh_IO%ele)
!
! ----  import & export
!
      call mpi_skip_read(IO_param, len(hd_fem_import()))
      call mpi_read_import_data(IO_param, mesh_IO%nod_comm)
!
      call mpi_skip_read(IO_param, len(hd_fem_export()))
      call mpi_read_export_data(IO_param, mesh_IO%nod_comm)
!
      end subroutine mpi_read_geometry_data
!
!------------------------------------------------------------------
!
      subroutine mpi_read_mesh_groups(IO_param, mesh_group_IO)
!
      use MPI_groups_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(mesh_groups), intent(inout) ::   mesh_group_IO
!
!
!   read node group
      call mpi_skip_read(IO_param, len(hd_fem_nodgrp()))
      call mpi_read_group_data                                          &
     &   (IO_param, mesh_group_IO%nod_grp)
!  read element group
      call mpi_skip_read(IO_param, len(hd_fem_elegrp()))
      call mpi_read_group_data                                          &
     &   (IO_param, mesh_group_IO%ele_grp)
!  read surface group
      call mpi_skip_read(IO_param, len(hd_fem_sfgrp()))
      call mpi_read_surf_grp_data                                       &
     &   (IO_param, mesh_group_IO%surf_grp)
!
      end subroutine mpi_read_mesh_groups
!
!------------------------------------------------------------------
!
      end module MPI_mesh_data_IO
