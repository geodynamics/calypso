!>@file   gz_MPI_mesh_file_IO.f90
!!@brief  module gz_MPI_mesh_file_IO
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_mpi_read_mesh                                     &
!!     &         (num_pe, id_rank, file_name, mesh_IO, group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   group_IO
!!
!!      subroutine gz_mpi_read_mesh_geometry                            &
!!     &         (num_pe, id_rank, file_name, mesh_IO)
!!      subroutine gz_mpi_read_node_size                                &
!!     &         (num_pe, id_rank, file_name, mesh_IO)
!!      subroutine gz_mpi_read_geometry_size                            &
!!     &         (num_pe, id_rank, file_name, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine gz_mpi_write_mesh_file                               &
!!     &         (num_pe, id_rank, file_name, mesh_IO, group_IO)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   group_IO
!!@endverbatim
!!
      module gz_MPI_mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_calypso_mpi_IO
      use t_mesh_data
      use t_calypso_mpi_IO_param
      use gz_MPI_mesh_data_IO
      use MPI_ascii_data_IO
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
      subroutine gz_mpi_read_mesh                                       &
     &         (num_pe, id_rank, file_name, mesh_IO, group_IO)
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(mesh_groups), intent(inout) ::   group_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped merged mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_geometry_data(IO_param, mesh_IO)
      call gz_mpi_read_mesh_groups(IO_param, group_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine gz_mpi_read_mesh_geometry                              &
     &         (num_pe, id_rank, file_name, mesh_IO)
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped merged mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
      call gz_mpi_read_geometry_data(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
       subroutine gz_mpi_read_node_size                                 &
     &         (num_pe, id_rank, file_name, mesh_IO)
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped merged mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
      call gz_mpi_read_num_node(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_node_size
!
!------------------------------------------------------------------
!
       subroutine gz_mpi_read_geometry_size                             &
      &         (num_pe, id_rank, file_name, mesh_IO)
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped merged mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
      call gz_mpi_read_num_node_ele(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_mesh_file                                 &
     &         (num_pe, id_rank, file_name, mesh_IO, group_IO)
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(in) :: mesh_IO
      type(mesh_groups), intent(in) ::   group_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped merged mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, num_pe, id_rank, IO_param)
      call gz_mpi_write_geometry_data(IO_param, mesh_IO)
      call gz_mpi_write_mesh_groups(IO_param, group_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module gz_MPI_mesh_file_IO
