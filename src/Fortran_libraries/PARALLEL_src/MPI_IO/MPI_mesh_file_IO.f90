!>@file  MPI_mesh_file_IO.f90
!!      module MPI_mesh_file_IO
!!
!!@author  H. Matsui
!!@date      Programmed in Aug., 2016
!!
!>@brief ASCII mesh file IO
!!
!!@verbatim
!!      subroutine mpi_read_mesh_file                                   &
!!     &         (num_pe, id_rank, file_name, mesh_IO, group_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   group_IO
!!      subroutine mpi_read_mesh_geometry                               &
!!     &         (num_pe, id_rank, file_name, mesh_IO)
!!      subroutine mpi_read_node_size                                   &
!!     &         (num_pe, id_rank, file_name, mesh_IO)
!!      subroutine mpi_read_geometry_size                               &
!!     &         (num_pe, id_rank, file_name, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine mpi_write_mesh_file                                  &
!!     &         (num_pe, id_rank, file_name, mesh_IO, group_IO)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   group_IO
!!@endverbatim
!
      module MPI_mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
      use MPI_mesh_data_IO
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
      subroutine mpi_read_mesh_file                                     &
     &         (num_pe, id_rank, file_name, mesh_IO, group_IO)
!
      use mesh_data_IO
      use groups_IO
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(mesh_groups), intent(inout) ::   group_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call mpi_read_geometry_data(IO_param, mesh_IO)
      call mpi_read_mesh_groups(IO_param, group_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_geometry                                 &
     &         (num_pe, id_rank, file_name, mesh_IO)
!
      use mesh_data_IO
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
      call mpi_read_geometry_data(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
!
      end subroutine mpi_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_node_size                                     &
     &         (num_pe, id_rank, file_name, mesh_IO)
!
      use mesh_data_IO
      use node_geometry_IO
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
      call mpi_read_num_node(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
!
      end subroutine mpi_read_node_size
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_size                                 &
     &         (num_pe, id_rank, file_name, mesh_IO)
!
      use mesh_data_IO
      use element_connect_IO
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
      call mpi_read_num_node_ele(IO_param, mesh_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_mesh_file                                    &
     &         (num_pe, id_rank, file_name, mesh_IO, group_IO)
!
      use m_machine_parameter
      use m_fem_mesh_labels
      use mesh_data_IO
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(in) :: mesh_IO
      type(mesh_groups), intent(in) ::   group_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call mpi_write_geometry_data(IO_param, mesh_IO)
      call mpi_write_mesh_groups(IO_param, group_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine mpi_write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module MPI_mesh_file_IO
