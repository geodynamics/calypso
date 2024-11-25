!>@file   gz_MPI_particle_file_IO_b.f90
!!@brief  module gz_MPI_particle_file_IO_b
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  particle file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_mpi_read_particle_file_b                          &
!!     &         (num_pe, id_rank, file_name, t_IO, particle_IO)
!!        integer, intent(in) :: num_pe, id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!        type(time_data), intent(inout) :: t_IO
!!      subroutine gz_mpi_write_particle_file_b                         &
!!     &         (file_name, t_IO, particle_IO)
!!        character(len=kchara), intent(in) :: file_name
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!        type(time_data), intent(in) :: t_IO
!!@endverbatim
      module gz_MPI_particle_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_calypso_mpi_IO
      use t_time_data
      use t_read_mesh_data
      use t_calypso_mpi_IO_param
      use gz_MPI_mesh_data_IO_b
      use MPI_ascii_data_IO
      use t_time_data
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
      subroutine gz_mpi_read_particle_file_b                            &
     &         (num_pe, id_rank, file_name, t_IO, particle_IO)
!
      use m_machine_parameter
      use gz_MPI_binary_datum_IO
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
      use gz_field_block_MPI_IO_b
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(surf_edge_IO_file), intent(inout) :: particle_IO
      type(time_data), intent(inout) :: t_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged particle file: ', trim(file_name)
!
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_domain_info_b(IO_param, particle_IO%comm)
      call gz_mpi_read_geometry_info_b(IO_param, particle_IO%node)
!  ----  read element data -------
      call gz_mpi_read_ele_info_b(IO_param, particle_IO%ele)

      call gz_mpi_read_vect_in_ele_b(IO_param, particle_IO%node,        &
     &                               particle_IO%sfed)
      call gz_mpi_read_scl_in_ele_b(IO_param, particle_IO%node,         &
     &                              particle_IO%sfed)
!
      call gz_read_step_data_mpi_b(IO_param,                            &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_particle_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_particle_file_b                           &
     &         (file_name, t_IO, particle_IO)
!
      use m_machine_parameter
      use gz_MPI_binary_datum_IO
      use MPI_binary_head_IO
      use MPI_ascii_data_IO
      use gz_field_block_MPI_IO_b
!
      character(len=kchara), intent(in) :: file_name
      type(surf_edge_IO_file), intent(in) :: particle_IO
      type(time_data), intent(in) :: t_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped binary merged perticle file: ', trim(file_name)
!
      call open_write_gz_mpi_file_b(file_name, IO_param)
      call gz_mpi_write_domain_info_b(IO_param, particle_IO%comm)
!
      call gz_mpi_write_geometry_info_b(IO_param, particle_IO%node)
      call gz_mpi_write_element_info_b(IO_param, particle_IO%ele)
!
      call gz_mpi_write_vect_in_ele_b(IO_param, particle_IO%node,      &
     &                                particle_IO%sfed)
      call gz_mpi_write_scl_in_ele_b(IO_param, particle_IO%node,       &
     &                               particle_IO%sfed)
!
      call gz_write_field_time_mpi_b(IO_param,                          &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_write_particle_file_b
!
!  ---------------------------------------------------------------------
!
      end module gz_MPI_particle_file_IO_b
