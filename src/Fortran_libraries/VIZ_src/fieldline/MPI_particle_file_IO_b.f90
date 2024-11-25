!>@file   MPI_particle_file_IO_b.f90
!!@brief  module MPI_particle_file_IO_b
!!
!!@author H.Matsui
!!@date      Programmed in June, 2024
!
!>@brief  particle file IO for gxipped format
!!
!!@verbatim
!!      subroutine mpi_read_particle_file_b(num_pe, id_rank, file_name, &
!!     &                                    t_IO, particle_IO)
!!        integer, intent(in) :: num_pe, id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!        type(time_data), intent(inout) :: t_IO
!!      subroutine mpi_write_particle_file_b                            &
!!     &         (file_name, t_IO, particle_IO)
!!        character(len=kchara), intent(in) :: file_name
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!        type(time_data), intent(in) :: t_IO
!!@endverbatim
!
      module MPI_particle_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_calypso_mpi_IO
      use t_read_mesh_data
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
      use t_time_data
!
      implicit none
!
      type(calypso_MPI_IO_params), private, save :: IO_param
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_particle_file_b(num_pe, id_rank, file_name,   &
     &                                    t_IO, particle_IO)
!
      use MPI_mesh_data_IO_b
      use MPI_groups_IO_b
      use MPI_domain_data_IO_b
      use MPI_node_geometry_IO_b
      use MPI_element_connect_IO_b
      use MPI_ascii_data_IO
      use field_block_MPI_IO_b
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(surf_edge_IO_file), intent(inout) :: particle_IO
      type(time_data), intent(inout) :: t_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged perticle file: ', trim(file_name)
!
      call open_read_mpi_file_b                                         &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call mpi_read_domain_info_b(IO_param, particle_IO%comm)
      call mpi_read_geometry_info_b(IO_param, particle_IO%node)
      call mpi_read_element_info_b(IO_param, particle_IO%ele)

      call mpi_read_vect_in_ele_b(IO_param, particle_IO%node,           &
     &                            particle_IO%sfed)
      call mpi_read_scl_in_ele_b(IO_param, particle_IO%node,            &
     &                           particle_IO%sfed)
!
      call read_field_time_mpi_b(num_pe, IO_param, t_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_particle_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_particle_file_b                              &
     &         (file_name, t_IO, particle_IO)
!
      use m_machine_parameter
      use MPI_mesh_data_IO_b
      use MPI_ascii_data_IO
      use MPI_domain_data_IO_b
      use MPI_node_geometry_IO_b
      use MPI_element_connect_IO_b
      use field_block_MPI_IO_b
!
      character(len=kchara), intent(in) :: file_name
      type(surf_edge_IO_file), intent(in) :: particle_IO
      type(time_data), intent(in) :: t_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped binary merged perticle file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param)
      call mpi_write_domain_info_b(IO_param, particle_IO%comm)
!
      call mpi_write_geometry_info_b(IO_param, particle_IO%node)
      call mpi_write_element_info_b(IO_param, particle_IO%ele)
!
      call mpi_write_vect_in_ele_b(IO_param, particle_IO%node,          &
     &                             particle_IO%sfed)
      call mpi_write_scl_in_ele_b(IO_param, particle_IO%node,           &
     &                            particle_IO%sfed)
!
      call write_field_time_mpi_b(IO_param,                             &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_write_particle_file_b
!
!  ---------------------------------------------------------------------
!
      end module MPI_particle_file_IO_b
