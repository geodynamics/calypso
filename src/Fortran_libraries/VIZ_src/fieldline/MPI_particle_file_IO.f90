!>@file  MPI_particle_file_IO.f90
!!      module MPI_particle_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine mpi_read_perticle_file                               &
!!     &         (num_pe, id_rank, file_name, t_IO, particle_IO)
!!        integer, intent(in) :: num_pe, id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(time_data), intent(inout) :: t_IO
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!      subroutine mpi_write_perticle_file(file_name, t_IO, particle_IO)
!!        character(len=kchara), intent(in) :: file_name
!!        type(time_data), intent(in) :: t_IO
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module MPI_particle_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_calypso_mpi_IO_param
      use t_read_mesh_data
      use t_time_data
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine mpi_read_perticle_file                                 &
     &         (num_pe, id_rank, file_name, t_IO, particle_IO)
!
      use m_fem_mesh_labels
      use mesh_data_IO
      use groups_IO
      use local_fline_restart_IO
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use MPI_node_geometry_IO
      use MPI_element_connect_IO
      use field_data_MPI_IO

!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(inout) :: t_IO
      type(surf_edge_IO_file), intent(inout) :: particle_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii particle file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call mpi_skip_read(IO_param, len(hd_fem_para()))
      call mpi_read_domain_info(IO_param, particle_IO%comm)
!
      call mpi_skip_read(IO_param, len(hd_fem_node()))
      call mpi_read_geometry_info(IO_param, particle_IO%node)
      call mpi_skip_read(IO_param, len(hd_particle_connect()))
      call mpi_read_element_info(IO_param, particle_IO%ele)
!
      call mpi_skip_read(IO_param, len(hd_particle_velocity()))
      call mpi_read_vect_in_ele(IO_param, particle_IO%node,             &
     &                          particle_IO%sfed)
!
      call mpi_skip_read(IO_param, len(hd_particle_marker()))
      call mpi_read_scl_in_ele(IO_param, particle_IO%node,              &
     &                         particle_IO%sfed)
!
      call read_field_time_mpi(IO_param%id_file, nprocs,                &
     &                         IO_param%ioff_gl, t_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_perticle_file
!
!------------------------------------------------------------------
!
      subroutine mpi_write_perticle_file(file_name, t_IO, particle_IO)
!
      use m_fem_mesh_labels
      use mesh_data_IO
      use time_data_IO
      use local_fline_restart_IO
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use MPI_node_geometry_IO
      use MPI_element_connect_IO
      use field_data_MPI_IO
!
      character(len=kchara), intent(in) :: file_name
      type(time_data), intent(in) :: t_IO
      type(surf_edge_IO_file), intent(in) :: particle_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii particle file: ', trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_para()), hd_fem_para())
      call mpi_write_domain_info(IO_param, particle_IO%comm)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_node()), hd_fem_node())
      call mpi_write_geometry_info(IO_param, particle_IO%node)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_particle_connect()), hd_particle_connect())
      call mpi_write_element_info(IO_param, particle_IO%ele)
!
      call mpi_write_charahead                                          &
     &  (IO_param, len(hd_particle_velocity()), hd_particle_velocity())
      call mpi_write_vect_in_ele(IO_param, particle_IO%node,            &
     &                          particle_IO%sfed)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_particle_marker()), hd_particle_marker())
      call mpi_write_scl_in_ele(IO_param, particle_IO%node,             &
     &                          particle_IO%sfed)

      call mpi_write_charahead(IO_param,                                &
     &                         len(step_data_buffer(my_rank, t_IO)),    &
     &                         step_data_buffer(my_rank, t_IO))
      call close_mpi_file(IO_param)
!
      end subroutine mpi_write_perticle_file
!
!------------------------------------------------------------------
!
      end module MPI_particle_file_IO
