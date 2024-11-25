!>@file  particle_file_IO.f90
!!      module particle_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine read_particle_file                                   &
!!     &         (id_rank, file_name, t_IO, particle_IO, ierr)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(time_data), intent(inout) :: t_IO
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine write_particle_file                                  &
!!     &         (id_rank, file_name, t_IO, particle_IO)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(time_data), intent(in) :: t_IO
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module particle_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_read_mesh_data
      use set_mesh_file_names
      use t_time_data
!
      implicit none
!
!   mesh file code
      integer(kind = kint), parameter ::  input_file_code = 14
      private :: input_file_code
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_particle_file                                     &
     &         (id_rank, file_name, t_IO, particle_IO, ierr)
!
      use m_fem_mesh_labels
      use mesh_data_IO
      use time_data_IO
      use local_fline_restart_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(time_data), intent(inout) :: t_IO
      type(surf_edge_IO_file), intent(inout) :: particle_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii particle file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
!
      call read_domain_info(input_file_code, id_rank,                   &
     &                      particle_IO%comm, ierr)
      if(ierr .ne. 0) go to 99
!
      call read_geometry_info(input_file_code, particle_IO%node, ierr)
      if(ierr .ne. 0) go to 99
!
      call read_element_info(input_file_code, particle_IO%ele, ierr)
      if(ierr .ne. 0) go to 99
!
      call read_vector_in_element(input_file_code, particle_IO%node,    &
     &                            particle_IO%sfed, ierr)
      if(ierr .ne. 0) go to 99
!
      call read_scalar_in_element(input_file_code, particle_IO%node,    &
     &                            particle_IO%sfed, ierr)
      if(ierr .ne. 0) go to 99
!
      call read_step_data(input_file_code, t_IO, ierr)

  99  continue
      close(input_file_code)
!
      end subroutine read_particle_file
!
!  ---------------------------------------------------------------------
!
      subroutine write_particle_file                                    &
     &         (id_rank, file_name, t_IO, particle_IO)
!
      use m_fem_mesh_labels
      use mesh_data_IO
      use time_data_IO
      use local_fline_restart_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(time_data), intent(in) :: t_IO
      type(surf_edge_IO_file), intent(in) :: particle_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii particle file: ', trim(file_name)
!
      open(input_file_code, file = file_name, form = 'formatted')
!
      write(input_file_code,'(a)', advance='NO') hd_fem_para()
      call write_domain_info(input_file_code, id_rank, particle_IO%comm)
!
      write(input_file_code,'(a)', advance='NO') hd_fem_node()
      call write_geometry_info(input_file_code, particle_IO%node)
!
      write(input_file_code,'(a)', advance='NO') hd_particle_connect()
      call write_element_info(input_file_code, particle_IO%ele)
!
      write(input_file_code,'(a)', advance='NO') hd_particle_velocity()
      call write_vector_in_element(input_file_code, particle_IO%node,   &
     &                             particle_IO%sfed)
!
      write(input_file_code,'(a)', advance='NO') hd_particle_marker()
      call write_scalar_in_element(input_file_code, particle_IO%node,   &
     &                            particle_IO%sfed)
!
      call write_step_data(input_file_code, id_rank, t_IO)
!
      close(input_file_code)
!
      end subroutine write_particle_file
!
!  ---------------------------------------------------------------------
!
      end module particle_file_IO
