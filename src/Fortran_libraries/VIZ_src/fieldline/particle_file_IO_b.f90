!>@file  particle_file_IO_b.f90
!!      module particle_file_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine read_particle_file_b(id_rank, file_name,             &
!!     &                                t_IO, particle_IO, ierr)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(time_data), intent(inout) :: t_IO
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine write_particle_file_b                                &
!!     &         (id_rank, file_name, t_IO, particle_IO, ierr)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(time_data), intent(in) :: t_IO
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module particle_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_read_mesh_data
      use t_binary_IO_buffer
      use t_time_data
      use set_mesh_file_names
      use binary_IO
!
      implicit none
!
      integer(kind = kint), parameter :: id_read_mesh =  21
      type(binary_IO_buffer) :: bbuf_p
      private :: id_read_mesh, bbuf_p
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_particle_file_b(id_rank, file_name,               &
     &                                t_IO, particle_IO, ierr)
!
      use domain_data_IO_b
      use node_geometry_IO_b
      use element_connect_IO_b
      use field_data_IO_b
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(inout) :: t_IO
      type(surf_edge_IO_file), intent(inout) :: particle_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      bbuf_p%id_binary = id_read_mesh
      call open_read_binary_file(file_name, id_rank, bbuf_p)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
!
      call read_domain_info_b(id_rank, bbuf_p, particle_IO%comm)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
!
      call read_geometry_info_b(bbuf_p, particle_IO%node)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
!
      call read_element_info_b(bbuf_p, particle_IO%ele)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
!
      call read_vector_in_element_b(bbuf_p, particle_IO%node,           &
     &                              particle_IO%sfed)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
      call read_scalar_in_element_b(bbuf_p, particle_IO%node,           &
     &                              particle_IO%sfed)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
!
      call read_step_data_b(bbuf_p, t_IO)
!
  99  continue
      call close_binary_file(bbuf_p)
      ierr = bbuf_p%ierr_bin
!
      end subroutine read_particle_file_b
!
!------------------------------------------------------------------
!
      subroutine write_particle_file_b                                  &
     &         (id_rank, file_name, t_IO, particle_IO, ierr)
!
      use domain_data_IO_b
      use field_data_IO_b
      use node_geometry_IO_b
      use element_connect_IO_b
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(in) :: t_IO
      type(surf_edge_IO_file), intent(in) :: particle_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      bbuf_p%id_binary = id_read_mesh
      call open_write_binary_file(file_name, bbuf_p)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
!
      call write_domain_info_b(id_rank, particle_IO%comm, bbuf_p)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
!
      call write_geometry_info_b(particle_IO%node, bbuf_p)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
!
      call write_element_info_b(particle_IO%ele, bbuf_p)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
!
      call write_vector_in_element_b(particle_IO%node,                  &
     &                               particle_IO%sfed, bbuf_p)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
      call write_scalar_in_element_b(particle_IO%node,                  &
     &                               particle_IO%sfed, bbuf_p)
      if(bbuf_p%ierr_bin .ne. 0) go to 99
!
      call write_step_data_b(id_rank, t_IO, bbuf_p)
!
  99  continue
      call close_binary_file(bbuf_p)
      ierr = bbuf_p%ierr_bin
!
      end subroutine write_particle_file_b
!
!------------------------------------------------------------------
!
      end module particle_file_IO_b
