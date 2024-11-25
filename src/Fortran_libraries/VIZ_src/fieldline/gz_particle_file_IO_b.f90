!>@file  gz_particle_file_IO_b.f90
!!      module gz_particle_file_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine gz_read_particle_file_b(id_rank, file_name,          &
!!     &                                   t_IO, particle_IO, ierr)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(time_data), intent(inout) :: t_IO
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine gz_write_particle_file_b                             &
!!     &         (id_rank, file_name, t_IO, particle_IO)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(time_data), intent(in) :: t_IO
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module gz_particle_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_read_mesh_data
      use t_buffer_4_gzip
      use t_time_data
      use set_mesh_file_names
      use binary_IO
!
      implicit none
!
      type(buffer_4_gzip), private, save :: zbuf_p
      character, pointer, private, save :: FPz_p
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_particle_file_b(id_rank, file_name,            &
     &                                   t_IO, particle_IO, ierr)
!
      use gzip_file_access
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
      use gz_field_data_IO_b
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(inout) :: t_IO
      type(surf_edge_IO_file), intent(inout) :: particle_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary particle file: ', trim(file_name)
!
      call open_rd_gzfile_b(FPz_p, file_name, id_rank, zbuf_p)
      if(zbuf_p%ierr_zlib .ne. 0) go to 99
!
      call gz_read_domain_info_b(FPz_p, id_rank, zbuf_p,                &
     &                           particle_IO%comm)
      if(zbuf_p%ierr_zlib .ne. 0) return
!
      call gz_read_geometry_info_b(FPz_p, zbuf_p, particle_IO%node)
      if(zbuf_p%ierr_zlib .ne. 0) return
!
      call gz_read_element_info_b(FPz_p, zbuf_p, particle_IO%ele)
      if(zbuf_p%ierr_zlib .ne. 0) return
!
      call gz_read_vector_in_element_b(FPz_p, zbuf_p, particle_IO%node, &
     &                                 particle_IO%sfed)
      if(zbuf_p%ierr_zlib .ne. 0) return
!
      call gz_read_scalar_in_element_b(FPz_p, zbuf_p, particle_IO%node, &
     &                                 particle_IO%sfed)
      if(zbuf_p%ierr_zlib .ne. 0) return
!
      call gz_read_step_data_b(FPz_p, zbuf_p, id_rank,                  &
     &                         t_IO%i_time_step, t_IO%time, t_IO%dt)
!
  99  continue
      call close_gzfile_b(FPz_p)
      ierr = zbuf_p%ierr_zlib
!
      end subroutine gz_read_particle_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_particle_file_b                               &
     &         (id_rank, file_name, t_IO, particle_IO)
!
      use gzip_file_access
      use gz_domain_data_IO_b
      use gz_node_geometry_IO_b
      use gz_element_connect_IO_b
      use gz_field_data_IO_b
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(time_data), intent(in) :: t_IO
      type(surf_edge_IO_file), intent(in) :: particle_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped binary particle file: ', trim(file_name)
!
      call open_wt_gzfile_b(FPz_p, file_name, zbuf_p)
!
      call gz_write_geometry_info_b(FPz_p, particle_IO%node, zbuf_p)
      if(zbuf_p%ierr_zlib .ne. 0) return
!
      call gz_write_element_info_b(FPz_p,particle_IO%ele, zbuf_p)
      if(zbuf_p%ierr_zlib .ne. 0) return
!
      call gz_write_vector_in_element_b(FPz_p, particle_IO%node,        &
     &                                  particle_IO%sfed, zbuf_p)
      if(zbuf_p%ierr_zlib .ne. 0) return
!
      call gz_write_scalar_in_element_b(FPz_p, particle_IO%node,        &
     &                                  particle_IO%sfed, zbuf_p)
!
      call gz_write_step_data_b(FPz_p, id_rank, t_IO%i_time_step,       &
     &                          t_IO%time, t_IO%dt, zbuf_p)
      call close_gzfile_b(FPz_p)
!
      end subroutine gz_write_particle_file_b
!
!------------------------------------------------------------------
!
      end module gz_particle_file_IO_b
