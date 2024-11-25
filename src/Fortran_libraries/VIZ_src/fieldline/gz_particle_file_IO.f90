!>@file  gz_particle_file_IO.f90
!!      module gz_particle_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine gz_read_particle_file                                &
!!     &         (id_rank, file_name, t_IO, particle_IO, ierr)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(time_data), intent(inout) :: t_IO
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine gz_write_particle_file                               &
!!     &         (id_rank, file_name, t_IO, particle_IO)
!!        integer, intent(in) :: id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(time_data), intent(in) :: t_IO
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module gz_particle_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_read_mesh_data
      use t_buffer_4_gzip
      use t_time_data
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_p
      character, pointer, private, save :: FPz_p
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_particle_file                                  &
     &         (id_rank, file_name, t_IO, particle_IO, ierr)
!
      use skip_gz_comment
      use local_fline_restart_IO
      use gzip_file_access
      use gz_domain_data_IO
      use gz_node_geometry_IO
      use gz_element_connect_IO
      use gz_field_data_IO
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
     &   'Read gzipped particle file: ', trim(file_name)
!
      call open_rd_gzfile_a(FPz_p, file_name, zbuf_p)
!
!      write(*,*) 'gz_read_domain_info'
      call gz_read_domain_info                                          &
     &    (FPz_p, id_rank, particle_IO%comm, zbuf_p, ierr)
!
      call gz_read_geometry_info(FPz_p, particle_IO%node,zbuf_p)
      call gz_read_element_info(FPz_p, particle_IO%ele, zbuf_p)
!
      call gz_read_vector_in_element(FPz_p, particle_IO%node,           &
     &                            particle_IO%sfed, zbuf_p)
      call gz_read_scalar_in_element(FPz_p, particle_IO%node,           &
     &                            particle_IO%sfed, zbuf_p)
!
      call read_gz_step_data(FPz_p, id_rank, t_IO%i_time_step,          &
     &                       t_IO%time, t_IO%dt, zbuf_p, ierr)
      call close_gzfile_a(FPz_p, zbuf_p)
!
      end subroutine gz_read_particle_file
!
!------------------------------------------------------------------
!
      subroutine gz_write_particle_file                                 &
     &         (id_rank, file_name, t_IO, particle_IO)
!
      use m_fem_mesh_labels
      use skip_gz_comment
      use local_fline_restart_IO
      use gzip_file_access
      use gz_domain_data_IO
      use gz_node_geometry_IO
      use gz_element_connect_IO
      use gz_field_data_IO
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(time_data), intent(in) :: t_IO
      type(surf_edge_IO_file), intent(in) :: particle_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped particle file: ', trim(file_name)
!
      call open_wt_gzfile_a(FPz_p, file_name, zbuf_p)
!
      zbuf_p%fixbuf(1) = hd_fem_node() // char(0)
      call gz_write_textbuf_no_lf(FPz_p, zbuf_p)
!
      call gz_write_geometry_info(FPz_p, particle_IO%node, zbuf_p)
!
      zbuf_p%fixbuf(1) = hd_particle_connect() // char(0)
      call gz_write_textbuf_no_lf(FPz_p, zbuf_p)
      call gz_write_element_info(FPz_p, particle_IO%ele, zbuf_p)
!
      zbuf_p%fixbuf(1) = hd_particle_velocity() // char(0)
      call gz_write_textbuf_no_lf(FPz_p, zbuf_p)
      call gz_write_vector_in_element(FPz_p, particle_IO%node,          &
     &                                particle_IO%sfed, zbuf_p)
!
      zbuf_p%fixbuf(1) = hd_particle_marker() // char(0)
      call gz_write_textbuf_no_lf(FPz_p, zbuf_p)
      call gz_write_scalar_in_element(FPz_p, particle_IO%node,          &
     &                                particle_IO%sfed, zbuf_p)
!
      call write_gz_step_data(FPz_p, id_rank, t_IO%i_time_step,         &
     &                         t_IO%time, t_IO%dt, zbuf_p)
      call close_gzfile_a(FPz_p, zbuf_p)
!
      end subroutine gz_write_particle_file
!
!------------------------------------------------------------------
!
      end module gz_particle_file_IO
