!>@file   particle_MPI_IO_select.f90
!!@brief  module particle_MPI_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine sel_mpi_read_particle_file(mesh_file, istep_file,    &
!!     &                                      t_IO, particle_IO)
!!        integer(kind = kint), intent(in) :: istep_file
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(time_data), intent(inout) :: t_IO
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!
!!      subroutine sel_mpi_write_particle_file(mesh_file, istep_file,   &
!!     &                                       t_IO, particle_IO)
!!        integer(kind = kint), intent(in) :: istep_file
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(time_data), intent(in) :: t_IO
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!@endverbatim
!
      module particle_MPI_IO_select
!
      use m_precision
      use calypso_mpi
!
      use m_file_format_switch
      use t_file_IO_parameter
      use t_mesh_data
      use t_time_data
!
      use particle_file_IO_select
!
      use MPI_particle_file_IO
      use MPI_particle_file_IO_b
      use mesh_file_name_by_param
      use element_mesh_IO_select
!
#ifdef ZLIB_IO
      use gz_MPI_particle_file_IO
      use gz_MPI_particle_file_IO_b
#endif
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_mpi_read_particle_file(mesh_file, istep_file,      &
     &                                      t_IO, particle_IO)
!
      use set_element_mesh_file_names
!
      integer(kind = kint), intent(in) :: istep_file
      type(field_IO_params), intent(in) ::  mesh_file
      type(time_data), intent(inout) :: t_IO
      type(surf_edge_IO_file), intent(inout) :: particle_IO
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = set_tracer_file_name(mesh_file%file_prefix,           &
     &                                 mesh_file%iflag_format,          &
     &                                 my_rank, istep_file)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_particle_file_b                                   &
     &     (nprocs, my_rank, file_name, t_IO, particle_IO)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_read_perticle_file                                     &
     &     (nprocs, my_rank, file_name, t_IO, particle_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_particle_file_b(nprocs, my_rank, file_name,    &
     &                                   t_IO, particle_IO)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_particle_file(nprocs, my_rank,                 &
     &                                 file_name, t_IO, particle_IO)
#endif
!
      else
        call sel_read_particle_file(mesh_file, my_rank, istep_file,     &
     &                              t_IO, particle_IO, ierr)
      end if 
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Tracer data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_particle_file
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_particle_file(mesh_file, istep_file,     &
     &                                       t_IO, particle_IO)
!
      use set_element_mesh_file_names
!
      integer(kind = kint), intent(in) :: istep_file
      type(field_IO_params), intent(in) ::  mesh_file
      type(time_data), intent(in) :: t_IO
      type(surf_edge_IO_file), intent(in) :: particle_IO
!
      character(len=kchara) :: file_name
!
      file_name = set_tracer_file_name(mesh_file%file_prefix,           &
     &                                 mesh_file%iflag_format,          &
     &                                 my_rank, istep_file)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_particle_file_b(file_name, t_IO, particle_IO)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_write_perticle_file(file_name, t_IO, particle_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_particle_file_b(file_name, t_IO, particle_IO)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_particle_file(file_name, t_IO, particle_IO)
#endif
!
      else
        call sel_write_particle_file(mesh_file, my_rank, istep_file,    &
     &                               t_IO, particle_IO)
      end if
!
      end subroutine sel_mpi_write_particle_file
!
!  ---------------------------------------------------------------------
!
      end module particle_MPI_IO_select
