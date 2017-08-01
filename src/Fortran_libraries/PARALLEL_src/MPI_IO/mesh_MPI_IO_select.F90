!>@file   mesh_MPI_IO_select.f90
!!@brief  module mesh_MPI_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine sel_mpi_read_mesh(mesh_file, fem_IO)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_data), intent(inout) :: fem_IO
!!
!!      subroutine sel_mpi_read_mesh_geometry(mesh_file, mesh_IO)
!!      subroutine sel_mpi_read_node_size(mesh_file, mesh_IO)
!!      subroutine sel_mpi_read_geometry_size(mesh_file, mesh_IO)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine sel_mpi_write_mesh_file(mesh_file, fem_IO)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_data), intent(inout) :: fem_IO
!!@endverbatim
!
      module mesh_MPI_IO_select
!
      use m_precision
      use calypso_mpi
!
      use m_file_format_switch
      use t_file_IO_parameter
      use t_mesh_data
!
      use mesh_IO_select
!
      use MPI_mesh_file_IO
      use MPI_mesh_file_IO_b
      use set_mesh_file_names
!
#ifdef ZLIB_IO
      use gz_MPI_mesh_file_IO
      use gz_MPI_mesh_file_IO_b
#endif
!
      implicit none
!
      character(len=kchara), private :: file_name
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_mpi_read_mesh(mesh_file, fem_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_data), intent(inout) :: fem_IO
!
      integer(kind = kint) :: ierr = 0
!
!
      call set_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_mesh_file_b(nprocs, my_rank, file_name, fem_IO)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_read_mesh_file(nprocs, my_rank, file_name, fem_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_mesh_file_b                                    &
     &     (nprocs, my_rank, file_name, fem_IO)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_mesh(nprocs, my_rank, file_name, fem_IO)
#endif
!
      else
        call sel_read_mesh(mesh_file, my_rank, fem_IO, ierr)
      end if 
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine sel_mpi_read_mesh_geometry(mesh_file, mesh_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(inout) :: mesh_IO
!
      integer(kind = kint) :: ierr = 0
!
!
      call set_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_mesh_geometry_b                                   &
     &     (nprocs, my_rank, file_name, mesh_IO)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_read_mesh_geometry                                     &
     &     (nprocs, my_rank, file_name, mesh_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_mesh_geometry_b                                &
     &     (nprocs, my_rank, file_name, mesh_IO)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_mesh_geometry                                  &
     &     (nprocs, my_rank, file_name, mesh_IO)
#endif
!
      else
        call sel_read_mesh_geometry(mesh_file, my_rank, mesh_IO, ierr)
      end if 
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine sel_mpi_read_node_size(mesh_file, mesh_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(inout) :: mesh_IO
!
      integer(kind = kint) :: ierr = 0
!
!
      call set_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_node_size_b(nprocs, my_rank, file_name, mesh_IO)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_read_node_size(nprocs, my_rank, file_name, mesh_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_node_size_b                                    &
     &     (nprocs, my_rank, file_name, mesh_IO)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_node_size(nprocs, my_rank, file_name, mesh_IO)
#endif
!
      else
        call sel_read_node_size(mesh_file, my_rank, mesh_IO, ierr)
      end if 
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_node_size
!
!------------------------------------------------------------------
!
       subroutine sel_mpi_read_geometry_size(mesh_file, mesh_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(inout) :: mesh_IO
!
      integer(kind = kint) :: ierr = 0
!
!
      call set_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_geometry_size_b                                   &
     &     (nprocs, my_rank, file_name, mesh_IO)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_read_geometry_size                                     &
     &     (nprocs, my_rank, file_name, mesh_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_read_geometry_size_b                                &
     &     (nprocs, my_rank, file_name, mesh_IO)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_geometry_size                                  &
     &     (nprocs, my_rank, file_name, mesh_IO)
#endif
!
      else
        call sel_read_geometry_size(mesh_file, my_rank, mesh_IO, ierr)
      end if 
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_mesh_file(mesh_file, fem_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_data), intent(inout) :: fem_IO
!
!
      call set_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_mesh_file_b(nprocs, my_rank, file_name, fem_IO)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_write_mesh_file(nprocs, my_rank, file_name, fem_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_write_mesh_file_b                                   &
     &     (nprocs, my_rank, file_name, fem_IO)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_mesh_file                                     &
     &     (nprocs, my_rank, file_name, fem_IO)
#endif
!
      else
        call sel_write_mesh_file(mesh_file, my_rank, fem_IO)
      end if
!
      end subroutine sel_mpi_write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module mesh_MPI_IO_select
