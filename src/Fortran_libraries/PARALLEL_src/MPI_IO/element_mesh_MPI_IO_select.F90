!>@file   element_mesh_MPI_IO_select.f90
!!@brief  module element_mesh_MPI_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine sel_mpi_read_ele_mesh(mesh_file, ele_mesh_IO)
!!      subroutine sel_mpi_read_surf_mesh(mesh_file, surf_mesh_IO)
!!      subroutine sel_mpi_read_edge_mesh(mesh_file, edge_mesh_IO)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine sel_mpi_write_ele_mesh_file(mesh_file, ele_mesh_IO)
!!      subroutine sel_mpi_write_surf_mesh_file(mesh_file, surf_mesh_IO)
!!      subroutine sel_mpi_write_edge_mesh_file(mesh_file, edge_mesh_IO)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!@endverbatim
!
      module element_mesh_MPI_IO_select
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
      use MPI_element_file_IO
      use MPI_element_file_IO_b
      use set_mesh_file_names
      use mesh_file_name_by_param
      use element_mesh_IO_select
!
#ifdef ZLIB_IO
      use gz_MPI_element_file_IO
      use gz_MPI_element_file_IO_b
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
      subroutine sel_mpi_read_ele_mesh(mesh_file, ele_mesh_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
      integer(kind = kint) :: ierr = 0
!
!
      call ele_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_input_element_file_b                                   &
     &     (nprocs, my_rank, file_name, ele_mesh_IO, ierr)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_input_element_file                                     &
     &     (nprocs, my_rank, file_name, ele_mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_input_element_file_b                                &
     &     (nprocs, my_rank, file_name, ele_mesh_IO, ierr)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_input_element_file                                  &
     &     (nprocs, my_rank, file_name, ele_mesh_IO, ierr)
#endif
!
      else
        call sel_read_ele_mesh(mesh_file, my_rank, ele_mesh_IO, ierr)
      end if 
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_ele_mesh
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_surf_mesh(mesh_file, surf_mesh_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
      integer(kind = kint) :: ierr = 0
!
!
      call surf_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_input_surface_file_b                                   &
     &     (nprocs, my_rank, file_name, surf_mesh_IO, ierr)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_input_surface_file                                     &
     &     (nprocs, my_rank, file_name, surf_mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_input_surface_file_b                                &
     &     (nprocs, my_rank, file_name, surf_mesh_IO, ierr)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_input_surface_file                                  &
     &     (nprocs, my_rank, file_name, surf_mesh_IO, ierr)
#endif
!
      else
        call sel_read_surf_mesh(mesh_file, my_rank, surf_mesh_IO, ierr)
      end if 
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_surf_mesh
!
!------------------------------------------------------------------
!
      subroutine sel_mpi_read_edge_mesh(mesh_file, edge_mesh_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
      integer(kind = kint) :: ierr = 0
!
!
      call edge_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_input_edge_file_b                                      &
     &     (nprocs, my_rank, file_name, edge_mesh_IO, ierr)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_input_edge_file                                        &
     &     (nprocs, my_rank, file_name, edge_mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_input_edge_file_b                                   &
     &     (nprocs, my_rank, file_name, edge_mesh_IO, ierr)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_input_edge_file                                     &
     &     (nprocs, my_rank, file_name, edge_mesh_IO, ierr)
#endif
!
      else
        call sel_read_edge_mesh(mesh_file, my_rank, edge_mesh_IO, ierr)
      end if 
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
      end if
!
      end subroutine sel_mpi_read_edge_mesh
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_mpi_write_ele_mesh_file(mesh_file, ele_mesh_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      call ele_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_output_element_file_b                                  &
     &     (nprocs, my_rank, file_name, ele_mesh_IO)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_output_element_file                                    &
     &     (nprocs, my_rank, file_name, ele_mesh_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_output_element_file_b                               &
     &     (nprocs, my_rank, file_name, ele_mesh_IO)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_output_element_file                                 &
     &     (nprocs, my_rank, file_name, ele_mesh_IO)
#endif
!
      else
        call sel_write_ele_mesh_file(mesh_file, my_rank, ele_mesh_IO)
      end if
!
      end subroutine sel_mpi_write_ele_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_mpi_write_surf_mesh_file(mesh_file, surf_mesh_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      call surf_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_output_surface_file_b                                  &
     &     (nprocs, my_rank, file_name, surf_mesh_IO)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_output_surface_file                                    &
     &     (nprocs, my_rank, file_name, surf_mesh_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_output_surface_file_b                               &
     &     (nprocs, my_rank, file_name, surf_mesh_IO)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_output_surface_file                                 &
     &     (nprocs, my_rank, file_name, surf_mesh_IO)
#endif
!
      else
        call sel_write_surf_mesh_file(mesh_file, my_rank, surf_mesh_IO)
      end if
!
      end subroutine sel_mpi_write_surf_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_mpi_write_edge_mesh_file(mesh_file, edge_mesh_IO)
!
      type(field_IO_params), intent(in) ::  mesh_file
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      call edge_mesh_file_name_by_param(mesh_file, my_rank, file_name)
!
      if(mesh_file%iflag_format                                         &
     &     .eq. iflag_single+id_binary_file_fmt) then
        call mpi_output_edge_file_b                                     &
     &     (nprocs, my_rank, file_name, edge_mesh_IO)
      else if(mesh_file%iflag_format .eq. iflag_single) then
        call mpi_output_edge_file                                       &
     &     (nprocs, my_rank, file_name, edge_mesh_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_output_edge_file_b                                  &
     &     (nprocs, my_rank, file_name, edge_mesh_IO)
      else if(mesh_file%iflag_format                                    &
     &        .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_output_edge_file                                    &
     &     (nprocs, my_rank, file_name, edge_mesh_IO)
#endif
!
      else
        call sel_write_edge_mesh_file(mesh_file, my_rank, edge_mesh_IO)
      end if
!
      end subroutine sel_mpi_write_edge_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module element_mesh_MPI_IO_select
