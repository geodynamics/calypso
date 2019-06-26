!>@file   mesh_IO_select.F90
!!@brief  module mesh_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine sel_read_mesh                                        &
!!     &         (mesh_file, id_rank, mesh_IO, group_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!        type(mesh_groups), intent(inout) ::   group_IO
!!
!!      subroutine sel_read_mesh_geometry                               &
!!     &         (mesh_file, id_rank, mesh_IO, ierr)
!!      subroutine sel_read_node_size                                   &
!!     &         (mesh_file, id_rank, mesh_IO, ierr)
!!      subroutine sel_read_geometry_size                               &
!!     &         (mesh_file, id_rank, mesh_IO, ierr)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine sel_write_mesh_file                                  &
!!     &         (mesh_file, id_rank, mesh_IO, group_IO)
!!        type(mesh_geometry), intent(in) :: mesh_IO
!!        type(mesh_groups), intent(in) ::   group_IO
!!@endverbatim
!
      module mesh_IO_select
!
      use m_precision
!
      use t_file_IO_parameter
      use t_mesh_data
      use m_file_format_switch
!
      use mesh_file_name_by_param
      use mesh_file_IO
      use mesh_file_IO_b
      use gz_mesh_file_IO
      use gz_mesh_file_IO_b
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
      subroutine sel_read_mesh                                          &
     &         (mesh_file, id_rank, mesh_IO, group_IO, ierr)
!
      use set_mesh_file_names
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      type(mesh_groups), intent(inout) ::   group_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      file_name = set_mesh_file_name                                    &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call read_mesh_file_b                                           &
     &     (id_rank, file_name, mesh_IO, group_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_read_mesh_file_b                                        &
     &     (id_rank, file_name, mesh_IO, group_IO, ierr)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_read_mesh(id_rank, file_name, mesh_IO, group_IO, ierr)
#endif
!
      else
        call read_mesh_file                                             &
     &     (id_rank, file_name, mesh_IO, group_IO, ierr)
      end if 
!
      end subroutine sel_read_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_mesh_geometry                                 &
     &         (mesh_file, id_rank, mesh_IO, ierr)
!
      use set_mesh_file_names
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      file_name = set_mesh_file_name                                    &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call read_mesh_geometry_b(id_rank, file_name, mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_read_mesh_geometry_b                                    &
     &     (id_rank, file_name, mesh_IO, ierr)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_read_mesh_geometry                                      &
     &     (id_rank, file_name, mesh_IO, ierr)
#endif
!
      else
        call read_mesh_geometry(id_rank, file_name, mesh_IO, ierr)
      end if 
!
      end subroutine sel_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_node_size                                     &
     &         (mesh_file, id_rank, mesh_IO, ierr)
!
      use set_mesh_file_names
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      file_name = set_mesh_file_name                                    &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call read_node_size_b(id_rank, file_name, mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_read_node_size_b                                        &
     &     (id_rank, file_name, mesh_IO, ierr)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_read_node_size(id_rank, file_name, mesh_IO, ierr)
#endif
!
      else
        call read_node_size(id_rank, file_name, mesh_IO, ierr)
      end if 
!
      end subroutine sel_read_node_size
!
!------------------------------------------------------------------
!
      subroutine sel_read_geometry_size                                 &
     &         (mesh_file, id_rank, mesh_IO, ierr)
!
      use set_mesh_file_names
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      file_name =  set_mesh_file_name                                   &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call read_geometry_size_b(id_rank, file_name, mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_read_geometry_size_b                                    &
     &     (id_rank, file_name, mesh_IO, ierr)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_read_geometry_size                                      &
     &     (id_rank, file_name, mesh_IO, ierr)
#endif
!
      else
        call read_geometry_size(id_rank, file_name, mesh_IO, ierr)
      end if 
!
      end subroutine sel_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_mesh_file                                    &
     &         (mesh_file, id_rank, mesh_IO, group_IO)
!
      use set_mesh_file_names
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
      type(mesh_geometry), intent(in) :: mesh_IO
      type(mesh_groups), intent(in) ::   group_IO

      integer(kind = kint) :: ierr = 0
!
!
      file_name = set_mesh_file_name                                    &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call write_mesh_file_b                                          &
     &     (id_rank, file_name, mesh_IO, group_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_write_mesh_file_b                                       &
     &     (id_rank, file_name, mesh_IO, group_IO)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_write_mesh_file(id_rank, file_name, mesh_IO, group_IO)
#endif
!
      else
        call write_mesh_file(id_rank, file_name, mesh_IO, group_IO)
      end if
!
      end subroutine sel_write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module mesh_IO_select
