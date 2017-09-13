!>@file   element_mesh_IO_select.f90
!!@brief  module element_mesh_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine sel_read_ele_mesh                                    &
!!     &         (mesh_file, my_rank_IO, ele_mesh_IO, ierr)
!!      subroutine sel_read_surf_mesh                                   &
!!     &         (mesh_file, my_rank_IO, surf_mesh_IO, ierr)
!!      subroutine sel_read_edge_mesh                                   &
!!     &         (mesh_file, my_rank_IO, edge_mesh_IO, ierr)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!
!!      subroutine sel_write_ele_mesh_file                              &
!!     &         (mesh_file, my_rank_IO, ele_mesh_IO)
!!      subroutine sel_write_surf_mesh_file                             &
!!     &         (mesh_file, my_rank_IO, surf_mesh_IO)
!!      subroutine sel_write_edge_mesh_file                             &
!!     &         (mesh_file, my_rank_IO, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!@endverbatim
!
      module element_mesh_IO_select
!
      use m_precision
!
      use t_file_IO_parameter
      use t_mesh_data
      use m_file_format_switch
!
      use mesh_file_name_by_param
      use element_file_IO
      use element_file_IO_b
#ifdef ZLIB_IO
      use gz_element_file_IO
      use gz_element_file_IO_b
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
      subroutine sel_read_ele_mesh                                      &
     &         (mesh_file, my_rank_IO, ele_mesh_IO, ierr)
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call ele_mesh_file_name_by_param                                  &
     &   (mesh_file, my_rank_IO, file_name)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call input_element_file_b                                       &
     &     (my_rank_IO, file_name, ele_mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_input_element_file_b                                    &
     &     (my_rank_IO, file_name, ele_mesh_IO, ierr)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_input_element_file                                      &
     &     (my_rank_IO, file_name, ele_mesh_IO, ierr)
#endif
!
      else
        call input_element_file                                         &
     &     (my_rank_IO, file_name, ele_mesh_IO, ierr)
      end if 
!
      end subroutine sel_read_ele_mesh
!
!------------------------------------------------------------------
!
      subroutine sel_read_surf_mesh                                     &
     &         (mesh_file, my_rank_IO, surf_mesh_IO, ierr)
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call surf_mesh_file_name_by_param                                 &
     &   (mesh_file, my_rank_IO, file_name)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call input_surface_file_b                                       &
     &     (my_rank_IO, file_name, surf_mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_input_surface_file_b                                    &
     &     (my_rank_IO, file_name, surf_mesh_IO, ierr)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_input_surface_file                                      &
     &     (my_rank_IO, file_name, surf_mesh_IO, ierr)
#endif
!
      else
        call input_surface_file                                         &
     &     (my_rank_IO, file_name, surf_mesh_IO, ierr)
      end if 
!
      end subroutine sel_read_surf_mesh
!
!------------------------------------------------------------------
!
      subroutine sel_read_edge_mesh                                     &
     &         (mesh_file, my_rank_IO, edge_mesh_IO, ierr)
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call edge_mesh_file_name_by_param                                 &
     &   (mesh_file, my_rank_IO, file_name)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call input_edge_file_b                                          &
     &     (my_rank_IO, file_name, edge_mesh_IO, ierr)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_input_edge_file_b                                       &
     &     (my_rank_IO, file_name, edge_mesh_IO, ierr)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_input_edge_file                                         &
     &     (my_rank_IO, file_name, edge_mesh_IO, ierr)
#endif
!
      else
        call input_edge_file                                            &
     &     (my_rank_IO, file_name, edge_mesh_IO, ierr)
      end if 
!
      end subroutine sel_read_edge_mesh
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_ele_mesh_file                                &
     &         (mesh_file, my_rank_IO, ele_mesh_IO)
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      call ele_mesh_file_name_by_param                                  &
     &   (mesh_file, my_rank_IO, file_name)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call output_element_file_b                                      &
     &     (my_rank_IO, file_name, ele_mesh_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_output_element_file_b                                   &
     &     (my_rank_IO, file_name, ele_mesh_IO)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_output_element_file                                     &
     &     (my_rank_IO, file_name, ele_mesh_IO)
#endif
!
      else
        call output_element_file                                        &
     &     (my_rank_IO, file_name, ele_mesh_IO)
      end if
!
      end subroutine sel_write_ele_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_write_surf_mesh_file                               &
     &         (mesh_file, my_rank_IO, surf_mesh_IO)
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      call surf_mesh_file_name_by_param                                 &
     &   (mesh_file, my_rank_IO, file_name)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call output_surface_file_b                                      &
     &     (my_rank_IO, file_name, surf_mesh_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_output_surface_file_b                                   &
     &     (my_rank_IO, file_name, surf_mesh_IO)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_output_surface_file                                     &
     &     (my_rank_IO, file_name, surf_mesh_IO)
#endif
!
      else
        call output_surface_file                                        &
     &     (my_rank_IO, file_name, surf_mesh_IO)
      end if
!
      end subroutine sel_write_surf_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_write_edge_mesh_file                               &
     &         (mesh_file, my_rank_IO, edge_mesh_IO)
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      call edge_mesh_file_name_by_param                                 &
     &   (mesh_file, my_rank_IO, file_name)
!
      if (mesh_file%iflag_format .eq. id_binary_file_fmt) then
        call output_edge_file_b                                         &
     &     (my_rank_IO, file_name, edge_mesh_IO)
!
#ifdef ZLIB_IO
      else if(mesh_file%iflag_format .eq. id_gzip_bin_file_fmt) then
        call gz_output_edge_file_b                                      &
     &     (my_rank_IO, file_name, edge_mesh_IO)
      else if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_output_edge_file                                        &
     &     (my_rank_IO, file_name, edge_mesh_IO)
#endif
!
      else
        call output_edge_file                                           &
     &     (my_rank_IO, file_name, edge_mesh_IO)
      end if
!
      end subroutine sel_write_edge_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module element_mesh_IO_select
