!>@file   mesh_file_name_by_param.f90
!!@brief  module mesh_file_name_by_param
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine set_mesh_file_name_by_param                          &
!!     &         (mesh_file, my_rank_IO, mesh_file_name)
!!      subroutine ele_mesh_file_name_by_param                          &
!!     &         (mesh_file, my_rank_IO, mesh_file_name)
!!      subroutine surf_mesh_file_name_by_param                         &
!!     &         (mesh_file, my_rank_IO, mesh_file_name)
!!      subroutine edge_mesh_file_name_by_param                         &
!!     &         (mesh_file, my_rank_IO, mesh_file_name)
!!
!!      integer(kind = kint) function check_exist_mesh                  &
!!     &                            (mesh_file, my_rank_IO)
!!      integer(kind = kint) function check_exist_ele_mesh              &
!!     &                            (mesh_file, my_rank_IO)
!!      integer(kind = kint) function check_exist_surf_mesh             &
!!     &                            (mesh_file, my_rank_IO)
!!      integer(kind = kint) function check_exist_edge_mesh             &
!!     &                            (mesh_file, my_rank_IO)
!!@endverbatim
!
      module mesh_file_name_by_param
!
      use m_precision
!
      use t_file_IO_parameter
      use m_file_format_switch
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_file_name_by_param                            &
     &         (mesh_file, my_rank_IO, mesh_file_name)
!
      use set_mesh_file_names
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
      character(len=kchara), intent(inout) :: mesh_file_name
!
!
      call set_mesh_file_name                                           &
     &   (mesh_file%file_prefix, mesh_file%iflag_format,                &
     &    my_rank_IO, mesh_file_name)
!
      end subroutine set_mesh_file_name_by_param
!
!  ---------------------------------------------------------------------
!
      subroutine ele_mesh_file_name_by_param                            &
     &         (mesh_file, my_rank_IO, mesh_file_name)
!
      use set_mesh_file_names
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
      character(len=kchara), intent(inout) :: mesh_file_name
!
!
      call set_ele_comm_file_name                                       &
     &   (mesh_file%file_prefix, mesh_file%iflag_format,                &
     &    my_rank_IO, mesh_file_name)
!
      end subroutine ele_mesh_file_name_by_param
!
!  ---------------------------------------------------------------------
!
      subroutine surf_mesh_file_name_by_param                           &
     &         (mesh_file, my_rank_IO, mesh_file_name)
!
      use set_mesh_file_names
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
      character(len=kchara), intent(inout) :: mesh_file_name
!
!
      call set_surf_mesh_file_name                                      &
     &   (mesh_file%file_prefix, mesh_file%iflag_format,                &
     &    my_rank_IO, mesh_file_name)
!
      end subroutine surf_mesh_file_name_by_param
!
!  ---------------------------------------------------------------------
!
      subroutine edge_mesh_file_name_by_param                           &
     &         (mesh_file, my_rank_IO, mesh_file_name)
!
      use set_mesh_file_names
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
      character(len=kchara), intent(inout) :: mesh_file_name
!
!
      call set_edge_mesh_file_name                                      &
     &   (mesh_file%file_prefix, mesh_file%iflag_format,                &
     &    my_rank_IO, mesh_file_name)
!
      end subroutine edge_mesh_file_name_by_param
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_mesh                    &
     &                            (mesh_file, my_rank_IO)
!
      use delete_data_files
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
!
      character(len=kchara) :: file_name
!
!
      call set_mesh_file_name_by_param                                  &
     &   (mesh_file, my_rank_IO, file_name)
!
      check_exist_mesh = check_file_exist(file_name)
!
      return
      end function check_exist_mesh
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_ele_mesh                &
     &                            (mesh_file, my_rank_IO)
!
      use delete_data_files
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
!
      character(len=kchara) :: file_name
!
!
      call ele_mesh_file_name_by_param                                  &
     &   (mesh_file, my_rank_IO, file_name)
!
      check_exist_ele_mesh = check_file_exist(file_name)
!
      return
      end function check_exist_ele_mesh
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_surf_mesh               &
     &                            (mesh_file, my_rank_IO)
!
      use delete_data_files
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
!
      character(len=kchara) :: file_name
!
!
      call surf_mesh_file_name_by_param                                 &
     &   (mesh_file, my_rank_IO, file_name)
!
      check_exist_surf_mesh = check_file_exist(file_name)
!
      return
      end function check_exist_surf_mesh
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_edge_mesh               &
     &                            (mesh_file, my_rank_IO)
!
      use delete_data_files
!
      integer(kind= kint), intent(in) :: my_rank_IO
      type(field_IO_params), intent(in) ::  mesh_file
!
      character(len=kchara) :: file_name
!
!
      call edge_mesh_file_name_by_param                                 &
     &   (mesh_file, my_rank_IO, file_name)
!
      check_exist_edge_mesh = check_file_exist(file_name)
!
      return
      end function check_exist_edge_mesh
!
!  ---------------------------------------------------------------------
!
      end module mesh_file_name_by_param
