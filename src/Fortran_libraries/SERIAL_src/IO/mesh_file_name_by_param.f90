!>@file   mesh_file_name_by_param.f90
!!@brief  module mesh_file_name_by_param
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      integer(kind = kint) function check_exist_mesh                  &
!!     &                            (mesh_file, id_rank)
!!      integer(kind = kint) function check_exist_ele_mesh              &
!!     &                            (mesh_file, id_rank)
!!      integer(kind = kint) function check_exist_surf_mesh             &
!!     &                            (mesh_file, id_rank)
!!      integer(kind = kint) function check_exist_edge_mesh             &
!!     &                            (mesh_file, id_rank)
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
      integer(kind = kint) function check_exist_mesh                    &
     &                            (mesh_file, id_rank)
!
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      character(len=kchara) :: file_name
!
!
      file_name =  set_mesh_file_name                                   &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      check_exist_mesh = check_file_exist(file_name)
!
      return
      end function check_exist_mesh
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_ele_mesh                &
     &                            (mesh_file, id_rank)
!
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      character(len=kchara) :: file_name
!
!
      file_name = set_ele_comm_file_name                                &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      check_exist_ele_mesh = check_file_exist(file_name)
!
      return
      end function check_exist_ele_mesh
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_surf_mesh               &
     &                            (mesh_file, id_rank)
!
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      character(len=kchara) :: file_name
!
!
      file_name = set_surf_mesh_file_name                               &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      check_exist_surf_mesh = check_file_exist(file_name)
!
      return
      end function check_exist_surf_mesh
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_edge_mesh               &
     &                            (mesh_file, id_rank)
!
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  mesh_file
!
      character(len=kchara) :: file_name
!
!
      file_name = set_edge_mesh_file_name                               &
     &   (mesh_file%file_prefix, mesh_file%iflag_format, id_rank)
!
      check_exist_edge_mesh = check_file_exist(file_name)
!
      return
      end function check_exist_edge_mesh
!
!  ---------------------------------------------------------------------
!
      end module mesh_file_name_by_param
