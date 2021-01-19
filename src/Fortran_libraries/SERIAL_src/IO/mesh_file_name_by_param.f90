!>@file   mesh_file_name_by_param.f90
!!@brief  module mesh_file_name_by_param
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      logical function check_exist_mesh(id_rank, mesh_file)
!!      logical function check_exist_ele_mesh(id_rank, mesh_file)
!!      logical function check_exist_surf_mesh(id_rank, mesh_file)
!!      logical function check_exist_edge_mesh(id_rank, mesh_file)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!
!!      logical function check_writable_mesh(id_rank, mesh_file)
!!      logical function check_writable_ele_mesh(id_rank, mesh_file)
!!      logical function check_writable_surf_mesh(id_rank, mesh_file)
!!      logical function check_writable_edge_mesh(id_rank, mesh_file)
!!        type(field_IO_params), intent(in) ::  mesh_file
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
      logical function check_exist_mesh(id_rank, mesh_file)
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
      logical function check_exist_ele_mesh(id_rank, mesh_file)
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
      logical function check_exist_surf_mesh(id_rank, mesh_file)
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
      logical function check_exist_edge_mesh(id_rank, mesh_file)
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
!  ---------------------------------------------------------------------
!
      logical function check_writable_mesh(id_rank, mesh_file)
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
      check_writable_mesh = check_file_writable(id_rank, file_name)
!
      return
      end function check_writable_mesh
!
!  ---------------------------------------------------------------------
!
      logical function check_writable_ele_mesh(id_rank, mesh_file)
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
      check_writable_ele_mesh                                           &
     &      = check_file_writable(id_rank, file_name)
!
      return
      end function check_writable_ele_mesh
!
!  ---------------------------------------------------------------------
!
      logical function check_writable_surf_mesh(id_rank, mesh_file)
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
      check_writable_surf_mesh                                          &
     &      = check_file_writable(id_rank, file_name)
!
      return
      end function check_writable_surf_mesh
!
!  ---------------------------------------------------------------------
!
      logical function check_writable_edge_mesh(id_rank, mesh_file)
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
      check_writable_edge_mesh                                          &
     &      = check_file_writable(id_rank, file_name)
!
      return
      end function check_writable_edge_mesh
!
!  ---------------------------------------------------------------------
!
      end module mesh_file_name_by_param
