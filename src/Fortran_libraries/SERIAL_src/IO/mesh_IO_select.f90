!>@file   mesh_IO_select.f90
!!@brief  module mesh_IO_select
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!>@brief  Choose mesh file to read
!!
!!@verbatim
!!      subroutine sel_read_mesh(my_rank)
!!      subroutine sel_read_mesh_geometry(my_rank)
!!
!!      subroutine sel_read_node_size(my_rank)
!!      subroutine sel_read_geometry_size(my_rank)
!!
!!      subroutine sel_write_mesh_file(my_rank)
!!      integer(kind = kint) function check_exist_mesh(my_rank)
!!
!!@endverbatim
!
      module mesh_IO_select
!
      use m_precision
!
      use m_read_mesh_data
      use m_file_format_switch
      use mesh_file_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_mesh(my_rank)
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      call read_mesh_file(my_rank)
!
      end subroutine sel_read_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_mesh_geometry(my_rank)
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      call read_mesh_geometry(my_rank)
!
      end subroutine sel_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
       subroutine sel_read_node_size(my_rank)
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      call read_node_size(my_rank)
!
      end subroutine sel_read_node_size
!
!------------------------------------------------------------------
!
       subroutine sel_read_geometry_size(my_rank)
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      call read_geometry_size(my_rank)
!
      end subroutine sel_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_write_mesh_file(my_rank)
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
!
      call write_mesh_file(my_rank)
!
      end subroutine sel_write_mesh_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_exist_mesh(my_rank)
!
      use delete_data_files
!
      integer(kind= kint), intent(in) :: my_rank
!
!
      call set_mesh_fname(my_rank)
      check_exist_mesh = check_file_exist(mesh_file_name)
!
      return
      end function check_exist_mesh
!
!  ---------------------------------------------------------------------
!
      end module mesh_IO_select
