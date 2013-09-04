!mesh_IO_select.F90
!      module mesh_IO_select
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine sel_read_mesh(my_rank)
!      subroutine sel_read_mesh_geometry(my_rank)
!
!      subroutine sel_read_node_size(my_rank)
!      subroutine sel_read_geometry_size(my_rank)
!
!      subroutine sel_write_mesh_file(my_rank)
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
!
      end module mesh_IO_select
