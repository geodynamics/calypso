!>@file  element_data_IO.f90
!!      module element_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine read_element_geometry(id_file, nod_IO, sfed_IO, iend)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine write_element_geometry(id_file, nod_IO, sfed_IO)
!!      subroutine write_element_geometry_sph(id_file, nod_IO, sfed_IO)
!!      subroutine write_element_geometry_cyl(id_file, nod_IO, sfed_IO)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!@endverbatim
!
      module element_data_IO
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
      use t_surf_edge_IO
      use m_fem_surface_labels
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_element_geometry(id_file, nod_IO, sfed_IO, iend)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      integer(kind = kint), intent(inout) :: iend
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.element information'
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.1 center of element (position) '
!      write(id_file,'(a)') '!'
!
      call read_number_of_node(id_file, nod_IO, iend)
      if(iend .ne. 0) return
      call read_geometry_info(id_file, nod_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.2 Volume of element '
!      write(id_file,'(a)') '!'
!
      call read_scalar_in_element(id_file, nod_IO, sfed_IO, iend)
!
      end subroutine read_element_geometry
!
!------------------------------------------------------------------
!
      subroutine write_element_geometry(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      write(id_file,'(a)', advance='NO') hd_ecomm_point()
      call write_geometry_info(id_file, nod_IO)
!
      write(id_file,'(a)', advance='NO') hd_ecomm_vol()
      call write_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      end subroutine write_element_geometry
!
!------------------------------------------------------------------
!
      subroutine write_element_geometry_sph(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.element information'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.1 center of element (r,theta,phi)'
      write(id_file,'(a)') '!'
!
      call write_geometry_info(id_file, nod_IO)
!
      write(id_file,'(a)', advance='NO') hd_ecomm_vol()
      call write_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      end subroutine write_element_geometry_sph
!
!------------------------------------------------------------------
!
      subroutine write_element_geometry_cyl(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.element information'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.1 center of element (r,theta,phi)'
      write(id_file,'(a)') '!'
!
      call write_geometry_info(id_file, nod_IO)
!
      write(id_file,'(a)', advance='NO') hd_ecomm_vol()
      call write_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      end subroutine write_element_geometry_cyl
!
!------------------------------------------------------------------
!
      end module element_data_IO
