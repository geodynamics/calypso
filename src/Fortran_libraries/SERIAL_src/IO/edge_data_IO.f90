!>@file  edge_data_IO.f90
!!      module edge_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief data IO orutines for edge
!!
!!@verbatim
!!      subroutine read_edge_connection                                 &
!!     &         (id_file, id_rank, comm_IO, ele_IO, sfed_IO, ierr)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine write_edge_connection                                &
!!     &         (id_file, id_rank, comm_IO, ele_IO, sfed_IO)
!!        type(communication_table), intent(in) :: comm_IO
!!        type(element_data), intent(in) :: ele_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!
!!      subroutine read_edge_geometry(id_file, nod_IO, sfed_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine write_edge_geometry(id_file, nod_IO, sfed_IO)
!!      subroutine write_edge_geometry_sph(id_file, nod_IO, sfed_IO)
!!      subroutine write_edge_geometry_cyl(id_file, nod_IO, sfed_IO)
!!        type(communication_table), intent(in) :: comm_IO
!!        type(node_data), intent(in) :: nod_IO
!!        type(element_data), intent(in) :: ele_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!@endverbatim
!
!
      module edge_data_IO
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_read_mesh_data
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
      subroutine read_edge_connection                                   &
     &         (id_file, id_rank, comm_IO, ele_IO, sfed_IO, ierr)
!
      use domain_data_IO
      use element_connect_IO
!
      integer (kind = kint), intent(in) :: id_file
      integer, intent(in) :: id_rank
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!      write(id_file,'(a)') '!' 
!      write(id_file,'(a)') '!  edge connectivity '
!      write(id_file,'(a)') '!  and communication table '
!      write(id_file,'(a)') '!' 
!      write(id_file,'(a)', advance='NO') hd_fem_para()
!
      call read_domain_info(id_file, id_rank, comm_IO, ierr)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  2  edge connectivity'
!      write(id_file,'(a)') '!  2.1  edge connectivity '
!      write(id_file,'(a)') '!      (type and connection) '
!      write(id_file,'(a)') '!'
!
      call read_number_of_element(id_file, ele_IO)
      call read_element_info(id_file, ele_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  2.2  edge id for each surface'
!      write(id_file,'(a)') '!'
!
      call read_surface_4_element(id_file, sfed_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  2.3   edge id for each element'
!      write(id_file,'(a)') '!'
!
      call read_edge_4_element(id_file, sfed_IO)
!
!
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.import / export information '
!      write(id_file,'(a)') '! 3.1 edge ID for import '
!      write(id_file,'(a)') '!'
!
      call read_import_data(id_file, comm_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.2 edge ID for export '
!      write(id_file,'(a)') '!'
!
      call read_export_data(id_file, comm_IO)
!
      end subroutine read_edge_connection
!
!------------------------------------------------------------------
!
      subroutine write_edge_connection                                  &
     &         (id_file, id_rank, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO
      use element_connect_IO
!
      integer (kind = kint), intent(in) :: id_file
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(element_data), intent(in) :: ele_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      write(id_file,'(a)', advance='NO') hd_edge_para()
      write(id_file,'(a)', advance='NO') hd_fem_para()
      call write_domain_info(id_file, id_rank, comm_IO)
!
      write(id_file,'(a)', advance='NO') hd_edge_connect()
      call write_element_info(id_file, ele_IO)
!
      write(id_file,'(a)', advance='NO') hd_edge_on_surf()
      call write_surface_4_element(id_file, sfed_IO)
!
      write(id_file,'(a)', advance='NO') hd_edge_on_ele()
      call write_edge_4_element(id_file, sfed_IO)
!
!
      write(id_file,'(a)', advance='NO') hd_edge_import()
      call write_import_data(id_file, comm_IO)
!
      write(id_file,'(a)', advance='NO') hd_edge_export()
      call write_export_data(id_file, comm_IO)
!
      end subroutine write_edge_connection
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_edge_geometry(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 4.   geometry of edge'
!      write(id_file,'(a)') '!  4.1. center of edge'
!      write(id_file,'(a)') '!'
!
      call read_number_of_node(id_file, nod_IO)
      call read_geometry_info(id_file, nod_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  4.2  direction of edge'
!      write(id_file,'(a)') '!'
!
      call read_vector_in_element(id_file, nod_IO, sfed_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '!  4.3  length of edge'
!      write(id_file,'(a)') '!'
!
      call read_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      end subroutine read_edge_geometry
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_geometry(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      write(id_file,'(a)', advance='NO') hd_edge_point()
      call write_geometry_info(id_file, nod_IO)
!
      write(id_file,'(a)', advance='NO') hd_edge_dir()
      call write_vector_in_element(id_file, nod_IO, sfed_IO)
!
      write(id_file,'(a)', advance='NO') hd_edge_length()
      call write_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      end subroutine write_edge_geometry
!
!------------------------------------------------------------------
!
      subroutine write_edge_geometry_sph(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 4.   geometry of edge'
      write(id_file,'(a)') '!  4.1. center of edge'
      write(id_file,'(a)') '!  (spherical coordinate) '
      write(id_file,'(a)') '!'
!
      call write_geometry_info(id_file, nod_IO)
!
      write(id_file,'(a)', advance='NO') hd_edge_dir()
      call write_vector_in_element(id_file, nod_IO, sfed_IO)
!
      write(id_file,'(a)', advance='NO') hd_edge_length()
      call write_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      end subroutine write_edge_geometry_sph
!
!------------------------------------------------------------------
!
      subroutine write_edge_geometry_cyl(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 4.   geometry of edge'
      write(id_file,'(a)') '!  4.1. center of edge'
      write(id_file,'(a)') '!  (cylindrical coordinate) '
      write(id_file,'(a)') '!'
!
      call write_geometry_info(id_file, nod_IO)
!
      write(id_file,'(a)', advance='NO') hd_edge_dir()
      call write_vector_in_element(id_file, nod_IO, sfed_IO)
!
      write(id_file,'(a)', advance='NO') hd_edge_length()
      call write_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      end subroutine write_edge_geometry_cyl
!
!------------------------------------------------------------------
!
      end module edge_data_IO
