!>@file  element_data_IO.f90
!!      module element_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine read_element_comm_table                              &
!!     &         (id_file, id_rank, comm_IO, ierr)
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine write_element_comm_table                             &
!!     &         (id_file, id_rank, comm_IO)
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine read_element_geometry(id_file, nod_IO, sfed_IO)
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
      subroutine read_element_comm_table                                &
     &         (id_file, id_rank, comm_IO, ierr)
!
      use m_fem_mesh_labels
      use domain_data_IO
!
      integer (kind = kint), intent(in) :: id_file
      integer, intent(in) :: id_rank
      type(communication_table), intent(inout) :: comm_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
!      write(id_file,'(a)') '!' 
!      write(id_file,'(a)') '!  element position '
!      write(id_file,'(a)') '!  and communication table '
!      write(id_file,'(a)') '!' 
!      write(id_file,'(a)', advance='NO') hd_fem_para()
!
      call read_domain_info(id_file, id_rank, comm_IO, ierr)
      if(ierr .ne. 0) return
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 2.import / export information '
!      write(id_file,'(a)') '! 2.1 element ID for import '
!      write(id_file,'(a)') '!'
!
      call read_import_data(id_file, comm_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 2.2 element ID for export '
!      write(id_file,'(a)') '! '
!
      call read_export_data(id_file, comm_IO)
!
      end subroutine read_element_comm_table
!
!------------------------------------------------------------------
!
      subroutine write_element_comm_table                               &
     &         (id_file, id_rank, comm_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO
!
      integer (kind = kint), intent(in) :: id_file
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
!
!
      write(id_file,'(a)', advance='NO') hd_ecomm_para()
      write(id_file,'(a)', advance='NO') hd_fem_para()
      call write_domain_info(id_file, id_rank, comm_IO)
!
      write(id_file,'(a)', advance='NO') hd_ecomm_import()
      call write_import_data(id_file, comm_IO)
!
      write(id_file,'(a)', advance='NO') hd_ecomm_export()
      call write_export_data(id_file, comm_IO)
!
      end subroutine write_element_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_element_geometry(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.element information'
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.1 center of element (position) '
!      write(id_file,'(a)') '!'
!
      call read_number_of_node(id_file, nod_IO)
      call read_geometry_info(id_file, nod_IO)
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 3.2 Volume of element '
!      write(id_file,'(a)') '!'
!
      call read_scalar_in_element(id_file, nod_IO, sfed_IO)
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
