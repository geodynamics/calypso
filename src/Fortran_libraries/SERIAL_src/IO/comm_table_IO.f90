!>@file  comm_table_IO.f90
!!      module comm_table_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine read_comm_table(id_file, id_rank, comm_IO, ierr)
!!        integer (kind = kint), intent(in) :: id_file
!!        integer, intent(in) :: id_rank
!!        type(communication_table), intent(inout) :: comm_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine write_comm_table(id_file, id_rank, comm_IO)
!!        integer (kind = kint), intent(in) :: id_file
!!        integer, intent(in) :: id_rank
!!        type(communication_table), intent(in) :: comm_IO
!!
!!      subroutine read_calypso_comm_tbl                                &
!!     &         (id_file, id_rank, import_IO, export_IO, ierr)
!!        integer (kind = kint), intent(in) :: id_file
!!        integer, intent(in) :: id_rank
!!        type(communication_table), intent(inout) :: import_IO
!!        type(communication_table), intent(inout) :: export_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine write_calypso_comm_tbl                               &
!!     &         (id_file, id_rank, import_IO, export_IO)
!!        integer (kind = kint), intent(in) :: id_file
!!        integer, intent(in) :: id_rank
!!        type(communication_table), intent(in) :: import_IO, export_IO
!!@endverbatim
!
      module comm_table_IO
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
      subroutine read_comm_table(id_file, id_rank, comm_IO, ierr)
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
      call read_import_data(id_file, comm_IO, ierr)
      if(ierr .ne. 0) return
!
!      write(id_file,'(a)') '!'
!      write(id_file,'(a)') '! 2.2 element ID for export '
!      write(id_file,'(a)') '! '
!
      call read_export_data(id_file, comm_IO, ierr)
!
      end subroutine read_comm_table
!
!------------------------------------------------------------------
!
      subroutine write_comm_table(id_file, id_rank, comm_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO
!
      integer (kind = kint), intent(in) :: id_file
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
!
!
      write(id_file,'(a)', advance='NO') hd_fem_para()
      call write_domain_info(id_file, id_rank, comm_IO)
!
      write(id_file,'(a)', advance='NO') hd_ecomm_import()
      call write_import_data(id_file, comm_IO)
!
      write(id_file,'(a)', advance='NO') hd_ecomm_export()
      call write_export_data(id_file, comm_IO)
!
      end subroutine write_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_calypso_comm_tbl                                  &
     &         (id_file, id_rank, import_IO, export_IO, ierr)
!
      use m_fem_mesh_labels
      use domain_data_IO
!
      integer, intent(in) :: id_rank
      integer (kind = kint), intent(in) :: id_file
      type(communication_table), intent(inout) :: import_IO
      type(communication_table), intent(inout) :: export_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_domain_info(id_file, id_rank, import_IO, ierr)
      if(ierr .ne. 0) return
      call read_import_data(id_file, import_IO, ierr)
      if(ierr .ne. 0) return
!
      call read_domain_info(id_file, id_rank, export_IO, ierr)
      if(ierr .ne. 0) return
      call read_export_data(id_file, export_IO, ierr)
!
      end subroutine read_calypso_comm_tbl
!
!------------------------------------------------------------------
!
      subroutine write_calypso_comm_tbl                                 &
     &         (id_file, id_rank, import_IO, export_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO
!
      integer (kind = kint), intent(in) :: id_file
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: import_IO, export_IO
!
!
      write(id_file,'(a)', advance='NO') hd_fem_para()
      call write_domain_info(id_file, id_rank, import_IO)
      write(id_file,'(a)', advance='NO') hd_ecomm_import()
      call write_import_data(id_file, import_IO)
!
      write(id_file,'(a)', advance='NO') hd_fem_para()
      call write_domain_info(id_file, id_rank, export_IO)
      write(id_file,'(a)', advance='NO') hd_ecomm_export()
      call write_export_data(id_file, export_IO)
!
      end subroutine write_calypso_comm_tbl
!
!------------------------------------------------------------------
!
      end module comm_table_IO
