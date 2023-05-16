!>@file  gz_comm_table_IO.f90
!!      module gz_comm_table_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine gz_read_comm_table                                   &
!!     &         (FPz_f, id_rank, comm_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(communication_table), intent(inout) :: comm_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine gz_write_comm_table(FPz_f, id_rank, comm_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_calypso_comm_tbl                            &
!!     &         (FPz_f, id_rank, zbuf, import_IO, export_IO, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(communication_table), intent(inout) :: import_IO
!!        type(communication_table), intent(inout) :: export_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!      subroutine gz_write_calypso_comm_tbl                            &
!!     &         (FPz_f, id_rank, import_IO, export_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(communication_table), intent(in) :: import_IO, export_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_comm_table_IO
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
      use t_surf_edge_IO
      use t_buffer_4_gzip
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
      subroutine gz_read_comm_table                                     &
     &         (FPz_f, id_rank, comm_IO, zbuf, ierr)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(communication_table), intent(inout) :: comm_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
!
!      write(zbuf%fixbuf(1),'(a,a1)', advance='NO')                     &
!     &                                   hd_fem_para(), char(0)
      call gz_read_domain_info(FPz_f, id_rank, comm_IO, zbuf, ierr)
      if(ierr .ne. 0) return
!
!      write(zbuf%fixbuf(1),'(a,a1)')                                   &
!     &                  '! 2.1 element ID for import ', char(0)
      call gz_read_import_data(FPz_f, comm_IO, zbuf)
!
!      write(zbuf%fixbuf(1),'(a,a1)')                                   &
!     &                  '! 2.2 element ID for export ', char(0)
      call gz_read_export_data(FPz_f, comm_IO, zbuf)
!
      end subroutine gz_read_comm_table
!
!------------------------------------------------------------------
!
      subroutine gz_write_comm_table(FPz_f, id_rank, comm_IO, zbuf)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_domain_info(FPz_f, id_rank, comm_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_ecomm_import() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_import_data(FPz_f, comm_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_ecomm_export() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      call gz_write_export_data(FPz_f, comm_IO, zbuf)
!
      end subroutine gz_write_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_calypso_comm_tbl                              &
     &         (FPz_f, id_rank, zbuf, import_IO, export_IO, ierr)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(communication_table), intent(inout) :: import_IO
      type(communication_table), intent(inout) :: export_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call gz_read_domain_info(FPz_f, id_rank, import_IO, zbuf, ierr)
      if(ierr .ne. 0) return
      call gz_read_import_data(FPz_f, import_IO, zbuf)
      if(ierr .ne. 0) return
!
      call gz_read_domain_info(FPz_f, id_rank, export_IO, zbuf, ierr)
      if(ierr .ne. 0) return
      call gz_read_export_data(FPz_f, export_IO, zbuf)
!
      end subroutine gz_read_calypso_comm_tbl
!
!------------------------------------------------------------------
!
      subroutine gz_write_calypso_comm_tbl                              &
     &         (FPz_f, id_rank, import_IO, export_IO, zbuf)
!
      use m_fem_mesh_labels
      use gz_domain_data_IO
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: import_IO, export_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      call gz_write_domain_info(FPz_f, id_rank, import_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_ecomm_import() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      call gz_write_import_data(FPz_f, import_IO, zbuf)
!
!
      zbuf%fixbuf(1) = hd_fem_para() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      call gz_write_domain_info(FPz_f, id_rank, export_IO, zbuf)
!
      zbuf%fixbuf(1) = hd_ecomm_export() // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
      call gz_write_export_data(FPz_f, export_IO, zbuf)
!
      end subroutine gz_write_calypso_comm_tbl
!
!------------------------------------------------------------------
!
      end module gz_comm_table_IO
