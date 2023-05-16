!>@file  gz_comm_table_IO_b.f90
!!      module gz_comm_table_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine gz_read_comm_table_b(FPz_f, id_rank, zbuf, comm_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine gz_write_comm_table_b(FPz_f, id_rank, comm_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(communication_table), intent(in) :: comm_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gz_read_calypso_comm_tbl_b                          &
!!     &         (FPz_f, id_rank, zbuf, import_IO, export_IO)
!!        character, pointer, intent(in) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(communication_table), intent(inout) :: import_IO
!!        type(communication_table), intent(inout) :: export_IO
!!      subroutine gz_write_calypso_comm_tbl_b                          &
!!     &         (FPz_f, id_rank, import_IO, export_IO, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer, intent(in) :: id_rank
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(communication_table), intent(in) :: import_IO, export_IO
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gz_comm_table_IO_b
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
      use t_surf_edge_IO
      use binary_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_comm_table_b(FPz_f, id_rank, zbuf, comm_IO)
!
      use gz_domain_data_IO_b
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(communication_table), intent(inout) :: comm_IO
!
!
!      write(textbuf,'(a,a1)') '!' , char(0)
!      write(textbuf,'(a,a1)') '!  element position ', char(0)
!      write(textbuf,'(a,a1)') '!  and communication table ', char(0)
!      write(textbuf,'(a,a1)') '!' , char(0)
!      write(textbuf,'(a,a1)', advance='NO') hd_fem_para(), char(0)
!
      call gz_read_domain_info_b(FPz_f, id_rank, zbuf, comm_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 2.import / export information ',      &
!     &                       char(0)
!      write(textbuf,'(a,a1)') '! 2.1 element ID for import ', char(0)
!      write(textbuf,'(a,a1)') '!', char(0)
!
      call gz_read_import_data_b(FPz_f, zbuf, comm_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
!      write(textbuf,'(a,a1)') '!', char(0)
!      write(textbuf,'(a,a1)') '! 2.2 element ID for export ', char(0)
!      write(textbuf,'(a,a1)') '! ', char(0)
!
      call gz_read_export_data_b(FPz_f, zbuf, comm_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_comm_table_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_comm_table_b(FPz_f, id_rank, comm_IO, zbuf)
!
      use gz_domain_data_IO_b
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
!      textbuf = hd_fem_para() // char(0)
      call gz_write_domain_info_b(FPz_f, id_rank, comm_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
!      textbuf = hd_ecomm_import() // char(0)
      call gz_write_import_data_b(FPz_f, comm_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
!      textbuf = hd_ecomm_export() // char(0)
      call gz_write_export_data_b(FPz_f, comm_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_comm_table_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_read_calypso_comm_tbl_b                             &
     &         (FPz_f, id_rank, zbuf, import_IO, export_IO)
!
      use gz_domain_data_IO_b
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(communication_table), intent(inout) :: import_IO
      type(communication_table), intent(inout) :: export_IO
!
!
      call gz_read_domain_info_b(FPz_f, id_rank, zbuf, import_IO)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_read_import_data_b(FPz_f, zbuf, import_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
      call gz_read_domain_info_b(FPz_f, id_rank, zbuf, export_IO)
      if(zbuf%ierr_zlib .ne. 0) return
      call gz_read_export_data_b(FPz_f, zbuf, export_IO)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_read_calypso_comm_tbl_b
!
!------------------------------------------------------------------
!
      subroutine gz_write_calypso_comm_tbl_b                            &
     &         (FPz_f, id_rank, import_IO, export_IO, zbuf)
!
      use gz_domain_data_IO_b
!
      character, pointer, intent(in) :: FPz_f
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: import_IO, export_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
!      textbuf = hd_fem_para() // char(0)
      call gz_write_domain_info_b(FPz_f, id_rank, import_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!      textbuf = hd_ecomm_import() // char(0)
      call gz_write_import_data_b(FPz_f, import_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
!      textbuf = hd_fem_para() // char(0)
      call gz_write_domain_info_b(FPz_f, id_rank, export_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!      textbuf = hd_ecomm_export() // char(0)
      call gz_write_export_data_b(FPz_f, export_IO, zbuf)
      if(zbuf%ierr_zlib .ne. 0) return
!
      end subroutine gz_write_calypso_comm_tbl_b
!
!------------------------------------------------------------------
!
      end module gz_comm_table_IO_b
