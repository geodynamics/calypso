!>@file   gz_domain_data_IO.f90
!!@brief  module gz_domain_data_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Routine for doimain data IO using zlib
!!
!!@verbatim
!!      subroutine gz_read_domain_info(id_rank, comm_IO, zbuf, ierr)
!!      subroutine gz_read_import_data(comm_IO, zbuf)
!!      subroutine gz_read_export_data(comm_IO, zbuf)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(buffer_4_gzip) , intent(inout):: zbuf
!!
!!      subroutine gz_write_domain_info(id_rank, comm_IO, zbuf)
!!      subroutine gz_write_import_data(comm_IO, zbuf)
!!      subroutine gz_write_export_data(comm_IO, zbuf)
!!        type(communication_table), intent(in) :: comm_IO
!!        type(buffer_4_gzip) , intent(inout):: zbuf
!!@endverbatim
!
      module gz_domain_data_IO
!
      use m_precision
!
      use t_comm_table
      use t_buffer_4_gzip
!
      implicit none
!
      private :: write_send_recv_data_gz, read_send_recv_item_gz
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_read_domain_info(id_rank, comm_IO, zbuf, ierr)
!
      use m_error_IDs
      use gzip_file_access
      use gz_data_IO
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
!
      type(communication_table), intent(inout) :: comm_IO
      type(buffer_4_gzip) , intent(inout):: zbuf
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: irank_read
!
!
      call skip_gz_comment_int(irank_read, zbuf)
!
      ierr = 0
      if(irank_read .ne. id_rank) then
        ierr = ierr_mesh
        return
      end if
!
      call get_one_line_text_from_gz(zbuf)
      read(zbuf%fixbuf(1),*) comm_IO%num_neib
!
      call alloc_neighbouring_id(comm_IO)
!
      if (comm_IO%num_neib .gt. 0) then
        call read_gz_multi_int                                          &
     &     (comm_IO%num_neib, comm_IO%id_neib, zbuf)
      end if
!
      end subroutine gz_read_domain_info
!
!------------------------------------------------------------------
!
      subroutine gz_read_import_data(comm_IO, zbuf)
!
      use gz_data_IO
!
      type(communication_table), intent(inout) :: comm_IO
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call alloc_import_num(comm_IO)
!
      comm_IO%istack_import(0) = 0
      if (comm_IO%num_neib .gt. 0) then
        call read_gz_integer_stack(comm_IO%num_neib,                    &
     &      comm_IO%istack_import, comm_IO%ntot_import, zbuf)
!
        call alloc_import_item(comm_IO)
        call read_send_recv_item_gz                                     &
     &     (comm_IO%ntot_import, comm_IO%item_import, zbuf)
      else
        comm_IO%ntot_import = 0
        call alloc_import_item(comm_IO)
      end if
!
      end subroutine gz_read_import_data
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_export_data(comm_IO, zbuf)
!
      use gz_data_IO
!
      type(communication_table), intent(inout) :: comm_IO
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call alloc_export_num(comm_IO)
!
      comm_IO%istack_export(0) = 0
      if (comm_IO%num_neib .gt. 0) then
        call read_gz_integer_stack(comm_IO%num_neib,                    &
     &      comm_IO%istack_export, comm_IO%ntot_export, zbuf)
!
        call alloc_export_item(comm_IO)
        call read_send_recv_item_gz                                     &
     &     (comm_IO%ntot_export, comm_IO%item_export, zbuf)
      else
        comm_IO%ntot_export = 0
        call alloc_export_item(comm_IO)
      end if
!
      end subroutine gz_read_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_domain_info(id_rank, comm_IO, zbuf)
!
      use m_sph_modes_grid_labels
      use gzip_file_access
      use gz_data_IO
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      write(zbuf%fixbuf(1),'(i16,2a1)') id_rank, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(i16,2a1)')                                &
     &                          comm_IO%num_neib, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      if (comm_IO%num_neib .gt. 0) then
        call write_gz_multi_int_8i16                                    &
     &     (comm_IO%num_neib, comm_IO%id_neib, zbuf)
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end if
!
      end subroutine gz_write_domain_info
!
!------------------------------------------------------------------
!
      subroutine gz_write_import_data(comm_IO, zbuf)
!
      type(communication_table), intent(in) :: comm_IO
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call write_send_recv_data_gz                                      &
     &   (comm_IO%num_neib, comm_IO%ntot_import,                        &
     &    comm_IO%istack_import, comm_IO%item_import, zbuf)
!
      end subroutine gz_write_import_data
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_export_data(comm_IO, zbuf)
!
      type(communication_table), intent(in) :: comm_IO
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call write_send_recv_data_gz                                      &
     &   (comm_IO%num_neib, comm_IO%ntot_export,                        &
     &    comm_IO%istack_export, comm_IO%item_export, zbuf)
!
      end subroutine gz_write_export_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_send_recv_item_gz(ntot_sr, inod_sr, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      integer(kind = kint), intent(in) :: ntot_sr
      integer(kind = kint), intent(inout) :: inod_sr(ntot_sr)
      type(buffer_4_gzip) , intent(inout):: zbuf
!
      integer(kind = kint) :: i
!
      call skip_gz_comment_int(inod_sr(1), zbuf)
      do i = 2, ntot_sr
        call get_one_line_text_from_gz(zbuf)
        read(zbuf%fixbuf(1),*) inod_sr(i)
      end do
!
      end subroutine read_send_recv_item_gz
!
! -----------------------------------------------------------------------
!
      subroutine write_send_recv_data_gz                                &
     &         (num_sr, ntot_sr, istack_sr, inod_sr, zbuf)
!
      use gzip_file_access
      use gz_data_IO
!
      integer(kind = kint), intent(in) :: num_sr, ntot_sr
      integer(kind = kint), intent(in) :: istack_sr(0:num_sr)
      integer(kind = kint), intent(in) :: inod_sr(ntot_sr)
      type(buffer_4_gzip) , intent(inout):: zbuf
!
      integer(kind = kint) :: i
!
      if (num_sr .gt. 0) then
        call write_gz_multi_int_8i16(num_sr, istack_sr(1), zbuf)
        do i = 1, ntot_sr
          write(zbuf%fixbuf(1),'(i16,2a1)')                             &
     &                              inod_sr(i), char(10), char(0)
          call gz_write_textbuf_no_lf(zbuf)
        end do
      else
        write(zbuf%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end if
!
      end subroutine write_send_recv_data_gz
!
! -----------------------------------------------------------------------
!
      end module gz_domain_data_IO
