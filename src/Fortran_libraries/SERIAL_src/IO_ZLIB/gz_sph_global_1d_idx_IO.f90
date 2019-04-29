!>@file  gz_sph_global_1d_idx_IO.f90
!!       module gz_sph_global_1d_idx_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine read_rtp_gl_1d_table_gz(sph_IO)
!!      subroutine read_rj_gl_1d_table_gz(sph_IO)
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine write_rtp_gl_1d_table_gz(sph_IO)
!!      subroutine write_rj_gl_1d_table_gz(sph_IO)
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module gz_sph_global_1d_idx_IO
!
      use m_precision
!
      use t_node_id_spherical_IO
      use skip_gz_comment
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rtp_gl_1d_table_gz(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
!
!
      sph_IO%numdir_sph = 3
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 1
      sph_IO%ncomp_table_1d(3) = 2
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      call skip_gz_comment_int( sph_IO%nidx_sph(1) )
      read(textbuf,*) sph_IO%nidx_sph(1),                               &
     &                sph_IO%ist_sph(1), sph_IO%ied_sph(1)
      call alloc_idx_sph_1d1_IO(sph_IO)
!
      do i = 1, sph_IO%nidx_sph(1)
        call get_one_line_from_gz_f
        read(textbuf,*) sph_IO%idx_gl_1(i), sph_IO%r_gl_1(i)
      end do
!
      call skip_gz_comment_int( sph_IO%nidx_sph(2) )
      read(textbuf,*) sph_IO%nidx_sph(2),                               &
     &                sph_IO%ist_sph(2), sph_IO%ied_sph(2)
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      do i = 1, sph_IO%nidx_sph(2)
        call get_one_line_from_gz_f
        read(textbuf,*) sph_IO%idx_gl_2(i,1:sph_IO%ncomp_table_1d(2))
      end do
!
      call skip_gz_comment_int( sph_IO%nidx_sph(3) )
      read(textbuf,*) sph_IO%nidx_sph(3),                               &
     &                sph_IO%ist_sph(3), sph_IO%ied_sph(3)
      call alloc_idx_sph_1d3_IO(sph_IO)
!
      do i = 1, sph_IO%nidx_sph(3)
        call get_one_line_from_gz_f
        read(textbuf,*) sph_IO%idx_gl_3(i,1:sph_IO%ncomp_table_1d(3))
      end do
!
      end subroutine read_rtp_gl_1d_table_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_rj_gl_1d_table_gz(sph_IO)
!
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: i
!
!
      sph_IO%numdir_sph = 2
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 3
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      call skip_gz_comment_int( sph_IO%nidx_sph(1) )
      read(textbuf,*) sph_IO%nidx_sph(1),                               &
     &                sph_IO%ist_sph(1), sph_IO%ied_sph(1)
      call alloc_idx_sph_1d1_IO(sph_IO)
!
      do i = 1, sph_IO%nidx_sph(1)
        call get_one_line_from_gz_f
        read(textbuf,*) sph_IO%idx_gl_1(i), sph_IO%r_gl_1(i)
      end do
!
      call skip_gz_comment_int( sph_IO%nidx_sph(2) )
      read(textbuf,*) sph_IO%nidx_sph(2),                               &
     &                sph_IO%ist_sph(2), sph_IO%ied_sph(2)
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      do i = 1, sph_IO%nidx_sph(2)
        call get_one_line_from_gz_f
        read(textbuf,*) sph_IO%idx_gl_2(i,1:sph_IO%ncomp_table_1d(2))
      end do
!
      end subroutine read_rj_gl_1d_table_gz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rtp_gl_1d_table_gz(sph_IO)
!
      use m_sph_modes_grid_labels
!
      type(sph_IO_data), intent(in) :: sph_IO
!
      character(len=kchara) :: fmt_txt
      integer(kind = kint) :: i
!
!
      textbuf = hd_rgrid() // char(0)
      call gz_write_textbuf_no_lf
!
      write(textbuf,'(3i16,a1)') sph_IO%nidx_sph(1),                    &
     &       sph_IO%ist_sph(1), sph_IO%ied_sph(1), char(0)
      call gz_write_textbuf_w_lf
!
      do i = 1, sph_IO%nidx_sph(1)
        write(textbuf,'(i16,1pE25.15e3,a1)')                            &
     &       sph_IO%idx_gl_1(i), sph_IO%r_gl_1(i), char(0)
        call gz_write_textbuf_w_lf
      end do
!
!
      textbuf = hd_tgrid() // char(0)
      call gz_write_textbuf_no_lf
!
      write(textbuf,'(3i16,a1)') sph_IO%nidx_sph(2),                    &
     &       sph_IO%ist_sph(2), sph_IO%ied_sph(2), char(0)
      call gz_write_textbuf_w_lf
!
      write(fmt_txt,'(a1,i3,a9)')                                       &
     &                '(', sph_IO%ncomp_table_1d(2), '(i16),a1)'
      do i = 1, sph_IO%nidx_sph(2)
        write(textbuf,fmt_txt)                                          &
     &        sph_IO%idx_gl_2(i,1:sph_IO%ncomp_table_1d(2)), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      textbuf = hd_pgrid() // char(0)
      call gz_write_textbuf_no_lf
!
      write(textbuf,'(3i16,a1)') sph_IO%nidx_sph(3),                    &
     &       sph_IO%ist_sph(3), sph_IO%ied_sph(3), char(0)
      call gz_write_textbuf_w_lf
!
      write(fmt_txt,'(a1,i3,a9)')                                       &
     &                '(', sph_IO%ncomp_table_1d(3), '(i16),a1)'
      do i = 1, sph_IO%nidx_sph(3)
        write(textbuf,fmt_txt)                                          &
     &       sph_IO%idx_gl_3(i,1:sph_IO%ncomp_table_1d(3)), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      end subroutine write_rtp_gl_1d_table_gz
!
! ----------------------------------------------------------------------
!
      subroutine write_rj_gl_1d_table_gz(sph_IO)
!
      use m_sph_modes_grid_labels
!
      type(sph_IO_data), intent(in) :: sph_IO
!
      character(len=kchara) :: fmt_txt
      integer(kind = kint) :: i
!
!
      textbuf = hd_rgrid() // char(0)
      call gz_write_textbuf_no_lf
!
      write(textbuf,'(3i16,a1)') sph_IO%nidx_sph(1),                    &
     &       sph_IO%ist_sph(1), sph_IO%ied_sph(1), char(0)
      call gz_write_textbuf_w_lf
!
      do i = 1, sph_IO%nidx_sph(1)
        write(textbuf,'(i16,1pE25.15e3,a1)')                            &
     &       sph_IO%idx_gl_1(i), sph_IO%r_gl_1(i), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      textbuf = hd_jmode() // char(0)
      call gz_write_textbuf_no_lf
!
      write(textbuf,'(3i16,a1)') sph_IO%nidx_sph(2),                    &
     &       sph_IO%ist_sph(2), sph_IO%ied_sph(2), char(0)
      call gz_write_textbuf_w_lf
!
      write(fmt_txt,'(a1,i3,a9)')                                       &
     &                '(', sph_IO%ncomp_table_1d(2), '(i16),a1)'
      do i = 1, sph_IO%nidx_sph(2)
        write(textbuf,fmt_txt)                                          &
     &          sph_IO%idx_gl_2(i,1:sph_IO%ncomp_table_1d(2)), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      end subroutine write_rj_gl_1d_table_gz
!
! ----------------------------------------------------------------------
!
      end module gz_sph_global_1d_idx_IO
