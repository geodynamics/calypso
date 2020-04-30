!>@file  gz_field_data_IO.f90
!!       module gz_field_data_IO
!!
!!@author H. Matsui
!!@date   Programmed on Oct., 2007
!!
!>@brief zlib and calypso_zlib_io_c are required
!!
!!@verbatim
!!      subroutine write_gz_step_data                                   &
!!     &         (id_rank, i_time_step_IO, time_IO, delta_t_IO, zbuf)
!!      subroutine read_gz_step_data                                    &
!!     &         (id_rank, i_time_step_IO, time_IO, delta_t_IO,         &
!!     &          zbuf, ierr_IO)
!!
!!      subroutine write_gz_field_data(nnod64, num_field, ntot_comp,    &
!!     &          ncomp_field, field_name, field_data, zbuf)
!!      subroutine read_gz_field_data(nnod64, num_field, ntot_comp,     &
!!     &          ncomp_field, field_name, field_data, zbuf)
!!@endverbatim
!
      module gz_field_data_IO
!
      use m_precision
      use t_buffer_4_gzip
!
      implicit none
!
      private :: write_gz_field_vect, read_gz_field_vect
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_gz_step_data                                     &
     &         (id_rank, i_time_step_IO, time_IO, delta_t_IO, zbuf)
!
      use gzip_file_access
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: i_time_step_IO
      real(kind = kreal), intent(in) :: time_IO, delta_t_IO
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      write(zbuf%fixbuf(1),'(a,2a1)'   ) '!  domain ID',                &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(i16,2a1)') id_rank, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)'   ) '!  time step number',         &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(i16,2a1)' ) i_time_step_IO,                &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(a,2a1)'   )   '!  time, Delta t',          &
     &                                   char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(1p2E25.15e3,2a1)') time_IO, delta_t_IO,    &
     &                                         char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      end subroutine write_gz_step_data
!
!-------------------------------------------------------------------
!
      subroutine read_gz_step_data                                      &
     &         (id_rank, i_time_step_IO, time_IO, delta_t_IO,           &
     &          zbuf, ierr_IO)
!
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
!
      integer(kind = kint), intent(inout) :: i_time_step_IO, ierr_IO
      real(kind = kreal), intent(inout) :: time_IO, delta_t_IO
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len = kchara) :: tmpchara
      integer(kind = kint) :: irank_read
!
!
      ierr_IO = 0
      call skip_gz_comment_int(irank_read, zbuf)
      if(int(irank_read) .ne. id_rank) then
        ierr_IO = 1
        write(*,*) 'Domain ID is different between process and data'
      end if
!
      call skip_gz_comment_int(i_time_step_IO, zbuf)
      call skip_gz_comment_chara(tmpchara, zbuf)
      read(tmpchara,*,err=99, end=99) time_IO, delta_t_IO
!
      go to 10
  99    write(*,*) 'no delta t data... continue'
        delta_t_IO = 0.0d0
  10  continue
!
      end subroutine read_gz_step_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_gz_field_data(nnod64, num_field, ntot_comp,      &
     &          ncomp_field, field_name, field_data, zbuf)
!
      use gzip_file_access
!
      integer(kind=kint_gl), intent(in)  :: nnod64
      integer(kind=kint), intent(in)  :: num_field, ntot_comp
      integer(kind=kint), intent(in)  :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: field_data(nnod64,ntot_comp)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind=kint)  :: i_fld, icou
      character(len=kchara) :: fmt_txt
!
!
      write(zbuf%fixbuf(1),'(a,2a1)'   )                                &
     &          '! number of field and component', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
      write(zbuf%fixbuf(1),'(2i16,2a1)')                                &
     &           nnod64, num_field, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      write(fmt_txt,'(a1,i3,a7)') '(', num_field, 'i4,2a1)'
      write(zbuf%fixbuf(1),fmt_txt) ncomp_field(1:num_field),           &
     &                      char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      icou = 0
      do i_fld = 1, num_field
!
        call write_gz_field_vect(field_name(i_fld),                     &
     &      nnod64, ncomp_field(i_fld), field_data(1,icou+1), zbuf)
        icou = icou + ncomp_field(i_fld)
      end do
!
      end subroutine write_gz_field_data
!
!------------------------------------------------------------------
!
      subroutine read_gz_field_data(nnod64, num_field, ntot_comp,       &
     &          ncomp_field, field_name, field_data, zbuf)
!
      use skip_gz_comment
!
      integer(kind=kint_gl), intent(in)  :: nnod64
      integer(kind=kint), intent(in)  :: num_field, ntot_comp
      integer(kind=kint), intent(in)  :: ncomp_field(num_field)
!
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: field_data(nnod64,ntot_comp)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind=kint)  :: i_fld, icou
!
!
      icou = 1
      do i_fld = 1, num_field
!
        call skip_gz_comment_chara(field_name(i_fld), zbuf)
        call read_gz_field_vect                                         &
     &     (nnod64, ncomp_field(i_fld), field_data(1,icou), zbuf)
        icou = icou + ncomp_field(i_fld)
      end do
!
      end subroutine read_gz_field_data
!
!------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_gz_field_vect                                    &
     &         (d_name, nnod64, ndir, vector, zbuf)
!
      use gzip_file_access
!
      integer(kind=kint_gl), intent(in) :: nnod64
      integer(kind=kint), intent(in) :: ndir
      real(kind = kreal), intent(in) :: vector(nnod64,ndir)
      character(len=kchara), intent(in) :: d_name
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl)  :: i
      character(len=kchara) :: fmt_txt
!
!
      write(zbuf%fixbuf(1),'(a,2a1)') trim(d_name), char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf)
!
      write(fmt_txt,'(a1,i3,a17)') '(', ndir, '(1pE25.15e3),2a1)'
      do i = 1, nnod64
        write(zbuf%fixbuf(1),fmt_txt) vector(i,1:ndir),                 &
     &                               char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf)
      end do
!
      end subroutine write_gz_field_vect
!
!------------------------------------------------------------------
!
      subroutine read_gz_field_vect(nnod64, ndir, vector, zbuf)
!
      use gzip_file_access
!
      integer(kind=kint_gl), intent(in) :: nnod64
      integer(kind=kint), intent(in) :: ndir
      real(kind = kreal), intent(inout) :: vector(nnod64,ndir)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind=kint_gl)  :: i
!
!
      do i = 1, nnod64
        call get_one_line_text_from_gz(zbuf)
        read(zbuf%fixbuf(1),*)  vector(i,1:ndir)
      end do
!
      end subroutine read_gz_field_vect
!
!------------------------------------------------------------------
!
      end module gz_field_data_IO
