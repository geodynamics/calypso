!>@file   gz_volume_spectr_monitor_IO.F90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!> @brief  Layerd mean square data output
!!
!!@verbatim
!!      subroutine swap_volume_spectr_to_IO(ltr, ntot_comp,             &
!!     &                                    rms_sph, spectr_IO)
!!        integer(kind = kint), intent(in) :: ltr, ntot_comp
!!        real(kind = kreal), intent(in) :: rms_sph(0:ltr,ntot_comp)
!!        real(kind = kreal), intent(inout) :: spectr_IO(ntot_comp,0:ltr)
!!      subroutine sel_gz_read_volume_spectr_mtr                        &
!!     &         (FPz_f, id_stream, flag_gzip, ltr, ntot_comp,          &
!!     &          i_step, time, i_mode, spectr_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: ltr
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        integer(kind = kint), intent(inout) :: i_step
!!        real(kind = kreal), intent(inout) :: time
!!        integer(kind = kint), intent(inout) :: i_mode(0:ltr)
!!        real(kind = kreal), intent(in) :: spectr_IO(ntot_comp,0:ltr)
!!        integer(kind = kint), intent(inout) :: ierr
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine sel_gz_write_volume_spectr_mtr(flag_gzip, id_file,   &
!!     &          i_step, time, ltr, ntot_comp, spectr_IO, zbuf)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: i_step
!!        integer(kind = kint), intent(in) :: ltr
!!        real(kind = kreal), intent(in) :: time
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(in) :: spectr_IO(ntot_comp,0:ltr)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
      module gz_volume_spectr_monitor_IO
!
      use m_precision
      use m_constants
      use t_buffer_4_gzip
      use t_read_sph_spectra
!
      implicit none
!
      private :: gz_write_volume_spectr_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine swap_volume_spectr_to_IO(ltr, ntot_comp,               &
     &                                    rms_sph, spectr_IO)
!
      integer(kind = kint), intent(in) :: ltr, ntot_comp
      real(kind = kreal), intent(in) :: rms_sph(0:ltr,ntot_comp)
      real(kind = kreal), intent(inout) :: spectr_IO(ntot_comp,0:ltr)
!
      integer(kind = kint) :: l
!
!
!$omp parallel do private(l)
      do l = 0, ltr
        spectr_IO(1:ntot_comp,l) = rms_sph(l,1:ntot_comp)
      end do
!$omp end parallel do
!
      end subroutine swap_volume_spectr_to_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_gz_read_volume_spectr_mtr                          &
     &         (FPz_f, id_stream, flag_gzip, ltr, ntot_comp,            &
     &          i_step, time, i_mode, spectr_IO, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: ltr
      integer(kind = kint), intent(in) :: ntot_comp
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) :: time
      integer(kind = kint), intent(inout) :: i_mode(0:ltr)
      real(kind = kreal), intent(inout) :: spectr_IO(ntot_comp,0:ltr)
      integer(kind = kint), intent(inout) :: ierr
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: l
!
!
      ierr = 1
      do l = 0, ltr
        call sel_read_line_gz_stream(FPz_f, id_stream, flag_gzip, zbuf)
        if(zbuf%len_used .lt. 0) return
!
        read(zbuf%fixbuf(1),*,err=99) i_step, time, i_mode(l),          &
     &                              spectr_IO(1:ntot_comp,l)
      end do
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine sel_gz_read_volume_spectr_mtr
!
!   --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine sel_gz_write_volume_spectr_mtr(flag_gzip, id_file,     &
     &          i_step, time, ltr, ntot_comp, spectr_IO, zbuf)
!
      use sph_monitor_data_text
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(in) :: ltr
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: spectr_IO(ntot_comp,0:ltr)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: l
!
!
#ifdef ZLIB_IO
      if(flag_gzip) then
        call gz_write_volume_spectr_monitor                             &
     &     (id_file, i_step, time, ltr, ntot_comp, spectr_IO, zbuf)
        return
      end if
#endif
!
      do l = 0, ltr
        write(id_file) volume_spectr_data_text(i_step, time, l,         &
     &                                       ntot_comp, spectr_IO(1,l))
      end do
!
      end subroutine sel_gz_write_volume_spectr_mtr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
#ifdef ZLIB_IO
      subroutine gz_write_volume_spectr_monitor                         &
     &         (id_file, i_step, time, ltr, n_comp, spectr_IO, zbuf)
!
      use sph_monitor_data_text
      use gzip_defleate
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step, ltr
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: n_comp
      real(kind = kreal), intent(in) :: spectr_IO(n_comp,0:ltr)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: l, line_len
!
!
      line_len = len(volume_spectr_data_text(i_step, time, izero,       &
     &                                       n_comp, spectr_IO(1,0)))
      zbuf%ilen_gz = int(dble((ltr+1)*line_len)*1.01 + 24,              &
     &                   KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
      call gzip_defleat_char_begin(line_len,                            &
     &    volume_spectr_data_text(i_step, time, izero,                  &
     &                            n_comp, spectr_IO(1,0)),              &
     &    int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      do l = 1, ltr - 1
        call gzip_defleat_char_cont(line_len,                           &
     &      volume_spectr_data_text(i_step, time, l, n_comp,            &
     &                              spectr_IO(1,l)), zbuf)
      end do
      call gzip_defleat_char_last(line_len,                             &
     &    volume_spectr_data_text(i_step, time, ltr, n_comp,            &
     &                            spectr_IO(1,ltr)), zbuf)
!
      write(id_file) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_volume_spectr_monitor
#endif
!
! -----------------------------------------------------------------------
!
      end module gz_volume_spectr_monitor_IO
