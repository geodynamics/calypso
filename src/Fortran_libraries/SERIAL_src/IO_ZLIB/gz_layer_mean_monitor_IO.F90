!>@file   gz_layer_mean_monitor_IO.F90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!> @brief  Layerd mean square data output
!!
!!@verbatim
!!      subroutine swap_layer_mean_to_IO(nri_sph, ntot_comp,            &
!!     &                                 rms_sph, spectr_IO)
!!        integer(kind = kint), intent(in) :: nri_sph, ntot_comp
!!        real(kind = kreal), intent(in) :: rms_sph(nri_sph, ntot_comp)
!!        real(kind = kreal), intent(inout)                             &
!!     &                     :: spectr_IO(ntot_comp,nri_sph)
!!
!!      subroutine sel_gz_read_layer_mean_mtr                           &
!!     &         (FPz_f, id_stream, flag_gzip, nri_sph, ntot_comp,      &
!!     &          i_step, time, kr_sph, r_sph, spectr_IO, zbuf, ierr)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: nri_sph, ntot_comp
!!        integer(kind = kint), intent(inout) :: i_step
!!        real(kind = kreal), intent(inout) :: time
!!        integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
!!        real(kind = kreal), intent(inout) :: r_sph(nri_sph)
!!        real(kind = kreal), intent(inout)                             &
!!     &                     :: spectr_IO(ntot_comp,nri_sph)
!!        integer(kind = kint), intent(inout) :: ierr
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine sel_gz_write_layer_mean_mtr                          &
!!     &         (flag_gzip, id_file, i_step, time,                     &
!!     &          nri_sph, kr_sph, r_sph, ntot_comp, spectr_IO, zbuf)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: i_step
!!        integer(kind = kint), intent(in) :: nri_sph
!!        integer(kind = kint), intent(in) :: kr_sph(nri_sph)
!!        real(kind = kreal), intent(in) :: time
!!        real(kind = kreal), intent(in) :: r_sph(nri_sph)
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        real(kind = kreal), intent(in) :: spectr_IO(ntot_comp,nri_sph)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
      module gz_layer_mean_monitor_IO
!
      use m_precision
      use m_constants
      use t_buffer_4_gzip
!
      implicit none
!
#ifdef ZLIB_IO
      private :: gz_write_layer_mean_monitor
#endif
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine swap_layer_mean_to_IO(nri_sph, ntot_comp,              &
     &                                 rms_sph, spectr_IO)
!
      integer(kind = kint), intent(in) :: nri_sph, ntot_comp
      real(kind = kreal), intent(in) :: rms_sph(nri_sph, ntot_comp)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ntot_comp,nri_sph)
!
      integer(kind = kint) :: kr
!
!
!$omp parallel do private(kr)
      do kr = 1, nri_sph
        spectr_IO(1:ntot_comp,kr) = rms_sph(kr,1:ntot_comp)
      end do
!$omp end parallel do
!
      end subroutine swap_layer_mean_to_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_gz_read_layer_mean_mtr                             &
     &         (FPz_f, id_stream, flag_gzip, nri_sph, ntot_comp,        &
     &          i_step, time, kr_sph, r_sph, spectr_IO, zbuf, ierr)
!
      use select_gz_stream_file_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: nri_sph, ntot_comp
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) :: time
      integer(kind = kint), intent(inout) :: kr_sph(nri_sph)
      real(kind = kreal), intent(inout) :: r_sph(nri_sph)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_IO(ntot_comp,nri_sph)
      integer(kind = kint), intent(inout) :: ierr
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: kr
!
!
      ierr = 1
      do kr = 1, nri_sph
        call sel_read_line_gz_stream(FPz_f, id_stream, flag_gzip, zbuf)
        if(zbuf%len_used .lt. 0) return
!
        read(zbuf%fixbuf(1),*,err=99) i_step, time,                     &
     &                 kr_sph(kr), r_sph(kr), spectr_IO(1:ntot_comp,kr)
      end do
      ierr = 0
      return
!
   99 continue
      return
!
      end subroutine sel_gz_read_layer_mean_mtr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine sel_gz_write_layer_mean_mtr                            &
     &         (flag_gzip, id_file, i_step, time,                       &
     &          nri_sph, kr_sph, r_sph, ntot_comp, spectr_IO, zbuf)
!
      use sph_monitor_data_text
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(in) :: nri_sph
      integer(kind = kint), intent(in) :: kr_sph(nri_sph)
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in) :: r_sph(nri_sph)
      integer(kind = kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: spectr_IO(ntot_comp,nri_sph)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: k
!
!
#ifdef ZLIB_IO
      if(flag_gzip) then
        call gz_write_layer_mean_monitor(id_file, i_step, time,         &
     &      nri_sph, kr_sph, r_sph, ntot_comp, spectr_IO, zbuf)
        return
      end if
#endif
!
      do k = 1, nri_sph
        write(id_file) layer_pwr_data_text(i_step, time,                &
     &                                     kr_sph(k), r_sph(k),         &
     &                                     ntot_comp, spectr_IO(1,k))
      end do
!
      end subroutine sel_gz_write_layer_mean_mtr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
#ifdef ZLIB_IO
      subroutine gz_write_layer_mean_monitor(id_file, i_step, time,     &
     &          nri_sph, kr_sph, r_sph, ntot_comp, spectr_IO, zbuf)
!
      use sph_monitor_data_text
      use gzip_defleate
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: nri_sph
      integer(kind = kint), intent(in) :: kr_sph(nri_sph)
      real(kind = kreal), intent(in) :: r_sph(nri_sph)
      integer(kind = kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: spectr_IO(ntot_comp,nri_sph)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: k, line_len
!
!
      line_len = len(layer_pwr_data_text(i_step, time,                  &
     &                                   kr_sph(1), r_sph(1),           &
     &                                   ntot_comp, spectr_IO(1,1)))
      zbuf%ilen_gz = int(dble(nri_sph*line_len)*1.01 + 24,              &
     &                   KIND(zbuf%ilen_gz))
      call alloc_zip_buffer(zbuf)
      zbuf%ilen_gzipped = 0
      if(nri_sph .eq. 1) then
        call gzip_defleat_char_once(line_len,                           &
     &      layer_pwr_data_text(i_step, time, kr_sph(1), r_sph(1),      &
     &                               ntot_comp, spectr_IO(1,1)),        &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
      else
        call gzip_defleat_char_begin(line_len,                          &
     &      layer_pwr_data_text(i_step, time, kr_sph(1), r_sph(1),      &
     &                               ntot_comp, spectr_IO(1,1)),        &
     &      int(zbuf%ilen_gz), zbuf, zbuf%gzip_buf(1))
        do k = 2, nri_sph-1
          call gzip_defleat_char_cont(line_len,                         &
     &        layer_pwr_data_text(i_step, time, kr_sph(k), r_sph(k),    &
     &                             ntot_comp, spectr_IO(1,k)), zbuf)
        end do
        k = nri_sph
        call gzip_defleat_char_last(line_len,                           &
     &      layer_pwr_data_text(i_step, time, kr_sph(k), r_sph(k),      &
     &                          ntot_comp, spectr_IO(1,k)), zbuf)
      end if
!
      write(id_file) zbuf%gzip_buf(1:zbuf%ilen_gzipped)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_layer_mean_monitor
#endif
!
! -----------------------------------------------------------------------
!
      end module gz_layer_mean_monitor_IO
