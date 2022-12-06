!>@file   gz_open_sph_monitor_file.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!> @brief Time spectrum data output routines for utilities
!!
!!@verbatim
!!      subroutine check_sph_vol_monitor_file(base_name, monitor_labels,&
!!     &          sph_OUT, flag_gzip_lc, error)
!!      subroutine check_sph_vol_spectr_file(id_file, base_name,        &
!!     &          flag_spectr, sph_OUT, zbuf, flag_gzip_lc, error)
!!      subroutine sel_open_sph_vol_monitor_file(id_file, base_name,    &
!!     &          monitor_labels, sph_OUT, zbuf, flag_gzip_lc)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len = kchara), intent(in) :: fname
!!        type(sph_spectr_head_labels), intent(in) :: monitor_labels
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        logical, intent(inout) :: flag_gzip_lc
!!
!!      subroutine check_sph_layer_mean_file(id_file, base_name,        &
!!     &          flag_spectr, sph_OUT, zbuf, flag_gzip_lc, error)
!!      subroutine sel_open_sph_layer_mean_file                         &
!!     &         (id_file, fname, sph_OUT, zbuf, flag_gzip_lc)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len = kchara), intent(in) :: fname
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        logical, intent(inout) :: flag_gzip_lc
!!
!!      subroutine write_sph_pwr_vol_head(flag_gzip, id_file,           &
!!     &                                  sph_OUT, zbuf)
!!      subroutine write_sph_pwr_layer_head(flag_gzip, id_file,         &
!!     &                                    sph_OUT, zbuf)
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: id_file
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!!
      module gz_open_sph_monitor_file
!
      use m_precision
      use m_constants
      use t_buffer_4_gzip
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_vol_monitor_file(base_name, monitor_labels,  &
     &          sph_OUT, flag_gzip_lc, error)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use compare_sph_monitor_header
!
      character(len = kchara), intent(in) :: base_name
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      logical, intent(inout) :: flag_gzip_lc, error
!
      integer(kind = kint), parameter :: id_stream = 44
      character, pointer :: FPz_fp
      type(buffer_4_gzip) :: zbuf_m
      type(read_sph_spectr_data) :: sph_IN_m
      type(sph_spectr_head_labels) :: sph_lbl_IN_m
      character(len = kchara) :: file_name
      logical :: flag_miss
!
!
      error = .FALSE.
      call sel_open_check_gz_stream_file(FPz_fp, id_stream, base_name,  &
     &    flag_gzip_lc, flag_miss, zbuf_m)
      if(flag_miss) go to 99
!
      call s_select_input_sph_series_head                               &
     &   (FPz_fp, id_stream, flag_gzip_lc, flag_current_fmt,            &
     &    spectr_off, volume_on, sph_lbl_IN_m, sph_IN_m, zbuf_m)
      call sel_close_read_gz_stream_file(FPz_fp, id_stream,             &
     &                                   flag_gzip_lc, zbuf_m)
!
      error = .not. cmp_sph_volume_monitor_heads                        &
     &                (sph_lbl_IN_m, sph_IN_m, monitor_labels, sph_OUT)
      call dealloc_sph_espec_name(sph_IN_m)
      return
!
  99  continue
      write(*,*) 'No file ', trim(file_name), '. Make it.'
!
      open(id_stream, file=file_name,                                   &
     &     FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_vol_head(flag_gzip_lc, id_stream,              &
     &                            monitor_labels, sph_OUT, zbuf_m)
      close(id_stream)
      return
!
      end subroutine check_sph_vol_monitor_file
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_vol_spectr_file(id_stream, base_name,        &
     &          sph_OUT, FPz_f, zbuf, flag_gzip_lc, error)
!
      use t_read_sph_spectra
      use sph_power_spectr_data_text
      use sel_gz_input_sph_mtr_head
      use select_gz_stream_file_IO
      use compare_sph_monitor_header
!
      integer(kind = kint), intent(in) :: id_stream
      character(len = kchara), intent(in) :: base_name
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      character, pointer, intent(inout) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc, error
!
      type(read_sph_spectr_data) :: sph_IN_f
      type(sph_spectr_head_labels) :: sph_lbl_IN_f
      character(len = kchara) :: fname
      logical :: flag_miss
!
!
      error = .FALSE.
      call sel_open_check_gz_stream_file(FPz_f, id_stream, base_name,   &
     &                                   flag_gzip_lc, flag_miss, zbuf)
      if(flag_miss) go to 99
!
      call s_select_input_sph_series_head                               &
     &   (FPz_f, id_stream, flag_gzip_lc, flag_current_fmt,             &
     &    spectr_on, volume_on, sph_lbl_IN_f, sph_IN_f, zbuf)
      sph_IN_f%nri_dat = 1
!
      call sel_close_read_gz_stream_file(FPz_f, id_stream,              &
     &                                   flag_gzip_lc, zbuf)
      error = .not. cmp_sph_volume_monitor_heads                        &
     &            (sph_lbl_IN_f, sph_IN_f, sph_pwr_labels, sph_OUT)
      call dealloc_sph_espec_name(sph_IN_f)
      return
!
   99 continue
!
      open(id_stream, file=fname, FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_vol_head(flag_gzip_lc, id_stream,              &
     &                            sph_pwr_labels, sph_OUT, zbuf)
      close(id_stream)
!
      end subroutine check_sph_vol_spectr_file
!
!  --------------------------------------------------------------------
!
      subroutine sel_open_sph_vol_monitor_file(id_file, base_name,      &
     &          monitor_labels, sph_OUT, zbuf, flag_gzip_lc)
!
      use t_read_sph_spectra
      use select_gz_stream_file_IO
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: base_name
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc
!
      character(len = kchara) :: fname
      logical :: flag_miss
!
!
      call check_gzip_or_ascii_file(base_name, fname,                   &
     &                              flag_gzip_lc, flag_miss)
!
      if(flag_miss) then
        open(id_file, file=fname, status='replace',                     &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
        call write_sph_pwr_vol_head(flag_gzip_lc, id_file,              &
     &                              monitor_labels, sph_OUT, zbuf)
      else
        open(id_file, file=fname, status='old', position='append',      &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      end if
!
      end subroutine sel_open_sph_vol_monitor_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_sph_layer_mean_file(id_stream, base_name,        &
     &          flag_spectr, sph_OUT, FPz_f, zbuf, flag_gzip_lc, error)
!
      use t_read_sph_spectra
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use compare_sph_monitor_header
!
      integer(kind = kint), intent(in) :: id_stream
      character(len = kchara), intent(in) :: base_name
      logical, intent(in) :: flag_spectr
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
      character, pointer, intent(inout) :: FPz_f
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc, error
!
      type(read_sph_spectr_data) :: sph_IN_f
      type(sph_spectr_head_labels) :: sph_lbl_IN_f
      character(len = kchara) :: fname
      logical :: flag_miss
!
!
      error = .FALSE.
      call sel_open_check_gz_stream_file(FPz_f, id_stream, base_name,   &
     &                                   flag_gzip_lc, flag_miss, zbuf)
      if(flag_miss) go to 99
!
      call s_select_input_sph_series_head                               &
     &   (FPz_f, id_stream, flag_gzip_lc, flag_current_fmt,             &
     &    flag_spectr, volume_off, sph_lbl_IN_f, sph_IN_f, zbuf)
      sph_IN_f%nri_dat = sph_IN_f%nri_sph
!
      error = .not. cmp_sph_layer_monitor_heads(sph_lbl_IN_f, sph_IN_f, &
     &                                         sph_pwr_labels, sph_OUT)
      call dealloc_sph_espec_name(sph_IN_f)
      return
!
   99 continue
!
      open(id_stream, file=fname, FORM='UNFORMATTED', ACCESS='STREAM')
      call write_sph_pwr_layer_head(flag_gzip_lc, id_stream,            &
     &                              sph_OUT, zbuf)
      close(id_stream)
!
      end subroutine check_sph_layer_mean_file
!
!  --------------------------------------------------------------------
!
      subroutine sel_open_sph_layer_mean_file                           &
     &         (id_file, base_name, sph_OUT, zbuf, flag_gzip_lc)
!
      use t_read_sph_spectra
      use select_gz_stream_file_IO
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: base_name
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc
!
      character(len = kchara) :: fname
      logical :: flag_miss
!
!
      fname = base_name
      call check_gzip_or_ascii_file(base_name, fname,                   &
     &                              flag_gzip_lc, flag_miss)
!
      if(flag_miss) then
        open(id_file, file=fname, status='replace',                     &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
        call write_sph_pwr_layer_head(flag_gzip_lc, id_file,            &
     &                                sph_OUT, zbuf)
      else
        open(id_file, file=fname, status='old', position='append',      &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      end if
!
      end subroutine sel_open_sph_layer_mean_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_pwr_vol_head(flag_gzip, id_file,             &
     &                                  monitor_labels, sph_OUT, zbuf)
!
      use sph_power_spectr_data_text
      use select_gz_stream_file_IO
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      call len_sph_vol_spectr_header(monitor_labels, sph_OUT,           &
     &                               len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip, id_file,                 &
     &    sph_vol_spectr_header_text(len_tot, len_each,                 &
     &                               monitor_labels, sph_OUT), zbuf)
!
      end subroutine write_sph_pwr_vol_head
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_pwr_layer_head(flag_gzip, id_file,           &
     &                                    sph_OUT, zbuf)
!
      use sph_power_spectr_data_text
      use select_gz_stream_file_IO
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot
!
!
      call len_sph_layer_spectr_header(sph_pwr_labels, sph_OUT,         &
     &                                 len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip, id_file,                 &
     &    sph_layer_spectr_header_text(len_tot, len_each,               &
     &                                 sph_pwr_labels, sph_OUT), zbuf)
!
      end subroutine write_sph_pwr_layer_head
!
!   --------------------------------------------------------------------
!
      end module gz_open_sph_monitor_file
