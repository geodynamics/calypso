!>@file   tave_sdev_sph_volume_spec.f90
!!        module tave_sdev_sph_volume_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum
!!         memory saveing version
!!
!!@verbatim
!!      subroutine time_ave_sdev_sph_vol_spec(fname_org,                &
!!     &                                      start_time, end_time)
!!        character(len = kchara), intent(in) :: fname_org
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!
!!      subroutine read_time_ave_sph_vol_spec(fname_org, sph_lbl_IN,    &
!!     &                                      sph_IN, WK_tave)
!!        character(len = kchara), intent(in) :: fname_org
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(vol_spectr_ave_sigma_work), intent(inout) :: WK_tave
!!@endverbatim
!
      module tave_sdev_sph_volume_spec
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
      use t_tave_sph_volume_spectr
      use t_buffer_4_gzip
!
      implicit none
!
!
      integer(kind = kint), parameter, private :: id_read_rms = 45
!
      private :: output_sph_vol_spectr_ave_sdev
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine time_ave_sdev_sph_vol_spec(fname_org,                  &
     &                                      start_time, end_time)
!
      use set_parallel_file_name
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use count_monitor_time_series
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in) :: start_time, end_time
!
      real(kind = kreal) :: true_start, true_end
      integer(kind = kint) :: n_line, icou_skip
!
      type(sph_spectr_head_labels), save :: sph_lbl_IN1
      type(read_sph_spectr_data), save :: sph_IN1
      type(vol_spectr_ave_sigma_work), save :: WK_tave1
      character, pointer :: FPz_f1
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
!
!
      call sel_open_read_gz_stream_file(FPz_f1, id_read_rms,            &
     &                                  fname_org, flag_gzip1, zbuf1)
      call read_sph_volume_spectr_head(FPz_f1, id_read_rms,             &
     &    flag_gzip1, sph_lbl_IN1, sph_IN1, zbuf1)
      call alloc_sph_spectr_data(sph_IN1%ltr_sph, sph_IN1)
!
      n_line = sph_IN1%ltr_sph + 1
      call s_count_monitor_time_start                                   &
     &   (.TRUE., FPz_f1, id_read_rms, flag_gzip1, n_line,              &
     &    start_time, icou_skip, zbuf1)
!
      call alloc_tave_sph_volume_spectr(sph_IN1, WK_tave1)
!
      call dealloc_sph_espec_name(sph_IN1)
      call sel_redwind_gz_stream_file(FPz_f1, id_read_rms, flag_gzip1)
      call read_sph_volume_spectr_head(FPz_f1, id_read_rms,             &
     &    flag_gzip1, sph_lbl_IN1, sph_IN1, zbuf1)
!
      call s_skip_monitor_time_series(.TRUE., FPz_f1, id_read_rms,      &
     &    flag_gzip1, n_line, icou_skip, zbuf1)
      call sph_vol_spectr_average(FPz_f1, id_read_rms, flag_gzip1,      &
     &    start_time, end_time, true_start, true_end,                   &
     &    sph_IN1, WK_tave1, zbuf1)
!
!
      call dealloc_sph_espec_name(sph_IN1)
      call sel_redwind_gz_stream_file(FPz_f1, id_read_rms, flag_gzip1)
      call read_sph_volume_spectr_head(FPz_f1, id_read_rms,             &
     &    flag_gzip1, sph_lbl_IN1, sph_IN1, zbuf1)
!
!
      call s_skip_monitor_time_series(.TRUE., FPz_f1, id_read_rms,      &
     &    flag_gzip1, n_line, icou_skip, zbuf1)
      call sph_vol_spectr_std_deviation                                 &
     &   (FPz_f1, id_read_rms, flag_gzip1, start_time, end_time,        &
     &    sph_IN1, WK_tave1, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_rms, flag_gzip1, zbuf1)
!
      call output_sph_vol_spectr_ave_sdev                               &
     &   (fname_org, true_start, true_end, sph_IN1, WK_tave1)
!
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, true_end
!      call check_time_ave_sph_vol_spectr(sph_IN1, WK_tave1)
!
      call dealloc_tave_sph_volume_spectr(WK_tave1)
      call dealloc_sph_espec_data(sph_IN1)
      call dealloc_sph_espec_name(sph_IN1)
!
      end subroutine time_ave_sdev_sph_vol_spec
!
!   --------------------------------------------------------------------
!
      subroutine read_time_ave_sph_vol_spec(fname_org, sph_lbl_IN,      &
     &                                      sph_IN, WK_tave)
!
      use set_parallel_file_name
      use sph_volume_monitor_snap_IO
!
      character(len = kchara), intent(in) :: fname_org
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(vol_spectr_ave_sigma_work), intent(inout) :: WK_tave
!
!
      character(len = kchara) :: tave_file_name, trms_file_name
      character(len = kchara) :: sdev_file_name
      character(len = kchara) :: directory, fname_no_dir, fname_tmp
!
!
      call split_directory(fname_org, directory, fname_no_dir)
!
      write(fname_tmp, '(a6,a)') 't_ave_', trim(fname_no_dir)
      tave_file_name = append_directory(directory, fname_tmp)
      write(fname_tmp, '(a6,a)') 't_rms_', trim(fname_no_dir)
      trms_file_name = append_directory(directory, fname_tmp)
      write(fname_tmp, '(a7,a)') 't_sdev_', trim(fname_no_dir)
      sdev_file_name = append_directory(directory, fname_tmp)
!
      call read_alloc_sph_vol_spec_head(tave_file_name,                 &
     &                                  sph_lbl_IN, sph_IN)
      call alloc_tave_sph_volume_spectr(sph_IN, WK_tave)
      call dealloc_sph_espec_name(sph_IN)
!
      call read_sph_vol_spec_snapshot(tave_file_name, sph_lbl_IN,       &
     &                                sph_IN, WK_tave%ave_spec_l)
      call dealloc_sph_espec_name(sph_IN)
!
      call read_sph_vol_spec_snapshot(trms_file_name, sph_lbl_IN,       &
     &                                sph_IN, WK_tave%rms_spec_l)
      call dealloc_sph_espec_name(sph_IN)
!
      call read_sph_vol_spec_snapshot(sdev_file_name, sph_lbl_IN,       &
     &                                sph_IN, WK_tave%sigma_pre_l)
!
      end subroutine read_time_ave_sph_vol_spec
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine output_sph_vol_spectr_ave_sdev                         &
     &         (fname_org, true_start, true_end, sph_IN, WK_tave)
!
      use set_parallel_file_name
      use sph_volume_monitor_snap_IO
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in) :: true_start, true_end
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(vol_spectr_ave_sigma_work), intent(in) :: WK_tave
!
      character(len = kchara) :: tave_file_name, trms_file_name
      character(len = kchara) :: sdev_file_name
      character(len = kchara) :: directory, fname_no_dir, fname_tmp
      character(len=2+23+25+25+1) :: comment_1
!
!
      write(comment_1,'(2a,a23,1p2E25.15e3,a1)') '#', char(10),         &
     &             '# Start and End time:  ', true_start, true_end,     &
     &             char(10)
!
      call split_directory(fname_org, directory, fname_no_dir)
!
      write(fname_tmp, '(a6,a)') 't_ave_', trim(fname_no_dir)
      tave_file_name = append_directory(directory, fname_tmp)
      write(fname_tmp, '(a6,a)') 't_rms_', trim(fname_no_dir)
      trms_file_name = append_directory(directory, fname_tmp)
      write(fname_tmp, '(a7,a)') 't_sdev_', trim(fname_no_dir)
      sdev_file_name = append_directory(directory, fname_tmp)
!
      call write_sph_vol_spectr_snapshot(.FALSE., tave_file_name,       &
     &    comment_1, sph_IN, WK_tave%ave_spec_l(1,0))
      call write_sph_vol_spectr_snapshot(.FALSE., trms_file_name,       &
     &    comment_1, sph_IN, WK_tave%rms_spec_l(1,0))
      call write_sph_vol_spectr_snapshot(.FALSE., sdev_file_name,       &
     &    comment_1, sph_IN, WK_tave%sigma_spec_l(1,0))
!
      end subroutine output_sph_vol_spectr_ave_sdev
!
!   --------------------------------------------------------------------
!
      end module tave_sdev_sph_volume_spec
