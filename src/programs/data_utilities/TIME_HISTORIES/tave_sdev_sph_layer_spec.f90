!>@file   tave_sdev_sph_layer_spec.f90
!!        module tave_sdev_sph_layer_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine time_ave_sdev_sph_layer_spec                         &
!!     &         (fname_org, start_time, end_time)
!!        character(len = kchara), intent(in) :: fname_org
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!      subroutine read_time_ave_sph_layer_spec(fname_org, sph_lbl_IN,  &
!!     &          sph_IN, tave_spectr, trms_spectr, sdev_spectr)
!!        character(len = kchara), intent(in) :: fname_org
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(layer_spectr_ave_sigma_work), intent(inout) :: WK_tave
!!@endverbatim
!
      module tave_sdev_sph_layer_spec
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
      use t_buffer_4_gzip
      use t_tave_sph_layer_spectr
!
      implicit none
!
!
      integer(kind = kint), parameter, private :: id_read_rms = 45
!
      logical, parameter, private :: flag_current_format = .FALSE.
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine time_ave_sdev_sph_layer_spec                           &
     &         (fname_org, start_time, end_time)
!
      use set_parallel_file_name
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use count_monitor_time_series
      use gz_open_sph_layer_mntr_file
      use gz_layer_spectr_monitor_IO
      use sph_layer_monitor_snap_IO
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(sph_spectr_head_labels), save :: sph_lbl_IN1
      type(read_sph_spectr_data), save :: sph_IN1
      type(layer_spectr_ave_sigma_work), save :: WK_tave1
!
      real(kind = kreal) :: true_start, true_end
      integer(kind = kint) :: n_line, icou_skip
!
      character, pointer :: FPz_f1
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
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
!
      call sel_open_read_gz_stream_file(FPz_f1, id_read_rms,            &
     &                                    fname_org, flag_gzip1, zbuf1)
      call read_sph_layer_spectr_head(FPz_f1, id_read_rms, flag_gzip1,  &
     &    (.FALSE.), sph_lbl_IN1, sph_IN1, zbuf1)
!
      call alloc_sph_spectr_data(sph_IN1%ltr_sph, sph_IN1)
!
      n_line = (sph_IN1%ltr_sph+1) * sph_IN1%nri_sph
      call s_count_monitor_time_start                                   &
     &   (.TRUE., FPz_f1, id_read_rms, flag_gzip1, n_line,              &
     &    start_time, icou_skip, zbuf1)
      call alloc_tave_sph_data(sph_IN1, WK_tave1)
!
      call dealloc_sph_espec_name(sph_IN1)
      call sel_redwind_gz_stream_file(FPz_f1, id_read_rms, flag_gzip1)
      call read_sph_layer_spectr_head(FPz_f1, id_read_rms, flag_gzip1,  &
     &    (.FALSE.), sph_lbl_IN1, sph_IN1, zbuf1)
!
      call s_skip_monitor_time_series(.TRUE., FPz_f1, id_read_rms,      &
     &    flag_gzip1, n_line, icou_skip, zbuf1)
      call sph_layer_spectr_average                                     &
     &   (FPz_f1, id_read_rms, flag_gzip1, flag_current_format,         &
     &    start_time, end_time, true_start, true_end,                   &
     &    sph_IN1, WK_tave1, zbuf1)
!
!
      call dealloc_sph_espec_name(sph_IN1)
      call sel_redwind_gz_stream_file(FPz_f1, id_read_rms, flag_gzip1)
      call read_sph_layer_spectr_head(FPz_f1, id_read_rms, flag_gzip1,  &
     &    (.FALSE.), sph_lbl_IN1, sph_IN1, zbuf1)
!
      call s_skip_monitor_time_series(.TRUE., FPz_f1, id_read_rms,      &
     &    flag_gzip1, n_line, icou_skip, zbuf1)
      call sph_layer_spectr_std_deviation                               &
     &   (FPz_f1, id_read_rms, flag_gzip1, flag_current_format,         &
     &    start_time, end_time, sph_IN1, WK_tave1, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_read_rms, flag_gzip1, zbuf1)
!
!  Output average
      call write_sph_layer_spectr_snapshot(.FALSE., tave_file_name,     &
     &    comment_1, sph_IN1, WK_tave1%ave_spec_l(1,0,1))
!  Output RMS
      call write_sph_layer_spectr_snapshot(.FALSE., trms_file_name,     &
     &    comment_1, sph_IN1, WK_tave1%rms_spec_l(1,0,1))
!  Output Standard deviation
      call write_sph_layer_spectr_snapshot(.FALSE., sdev_file_name,     &
     &    comment_1, sph_IN1, WK_tave1%sigma_spec_l(1,0,1))
!
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, true_end
!      call check_time_ave_sph_layer_spec(sph_IN1, WK_tave1)
!
      call dealloc_tave_sph_data(WK_tave1)
      call dealloc_sph_espec_data(sph_IN1)
      call dealloc_sph_espec_name(sph_IN1)
!
      end subroutine time_ave_sdev_sph_layer_spec
!
!   --------------------------------------------------------------------
!
      subroutine read_time_ave_sph_layer_spec(fname_org, sph_lbl_IN,    &
     &                                        sph_IN, WK_tave)
!
      use set_parallel_file_name
      use sph_layer_monitor_snap_IO
!
      character(len = kchara), intent(in) :: fname_org
!
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(layer_spectr_ave_sigma_work), intent(inout) :: WK_tave
!
      character(len = kchara) :: tave_file_name, trms_file_name
      character(len = kchara) :: sdev_file_name
      character(len = kchara) :: directory, fname_no_dir, fname_tmp
      type(read_sph_spectr_data) :: sph_IN_t
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
      call read_alloc_sph_layer_spec_head(tave_file_name, sph_lbl_IN,   &
     &                                    sph_IN)
      call alloc_tave_sph_data(sph_IN, WK_tave)
!
      call read_sph_layer_spec_snapshot(tave_file_name, sph_lbl_IN,     &
     &    sph_IN_t, WK_tave%ave_spec_l(1,0,1))
      call dealloc_sph_espec_name(sph_IN_t)
      call read_sph_layer_spec_snapshot(trms_file_name, sph_lbl_IN,     &
     &    sph_IN_t, WK_tave%rms_spec_l(1,0,1))
      call dealloc_sph_espec_name(sph_IN_t)
      call read_sph_layer_spec_snapshot(sdev_file_name, sph_lbl_IN,     &
     &    sph_IN_t, WK_tave%sigma_pre_l(1,0,1))
      call dealloc_sph_espec_name(sph_IN_t)
!
      end subroutine read_time_ave_sph_layer_spec
!
!   --------------------------------------------------------------------
!
      end module tave_sdev_sph_layer_spec
