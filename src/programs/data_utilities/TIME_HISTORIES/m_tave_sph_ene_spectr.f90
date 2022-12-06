!>@file   m_tave_sph_ene_spectr.f90
!!        module m_tave_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine time_ave_sdev_sph_spectr                             &
!!     &         (fname_org, flag_spectr, flag_vol_ave,                 &
!!     &          start_time, end_time)
!!      subroutine time_ave_sdev_sph_old_spectr                         &
!!     &         (fname_org, flag_spectr, flag_vol_ave,                 &
!!     &          start_time, end_time)
!!        character(len = kchara), intent(in) :: fname_org
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!
!!      subroutine read_time_ave_sdev_sph_spectr                        &
!!     &         (tave_file_name, sdev_file_name,                       &
!!     &          flag_spectr, flag_vol_ave, tave_sph_IN, sdev_sph_IN)
!!        character(len = kchara), intent(in) :: tave_file_name
!!        character(len = kchara), intent(in) :: sdev_file_name
!!        logical, intent(in) :: flag_spectr, flag_vol_ave
!!        type(read_sph_spectr_params), intent(inout) :: tave_sph_IN
!!        type(read_sph_spectr_params), intent(inout) :: sdev_sph_IN
!!@endverbatim
!
      module m_tave_sph_ene_spectr
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_file_rms =      34
!
      type spectr_ave_sigma_work
        real(kind = kreal), allocatable :: ave_spec_l(:,:,:)
        real(kind = kreal), allocatable :: sigma_spec_l(:,:,:)
        real(kind = kreal), allocatable :: spectr_pre_l(:,:,:)
      end type spectr_ave_sigma_work
!
      logical, parameter, private :: flag_old_format =     .TRUE.
      logical, parameter, private :: flag_current_format = .FALSE.
!
      type(read_sph_spectr_params), save, private :: sph_IN1
      type(spectr_ave_sigma_work), save, private :: WK_tave1
!
      private :: id_file_rms
      private :: alloc_tave_sph_data, dealloc_tave_sph_data
      private :: sph_spectr_average, sph_spectr_std_deviation
!
      private :: read_sph_spectr_snapshot
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine time_ave_sdev_sph_spectr                               &
     &         (fname_org, flag_spectr, flag_vol_ave,                   &
     &          start_time, end_time)
!
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_spectr, flag_vol_ave
      real(kind = kreal), intent(in) :: start_time, end_time
!
      real(kind = kreal) :: true_start, true_end
      integer(kind = kint) :: i, num_tlabel, kr
!
      call sph_spectr_average                                           &
     &   (flag_current_format, fname_org, flag_spectr, flag_vol_ave,    &
     &    start_time, end_time, true_start, true_end,                   &
     &    sph_IN1, WK_tave1)
      call sph_spectr_std_deviation                                     &
     &   (flag_current_format, fname_org, flag_spectr, flag_vol_ave,    &
     &    start_time, end_time, sph_IN1, WK_tave1)
!
      write(*,'(a,1p2e25.15e3)') 'Start and end time:     ',            &
     &                          true_start, true_end
      if(flag_spectr .eqv. .FALSE.) then
        num_tlabel = sph_IN1%num_time_labels
        if(flag_vol_ave) then
          write(*,*) 'Time_average, standard_deviation, field_name'
          do i = 1, sph_IN1%ntot_sph_spec
            write(*,'(1p2e23.15e3,2a)') WK_tave1%ave_spec_l(i,0,1),     &
     &           WK_tave1%sigma_spec_l(i,0,1), '    ',                  &
     &           trim(sph_IN1%ene_sph_spec_name(i+num_tlabel))
          end do
        else
          do kr = 1, sph_IN1%nri_sph
            write(*,*) 'Radial level: ', sph_IN1%kr_sph(kr),            &
     &                                   sph_IN1%r_sph(kr)
            write(*,*) 'Time_average, standard_deviation, field_name'
            do i = 1, sph_IN1%ntot_sph_spec
              write(*,'(1p2e23.15e3,2a)') WK_tave1%ave_spec_l(i,0,kr),  &
     &           WK_tave1%sigma_spec_l(i,0,kr), '    ',                 &
     &           trim(sph_IN1%ene_sph_spec_name(i+num_tlabel))
            end do
          end do
        end if
      end if
!
      call dealloc_tave_sph_data(WK_tave1)
      call dealloc_sph_espec_data(sph_IN1)
      call dealloc_sph_espec_name(sph_IN1)
!
      end subroutine time_ave_sdev_sph_spectr
!
!   --------------------------------------------------------------------
!
      subroutine time_ave_sdev_sph_old_spectr                           &
     &         (fname_org, flag_spectr, flag_vol_ave,                   &
     &          start_time, end_time)
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_spectr, flag_vol_ave
      real(kind = kreal), intent(in) :: start_time, end_time
!
      real(kind = kreal) :: true_start, true_end
!
      call sph_spectr_average                                           &
     &   (flag_old_format, fname_org, flag_spectr, flag_vol_ave,        &
     &    start_time, end_time, true_start, true_end,                   &
     &    sph_IN1, WK_tave1)
      call sph_spectr_std_deviation                                     &
     &   (flag_old_format, fname_org, flag_spectr, flag_vol_ave,        &
     &    start_time, end_time, sph_IN1, WK_tave1)
!
      call dealloc_tave_sph_data(WK_tave1)
      call dealloc_sph_espec_data(sph_IN1)
      call dealloc_sph_espec_name(sph_IN1)
!
      end subroutine time_ave_sdev_sph_old_spectr
!
!   --------------------------------------------------------------------
!
      subroutine read_time_ave_sdev_sph_spectr                          &
     &         (tave_file_name, sdev_file_name,                         &
     &          flag_spectr, flag_vol_ave, tave_sph_IN, sdev_sph_IN)
!
      use t_read_sph_series
!
      character(len = kchara), intent(in) :: tave_file_name
      character(len = kchara), intent(in) :: sdev_file_name
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_params), intent(inout) :: tave_sph_IN
      type(read_sph_spectr_params), intent(inout) :: sdev_sph_IN
!
!
      call read_sph_spectr_snapshot                                     &
     &   (tave_file_name, flag_spectr, flag_vol_ave, tave_sph_IN)
      call read_sph_spectr_snapshot                                     &
     &   (sdev_file_name, flag_spectr, flag_vol_ave, sdev_sph_IN)
!
      end subroutine read_time_ave_sdev_sph_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine sph_spectr_average                                     &
     &         (flag_old_fmt, fname_org, flag_spectr, flag_vol_ave,     &
     &          start_time, end_time, true_start, true_end,             &
     &          sph_IN, WK_tave)
!
      use t_buffer_4_gzip
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use write_sph_monitor_data
      use cal_tave_sph_ene_spectr
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_spectr, flag_vol_ave, flag_old_fmt
      real(kind = kreal), intent(in) :: start_time, end_time
      real(kind = kreal), intent(inout) :: true_start, true_end
      type(read_sph_spectr_params), intent(inout) :: sph_IN
      type(spectr_ave_sigma_work), intent(inout) :: WK_tave
!
      character(len = kchara) :: file_name, extension
      character(len = kchara) :: directory, fname_no_dir, fname_tmp
      real(kind = kreal) :: prev_time
      integer(kind = kint) :: icou, ierr, ist_true, i
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1, zbuf_s
      character, pointer :: FPz_f1
      type(sph_spectr_head_labels) :: sph_lbl_IN_t
!
!
      write(*,*) 'Open file ', trim(fname_org)
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms,            &
     &                                    fname_org, flag_gzip1, zbuf1)
      call s_select_input_sph_series_head                               &
     &   (FPz_f1, id_file_rms, flag_gzip1,                              &
     &    flag_old_fmt, flag_spectr, flag_vol_ave,                      &
     &    sph_lbl_IN_t, sph_IN, zbuf1)
!
      sph_IN%nri_dat = sph_IN%nri_sph
      if(flag_vol_ave) sph_IN%nri_dat = 1
      if(flag_spectr .eqv. .FALSE.) then
        call alloc_sph_spectr_data(izero, sph_IN)
      else
        call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
      end if
!
      call alloc_tave_sph_data(sph_IN, WK_tave)
!
      icou = 0
      ist_true = -1
      prev_time = sph_IN%time
      write(*,'(a6,i12,a30,i12)',advance="NO")                         &
     &       'step= ', sph_IN%i_step,                                  &
     &       ' averaging finished. Count=  ', icou
      do
        call sel_gz_input_sph_series_data                               &
     &     (FPz_f1, id_file_rms, flag_gzip1,                            &
     &      flag_old_fmt, flag_spectr, flag_vol_ave, sph_IN, zbuf1,     &
     &      ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. start_time) then
          if (ist_true .eq. -1) then
            ist_true =   sph_IN%i_step
            true_start = sph_IN%time
!
            call copy_ene_spectr_2_pre                                  &
     &         (sph_IN%time, prev_time, sph_IN%nri_sph, sph_IN%ltr_sph, &
     &          sph_IN%ntot_sph_spec, sph_IN%spectr_IO,                 &
     &          WK_tave%ave_spec_l, WK_tave%spectr_pre_l)
          else
!
            call sum_average_ene_spectr                                 &
     &         (sph_IN%time, prev_time, sph_IN%nri_sph, sph_IN%ltr_sph, &
     &          sph_IN%ntot_sph_spec, sph_IN%spectr_IO,                 &
     &          WK_tave%ave_spec_l, WK_tave%spectr_pre_l)

            icou = icou + 1
          end if
        end if
!
        write(*,'(60a1,a6,i12,a30,i12)',advance="NO") (char(8),i=1,60),&
     &       'step= ', sph_IN%i_step,                                  &
     &       ' averaging finished. Count=   ', icou
        if (sph_IN%time .ge. end_time) then
          true_end = sph_IN%time
          exit
        end if
      end do
!
   99 continue
      write(*,*)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file_rms, flag_gzip1, zbuf1)
!
!
      call divide_average_ene_spectr(sph_IN%time, true_start,           &
     &    sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,         &
     &    WK_tave%ave_spec_l, sph_IN%spectr_IO)
!
!  Output average
      if(flag_gzip1) then
        call split_extrension(fname_org, fname_tmp, extension)
      else
        fname_tmp = fname_org
      end if
      call split_directory(fname_tmp, directory, fname_no_dir)
      write(fname_tmp, '(a6,a)') 't_ave_', trim(fname_no_dir)
      file_name = append_directory(directory, fname_tmp)
!
      write(*,*) 'average file_name: ', trim(file_name)
      open(id_file_rms, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (.FALSE., id_file_rms, flag_vol_ave, sph_IN, zbuf_s)
!
      call select_output_sph_series_data                                &
     &   (id_file_rms, flag_spectr, flag_vol_ave, sph_IN)
      close(id_file_rms)
!
      call dealloc_sph_espec_data(sph_IN)
      call dealloc_sph_espec_name(sph_IN)
!
      end subroutine sph_spectr_average
!
!   --------------------------------------------------------------------
!
      subroutine sph_spectr_std_deviation                               &
     &         (flag_old_fmt, fname_org, flag_spectr, flag_vol_ave,     &
     &          start_time, end_time, sph_IN, WK_tave)
!
      use t_buffer_4_gzip
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use write_sph_monitor_data
      use cal_tave_sph_ene_spectr
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_spectr, flag_vol_ave, flag_old_fmt
      real(kind = kreal), intent(in) :: start_time, end_time
      type(read_sph_spectr_params), intent(inout) :: sph_IN
      type(spectr_ave_sigma_work), intent(inout) :: WK_tave
!
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1, zbuf_s
      character, pointer :: FPz_f1
      type(sph_spectr_head_labels) :: sph_lbl_IN_t
!
      character(len = kchara) :: file_name, extension
      character(len = kchara) :: directory, fname_no_dir, fname_tmp
      real(kind = kreal) :: true_start, prev_time
      integer(kind = kint) :: icou, ierr, ist_true
!
!  Evaluate standard deviation
!
      write(*,*) 'Open file ', trim(fname_org), ' again'
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms,            &
     &                                    fname_org, flag_gzip1, zbuf1)
      call s_select_input_sph_series_head                               &
     &   (FPz_f1, id_file_rms, flag_gzip1,                              &
     &    flag_old_fmt, flag_spectr, flag_vol_ave,                      &
     &    sph_lbl_IN_t, sph_IN, zbuf1)
!
      sph_IN%nri_dat = sph_IN%nri_sph
      if(flag_vol_ave) sph_IN%nri_dat = 1
      if(flag_spectr .eqv. .FALSE.) then
        call alloc_sph_spectr_data(izero, sph_IN)
      else
        call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
      end if
!
      icou = 0
      ist_true = -1
      prev_time = sph_IN%time
      WK_tave%sigma_spec_l = 0.0d0
!      write(*,'(a6,i12,a30,i12)',advance="NO")                         &
!     &       'step= ', sph_IN%i_step,                                  &
!     &       ' deviation finished. Count=  ', icou
      do
        call sel_gz_input_sph_series_data                               &
     &     (FPz_f1, id_file_rms, flag_gzip1,                            &
     &      flag_old_fmt, flag_spectr, flag_vol_ave, sph_IN, zbuf1,     &
     &      ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. start_time) then
          if (ist_true .eq. -1) then
            ist_true = sph_IN%i_step
            true_start = sph_IN%time
            call copy_deviation_ene_2_pre(sph_IN%time, prev_time,       &
     &          sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,   &
     &          sph_IN%spectr_IO, WK_tave%ave_spec_l,                   &
     &          WK_tave%sigma_spec_l, WK_tave%spectr_pre_l)
!
          else
            call sum_deviation_ene_spectr(sph_IN%time, prev_time,       &
     &          sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,   &
     &          sph_IN%spectr_IO, WK_tave%ave_spec_l,                   &
     &          WK_tave%sigma_spec_l, WK_tave%spectr_pre_l)
!
            icou = icou + 1
          end if
        end if
!
!        write(*,'(60a1,a6,i12,a30,i12)',advance="NO") (char(8),i=1,60),&
!     &       'step= ', sph_IN%i_step,                                  &
!     &       ' deviation finished. Count=   ', icou
        if (sph_IN%time .ge. end_time) exit
      end do
   99 continue
      write(*,*)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file_rms, flag_gzip1, zbuf1)
!
      call divide_deviation_ene_spectr(sph_IN%time, true_start,         &
     &    sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,         &
     &    WK_tave%sigma_spec_l, sph_IN%spectr_IO)
!
      if(flag_gzip1) then
        call split_extrension(fname_org, fname_tmp, extension)
      else
        fname_tmp = fname_org
      end if
      call split_directory(fname_tmp, directory, fname_no_dir)
      write(fname_tmp, '(a8,a)') 't_sigma_', trim(fname_no_dir)
      file_name = append_directory(directory, fname_tmp)
!
      write(*,*) 'standard deviation file_name: ', trim(file_name)
      open(id_file_rms, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (.FALSE., id_file_rms, flag_vol_ave, sph_IN, zbuf_s)
!
      call select_output_sph_series_data                                &
     &   (id_file_rms, flag_spectr, flag_vol_ave, sph_IN)
      close(id_file_rms)
!
      end subroutine sph_spectr_std_deviation
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_spectr_snapshot                               &
     &         (fname_org, flag_spectr, flag_vol_ave, sph_IN)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
!
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_params), intent(inout) :: sph_IN
!
      integer(kind = kint) :: ierr
      logical, parameter :: current_fmt = .FALSE.
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
      character, pointer :: FPz_f1
      type(sph_spectr_head_labels) :: sph_lbl_IN_t
!
!  Read spectr data file
!
      write(*,*) 'Open file ', trim(fname_org), ' again'
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms,            &
     &                                    fname_org, flag_gzip1, zbuf1)
!
      call s_select_input_sph_series_head                               &
     &   (FPz_f1, id_file_rms, flag_gzip1,                              &
     &    current_fmt, flag_spectr, flag_vol_ave,                       &
     &    sph_lbl_IN_t, sph_IN, zbuf1)
!
      sph_IN%nri_dat = sph_IN%nri_sph
      if(flag_vol_ave) sph_IN%nri_dat = 1
      if(flag_spectr .eqv. .FALSE.) then
        call alloc_sph_spectr_data(izero, sph_IN)
      else
        call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
      end if
!
      call sel_gz_input_sph_series_data                                 &
     &   (FPz_f1, id_file_rms, flag_gzip1,                              &
     &    current_fmt, flag_spectr, flag_vol_ave, sph_IN, zbuf1, ierr)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file_rms, flag_gzip1, zbuf1)
!
      end subroutine read_sph_spectr_snapshot
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_tave_sph_data(sph_IN, WK_tave)
!
      type(read_sph_spectr_params), intent(in) :: sph_IN
      type(spectr_ave_sigma_work), intent(inout) :: WK_tave
      integer(kind = kint) :: ltr, ncomp
!
!
      ncomp = sph_IN%ntot_sph_spec
      ltr =   sph_IN%ltr_sph
      allocate( WK_tave%ave_spec_l(ncomp,0:ltr,sph_IN%nri_sph))
      allocate( WK_tave%sigma_spec_l(ncomp,0:ltr,sph_IN%nri_sph))
      allocate( WK_tave%spectr_pre_l(ncomp,0:ltr,sph_IN%nri_sph))
!
      if(ncomp .le. 0) return
      WK_tave%ave_spec_l =  0.0d0
      WK_tave%sigma_spec_l =  0.0d0
      WK_tave%spectr_pre_l = 0.0d0
!
      end subroutine alloc_tave_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_tave_sph_data(WK_tave)
!
      type(spectr_ave_sigma_work), intent(inout) :: WK_tave
!
      deallocate(WK_tave%ave_spec_l, WK_tave%sigma_spec_l)
      deallocate(WK_tave%spectr_pre_l)
!
      end subroutine dealloc_tave_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine load_spectr_mean_square_file                           &
     &         (flag_old_fmt, fname_org, flag_spectr, flag_vol_ave,     &
     &          start_time, end_time, true_start, true_end,             &
     &          sph_IN, sph_series)
!
      use t_read_sph_series
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use set_parallel_file_name
      use count_monitor_time_series
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_spectr, flag_vol_ave, flag_old_fmt
      real(kind = kreal), intent(in) :: start_time, end_time
      real(kind = kreal), intent(inout) :: true_start, true_end
      type(read_sph_spectr_params), intent(inout) :: sph_IN
      type(read_sph_spectr_series), intent(inout) :: sph_series
!
      real(kind = kreal) :: prev_time
      integer(kind = kint) :: icou, ierr, ist_true, i, num, num_count
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
      character, pointer :: FPz_f1
      type(sph_spectr_head_labels) :: sph_lbl_IN_t
!
!
      write(*,*) 'Open file ', trim(fname_org)
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms,            &
     &                                    fname_org, flag_gzip1, zbuf1)
      call s_select_input_sph_series_head                               &
     &   (FPz_f1, id_file_rms, flag_gzip1,                              &
     &    flag_old_fmt, flag_spectr, flag_vol_ave,                      &
     &    sph_lbl_IN_t, sph_IN, zbuf1)
!
      num = sph_IN%nri_sph
      if(flag_vol_ave) num = 1
      if(flag_spectr)  num = num * (sph_IN%ltr_sph + 1)
      call s_count_monitor_time_series                                  &
     &   (.TRUE., FPz_f1, id_file_rms, flag_gzip1, num,                 &
     &    start_time, end_time, true_start, true_end, num_count, zbuf1)
      call dealloc_sph_espec_name(sph_IN)
!
      if(flag_gzip1) then
        ierr =  rewind_gzfile(FPz_f1)
      else
        rewind(id_file_rms)
      end if
!
      call s_select_input_sph_series_head                               &
     &   (FPz_f1, id_file_rms, flag_gzip1,                              &
     &    flag_old_fmt, flag_spectr, flag_vol_ave,                      &
     &    sph_lbl_IN_t, sph_IN, zbuf1)
!
      sph_IN%nri_dat = sph_IN%nri_sph
      if(flag_vol_ave) sph_IN%nri_dat = 1
      if(flag_spectr) then
        call alloc_sph_spectr_data(sph_IN%ltr_sph, sph_IN)
        call alloc_sph_spectr_series(sph_IN%ltr_sph, sph_IN,            &
     &                               num_count, sph_series)
      else
        call alloc_sph_spectr_data(izero, sph_IN)
        call alloc_sph_spectr_series(izero, sph_IN,                     &
     &                               num_count, sph_series)
      end if
!
      icou = 0
      ist_true = -1
      prev_time = sph_IN%time
      write(*,'(a6,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN%i_step,                                   &
     &       ' averaging finished. Count=  ', icou
      do
        call sel_gz_input_sph_series_data                               &
     &     (FPz_f1, id_file_rms, flag_gzip1,                            &
     &      flag_old_fmt, flag_spectr, flag_vol_ave, sph_IN, zbuf1,     &
     &      ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. start_time) then
          icou = icou + 1
          if(flag_spectr) then
            call copy_spectr_IO_to_series(icou, sph_IN%ltr_sph,         &
     &                                    sph_IN, sph_series)
          else
            call copy_spectr_IO_to_series(icou, izero,                  &
     &                                    sph_IN, sph_series)
          end if
        end if
!
        write(*,'(60a1,a6,i12,a30,i12)',advance="NO") (char(8),i=1,60),&
     &       'step= ', sph_IN%i_step,                                  &
     &       ' load finished. Count=   ', icou
        if (sph_IN%time .ge. end_time) then
          true_end = sph_IN%time
          exit
        end if
      end do
!
   99 continue
      write(*,*)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file_rms, flag_gzip1, zbuf1)
!
      end subroutine load_spectr_mean_square_file
!
! -------------------------------------------------------------------
!
      end module m_tave_sph_ene_spectr
