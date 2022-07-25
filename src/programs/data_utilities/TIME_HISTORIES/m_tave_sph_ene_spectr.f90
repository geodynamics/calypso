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
!!        type(read_sph_spectr_data), intent(inout) :: tave_sph_IN
!!        type(read_sph_spectr_data), intent(inout) :: sdev_sph_IN
!!@endverbatim
!
      module m_tave_sph_ene_spectr
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
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
      type(read_sph_spectr_data), save, private :: sph_IN1
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
      integer(kind = kint) :: i
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
      if(flag_vol_ave .and. (flag_spectr .eqv. .FALSE.)) then
        write(*,*) 'Time_average, standard_deviation, field_name'
        do i = 1, sph_IN1%ntot_sph_spec
          write(*,'(1p2e23.15e3,2a)') WK_tave1%ave_spec_l(i,0,1),       &
     &       WK_tave1%sigma_spec_l(i,0,1), '    ',                      &
     &      trim(sph_IN1%ene_sph_spec_name(i+sph_IN1%num_time_labels))
        end do
      end if
!
      call dealloc_tave_sph_data(WK_tave1)
      call dealloc_sph_espec_data(sph_IN1)
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
!
      end subroutine time_ave_sdev_sph_old_spectr
!
!   --------------------------------------------------------------------
!
      subroutine read_time_ave_sdev_sph_spectr                          &
     &         (tave_file_name, sdev_file_name,                         &
     &          flag_spectr, flag_vol_ave, tave_sph_IN, sdev_sph_IN)
!
      character(len = kchara), intent(in) :: tave_file_name
      character(len = kchara), intent(in) :: sdev_file_name
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: tave_sph_IN
      type(read_sph_spectr_data), intent(inout) :: sdev_sph_IN
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
      use simple_sph_spectr_head_IO
      use sph_mean_square_IO_select
      use cal_tave_sph_ene_spectr
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_spectr, flag_vol_ave, flag_old_fmt
      real(kind = kreal), intent(in) :: start_time, end_time
      real(kind = kreal), intent(inout) :: true_start, true_end
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(spectr_ave_sigma_work), intent(inout) :: WK_tave
!
      character(len = kchara) :: file_name
      real(kind = kreal) :: prev_time
      integer(kind = kint) :: i, icou, ierr, ist_true
!
!
      write(*,*) 'Open file ', trim(fname_org)
      open(id_file_rms, file=fname_org)
      call select_input_sph_series_head(id_file_rms,                    &
     &    flag_old_fmt, flag_spectr, flag_vol_ave, sph_IN)
      call check_sph_spectr_name(sph_IN)
!
      call alloc_tave_sph_data(sph_IN, WK_tave)
!
      icou = 0
      ist_true = -1
      prev_time = sph_IN%time
!      write(*,'(a6,i12,a30,i12)',advance="NO")                         &
!     &       'step= ', sph_IN%i_step,                                  &
!     &       ' averaging finished. Count=  ', icou
      do
        call select_input_sph_series_data(id_file_rms, flag_old_fmt,    &
     &      flag_spectr, flag_vol_ave, sph_IN, ierr)
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
!        write(*,'(60a1,a6,i12,a30,i12)',advance="NO") (char(8),i=1,60),&
!     &       'step= ', sph_IN%i_step,                                  &
!     &       ' averaging finished. Count=   ', icou
        if (sph_IN%time .ge. end_time) then
          true_end = sph_IN%time
          exit
        end if
      end do
!
   99 continue
      write(*,*)
      close(id_file_rms)
!
!
      call divide_average_ene_spectr(sph_IN%time, true_start,           &
     &    sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,         &
     &    WK_tave%ave_spec_l, sph_IN%spectr_IO)
!
!  Output average
      write(file_name, '(a6,a)') 't_ave_', trim(fname_org)
      open(id_file_rms, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (id_file_rms, flag_vol_ave, sph_IN)
!
      call select_output_sph_series_data                                &
     &   (id_file_rms, flag_spectr, flag_vol_ave, sph_IN)
      close(id_file_rms)
!
      call dealloc_sph_espec_data(sph_IN)
!
      end subroutine sph_spectr_average
!
!   --------------------------------------------------------------------
!
      subroutine sph_spectr_std_deviation                               &
     &         (flag_old_fmt, fname_org, flag_spectr, flag_vol_ave,     &
     &          start_time, end_time, sph_IN, WK_tave)
!
      use simple_sph_spectr_head_IO
      use sph_mean_square_IO_select
      use cal_tave_sph_ene_spectr
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_spectr, flag_vol_ave, flag_old_fmt
      real(kind = kreal), intent(in) :: start_time, end_time
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(spectr_ave_sigma_work), intent(inout) :: WK_tave
!
      character(len = kchara) :: file_name
      real(kind = kreal) :: true_start, prev_time
      integer(kind = kint) :: i, icou, ierr, ist_true
!
!  Evaluate standard deviation
!
      write(*,*) 'Open file ', trim(fname_org), ' again'
      open(id_file_rms, file=fname_org)
      call select_input_sph_series_head(id_file_rms,                    &
     &    flag_old_fmt, flag_spectr, flag_vol_ave, sph_IN)
!
      icou = 0
      ist_true = -1
      prev_time = sph_IN%time
      WK_tave%sigma_spec_l = 0.0d0
!      write(*,'(a6,i12,a30,i12)',advance="NO")                         &
!     &       'step= ', sph_IN%i_step,                                  &
!     &       ' deviation finished. Count=  ', icou
      do
        call select_input_sph_series_data(id_file_rms, flag_old_fmt,    &
     &      flag_spectr, flag_vol_ave, sph_IN, ierr)
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
      close(id_file_rms)
!
      call divide_deviation_ene_spectr(sph_IN%time, true_start,         &
     &    sph_IN%nri_sph, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,         &
     &    WK_tave%sigma_spec_l, sph_IN%spectr_IO)
!
      write(file_name, '(a8,a)') 't_sigma_', trim(fname_org)
      open(id_file_rms, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (id_file_rms, flag_vol_ave, sph_IN)
!
      call select_output_sph_series_data                                &
     &   (id_file_rms, flag_spectr, flag_vol_ave, sph_IN)
!
      close(id_file_rms)
!
      end subroutine sph_spectr_std_deviation
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_spectr_snapshot                               &
     &         (fname_org, flag_spectr, flag_vol_ave, sph_IN)
!
      use simple_sph_spectr_head_IO
      use sph_mean_square_IO_select
!
!
      character(len = kchara), intent(in) :: fname_org
      logical, intent(in) :: flag_spectr, flag_vol_ave
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      integer(kind = kint) :: ierr
      logical, parameter :: current_fmt = .FALSE.
!
!  Read spectr data file
!
      write(*,*) 'Open file ', trim(fname_org), ' again'
      open(id_file_rms, file=fname_org)
!
      call select_input_sph_series_head(id_file_rms,                    &
     &    current_fmt, flag_spectr, flag_vol_ave, sph_IN)
      call check_sph_spectr_name(sph_IN)
!
      call select_input_sph_series_data(id_file_rms, current_fmt,       &
     &    flag_spectr, flag_vol_ave, sph_IN, ierr)
      close(id_file_rms)
!
      end subroutine read_sph_spectr_snapshot
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_tave_sph_data(sph_IN, WK_tave)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
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
      end module m_tave_sph_ene_spectr
