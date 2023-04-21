!>@file   t_sph_volume_spectr_series.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time series of voume mean data
!!
!!@verbatim
!!      subroutine load_sph_volume_spec_file(fname_org,                 &
!!     &          start_time, end_time, true_start, true_end,           &
!!     &          sph_lbl_IN, sph_IN, vs_srs)
!!      subroutine dealloc_sph_volume_spec_series(vs_srs)
!!        logical, intent(in) :: flag_old_fmt
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        real(kind = kreal), intent(inout) :: true_start, true_end
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(sph_volume_spectr_series), intent(inout) :: vs_srs
!!@endverbatim
      module t_sph_volume_spectr_series
!
      use m_precision
      use m_constants
!
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
!
      type sph_volume_spectr_series
!>        Number of time series
        integer(kind = kint) :: n_step =    0
!>        Number of time series
        integer(kind = kint) :: ntot_comp = 0
!>        Number of Radial points
        integer(kind = kint) :: ltr_srs =   0
!>        Number of data for each step
        integer(kind = kint), allocatable :: i_step(:)
!>        time
        real(kind = kreal), allocatable :: d_time(:)
!>        spectr time series
        real(kind = kreal), allocatable :: vspec_series(:,:,:)
!>        spectr snap shot to loading
        real(kind = kreal), allocatable :: vspec_snap(:,:)
      end type sph_volume_spectr_series
!
      integer(kind = kint), parameter, private :: id_file_rms = 34
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine load_sph_volume_spec_file(fname_org,                   &
     &          start_time, end_time, true_start, true_end,             &
     &          sph_lbl_IN, sph_IN, vs_srs)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_volume_spectr_monitor_IO
      use set_parallel_file_name
      use count_monitor_time_series
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in) :: start_time, end_time
      real(kind = kreal), intent(inout) :: true_start, true_end
!
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(sph_volume_spectr_series), intent(inout) :: vs_srs
!
      real(kind = kreal) :: prev_time
      integer(kind = kint) :: icou, ierr, ist_true, i, num
      integer(kind = kint) :: num_count, icou_skip
      logical :: flag_gzip1
      type(buffer_4_gzip) :: zbuf1
      character, pointer :: FPz_f1
!
!
      call sel_open_read_gz_stream_file(FPz_f1, id_file_rms,            &
     &                                  fname_org, flag_gzip1, zbuf1)
      call read_sph_volume_spectr_head(FPz_f1, id_file_rms, flag_gzip1, &
     &                                 sph_lbl_IN, sph_IN, zbuf1)
!
      num = sph_IN%ltr_sph + 1
      call s_count_monitor_time_series                                  &
     &   (.TRUE., FPz_f1, id_file_rms, flag_gzip1, num,                 &
     &    start_time, end_time, true_start, true_end,                   &
     &    num_count, icou_skip, zbuf1)
      call dealloc_sph_espec_name(sph_IN)
!
      call sel_redwind_gz_stream_file(FPz_f1, id_file_rms, flag_gzip1)
      call read_sph_volume_spectr_head(FPz_f1, id_file_rms, flag_gzip1, &
     &                                 sph_lbl_IN, sph_IN, zbuf1)
      call s_skip_monitor_time_series                                   &
     &   (.TRUE., FPz_f1, id_file_rms, flag_gzip1, num,                 &
     &    icou_skip, zbuf1)
!
!
      call alloc_sph_spectr_data(izero, sph_IN)
      call alloc_sph_volume_spec_series                                 &
     &   (num_count, sph_IN%ltr_sph, sph_IN%ntot_sph_spec, vs_srs)
!
      icou = 0
      ist_true = -1
      prev_time = sph_IN%time
      write(*,'(a6,i12,a8,f12.6,a15,i12)',advance="NO")                 &
     &       'step= ', sph_IN%i_step, ', time= ', sph_IN%time,          &
     &       ', Load Count:  ', icou
      do
        call sel_gz_read_volume_spectr_mtr(FPz_f1, id_file_rms,         &
     &      flag_gzip1, sph_IN%ltr_sph, sph_IN%ntot_sph_spec,           &
     &      sph_IN%i_step, sph_IN%time, sph_IN%i_mode,                  &
     &      vs_srs%vspec_snap(1,0), zbuf1, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. start_time) then
          icou = icou + 1
          call copy_sph_volume_spec_series                              &
     &       (icou, sph_IN%i_step, sph_IN%time, vs_srs)
        end if
!
        write(*,'(65a1,a6,i12,a8,f12.6,a15,i12)',advance="NO")          &
     &       (char(8),i=1,65),                                          &
     &       'step= ', sph_IN%i_step, ', time= ', sph_IN%time,          &
     &       ', Load Count:  ', icou
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
      end subroutine load_sph_volume_spec_file
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_volume_spec_series(vs_srs)
!
      type(sph_volume_spectr_series), intent(inout) :: vs_srs
!
!
      deallocate( vs_srs%i_step, vs_srs%d_time)
      deallocate( vs_srs%vspec_series, vs_srs%vspec_snap)
!
      end subroutine dealloc_sph_volume_spec_series
!
! --------------------------------------------------------------------
! --------------------------------------------------------------------
!
      subroutine alloc_sph_volume_spec_series(n_step, ltr, ncomp,       &
     &                                        vs_srs)
!
      integer(kind = kint), intent(in) :: n_step, ltr, ncomp
      type(sph_volume_spectr_series), intent(inout) :: vs_srs
!
!
      vs_srs%ntot_comp = ncomp
      vs_srs%ltr_srs = ltr
      vs_srs%n_step = n_step
      allocate(vs_srs%i_step(vs_srs%n_step))
      allocate(vs_srs%d_time(vs_srs%n_step))
      allocate(vs_srs%vspec_series(ncomp,0:ltr,vs_srs%n_step))
      allocate(vs_srs%vspec_snap(ncomp,0:ltr))
!
!$omp parallel workshare
      vs_srs%i_step(1:vs_srs%n_step) = izero
      vs_srs%d_time(1:vs_srs%n_step) = zero
!$omp end parallel workshare
!$omp parallel workshare
      vs_srs%vspec_series(1:ncomp,0:ltr,1:vs_srs%n_step) =  zero
!$omp end parallel workshare
!$omp parallel workshare
      vs_srs%vspec_snap(1:ncomp,0:ltr) =  zero
!$omp end parallel workshare
!
      end subroutine alloc_sph_volume_spec_series
!
!   --------------------------------------------------------------------
!
      subroutine copy_sph_volume_spec_series(icou, i_step, time,        &
     &                                       vs_srs)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(sph_volume_spectr_series), intent(inout) :: vs_srs
!
!
      vs_srs%i_step(icou) = i_step
      vs_srs%d_time(icou) = time
!$omp parallel workshare
      vs_srs%vspec_series(1:vs_srs%ntot_comp,0:vs_srs%ltr_srs,icou)     &
     &        = vs_srs%vspec_snap(1:vs_srs%ntot_comp,0:vs_srs%ltr_srs)
!$omp end parallel workshare
!
      end subroutine copy_sph_volume_spec_series
!
!   --------------------------------------------------------------------
!
      end module t_sph_volume_spectr_series
