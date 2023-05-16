!>@file   t_sph_volume_mean_series.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time series of voume mean data
!!
!!@verbatim
!!      subroutine load_sph_volume_mean_file(fname_org,                 &
!!     &          start_time, end_time, true_start, true_end,           &
!!     &          sph_lbl_IN, sph_IN, vm_srs)
!!      subroutine dealloc_sph_volume_mean_series(vm_srs)
!!        logical, intent(in) :: flag_old_fmt
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        real(kind = kreal), intent(inout) :: true_start, true_end
!!        type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!        type(sph_volume_mean_series), intent(inout) :: vm_srs
!!@endverbatim
      module t_sph_volume_mean_series
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
      type sph_volume_mean_series
!>        Number of time series
        integer(kind = kint) :: n_step =    0
!>        Number of time series
        integer(kind = kint) :: ntot_comp = 0
!>        Number of data for each step
        integer(kind = kint), allocatable :: i_step(:)
!>        time
        real(kind = kreal), allocatable :: d_time(:)
!>        spectr time series
        real(kind = kreal), allocatable :: vmean_series(:,:)
!>        spectr snap shot to loading
        real(kind = kreal), allocatable :: vmean_snap(:)
      end type sph_volume_mean_series
!
      integer(kind = kint), parameter, private :: id_file_rms = 34
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine load_sph_volume_mean_file(fname_org,                   &
     &          start_time, end_time, true_start, true_end,             &
     &          sph_lbl_IN, sph_IN, vm_srs)
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use gz_spl_sph_spectr_data_IO
      use set_parallel_file_name
      use count_monitor_time_series
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in) :: start_time, end_time
      real(kind = kreal), intent(inout) :: true_start, true_end
!
      type(sph_spectr_head_labels), intent(inout) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      type(sph_volume_mean_series), intent(inout) :: vm_srs
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
      call read_sph_volume_mean_head(FPz_f1, id_file_rms, flag_gzip1,   &
     &                                sph_lbl_IN, sph_IN, zbuf1)
!
      num = 1
      call s_count_monitor_time_series                                  &
     &   (.TRUE., FPz_f1, id_file_rms, flag_gzip1, num,                 &
     &    start_time, end_time, true_start, true_end,                   &
     &    num_count, icou_skip, zbuf1)
      call dealloc_sph_espec_name(sph_IN)
!
      call sel_redwind_gz_stream_file(FPz_f1, id_file_rms, flag_gzip1)
      call read_sph_volume_mean_head(FPz_f1, id_file_rms, flag_gzip1,   &
     &                                sph_lbl_IN, sph_IN, zbuf1)
!
      sph_IN%nri_dat = 1
      call alloc_sph_spectr_data(izero, sph_IN)
      call alloc_sph_volume_mean_series                                 &
     &   (num_count, sph_IN%ntot_sph_spec, vm_srs)
!
      call s_skip_monitor_time_series                                   &
     &   (.TRUE., FPz_f1, id_file_rms, flag_gzip1, num,                 &
     &    icou_skip, zbuf1)
!
      icou = 0
      ist_true = -1
      prev_time = sph_IN%time
      write(*,'(a6,i12,a8,f12.6,a15,i12)',advance="NO")                 &
     &       'step= ', sph_IN%i_step, ', time= ', sph_IN%time,          &
     &       ', Load Count:  ', icou
      do
        call gz_read_volume_pwr_sph(FPz_f1, id_file_rms, flag_gzip1,    &
     &      sph_IN%ntot_sph_spec, sph_IN%i_step, sph_IN%time,           &
     &      vm_srs%vmean_snap(1), zbuf1, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN%time .ge. start_time) then
          icou = icou + 1
          call copy_sph_volume_mean_series                              &
     &       (icou, sph_IN%i_step, sph_IN%time, vm_srs)
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
      end subroutine load_sph_volume_mean_file
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_volume_mean_series(vm_srs)
!
      type(sph_volume_mean_series), intent(inout) :: vm_srs
!
!
      deallocate(vm_srs%i_step, vm_srs%d_time)
      deallocate(vm_srs%vmean_series, vm_srs%vmean_snap)
!
      end subroutine dealloc_sph_volume_mean_series
!
! --------------------------------------------------------------------
! --------------------------------------------------------------------
!
      subroutine alloc_sph_volume_mean_series(n_step, ncomp, vm_srs)
!
      integer(kind = kint), intent(in) :: n_step, ncomp
      type(sph_volume_mean_series), intent(inout) :: vm_srs
!
!
      vm_srs%ntot_comp = ncomp
      vm_srs%n_step = n_step
      allocate( vm_srs%i_step(vm_srs%n_step) )
      allocate( vm_srs%d_time(vm_srs%n_step) )
      allocate( vm_srs%vmean_series(vm_srs%ntot_comp,vm_srs%n_step))
      allocate( vm_srs%vmean_snap(vm_srs%ntot_comp))
!
!$omp parallel workshare
      vm_srs%i_step(1:vm_srs%n_step) = izero
      vm_srs%d_time(1:vm_srs%n_step) = zero
!$omp end parallel workshare
!$omp parallel workshare
      vm_srs%vmean_series(1:vm_srs%ntot_comp,1:vm_srs%n_step) =  zero
!$omp end parallel workshare
!$omp parallel workshare
      vm_srs%vmean_snap(1:vm_srs%ntot_comp) =  zero
!$omp end parallel workshare
!
      end subroutine alloc_sph_volume_mean_series
!
!   --------------------------------------------------------------------
!
      subroutine copy_sph_volume_mean_series(icou, i_step, time,        &
     &                                       vm_srs)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(sph_volume_mean_series), intent(inout) :: vm_srs
!
!
      vm_srs%i_step(icou) = i_step
      vm_srs%d_time(icou) = time
!$omp parallel workshare
      vm_srs%vmean_series(1:vm_srs%ntot_comp,icou)                      &
     &                          = vm_srs%vmean_snap(1:vm_srs%ntot_comp)
!$omp end parallel workshare
!
      end subroutine copy_sph_volume_mean_series
!
!   --------------------------------------------------------------------
!
      end module t_sph_volume_mean_series
