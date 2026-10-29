!>@file   t_time_ave_sph_volume_mean.f90
!!        module t_time_ave_sph_volume_mean
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine alloc_t_average_volume_mean(nstep, ncomp, tave_vm)
!!      subroutine dealloc_t_average_volume_mean(tave_vm)
!!        integer(kind = kint), intent(in) :: nstep, ncomp
!!        type(time_average_volume_mean), intent(inout) :: tave_vm
!!
!!      subroutine time_ave_sdev_sph_volume_mean(fname_org,             &
!!     &                                         start_time, end_time)
!!        character(len = kchara), intent(in) :: fname_org
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        type(read_sph_spectr_data), save :: sph_IN1
!!        type(sph_volume_mean_series), save:: vm_srs1
!!        real(kind = kreal), allocatable :: ave_mean(:)
!!        real(kind = kreal), allocatable :: rms_mean(:)
!!        real(kind = kreal), allocatable :: sdev_mean(:)
!!        integer(kind = kint), allocatable :: iflag_all(:)
!!        real(kind = kreal) :: true_start, true_end
!!@endverbatim
!
      module t_time_ave_sph_volume_mean
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_stream = 46
!
      type time_average_volume_mean
        integer(kind = kint) :: ncomp_tave
        real(kind = kreal), allocatable :: ave_mean(:)
        real(kind = kreal), allocatable :: rms_mean(:)
        real(kind = kreal), allocatable :: sdev_mean(:)
!
        integer(kind = kint) :: nstep_tave
        integer(kind = kint), allocatable :: iflag_all(:)
      end type time_average_volume_mean
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_t_average_volume_mean(nstep, ncomp, tave_vm)
!
      integer(kind = kint), intent(in) :: nstep, ncomp
      type(time_average_volume_mean), intent(inout) :: tave_vm
!
!
      tave_vm%nstep_tave = nstep
      allocate(tave_vm%iflag_all(tave_vm%nstep_tave))
      if(nstep .gt. 0) tave_vm%iflag_all(1:tave_vm%nstep_tave) = 1
!
      tave_vm%ncomp_tave = ncomp
      allocate(tave_vm%ave_mean(tave_vm%ncomp_tave))
      allocate(tave_vm%rms_mean(tave_vm%ncomp_tave))
      allocate(tave_vm%sdev_mean(tave_vm%ncomp_tave))
!
      if(tave_vm%ncomp_tave .le. 0) return
      tave_vm%ave_mean(1:tave_vm%ncomp_tave) =   0.0d0
      tave_vm%rms_mean(1:tave_vm%ncomp_tave) =   0.0d0
      tave_vm%sdev_mean(1:tave_vm%ncomp_tave) =  0.0d0
!
      end subroutine alloc_t_average_volume_mean
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_t_average_volume_mean(tave_vm)
!
      type(time_average_volume_mean), intent(inout) :: tave_vm
!
!
      deallocate(tave_vm%ave_mean,  tave_vm%rms_mean)
      deallocate(tave_vm%sdev_mean, tave_vm%iflag_all)
!
      end subroutine dealloc_t_average_volume_mean
!
!   --------------------------------------------------------------------
!
      subroutine time_ave_sdev_sph_volume_mean(fname_org,               &
     &                                         start_time, end_time)
!
      use t_sph_volume_mean_series
      use count_monitor_time_series
      use sph_monitor_data_text
      use set_parallel_file_name
      use sph_volume_monitor_snap_IO
!'
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(sph_spectr_head_labels), save ::   sph_lbl_IN1
      type(read_sph_spectr_data), save ::     sph_IN1
      type(sph_volume_mean_series), save ::   vm_srs1
      type(time_average_volume_mean), save :: tave_vm1
      real(kind = kreal) :: true_start, true_end
!
      character(len=2+23+25+25+1) :: comment_1
!
      integer(kind = kint) :: i
      character(len = kchara) :: ave_fname, fname_tmp
      character(len = kchara) :: directory, fname_no_dir
!
!
      call load_sph_volume_mean_file(fname_org, start_time, end_time,   &
     &    true_start, true_end, sph_lbl_IN1, sph_IN1, vm_srs1)
      write(comment_1,'(2a,a23,1p2E25.15e3,a1)') '#', char(10),         &
     &             '# Start and End time:  ', true_start, true_end,     &
     &             char(10)
!
      call alloc_t_average_volume_mean                                  &
     &   (vm_srs1%n_step, vm_srs1%ntot_comp, tave_vm1)
      call cal_time_ave_picked_sph_spectr                               &
     &   (vm_srs1%n_step, vm_srs1%d_time, tave_vm1%iflag_all,           &
     &    tave_vm1%ncomp_tave, vm_srs1%vmean_series,                    &
     &    tave_vm1%ave_mean, tave_vm1%rms_mean, tave_vm1%sdev_mean)
!
      write(*,'(a)') 'Average, R.M.S., standard_deviation, Item_name'
      do i = 1, tave_vm1%ncomp_tave
        write(*,'(1p3E25.15e3, 2a)')                                    &
     &       tave_vm1%ave_mean(i), tave_vm1%rms_mean(i),                &
     &       tave_vm1%sdev_mean(i), ':   ',                             &
     &       trim(sph_IN1%ene_sph_spec_name(i+sph_IN1%num_time_labels))
      end do
!
      call split_directory(fname_org, directory, fname_no_dir)
      write(fname_tmp, '(a12,a)') 't_ave_sigma_', trim(fname_no_dir)
      ave_fname = append_directory(directory, fname_tmp)
!
      call write_sph_vol_mean_tave_sdev(.FALSE., ave_fname,             &
     &    comment_1, sph_lbl_IN1, sph_IN1, tave_vm1%ncomp_tave,         &
     &    tave_vm1%ave_mean, tave_vm1%rms_mean, tave_vm1%sdev_mean)
!
      call dealloc_t_average_volume_mean(tave_vm1)
      call dealloc_sph_volume_mean_series(vm_srs1)
      call dealloc_sph_espec_name(sph_IN1)
      call dealloc_sph_espec_data(sph_IN1)
!
      end subroutine time_ave_sdev_sph_volume_mean
!
!   --------------------------------------------------------------------
!
      end module t_time_ave_sph_volume_mean
