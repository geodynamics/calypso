!>@file   time_ave_sph_volume_mean.f90
!!        module time_ave_sph_volume_mean
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
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
      module time_ave_sph_volume_mean
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
!   --------------------------------------------------------------------
!
      contains
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
      type(sph_spectr_head_labels), save :: sph_lbl_IN1
      type(read_sph_spectr_data), save :: sph_IN1
      type(sph_volume_mean_series), save:: vm_srs1
      real(kind = kreal), allocatable :: ave_mean(:)
      real(kind = kreal), allocatable :: rms_mean(:)
      real(kind = kreal), allocatable :: sdev_mean(:)
      integer(kind = kint), allocatable :: iflag_all(:)
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
      allocate(ave_mean(vm_srs1%ntot_comp))
      allocate(rms_mean(vm_srs1%ntot_comp))
      allocate(sdev_mean(vm_srs1%ntot_comp))
      allocate(iflag_all(vm_srs1%n_step))
      iflag_all(1:vm_srs1%n_step) = 1
      ave_mean(1:vm_srs1%ntot_comp) =   0.0d0
      rms_mean(1:vm_srs1%ntot_comp) =   0.0d0
      sdev_mean(1:vm_srs1%ntot_comp) =  0.0d0
      call cal_time_ave_picked_sph_spectr                               &
     &   (vm_srs1%n_step, vm_srs1%d_time, iflag_all,                    &
     &    vm_srs1%ntot_comp, vm_srs1%vmean_series,                      &
     &    ave_mean, rms_mean, sdev_mean)
!
      write(*,'(a)') 'Average, R.M.S., standard_deviation, Item_name'
      do i = 1, vm_srs1%ntot_comp
        write(*,'(1p3E25.15e3, 2a)')                                    &
     &       ave_mean(i), rms_mean(i), sdev_mean(i), ':   ',            &
     &       trim(sph_IN1%ene_sph_spec_name(i+sph_IN1%num_time_labels))
      end do
!
      call split_directory(fname_org, directory, fname_no_dir)
      write(fname_tmp, '(a12,a)') 't_ave_sigma_', trim(fname_no_dir)
      ave_fname = append_directory(directory, fname_tmp)
!
      call write_sph_vol_mean_tave_sdev(.FALSE., ave_fname,             &
     &    comment_1, sph_lbl_IN1, sph_IN1, vm_srs1%ntot_comp,           &
     &    ave_mean, rms_mean, sdev_mean)
!
      deallocate(ave_mean, rms_mean, sdev_mean, iflag_all)
      call dealloc_sph_volume_mean_series(vm_srs1)
      call dealloc_sph_espec_name(sph_IN1)
      call dealloc_sph_espec_data(sph_IN1)
!
      end subroutine time_ave_sdev_sph_volume_mean
!
!   --------------------------------------------------------------------
!
      end module time_ave_sph_volume_mean
