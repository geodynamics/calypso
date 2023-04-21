!>@file   time_ave_sph_monitor_data.f90
!!        module time_ave_sph_monitor_data
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine time_ave_sdev_sph_volume_spec(fname_org,             &
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
      module time_ave_sph_monitor_data
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
      subroutine time_ave_sdev_sph_volume_spec(fname_org,               &
     &                                         start_time, end_time)
!
      use t_sph_volume_spectr_series
      use count_monitor_time_series
      use sph_monitor_data_text
      use set_parallel_file_name
      use select_gz_stream_file_IO
      use gz_open_sph_vol_mntr_file
      use gz_volume_spectr_monitor_IO
!'
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in) :: start_time, end_time
!
      type(buffer_4_gzip), save :: zbuf_m
      type(sph_spectr_head_labels), save :: sph_lbl_IN1
      type(read_sph_spectr_data), save :: sph_IN1
      type(sph_volume_spectr_series), save:: vs_srs1
      real(kind = kreal), allocatable :: ave_mean(:,:)
      real(kind = kreal), allocatable :: rms_mean(:,:)
      real(kind = kreal), allocatable :: sdev_mean(:,:)
      integer(kind = kint), allocatable :: iflag_all(:)
      real(kind = kreal) :: true_start, true_end
!
      character(len=2+23+25+25+1) :: comment_1
      character(len=24+28+30+1+4), parameter :: comment_2               &
     &          =  '# 1st data: Time average' // char(10)               &
     &          // '# 2nd data: R.M.S. over time' // char(10)           &
     &          // '# 3rd data: Standard Deviation' // char(10)         &
     &          // '#' // char(10)
!
      integer(kind = kint) :: ntot
!      integer(kind = kint) :: i, l
      character(len = kchara) :: ave_fname, fname_tmp
      character(len = kchara) :: directory, fname_no_dir
!
!
      call load_sph_volume_spec_file(fname_org, start_time, end_time,   &
     &    true_start, true_end, sph_lbl_IN1, sph_IN1, vs_srs1)
      write(comment_1,'(2a,a23,1p2E25.15e3,a1)') '#', char(10),         &
     &             '# Start and End time:  ', true_start, true_end,     &
     &             char(10)
!
      allocate(ave_mean(vs_srs1%ntot_comp,0:vs_srs1%ltr_srs))
      allocate(rms_mean(vs_srs1%ntot_comp,0:vs_srs1%ltr_srs))
      allocate(sdev_mean(vs_srs1%ntot_comp,0:vs_srs1%ltr_srs))
      allocate(iflag_all(vs_srs1%n_step))
      iflag_all(1:vs_srs1%n_step) = 1
!$omp parallel workshare
      ave_mean(1:vs_srs1%ntot_comp,0:vs_srs1%ltr_srs) =   0.0d0
      rms_mean(1:vs_srs1%ntot_comp,0:vs_srs1%ltr_srs) =   0.0d0
      sdev_mean(1:vs_srs1%ntot_comp,0:vs_srs1%ltr_srs) =  0.0d0
!$omp end parallel workshare

      ntot = vs_srs1%ntot_comp * (vs_srs1%ltr_srs + 1)
      call cal_time_ave_picked_sph_spectr                              &
     &   (vs_srs1%n_step, vs_srs1%d_time, iflag_all,                   &
     &    ntot, vs_srs1%vspec_series(1,0,1),                           &
     &    ave_mean(1,0), rms_mean(1,0), sdev_mean(1,0))
!
!      do i = 1, vs_srs1%ntot_comp
!        write(*,'(a)')                                                 &
!     &     trim(sph_IN1%ene_sph_spec_name(i+sph_IN1%num_time_labels))
!        write(*,'(2a)')  trim(sph_IN1%ene_sph_spec_name(3)),           &
!     &     '  Average, R.M.S., standard_deviation, Item_name'
!        do l = 0, vs_srs1%ltr_srs
!          write(*,'(i8,1p3E25.15e3)')                                  &
!     &       l, ave_mean(i,l), rms_mean(i,l), sdev_mean(i,l)
!        end do
!      end do
!
      call split_directory(fname_org, directory, fname_no_dir)
      write(fname_tmp, '(2a)') 't_ave_', trim(fname_no_dir)
      ave_fname = append_directory(directory, fname_tmp)
!
      write(*,*) 'Write ASCII file: ', trim(ave_fname)
      open(id_stream, file=ave_fname, status='replace',                 &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      write(id_stream) comment_1
      write(id_stream) comment_2
      call write_sph_pwr_vol_head(.FALSE., id_stream,                   &
     &                            sph_lbl_IN1, sph_IN1, zbuf_m)
!
      call sel_gz_write_volume_spectr_mtr(.FALSE., id_stream,           &
     &    sph_IN1%i_step, sph_IN1%time, vs_srs1%ltr_srs,                &
     &    vs_srs1%ntot_comp, ave_mean(1,0), zbuf_m)
      close(id_stream)
!
      call split_directory(fname_org, directory, fname_no_dir)
      write(fname_tmp, '(a6,a)') 't_rms_', trim(fname_no_dir)
      ave_fname = append_directory(directory, fname_tmp)
!
      write(*,*) 'Write ASCII file: ', trim(ave_fname)
      open(id_stream, file=ave_fname, status='replace',                 &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      write(id_stream) comment_1
      write(id_stream) comment_2
      call write_sph_pwr_vol_head(.FALSE., id_stream,                   &
     &                            sph_lbl_IN1, sph_IN1, zbuf_m)
!
      call sel_gz_write_volume_spectr_mtr(.FALSE., id_stream,           &
     &    sph_IN1%i_step, sph_IN1%time, vs_srs1%ltr_srs,                &
     &    vs_srs1%ntot_comp, rms_mean(1,0), zbuf_m)
      close(id_stream)
!
      call split_directory(fname_org, directory, fname_no_dir)
      write(fname_tmp, '(a7,a)') 't_sdev_', trim(fname_no_dir)
      ave_fname = append_directory(directory, fname_tmp)
!
      write(*,*) 'Write ASCII file: ', trim(ave_fname)
      open(id_stream, file=ave_fname, status='replace',                 &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      write(id_stream) comment_1
      write(id_stream) comment_2
      call write_sph_pwr_vol_head(.FALSE., id_stream,                   &
     &                            sph_lbl_IN1, sph_IN1, zbuf_m)
!
      call sel_gz_write_volume_spectr_mtr(.FALSE., id_stream,           &
     &    sph_IN1%i_step, sph_IN1%time, vs_srs1%ltr_srs,                &
     &    vs_srs1%ntot_comp, sdev_mean(1,0), zbuf_m)
      close(id_stream)
!
      deallocate(ave_mean, rms_mean, sdev_mean, iflag_all)
      call dealloc_sph_volume_spec_series(vs_srs1)
      call dealloc_sph_espec_name(sph_IN1)
      call dealloc_sph_espec_data(sph_IN1)
!
      end subroutine time_ave_sdev_sph_volume_spec
!
!   --------------------------------------------------------------------
!
      end module time_ave_sph_monitor_data
