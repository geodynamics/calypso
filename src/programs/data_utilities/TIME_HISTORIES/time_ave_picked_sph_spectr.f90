!>@file   time_ave_picked_sph_spectr.f90
!!@brief  module time_ave_picked_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Top subroutines for time averaging of spectrum data
!!
!!@verbatim
!!      integer(c_int) function                                         &
!!      &     time_ave_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
!!        character(1,C_char), intent(in) :: cname(*)
!!        real(C_double), Value :: cstart, cend
!!      subroutine get_each_tave_picked_spectr_f                        &
!!     &         (yname, kr_value, l_value, m_value, r, ave, rms, sdev) &
!!     &          bind(c, name="get_each_tave_picked_spectr_f")
!!        character(1,C_char), intent(in) :: yname(*)
!!        integer(C_int), Value :: kr_value, l_value, m_value
!!        real(c_double), intent(inout) :: ave(1), rms(1), sdev(1)
!!
!!      subroutine s_time_ave_picked_sph_spectr                         &
!!     &         (flag_log, file_name, start_time, end_time, p_ave)
!!        logical, intent(in) :: flag_log
!!        character(len=kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(in) :: start_time, end_time
!!        type(time_ave_picked_sph), intent(inout) :: p_ave
!!@endverbatim
!
      module time_ave_picked_sph_spectr
!
      use ISO_C_BINDING
!
      use m_precision
      use m_constants
!
      use t_read_sph_spectra
      use t_picked_sph_spectr_data_IO
!
      implicit  none
!
      type time_ave_picked_sph
        real(kind = kreal), allocatable :: ave_spec(:)
        real(kind = kreal), allocatable :: rms_spec(:)
        real(kind = kreal), allocatable :: sdev_spec(:)
      end type time_ave_picked_sph
!
      type(picked_spectrum_data_IO), save, private :: pick_IO_a
      type(time_ave_picked_sph), save, private :: p_ave_a
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      integer(c_int) function                                           &
    &     time_ave_picked_sph_spectr_f(cname, cstart, cend) Bind(C)
!
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: cname(*)
      real(C_double), Value :: cstart, cend
!
      real(kind = kreal) :: start_time, end_time
      character(len=kchara) :: file_name
!
      write(file_name,'(a)') trim(c_to_fstring(cname))
      start_time = cstart
      end_time = cend
      call s_time_ave_picked_sph_spectr                                 &
     &   (.FALSE., file_name, start_time, end_time)
!
      time_ave_picked_sph_spectr_f = 0
      end function time_ave_picked_sph_spectr_f
!
! -------------------------------------------------------------------
!
      subroutine get_each_tave_picked_spectr_f                          &
     &         (yname, kr_value, l_value, m_value, r, ave, rms, sdev)   &
     &          bind(c, name="get_each_tave_picked_spectr_f")
!
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: yname(*)
      integer(C_int), Value :: kr_value, l_value, m_value
!
      real(c_double), intent(inout) :: r(1), ave(1), rms(1), sdev(1)
!
      integer(kind = kint) :: id_radius, in_degree, in_order
      integer(kind = kint) :: idx, id_comp, id_mode
      character(len=kchara) :: draw_name
!
      draw_name = c_to_fstring(yname)
      id_comp = get_each_picked_fld_address(draw_name, pick_IO_a)
!
      id_radius = kr_value
      in_degree = l_value
      in_order =  m_value
      id_mode = get_each_picked_sph_address                             &
     &        (id_radius, in_degree, in_order, pick_IO_a)
!
      idx = id_comp + (id_mode-1) * pick_IO_a%ntot_comp
      r(1) =    pick_IO_a%radius(id_mode)
      ave(1) =  p_ave_a%ave_spec(idx)
      rms(1) =  p_ave_a%rms_spec(idx)
      sdev(1) = p_ave_a%sdev_spec(idx)
!
      end subroutine get_each_tave_picked_spectr_f
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine s_time_ave_picked_sph_spectr                           &
     &         (flag_log, file_name, start_time, end_time)
!
      use picked_sph_spectr_data_IO
      use count_monitor_time_series
      use set_parallel_file_name
      use write_snap_pick_sph_spectr
!
      logical, intent(in) :: flag_log
      character(len=kchara), intent(in) :: file_name
      real(kind = kreal), intent(in) :: start_time, end_time
!
      character(len=kchara) :: directory, fname_no_dir, fname_tmp
      character(len=kchara) :: tave_fname
      character(len=kchara) :: trms_fname
      character(len=kchara) :: sdev_fname
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint), allocatable :: imask(:)
      real(kind = kreal) :: true_start, true_end
!
!
      call split_directory(file_name, directory, fname_no_dir)
!
      write(fname_tmp,'(a6,a)') 't_ave_', trim(fname_no_dir)
      tave_fname = append_directory(directory, fname_tmp)
      write(fname_tmp,'(a6,a)') 't_rms_', trim(fname_no_dir)
      trms_fname = append_directory(directory, fname_tmp)
      write(fname_tmp,'(a8,a)') 't_sigma_', trim(fname_no_dir)
      sdev_fname = append_directory(directory, fname_tmp)
!
!      Load picked mode file
      if(flag_log) call check_picked_sph_spectr(file_name, pick_IO_a)
      call load_picked_sph_spectr_series                                &
     &   (flag_log, file_name, start_time, end_time,                    &
     &    true_start, true_end, pick_IO_a)
!
      allocate(imask(pick_IO_a%n_step))
      imask(1:pick_IO_a%n_step) = 1
!
      call alloc_picked_t_avetage(pick_IO_a%ntot_data, p_ave_a)
      call cal_time_ave_picked_sph_spectr(pick_IO_a%n_step,             &
     &  pick_IO_a%d_time, imask, pick_IO_a%ntot_data, pick_IO_a%d_pick, &
     &  p_ave_a%ave_spec, p_ave_a%rms_spec, p_ave_a%sdev_spec)
!
!$omp parallel workshare
      pick_IO_a%d_pk(1:pick_IO_a%ntot_data)                             &
     &      = p_ave_a%ave_spec(1:pick_IO_a%ntot_data)
!$omp end parallel workshare
      call write_tave_sph_spec_monitor                                  &
     &   (tave_fname, pick_IO_a%i_step(pick_IO_a%n_step),               &
     &    true_end, true_start, pick_IO_a)
!
!    output RMS deviation
!
!$omp parallel workshare
      pick_IO_a%d_pk(1:pick_IO_a%ntot_data)                             &
     &      = p_ave_a%rms_spec(1:pick_IO_a%ntot_data)
!$omp end parallel workshare
!
      call write_tave_sph_spec_monitor                                  &
     &   (trms_fname, pick_IO_a%i_step(pick_IO_a%n_step),               &
     &    true_end, true_start, pick_IO_a)
!
!    output standard deviation
!
!$omp parallel workshare
      pick_IO_a%d_pk(1:pick_IO_a%ntot_data)                             &
     &      = p_ave_a%sdev_spec(1:pick_IO_a%ntot_data)
!$omp end parallel workshare
!
      call write_tave_sph_spec_monitor                                  &
     &   (sdev_fname, pick_IO_a%i_step(pick_IO_a%n_step),               &
     &    true_end, true_start, pick_IO_a)
!
      call dealloc_pick_sph_monitor_IO(pick_IO_a)
      call dealloc_pick_sph_series(pick_IO_a)
      call dealloc_picked_t_avetage(p_ave_a)
!
      end subroutine s_time_ave_picked_sph_spectr
!
! -------------------------------------------------------------------
!
      subroutine alloc_picked_t_avetage(ntot_data, p_ave)
!
      integer(kind = kint), intent(in) :: ntot_data
      type(time_ave_picked_sph), intent(inout) :: p_ave
!
!
      allocate(p_ave%ave_spec(ntot_data))
      allocate(p_ave%rms_spec(ntot_data))
      allocate(p_ave%sdev_spec(ntot_data))
!
      end subroutine alloc_picked_t_avetage
!
! -------------------------------------------------------------------
!
      subroutine dealloc_picked_t_avetage(p_ave)
!
      type(time_ave_picked_sph), intent(inout) :: p_ave
!
!
      deallocate(p_ave%ave_spec, p_ave%sdev_spec, p_ave%rms_spec)
!
      end subroutine dealloc_picked_t_avetage
!
! -------------------------------------------------------------------
!
      end module time_ave_picked_sph_spectr
