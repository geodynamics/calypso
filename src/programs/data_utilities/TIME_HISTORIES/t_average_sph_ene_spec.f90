!>@file   t_average_sph_ene_spec.f90
!!        program t_average_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Evaluate time average and standard deviation 
!!        from spherical harmonics spectrum data
!!
!!@verbatim
!! -----------------------------------------------------------------
!!
!!      control file: control_sph_time_average
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    old_format_flag     'Off'
!!    degree_range_ctl     1   12
!!
!!    begin monitor_data_list_ctl
!!      array volume_integrate_prefix
!!        volume_integrate_prefix     'sph_ave_volume'
!!        volume_integrate_prefix     'sph_pwr_volume_s'
!!        volume_integrate_prefix     'sph_pwr_volume_m0'
!!      end array volume_integrate_prefix
!!
!!      array volume_sph_spectr_prefix
!!        volume_sph_spectr_prefix     'sph_pwr_volume_l'
!!        volume_sph_spectr_prefix     'sph_pwr_volume_m'
!!        volume_sph_spectr_prefix     'sph_pwr_volume_lm'
!!      end array volume_sph_spectr_prefix
!!
!!      array sphere_integrate_prefix
!!        sphere_integrate_prefix     'sph_pwr_layer_s'
!!        sphere_integrate_prefix     'sph_pwr_layer_m0'
!!      end array sphere_integrate_prefix
!!
!!      array layer_sph_spectr_prefix
!!        layer_sph_spectr_prefix     'sph_pwr_layer_l'
!!        layer_sph_spectr_prefix     'sph_pwr_layer_m'
!!        layer_sph_spectr_prefix     'sph_pwr_layer_lm'
!!      end array layer_sph_spectr_prefix
!!
!!      array picked_sph_prefix
!!        picked_sph_prefix        'monitor/picked_mode'
!!        picked_sph_prefix        'monitor/picked_mode_l2_m0c'
!!      end array picked_sph_prefix
!!    end monitor_data_list_ctl
!!  end time_averaging_sph_monitor
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      program t_average_sph_ene_spec
!
      use m_precision
      use m_constants
!
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
      use t_sph_volume_mean_series
      use t_tave_sph_volume_spectr
      use t_tave_sph_layer_mean
      use t_tave_sph_layer_spectr
      use tave_sdev_sph_layer_spec
      use tave_sdev_sph_layer_mean
      use tave_sdev_sph_volume_spec
      use time_ave_picked_sph_spectr
      use set_parallel_file_name
      use count_monitor_time_series
      use time_ave_sph_monitor_data
      use time_ave_sph_volume_mean
!
      implicit none
!
!>        Control file name
      character(len = kchara), parameter                                &
     &           :: fname_ctl_tave_sph_mtr = 'control_sph_time_average'
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
!
      integer :: i, j
!
      call read_control_file_sph_monitor(0, fname_ctl_tave_sph_mtr,     &
     &                                   tave_sph_ctl1)
      call set_spec_series_file_and_time(tave_sph_ctl1, spec_evo_p1)
      call dealloc_ctl_tave_sph_monitor(tave_sph_ctl1)
!
      do i = 1, spec_evo_p1%vol_series%num_file
        call time_ave_sdev_sph_volume_mean                              &
     &     (spec_evo_p1%vol_series%evo_file_name(i),                    &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
      end do
!
      do i = 1, spec_evo_p1%vol_spec_series%num_file
        call time_ave_sdev_sph_vol_spec                                 &
     &     (spec_evo_p1%vol_spec_series%evo_file_name(i),               &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
      end do
!
!
      do i = 1, spec_evo_p1%layer_series%num_file
        call time_ave_sdev_sph_layer_mean                               &
     &     (spec_evo_p1%layer_series%evo_file_name(i),                  &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
      end do
!
      do i = 1, spec_evo_p1%layer_spec_series%num_file
        call time_ave_sdev_sph_layer_spec                               &
     &     (spec_evo_p1%layer_spec_series%evo_file_name(i),             &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
      end do
!
      do i = 1, spec_evo_p1%pick_spec_series%num_file
        call s_time_ave_picked_sph_spectr                               &
     &     (.TRUE., spec_evo_p1%pick_spec_series%evo_file_name(i),      &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
      end do
!
      call dealloc_spec_series_file_param(spec_evo_p1)
      stop
!
      end program t_average_sph_ene_spec
