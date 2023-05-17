!tave_picked_sph_spec_data.f90
!      program tave_picked_sph_spec_data
!
!        programmed by H.Matsui on Dec., 2012
!
!! -----------------------------------------------------------------
!!    Input control file:  control_sph_time_average
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    array picked_sph_prefix
!!      picked_sph_prefix        'monitor/picked_mode'
!!      picked_sph_prefix        'monitor/picked_mode_l2_m0c'
!!    end array picked_sph_prefix
!!  end time_averaging_sph_monitor
!! -----------------------------------------------------------------
!
      program tave_picked_sph_spec_data
!
      use m_precision
      use m_constants
!
      use t_ctl_data_tave_sph_monitor
      use t_ctl_param_sph_series_util
      use time_ave_picked_sph_spectr
!
      implicit  none
!
!>      Structure for control data
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(sph_spectr_file_param), save :: spec_evo_p1
!
      integer(kind = kint) :: i
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
      call set_spec_series_file_and_time(tave_sph_ctl1, spec_evo_p1)
!
      do i = 1, spec_evo_p1%pick_spec_series%num_file
        call s_time_ave_picked_sph_spectr                               &
     &     (.TRUE., spec_evo_p1%pick_spec_series%evo_file_name(i),      &
     &      spec_evo_p1%start_time, spec_evo_p1%end_time)
      end do
!
      write(*,*) '***** program finished *****'
      stop
!
      end program tave_picked_sph_spec_data
