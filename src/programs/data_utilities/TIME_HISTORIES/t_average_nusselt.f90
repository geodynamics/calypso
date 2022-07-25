!t_average_nusselt.f90
!      program t_average_nusselt
!
!        programmed by H.Matsui on Apr., 2014
!
!
!! -----------------------------------------------------------------
!!    Input control file:  control_sph_time_average
!!
!!  begin time_averaging_sph_monitor
!!    start_time_ctl     1.0
!!    end_time_ctl       2.0
!!
!!    nusselt_number_prefix        'Nusselt'
!!  end time_averaging_sph_monitor
!! -----------------------------------------------------------------
!

      program t_average_nusselt
!
      use m_precision
!
      use t_no_heat_Nusselt
      use t_ctl_data_tave_sph_monitor
      use time_average_nusselt
!
      implicit  none
!
      type(tave_sph_monitor_ctl), save :: tave_sph_ctl1
      type(nusselt_number_data), save :: Nu_t1
!
      real(kind = kreal) :: start_time, end_time
!
!
      call read_control_file_sph_monitor(0, tave_sph_ctl1)
      call set_control_tave_Nu                                          &
     &   (tave_sph_ctl1, Nu_t1, start_time, end_time)
      call s_time_average_nusselt                                       &
     &   (Nu_t1%Nusselt_file_name, start_time, end_time)
!
      write(*,*) '***** program finished *****'
      stop
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_control_tave_Nu                                    &
     &        (tave_sph_ctl, Nu_t, start_time, end_time)
!
      use t_no_heat_Nusselt
      use t_ctl_data_tave_sph_monitor
      use set_parallel_file_name
!
      implicit  none
!
      type(tave_sph_monitor_ctl), intent(in) :: tave_sph_ctl
      type(nusselt_number_data), intent(inout) :: Nu_t
      real(kind = kreal), intent(inout) :: start_time, end_time
!
      character(len=kchara) :: file_prefix
!
      if(tave_sph_ctl%Nusselt_file_prefix%iflag .eq. 0) then
        write(*,*) 'Set File prefix for Nusselt number'
        stop
      end if
      file_prefix = tave_sph_ctl%Nusselt_file_prefix%charavalue
      Nu_t%Nusselt_file_name = add_dat_extension(file_prefix)
!
      if(tave_sph_ctl%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set start time'
        stop
      end if
      start_time = tave_sph_ctl%start_time_ctl%realvalue
!
      if(tave_sph_ctl%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set end time'
        stop
      end if
      end_time = tave_sph_ctl%end_time_ctl%realvalue
!
      end subroutine set_control_tave_Nu
!
! -------------------------------------------------------------------
!
      end program t_average_nusselt
