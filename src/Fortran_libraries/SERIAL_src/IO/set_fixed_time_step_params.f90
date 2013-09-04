!set_fixed_time_step_params.f90
!      module set_fixed_time_step_params
!
!        programmed by H.Matsui on Sep., 2006
!
!      subroutine s_set_fixed_time_step_params
!
      module set_fixed_time_step_params
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_fixed_time_step_params(ierr, errmsg)
!
      use m_ctl_data_4_time_steps
      use m_t_step_parameter
      use m_t_int_parameter
!
      integer(kind = kint), intent(inout) :: ierr
      character(len=kchara), intent(inout) :: errmsg
!
!
      if (i_i_step_init.eq.0) then
        i_step_init   = 0
      else
        i_step_init   = i_step_init_ctl
      end if
!
      if (i_i_step_number.eq.0) then
        ierr = 90
        errmsg = 'Set step number to finish'
        return
      else
        i_step_number = i_step_number_ctl
      end if
!
      call set_monitor_param_4_fixed_step(ione, i_i_step_check,         &
     &    i_delta_t_check, i_step_check_ctl, delta_t_check_ctl,         &
     &    i_step_check, delta_t_step_check)
!
!
      call set_monitor_param_4_fixed_step(ione, i_i_step_rst,           &
     &    i_delta_t_rst, i_step_rst_ctl, delta_t_rst_ctl,               &
     &    i_step_output_rst, delta_t_output_rst)
!
      call set_monitor_param_4_fixed_step(ione, i_i_step_ucd,           &
     &    i_delta_t_ucd, i_step_ucd_ctl, delta_t_field_ctl,             &
     &    i_step_output_ucd, delta_t_output_ucd)
!
      if(i_step_output_rst .gt. 0) then
        istep_rst_start = int(i_step_init /   i_step_output_rst)
        istep_rst_end =   int(i_step_number / i_step_output_rst)
      else
        istep_rst_start = i_step_init 
        istep_rst_end =   i_step_number
      end if
!
      if(i_step_init .eq. -1)   istep_rst_start = -1
      if(i_step_number .eq. -1) istep_rst_end =   -1
!
      call set_monitor_param_4_fixed_step(izero, i_i_step_psf,          &
     &    i_delta_t_psf, i_step_psf_ctl, delta_t_psf_ctl,               &
     &    i_step_output_psf, delta_t_output_psf)
!
      call set_monitor_param_4_fixed_step(izero, i_i_step_iso,          &
     &    i_delta_t_iso, i_step_iso_ctl, delta_t_iso_ctl,               &
     &    i_step_output_iso, delta_t_output_iso)
!
      call set_monitor_param_4_fixed_step(izero, i_i_step_pvr,          &
     &    i_delta_t_pvr, i_step_pvr_ctl, delta_t_pvr_ctl,               &
     &    i_step_output_pvr, delta_t_output_pvr)
!
      call set_monitor_param_4_fixed_step(izero, i_i_step_fline,        &
     &    i_delta_t_fline, i_step_fline_ctl, delta_t_fline_ctl,         &
     &    i_step_output_fline, delta_t_output_fline)
!
!
      if (i_step_number.eq.-1) then
        if (i_elapsed_time.eq.0) then
          ierr = 90
          errmsg = 'Set elapsed time to finish (second)'
          return
        else
          elapsed_time  = elapsed_time_ctl
        end if
      end if
!
      ierr = 0
!
      end subroutine s_set_fixed_time_step_params
!
! -----------------------------------------------------------------------
!
      subroutine set_monitor_param_4_fixed_step(istep_def,              &
     &          ictl_step, ictl_dt, istep_in, dt_in, istep_out, dt_out)
!
      use m_t_int_parameter
!
      integer(kind = kint), intent(in) :: istep_def
      integer(kind = kint), intent(in) :: ictl_step, ictl_dt
      integer(kind = kint), intent(in) :: istep_in
      real(kind = kreal), intent(in) :: dt_in
!
      integer(kind = kint), intent(inout) :: istep_out
      real(kind = kreal), intent(inout) :: dt_out
!
!
      if ( (ictl_step+ictl_dt) .eq. 0) then
        istep_out =   istep_def
        dt_out = dble(istep_def) * dt
      else if(ictl_step .eq. 0) then
        dt_out =    dt_in
        istep_out = nint(dt_in / dt)
      else
        istep_out =   istep_in
        dt_out = dble(istep_in) * dt
      end if
!
      end subroutine set_monitor_param_4_fixed_step
!
! -----------------------------------------------------------------------
!
      end module set_fixed_time_step_params
