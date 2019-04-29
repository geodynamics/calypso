!>@file   set_control_4_time_steps.f90
!!@brief  module set_control_4_time_steps
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Okuda in 2000
!!@n    modified by H. Matsui in 2001
!!@n    modified by H. Matsui in Sep., 2006
!
!> @brief set parameters for time stepping
!!
!!@verbatim
!!      subroutine s_set_control_4_time_steps(mr_ctl, tctl, MHD_step)
!!        type(mhd_restart_control), intent(in) :: mr_ctl
!!        type(time_data_control), intent(in) :: tctl
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!@endverbatim
!
      module set_control_4_time_steps
!
      use m_precision
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_MHD_step_parameter
      use t_time_data
      use t_step_parameter
      use t_ctl_data_4_time_steps
      use t_VIZ_step_parameter
      use t_MHD_step_parameter
      use t_flex_delta_t_data
!
      implicit  none
!
      private :: set_control_flex_time_steps
      private :: set_flex_time_step_params
      private :: set_flex_time_step_controls
      private :: set_fixed_time_step_controls
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_time_steps(mr_ctl, tctl, MHD_step)
!
      use t_time_data
      use t_ctl_data_mhd_evo_scheme
      use m_initial_field_control
      use cal_num_digits
      use skip_comment_f
!
      type(mhd_restart_control), intent(in) :: mr_ctl
      type(time_data_control), intent(in) :: tctl
      type(MHD_step_param), intent(inout) :: MHD_step
!
!
!  control for restert
!
      call set_initial_field_id                                         &
     &   (mr_ctl%restart_flag_ctl, tctl, MHD_step%init_d%time)
!
        MHD_step%flex_p%iflag_flexible_step = iflag_fixed_step
        if(tctl%flexible_step_ctl%iflag .gt. 0                          &
     &     .and. yes_flag(tctl%flexible_step_ctl%charavalue)) then
          MHD_step%flex_p%iflag_flexible_step = iflag_flex_step
        end if
!
      if (tctl%dt_ctl%iflag .eq. 0) then
        e_message = 'Set delta t'
        call calypso_MPI_abort(ierr_evo, e_message)
      else
        MHD_step%init_d%dt = tctl%dt_ctl%realvalue
      end if
!
      call set_control_flex_time_steps                                  &
     &   (tctl, MHD_step%init_d, MHD_step%flex_p)
!
!   parameters for time evolution
!
      if(MHD_step%flex_p%iflag_flexible_step .eq. iflag_flex_step) then
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &    write(*,*) 'set_flex_time_step_controls'
        call set_flex_time_step_controls(tctl, MHD_step)
      else
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &    write(*,*) 'set_fixed_time_step_controls'
        call set_fixed_time_step_controls(tctl, MHD_step)
      end if
!
      if (MHD_step%finish_d%i_end_step .eq. -1) then
        if (tctl%elapsed_time_ctl%iflag .eq. 0) then
          e_message                                                     &
     &      = 'Set elapsed time to finish (second)'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          MHD_step%finish_d%elapsed_time                                &
     &      = tctl%elapsed_time_ctl%realvalue
        end if
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'dt', MHD_step%init_d%dt,                            &
     &            MHD_step%flex_p%dt_fact, MHD_step%flex_p%idt_digit
        write(*,*) 'i_step_init ', MHD_step%init_d%i_time_step
        write(*,*) 'i_step_number ', MHD_step%finish_d%i_end_step
        write(*,*) 'elapsed_time ',  MHD_step%finish_d%elapsed_time
        write(*,*) 'i_step_check ',  MHD_step%rms_step%increment
        write(*,*) 'i_step_output_rst ', MHD_step%rst_step%increment
        write(*,*) 'i_step_output_ucd ', MHD_step%ucd_step%increment
      end if
!
      end subroutine s_set_control_4_time_steps
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_flex_time_steps(tctl, init_d, flex_p)
!
      use t_time_data
      use t_ctl_data_mhd_evo_scheme
      use m_initial_field_control
      use cal_num_digits
      use skip_comment_f
!
      type(time_data_control), intent(in) :: tctl
      type(time_data), intent(in) :: init_d
      type(flexible_stepping_parameter), intent(inout) :: flex_p
!
!
      call cal_num_digit_real                                           &
     &     (init_d%dt, flex_p%dt_fact, flex_p%idt_digit)
!
      if(flex_p%iflag_flexible_step .eq. iflag_flex_step) then
        if (tctl%min_delta_t_ctl%iflag .eq. 0) then
          e_message = 'Set maximum delta t'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          flex_p%dt_min = tctl%min_delta_t_ctl%realvalue
        end if
!
        if (tctl%max_delta_t_ctl%iflag .eq. 0) then
          e_message = 'Set maximum delta t'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          flex_p%dt_max = tctl%max_delta_t_ctl%realvalue
        end if
!
        if (tctl%max_eps_to_shrink_ctl%iflag .eq. 0) then
          e_message = 'Set maximum error to shrink delta t'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          flex_p%max_eps_to_shrink                                      &
     &              = tctl%max_eps_to_shrink_ctl%realvalue
        end if
!
        if (tctl%min_eps_to_expand_ctl%iflag .eq. 0) then
          e_message = 'Set minimum error to expand delta t'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          flex_p%min_eps_to_expand                                      &
     &              = tctl%min_eps_to_expand_ctl%realvalue
        end if
!
        flex_p%istep_flex_to_max = izero
!
        if(init_d%dt .gt. zero) then
          flex_p%interval_flex_2_max                                    &
     &              = nint(flex_p%dt_max / init_d%dt)
        end if
      else
        flex_p%dt_max = init_d%dt
        flex_p%dt_min = init_d%dt
        flex_p%interval_flex_2_max = ione
        flex_p%istep_flex_to_max = izero
      end if
!
      end subroutine set_control_flex_time_steps
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_time_step_controls(tctl, MHD_step)
!
      type(time_data_control), intent(in) :: tctl
      type(MHD_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: ierr
!
!
      call s_set_fixed_time_step_params                                 &
     &   (tctl, MHD_step%init_d, MHD_step%finish_d,                     &
     &    MHD_step%rst_step, MHD_step%ucd_step, ierr, e_message)
      call viz_fixed_time_step_params                                   &
     &   (MHD_step%init_d%dt, tctl, MHD_step%viz_step)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
      call output_step_4_fixed_step_ctl(ione, MHD_step%init_d%dt,       &
     &    tctl%i_step_check_ctl, tctl%delta_t_check_ctl,                &
     &    MHD_step%rms_step)
!
      call output_step_4_fixed_step_ctl(izero, MHD_step%init_d%dt,      &
     &    tctl%i_step_sgs_coefs_ctl, tctl%delta_t_sgs_coefs_ctl,        &
     &    MHD_step%sgs_IO_step)
!
      call output_step_4_fixed_step_ctl(izero, MHD_step%init_d%dt,      &
     &    tctl%i_step_monitor_ctl, tctl%delta_t_monitor_ctl,            &
     &    MHD_step%point_step)
!
      call output_step_4_fixed_step_ctl(izero, MHD_step%init_d%dt,      &
     &    tctl%i_step_boundary_ctl, tctl%delta_t_boundary_ctl,          &
     &    MHD_step%boundary_step)
!
      end subroutine set_fixed_time_step_controls
!
! -----------------------------------------------------------------------
!
      subroutine set_flex_time_step_controls(tctl, MHD_step)
!
      type(time_data_control), intent(in) :: tctl
      type(MHD_step_param), intent(inout) :: MHD_step
!
!
      call set_flex_time_step_params                                    &
     &   (tctl, MHD_step%flex_p, MHD_step%init_d, MHD_step%finish_d,    &
     &    MHD_step%rst_step, MHD_step%ucd_step)
!
      call output_step_4_flex_step_ctl(ione, MHD_step%flex_p%dt_max,    &
     &    tctl%i_step_check_ctl, tctl%delta_t_check_ctl,                &
     &    MHD_step%rms_step)
!
      call output_step_4_flex_step_ctl(izero, MHD_step%flex_p%dt_max,   &
     &    tctl%i_step_sgs_coefs_ctl, tctl%delta_t_sgs_coefs_ctl,        &
     &    MHD_step%sgs_IO_step)
!
      call output_step_4_flex_step_ctl(izero, MHD_step%flex_p%dt_max,   &
     &    tctl%i_step_monitor_ctl, tctl%delta_t_monitor_ctl,            &
     &    MHD_step%point_step)
!
      call output_step_4_flex_step_ctl(izero, MHD_step%flex_p%dt_max,   &
     &    tctl%i_step_boundary_ctl, tctl%delta_t_boundary_ctl,          &
     &    MHD_step%boundary_step)
!
      call viz_flex_time_step_controls                                  &
     &   (tctl, MHD_step%init_d%dt, MHD_step%viz_step)
!
      end subroutine set_flex_time_step_controls
!
! -----------------------------------------------------------------------
!
      subroutine set_flex_time_step_params(tctl, flex_p,                &
     &          init_d, finish_d, rst_step, ucd_step)
!
      type(time_data_control), intent(in) :: tctl
      type(time_data), intent(inout) :: init_d
      type(finish_data), intent(inout) :: finish_d
!
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(IO_step_param), intent(inout) :: rst_step, ucd_step
!
!>      Start step for restarting file
      integer(kind=kint) :: istep_rst_start, istep_rst_end
!
!
      istep_rst_start   = 0
      if (tctl%start_rst_step_ctl%iflag .gt. 0) then
        istep_rst_start = tctl%start_rst_step_ctl%intvalue
      end if
!
      if (tctl%end_rst_step_ctl%iflag .eq. 0) then
        e_message = 'Set time to finish'
          call calypso_MPI_abort(ierr_evo, e_message)
      else
        istep_rst_end = tctl%end_rst_step_ctl%intvalue
      end if
!
!
      call output_step_4_flex_step_ctl(ione, flex_p%dt_max,             &
     &    tctl%i_step_rst_ctl, tctl%delta_t_rst_ctl, rst_step)
!
      call output_step_4_flex_step_ctl(ione, flex_p%dt_max,             &
     &   tctl%i_step_ucd_ctl, tctl%delta_t_field_ctl, ucd_step)
!
      init_d%i_time_step =  istep_rst_start * rst_step%increment
      finish_d%i_end_step = istep_rst_end *   rst_step%increment
      flex_p%time_to_finish = istep_rst_end * rst_step%delta_t
!
      if (istep_rst_end .eq. -1) then
        if (tctl%elapsed_time_ctl%iflag .eq. 0) then
          e_message                                                     &
     &      = 'Set elapsed time to finish (second)'
          call calypso_MPI_abort(ierr_evo, e_message)
        else
          finish_d%elapsed_time  = tctl%elapsed_time_ctl%realvalue
        end if
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'istep_rst_start ', istep_rst_start
        write(*,*) 'istep_rst_end ',  istep_rst_end
        write(*,*) 'time_to_finish ',  flex_p%time_to_finish
      end if
!
      end subroutine set_flex_time_step_params
!
! -----------------------------------------------------------------------
!
      end module set_control_4_time_steps
