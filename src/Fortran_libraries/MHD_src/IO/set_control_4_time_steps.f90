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
!!      subroutine s_set_control_4_time_steps
!!@endverbatim
!
      module set_control_4_time_steps
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
      use m_control_parameter
      use m_ctl_data_4_time_steps
      use m_t_step_parameter
      use m_t_int_parameter
!
      implicit  none
!
      private :: set_control_4_initial
      private :: set_monitor_param_4_flex_step
      private :: set_fixed_time_step_controls
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_time_steps
!
      use m_ctl_data_mhd_evo_scheme
      use cal_num_digits
!
!
!  control for restert
!
      call set_control_4_initial
!
        iflag_flexible_step = iflag_fixed_step
        if(i_flexible_step .gt. 0) then
          if    (flexible_step_ctl .eq. 'on'                            &
     &      .or. flexible_step_ctl .eq. 'On'                            &
     &      .or. flexible_step_ctl .eq. 'ON') then
            iflag_flexible_step = iflag_flex_step
          end if
        end if
!
        if (i_dt.eq.0) then
          e_message = 'Set delta t'
          call parallel_abort(90, e_message)
        else
          dt = dt_ctl
          ddt = 1.0d0 / dt
          call cal_num_digit_real(dt, dt_fact, idt_digit)
        end if
!
        if(iflag_flexible_step .eq. iflag_flex_step) then
          if (i_min_delta_t.eq.0) then
            e_message = 'Set maximum delta t'
            call parallel_abort(90, e_message)
          else
            dt_min = min_delta_t_ctl
          end if
!
          if (i_max_delta_t.eq.0) then
            e_message = 'Set maximum delta t'
            call parallel_abort(90, e_message)
          else
            dt_max = max_delta_t_ctl
          end if
!
          if (i_max_eps_to_shrink.eq.0) then
            e_message = 'Set maximum error to shrink delta t'
            call parallel_abort(90, e_message)
          else
            max_eps_to_shrink_dt = max_eps_to_shrink_ctl
          end if
!
          if (i_min_eps_to_expand.eq.0) then
            e_message = 'Set minimum error to expand delta t'
            call parallel_abort(90, e_message)
          else
            min_eps_to_expand_dt = min_eps_to_expand_ctl
          end if
!
          istep_flex_to_max = izero
!
          if(dt .gt. zero) i_interval_flex_2_max = nint(dt_max / dt)
        else
          dt_max = dt
          dt_min = dt
          i_interval_flex_2_max = ione
          istep_flex_to_max = izero
        end if
!
!   parameters for time evolution
!
      if(iflag_flexible_step .eq. iflag_flex_step) then
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &    write(*,*) 'set_flex_time_step_controls'
        call set_flex_time_step_controls
      else
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &    write(*,*) 'set_fixed_time_step_controls'
        call set_fixed_time_step_controls
      end if
!
      if (i_step_number.eq.-1) then
        if (i_elapsed_time.eq.0) then
          e_message                                                     &
     &      = 'Set elapsed time to finish (second)'
          call parallel_abort(90, e_message)
        else
          elapsed_time  = elapsed_time_ctl
        end if
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'dt', dt, dt_fact, idt_digit
        write(*,*) 'i_step_init ',i_step_init
        write(*,*) 'i_step_number ',i_step_number
        write(*,*) 'istep_rst_start ', istep_rst_start
        write(*,*) 'istep_rst_end ',  istep_rst_end
        write(*,*) 'elapsed_time ',elapsed_time
        write(*,*) 'i_step_check ',i_step_check
        write(*,*) 'i_step_output_rst ',i_step_output_rst
        write(*,*) 'i_step_output_ucd ',i_step_output_ucd
      end if
!
      end subroutine s_set_control_4_time_steps
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_time_step_controls
!
      use set_fixed_time_step_params
!
      integer(kind = kint) :: ierr
!
      call s_set_fixed_time_step_params(ierr, e_message)
      if(ierr .gt. 0) call parallel_abort(ierr, e_message)
!
      i_step_sgs_coefs = 1
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        call set_monitor_param_4_fixed_step(ione, i_i_step_sgs_coefs,   &
     &    i_delta_t_sgs_coefs, i_step_sgs_coefs_ctl,                    &
     &    delta_t_sgs_coefs_ctl, i_step_sgs_output, delta_t_sgs_output)
      end if
!
      call set_monitor_param_4_fixed_step(izero, i_i_step_monitor,      &
     &    i_delta_t_monitor, i_step_monitor_ctl, delta_t_monitor_ctl,   &
     &    i_step_output_monitor, delta_t_output_monitor)
!
      call set_monitor_param_4_fixed_step(izero,                        &
     &    i_i_step_boundary, i_delta_t_boundary,                        &
     &    i_step_boundary_ctl, delta_t_boundary_ctl,                    &
     &    i_step_output_boundary, delta_t_output_boundary)
!
      end subroutine set_fixed_time_step_controls
!
! -----------------------------------------------------------------------
!
      subroutine set_flex_time_step_controls
!
!
      if (i_start_rst_step .eq. 0) then
        istep_rst_start   = 0
      else
        istep_rst_start   = start_rst_step_ctl
      end if
!
      if (i_end_rst_step .eq. 0) then
        e_message = 'Set time to finish'
          call parallel_abort(90, e_message)
      else
        istep_rst_end = end_rst_step_ctl
      end if
!
      call set_monitor_param_4_flex_step(ione, i_i_step_check,          &
     &    i_delta_t_check, i_step_check_ctl, delta_t_check_ctl,         &
     &    i_step_check, delta_t_step_check)
!
!
      call set_monitor_param_4_flex_step(ione, i_i_step_rst,            &
     &    i_delta_t_rst, i_step_rst_ctl, delta_t_rst_ctl,               &
     &    i_step_output_rst, delta_t_output_rst)
!
      call set_monitor_param_4_flex_step(ione, i_i_step_ucd,            &
     &    i_delta_t_ucd, i_step_ucd_ctl, delta_t_field_ctl,             &
     &    i_step_output_ucd, delta_t_output_ucd)
!
      i_step_init =   istep_rst_start * i_step_output_rst
      i_step_number = istep_rst_end *   i_step_output_rst
!
      call set_monitor_param_4_flex_step(izero, i_i_step_psf,           &
     &    i_delta_t_psf, i_step_psf_ctl, delta_t_psf_ctl,               &
     &    i_step_output_psf, delta_t_output_psf)
!
      call set_monitor_param_4_flex_step(izero, i_i_step_iso,           &
     &    i_delta_t_iso, i_step_iso_ctl, delta_t_iso_ctl,               &
     &    i_step_output_iso, delta_t_output_iso)
!
      call set_monitor_param_4_flex_step(izero, i_i_step_pvr,           &
     &    i_delta_t_pvr, i_step_pvr_ctl, delta_t_pvr_ctl,               &
     &    i_step_output_pvr, delta_t_output_pvr)
!
      call set_monitor_param_4_flex_step(izero, i_i_step_fline,         &
     &    i_delta_t_fline, i_step_fline_ctl, delta_t_fline_ctl,         &
     &    i_step_output_fline, delta_t_output_fline)
!
!
      i_step_sgs_coefs = 1
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        call set_monitor_param_4_flex_step(ione, i_i_step_sgs_coefs,    &
     &    i_delta_t_sgs_coefs, i_step_sgs_coefs_ctl,                    &
     &    delta_t_sgs_coefs_ctl, i_step_sgs_output, delta_t_sgs_output)
      end if
!
      call set_monitor_param_4_flex_step(izero, i_i_step_monitor,       &
     &    i_delta_t_monitor, i_step_monitor_ctl, delta_t_monitor_ctl,   &
     &    i_step_output_monitor, delta_t_output_monitor)
!
      call set_monitor_param_4_flex_step(izero,                         &
     &    i_i_step_boundary, i_delta_t_boundary,                        &
     &    i_step_boundary_ctl, delta_t_boundary_ctl,                    &
     &    i_step_output_boundary, delta_t_output_boundary)
!
      if (istep_rst_end .eq. -1) then
        if (i_elapsed_time.eq.0) then
          e_message                                                     &
     &      = 'Set elapsed time to finish (second)'
          call parallel_abort(90, e_message)
        else
          elapsed_time  = elapsed_time_ctl
        end if
      end if
!
      end subroutine set_flex_time_step_controls
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_initial
!
      use m_ctl_data_mhd_evo_scheme
      use m_initial_field_control
!
!  control for restert
!
        if (i_rst_flag.eq.0.0d0) then
          e_message  = 'Set initial condition'
          call parallel_abort(90, e_message)
        else
          if(     restart_flag_ctl .eq. '0'                             &
     &       .or. restart_flag_ctl .eq. 'no_data'                       &
     &       .or. restart_flag_ctl .eq. 'No_data'                       &
     &       .or. restart_flag_ctl .eq. 'NO_DATA') then
            iflag_restart = i_rst_no_file
          else if(restart_flag_ctl .eq. '1'                             &
     &       .or. restart_flag_ctl .eq. 'start_from_rst_file'           &
     &       .or. restart_flag_ctl .eq. 'Start_from_rst_file'           &
     &       .or. restart_flag_ctl .eq. 'START_FROM_RST_FILE') then
            iflag_restart = i_rst_by_file
          else if(restart_flag_ctl .eq. '-1'                            &
     &       .or. restart_flag_ctl .eq. 'dynamo_benchmark_0'            &
     &       .or. restart_flag_ctl .eq. 'Dynamo_benchmark_0'            &
     &       .or. restart_flag_ctl .eq. 'DYNAMO_BENCHMARK_0') then
            iflag_restart = i_rst_dbench0
          else if(restart_flag_ctl .eq. '-2'                            &
     &       .or. restart_flag_ctl .eq. 'dynamo_benchmark_1'            &
     &       .or. restart_flag_ctl .eq. 'Dynamo_benchmark_1'            &
     &       .or. restart_flag_ctl .eq. 'DYNAMO_BENCHMARK_1') then
            iflag_restart = i_rst_dbench1
          else if(restart_flag_ctl .eq. '-2'                            &
     &       .or. restart_flag_ctl .eq. 'dynamo_benchmark_2'            &
     &       .or. restart_flag_ctl .eq. 'Dynamo_benchmark_2'            &
     &       .or. restart_flag_ctl .eq. 'DYNAMO_BENCHMARK_2') then
            iflag_restart = i_rst_dbench2
          else if(restart_flag_ctl .eq. '-3'                            &
     &       .or. restart_flag_ctl .eq. 'pseudo_vacuum_benchmark'       &
     &       .or. restart_flag_ctl .eq. 'Pseudo_vacuum_benchmark'       &
     &       .or. restart_flag_ctl .eq. 'PSEUDO_VACUUM_BENCHMARK') then
            iflag_restart = i_rst_dbench_qcv
          else if(restart_flag_ctl .eq. '-11'                           &
     &       .or. restart_flag_ctl .eq. 'rotate_x'                      &
     &       .or. restart_flag_ctl .eq. 'Rotate_x'                      &
     &       .or. restart_flag_ctl .eq. 'ROTATE_X') then
            iflag_restart = i_rst_rotate_x
          else if(restart_flag_ctl .eq. '-12'                           &
     &       .or. restart_flag_ctl .eq. 'rotate_y'                      &
     &       .or. restart_flag_ctl .eq. 'Rotate_y'                      &
     &       .or. restart_flag_ctl .eq. 'ROTATE_Y') then
            iflag_restart = i_rst_rotate_y
          else if(restart_flag_ctl .eq. '-13'                           &
     &       .or. restart_flag_ctl .eq. 'rotate_z'                      &
     &       .or. restart_flag_ctl .eq. 'Rotate_z'                      &
     &       .or. restart_flag_ctl .eq. 'ROTATE_Z') then
            iflag_restart = i_rst_rotate_z
          else if(restart_flag_ctl .eq. '20'                            &
     &       .or. restart_flag_ctl .eq. 'kinematic'                     &
     &       .or. restart_flag_ctl .eq. 'Kinematic'                     &
     &       .or. restart_flag_ctl .eq. 'KINEMATIC') then
            iflag_restart = i_rst_kinematic
          else if(restart_flag_ctl .eq. '-20'                           &
     &       .or. restart_flag_ctl .eq. 'linear_conveciton'             &
     &       .or. restart_flag_ctl .eq. 'Linear_conveciton'             &
     &       .or. restart_flag_ctl .eq. 'LINEAR_CONVECTION') then
            iflag_restart = i_rst_licv
          end if
        end if
!
        if (iflag_restart .eq. i_rst_no_file) then
          if (i_dt .eq. 0) then
            e_message  = 'Set initial time'
            call parallel_abort(90, e_message)
          else
            time_init = time_init_ctl
          end if
        end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_restart ',iflag_restart
        write(*,*) 'time_init ',time_init
      end if
!
      end subroutine set_control_4_initial
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_monitor_param_4_flex_step(istep_def,               &
     &          ictl_step, ictl_dt, istep_in, dt_in, istep_out, dt_out)
!
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
        dt_out = dble(istep_def) * dt_max
      else if(ictl_dt .eq. 0) then
        istep_out =   istep_in
        dt_out = dble(istep_out) * dt_max
      else
        dt_out =    dt_in
        istep_out = nint(dt_in / dt_max)
      end if
!
      end subroutine set_monitor_param_4_flex_step
!
! -----------------------------------------------------------------------
!
      end module set_control_4_time_steps
