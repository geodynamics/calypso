!>@file   t_MHD_step_parameter.f90
!!@brief  module t_MHD_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!
!!@verbatim
!!      subroutine s_set_control_4_time_steps(mr_ctl, tctl, MHD_step)
!!        type(mhd_restart_control), intent(in) :: mr_ctl
!!        type(time_data_control), intent(in) :: tctl
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!@endverbatim
!
      module  t_MHD_step_parameter
!
      use m_precision
      use t_time_data
      use t_step_parameter
      use t_IO_step_parameter
      use t_VIZ_step_parameter
      use t_flex_delta_t_parameter
      use t_ctl_data_4_time_steps
!
      implicit  none
!
!
      type MHD_step_param
!>        Structure for time data
        type(time_data) :: time_d
!>        Structure for initial time data
        type(time_data) :: init_d
!>        Structure for end time data
        type(finish_data) :: finish_d
!
!>        Flag for initial step to use Euler scheme
!!        insted of Adams-BAshforth
        integer(kind=kint) :: iflag_initial_step = 0
!
!>        Increment for mean restart data
        type(IO_step_param) :: rst_step
!>        Increment for mean field data
        type(IO_step_param) :: ucd_step
!>        Increment for mean square output
        type(IO_step_param) :: rms_step
!>        Increment for nodal monitor
        type(IO_step_param) :: point_step
!>        Increment for boundary condition file
        type(IO_step_param) :: boundary_step
!>        Increment for model coefficient file
        type(IO_step_param) :: sgs_IO_step
!
!>        Increment for visualizations
        type(VIZ_step_params) :: viz_step
!
!>        Flexible step information
        type(flexible_stepping_parameter) :: flex_p
      end type MHD_step_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_time_steps(mr_ctl, tctl, MHD_step)
!
      use t_ctl_data_mhd_evo_scheme
      use t_ctl_data_mhd_restart
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
        MHD_step%time_d%flag_flex_step = .FALSE.
        if(tctl%flexible_step_ctl%iflag .gt. 0                          &
     &     .and. yes_flag(tctl%flexible_step_ctl%charavalue)) then
          MHD_step%time_d%flag_flex_step = .TRUE.
        end if
!
      if (tctl%dt_ctl%iflag .eq. 0) then
        e_message = 'Set delta t'
        call calypso_MPI_abort(ierr_evo, e_message)
      else
        MHD_step%init_d%dt = tctl%dt_ctl%realvalue
      end if
!
!   parameters for time evolution
!
      if(MHD_step%time_d%flag_flex_step) then
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &    write(*,*) 'set_flex_time_step_controls'
        call set_flex_time_step_controls(tctl, MHD_step)
      else
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &    write(*,*) 'set_fixed_time_step_controls'
        call set_fixed_time_step_controls(tctl, MHD_step)
      end if
      MHD_step%init_d%flag_flex_step = MHD_step%time_d%flag_flex_step
!
      call set_control_flex_time_steps                                  &
     &   (tctl, MHD_step%init_d, MHD_step%flex_p)
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
      subroutine set_fixed_time_step_controls(tctl, MHD_step)
!
      use set_time_step_params
!
      type(time_data_control), intent(in) :: tctl
      type(MHD_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: ierr
!
!
      call s_set_fixed_time_step_params                                 &
     &   (tctl, MHD_step%init_d, MHD_step%finish_d, ierr, e_message)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
      call output_step_4_fixed_step_ctl(ione, MHD_step%init_d%dt,       &
     &    tctl%i_step_rst_ctl, tctl%delta_t_rst_ctl, MHD_step%rst_step)
!
      call output_step_4_fixed_step_ctl(ione, MHD_step%init_d%dt,       &
     &    tctl%i_step_ucd_ctl, tctl%delta_t_field_ctl,                  &
     &    MHD_step%ucd_step)
      call viz_fixed_time_step_params                                   &
     &   (MHD_step%init_d%dt, tctl, MHD_step%viz_step)
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
     &    MHD_step%rst_step)
!
      call output_step_4_flex_step_ctl(ione, MHD_step%flex_p%dt_max,    &
     &    tctl%i_step_ucd_ctl, tctl%delta_t_field_ctl,                  &
     &    MHD_step%ucd_step)
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
      end module  t_MHD_step_parameter
