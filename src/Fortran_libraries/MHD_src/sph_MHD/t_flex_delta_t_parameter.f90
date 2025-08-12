!>@file   t_flex_delta_t_parameter.f90
!!@brief  module t_flex_delta_t_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for flexible time steppings
!
!!@verbatim
!!      subroutine set_flex_time_step_params(tctl, flex_p,              &
!!     &          init_d, finish_d, rst_step)
!!        type(time_data_control), intent(in) :: tctl
!!        type(time_data), intent(inout) :: init_d
!!        type(finish_data), intent(inout) :: finish_d
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!        type(IO_step_param), intent(inout) :: rst_step
!!      subroutine set_control_flex_time_steps(tctl, init_d, flex_p)
!!        type(time_data_control), intent(in) :: tctl
!!        type(time_data), intent(in) :: init_d
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!@endverbatim
!
      module t_flex_delta_t_parameter
!
      use m_precision
      use m_constants
      use m_error_IDs
      use calypso_mpi
!
      use t_time_data
      use t_IO_step_parameter
      use t_ctl_data_4_time_steps
!
      implicit  none
!
      type flexible_stepping_parameter
!>        Integer flag if time stepping is changed
        integer(kind= kint) :: iflag_flex_step_changed = id_turn_OFF
!
!>        End time
        real(kind=kreal) :: time_to_finish
!
!>        significand of @f$ \Delta t @f$
        real(kind=kreal) :: dt_fact
!>        exponent of @f$ \Delta t @f$
        integer(kind = kint) :: idt_digit
!
!>        Maximum length of time step
        integer(kind = kint) :: istep_max_dt
!
!>      Flexible time step number for maximum lenth of each step
        integer(kind = kint) :: istep_flex_to_max = 0
!>
        integer(kind = kint) :: interval_flex_2_max
!
!>        Maximum error to shrink time step
        real(kind = kreal) :: max_eps_to_shrink
!>        Minimum error to expand time step
        real(kind = kreal) :: min_eps_to_expand
!
!>        Ratio to CFL condition
        real(kind = kreal) :: ratio_to_cfl
!
!>        Maximum @f$ \Delta t @f$
        real(kind = kreal) :: dt_max
!>        Mimimum @f$ \Delta t @f$
        real(kind = kreal) :: dt_min
      end type flexible_stepping_parameter
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_flex_time_step_params(tctl, flex_p,                &
     &          init_d, finish_d, rst_step)
!
      type(time_data_control), intent(in) :: tctl
      type(time_data), intent(inout) :: init_d
      type(finish_data), intent(inout) :: finish_d
!
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(IO_step_param), intent(inout) :: rst_step
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
      istep_rst_end = 0
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
      subroutine set_control_flex_time_steps(tctl, init_d, flex_p)
!
      use cal_num_digits
!
      type(time_data_control), intent(in) :: tctl
      type(time_data), intent(in) :: init_d
      type(flexible_stepping_parameter), intent(inout) :: flex_p
!
!
      call cal_num_digit_real                                           &
     &     (init_d%dt, flex_p%dt_fact, flex_p%idt_digit)
!
      if(init_d%flag_flex_step) then
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
      end module t_flex_delta_t_parameter
