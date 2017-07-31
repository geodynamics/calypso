!>@file   t_IO_step_parameter.f90
!!@brief  module t_IO_step_parameter
!!
!!@author H. Matsui
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2017
!
!> @brief Parameteres for time steppings
!!
!!@verbatim
!!      subroutine set_output_step_4_fixed_step(istep_def, dt,          &
!!     &          istep_ctl, delta_t_ctl, IO_step)
!!      subroutine set_output_step_4_flex_step                          &
!!     &         (istep_def, dt_max, step_ctl, delta_t_ctl, IO_step)
!!        type(read_integer_item), intent(inout) :: step_ctl
!!        type(read_real_item), intent(inout) :: delta_t_ctl
!!        type(IO_step_param), intent(inout) :: IO_step
!!      subroutine accum_output_flag(i_step, IO_step, i_flag)
!!      integer(kind = kint) function output_IO_flag(i_step, IO_step)
!!        type(IO_step_param), intent(in) :: IO_step
!!      integer(kind = kint) function set_IO_step_flag(i_step, IO_step)
!!        type(IO_step_param), intent(inout) :: IO_step
!!
!!      subroutine accum_flag_to_visualization(i_step, IO_step, visval)
!!        type(IO_step_param), intent(in) :: IO_step
!!
!!      subroutine set_monitor_param_4_fixed_step(istep_def, istep_ctl, &
!!     &          delta_t_ctl, istep_out, dt_out)
!!      subroutine set_monitor_param_4_flex_step(istep_def, dt_max,     &
!!     &          istep_ctl, delta_t_ctl, istep_out, dt_out)
!!      integer(kind = kint) function output_flag(i_step, increment)
!!      subroutine set_viz_file_step(istep_ref, i_step_viz, iviz, i_cnt)
!!      subroutine set_viz_flex_file_step(time_d, IO_step, iviz)
!!end@verbatim
!!
      module  t_IO_step_parameter
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      type IO_step_param
!>        Increment of time step
        integer(kind=kint) :: increment
!>        Time interval for output
        real(kind=kreal)   :: delta_t
!
!>        step number for data file
        integer(kind=kint) :: istep_file
      end type IO_step_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine accum_output_flag(i_step, IO_step, i_flag)
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: IO_step
      integer (kind =kint), intent(inout) :: i_flag
!
!
      i_flag = i_flag * output_flag(i_step, IO_step%increment)
!
      end subroutine accum_output_flag
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function output_IO_flag(i_step, IO_step)
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: IO_step
!
!
      output_IO_flag = output_flag(i_step, IO_step%increment)
!
      end function output_IO_flag
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function set_IO_step_flag(i_step, IO_step)
!
      integer (kind =kint), intent(in) :: i_step
      type(IO_step_param), intent(inout) :: IO_step
!
!
      set_IO_step_flag = output_flag(i_step, IO_step%increment)
      if(IO_step%increment .gt. 0) then
         IO_step%istep_file = i_step / IO_step%increment
       end if
!
      end function set_IO_step_flag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine accum_flag_to_visualization(i_step, IO_step, visval)
!
      integer (kind =kint), intent(in) :: i_step
      integer(kind=kint ), intent(inout) :: visval
      type(IO_step_param), intent(inout) :: IO_step
!
      integer(kind=kint ) :: iflag
!
!
      call set_viz_file_step(i_step, IO_step%increment,                 &
     &    iflag, IO_step%istep_file)
      visval = visval * iflag
!
      end subroutine accum_flag_to_visualization
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_output_step_4_fixed_step(istep_def, dt,            &
     &          istep_ctl, delta_t_ctl, IO_step)
!
      use t_control_elements
!
      integer(kind = kint), intent(in) :: istep_def
      real(kind = kreal), intent(in) :: dt
      type(read_real_item), intent(in) :: delta_t_ctl
      type(read_integer_item), intent(in) :: istep_ctl
!
      type(IO_step_param), intent(inout) :: IO_step
!
!
      if ( (istep_ctl%iflag + delta_t_ctl%iflag) .eq. 0) then
        IO_step%increment =   istep_def
        IO_step%delta_t = dble(istep_def) * dt
      else if(istep_ctl%iflag .eq. 0) then
        IO_step%delta_t =    delta_t_ctl%realvalue
        IO_step%increment = nint(IO_step%delta_t / dt)
      else
        IO_step%increment =   istep_ctl%intvalue
        IO_step%delta_t = dble(IO_step%increment) * dt
      end if
!
      end subroutine set_output_step_4_fixed_step
!
! -----------------------------------------------------------------------
!
      subroutine set_output_step_4_flex_step(istep_def, dt_max,         &
     &          istep_ctl, delta_t_ctl, IO_step)
!
      use t_control_elements
!
      integer(kind = kint), intent(in) :: istep_def
      real(kind = kreal), intent(in) :: dt_max
      type(read_real_item), intent(in) :: delta_t_ctl
      type(read_integer_item), intent(in) :: istep_ctl
!
      type(IO_step_param), intent(inout) :: IO_step
!
!
      if ( (istep_ctl%iflag+delta_t_ctl%iflag) .eq. 0) then
        IO_step%increment =   istep_def
        IO_step%delta_t = dble(istep_def) * dt_max
      else if(delta_t_ctl%iflag .eq. 0) then
        IO_step%increment =   istep_ctl%intvalue
        IO_step%delta_t = dble(IO_step%increment) * dt_max
      else
        IO_step%delta_t =    delta_t_ctl%realvalue
        IO_step%increment = nint(IO_step%delta_t / dt_max)
      end if
!
      end subroutine set_output_step_4_flex_step
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function output_flag(i_step, increment)
!
      integer (kind = kint), intent(in) :: i_step, increment
!
!
      if ( increment .eq. 0) then
        output_flag = ione
      else
        output_flag = mod(i_step,increment)
      end if
!
      end function output_flag
!
! -----------------------------------------------------------------------
!
      subroutine set_viz_file_step(istep_ref, i_step_viz, iviz, i_cnt)
!
      integer(kind = kint), intent(in) :: istep_ref, i_step_viz
      integer(kind = kint), intent(inout) :: iviz, i_cnt
!
!
      if(i_step_viz .eq. izero) then
        iviz =  ione
        i_cnt =-ione
      else 
        iviz = mod(istep_ref,i_step_viz)
        if (iviz .eq. izero) then
          i_cnt = istep_ref / i_step_viz
        else 
          i_cnt = -ione
        end if
      end if
!
      end subroutine set_viz_file_step
!
! -----------------------------------------------------------------------
!
      subroutine set_viz_flex_file_step(time_d, IO_step, iviz)
!
      use t_time_data
!
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(inout) :: IO_step
      integer(kind = kint), intent(inout) :: iviz
!
      integer(kind = kint) :: istep, iref
!
!
      istep = int(time_d%time / time_d%dt)
      if (IO_step%delta_t .eq. zero) then
        iviz =   ione
        IO_step%istep_file = -ione
      else
         iref =  int(IO_step%delta_t / time_d%dt)
         iviz = mod(istep, iref)
        if (iviz .eq. izero) then
          IO_step%istep_file = istep / iref
        else 
          IO_step%istep_file = -ione
        end if
      end if
!
      end subroutine set_viz_flex_file_step
!
! -----------------------------------------------------------------------
!
      end module t_IO_step_parameter
