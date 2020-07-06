!>@file   set_time_step_params.f90
!!@brief  module set_time_step_params
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!!@n    Modified by H. Matsui in 2020
!
!> @brief Parameteres for time steppings
!
!!@verbatim
!!      subroutine set_from_initial_step(init_d, time_d)
!!      subroutine s_set_fixed_time_step_params                         &
!!     &         (tctl, init_d, finish_d, ierr, errmsg)
!!        type(time_data_control), intent(in) :: tctl
!!        type(time_data), intent(inout) :: init_d
!!        type(finish_data), intent(inout) :: finish_d
!!@endverbatim
!
      module set_time_step_params
!
      use m_constants
      use m_machine_parameter
      use t_time_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_from_initial_step(init_d, time_d)
!
      type(time_data), intent(in) :: init_d
      type(time_data), intent(inout) :: time_d
!
!
      time_d%i_time_step = init_d%i_time_step - 1
!
      end subroutine set_from_initial_step
!
! -----------------------------------------------------------------------
!
      subroutine s_set_fixed_time_step_params                           &
     &         (tctl, init_d, finish_d, ierr, errmsg)
!
      use t_ctl_data_4_time_steps
      use m_error_IDs
!
      type(time_data_control), intent(in) :: tctl
!
      type(time_data), intent(inout) :: init_d
      type(finish_data), intent(inout) :: finish_d
      integer(kind = kint), intent(inout) :: ierr
      character(len=kchara), intent(inout) :: errmsg
!
!
      init_d%i_time_step   = 0
      if (tctl%i_step_init_ctl%iflag .gt. 0) then
        init_d%i_time_step = tctl%i_step_init_ctl%intvalue
      end if
!
      if (tctl%i_step_number_ctl%iflag .eq. 0) then
        ierr = ierr_evo
        errmsg = 'Set step number to finish'
        return
      else
        finish_d%i_end_step = tctl%i_step_number_ctl%intvalue
      end if
!
      if (finish_d%i_end_step .eq. -1) then
        if (tctl%elapsed_time_ctl%iflag .eq. 0) then
          ierr = ierr_evo
          errmsg = 'Set elapsed time to finish (second)'
          return
        else
          finish_d%elapsed_time  = tctl%elapsed_time_ctl%realvalue
        end if
      end if
!
      ierr = 0
!
      end subroutine s_set_fixed_time_step_params
!
! -----------------------------------------------------------------------
!
      end module set_time_step_params
