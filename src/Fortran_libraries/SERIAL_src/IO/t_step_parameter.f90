!>@file   t_step_parameter.f90
!!@brief  module t_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!
!!@verbatim
!!      subroutine set_fixed_time_step_params                           &
!!     &         (tctl, t_param, ierr, errmsg)
!!        type(time_data_control), intent(in) :: tctl
!!        type(time_step_param), intent(inout) :: t_param
!!        integer(kind = kint), intent(inout) :: ierr
!!        character(len=kchara), intent(inout) :: errmsg
!!      subroutine s_initialize_time_step(init_d, time_d)
!!      subroutine s_set_fixed_time_step_params(tctl, init_d, finish_d, &
!!     &          rst_step, ucd_step, ierr, errmsg)
!!        type(time_data_control), intent(in) :: tctl
!!        type(time_data), intent(inout) :: init_d
!!        type(finish_data), intent(inout) :: finish_d
!!        type(IO_step_param), intent(inout) :: rst_step, ucd_step
!!@endverbatim
!
      module t_step_parameter
!
!
      use m_constants
      use m_machine_parameter
      use t_time_data
      use t_IO_step_parameter
!
      implicit  none
!
!
!
!       Structure for time stepping parameters
      type time_step_param
!>        Structure for time data
        type(time_data) :: time_d
!>        Structure for initial time data
        type(time_data) :: init_d
!>        Structure for end time data
        type(finish_data) :: finish_d
!
!>        Increment for mean restart data
        type(IO_step_param) :: rst_step
!>        Increment for mean field data
        type(IO_step_param) :: ucd_step
      end type time_step_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_time_step_params                             &
     &         (tctl, t_param, ierr, errmsg)
!
      use t_ctl_data_4_time_steps
!
      type(time_data_control), intent(in) :: tctl
!
      type(time_step_param), intent(inout) :: t_param
      integer(kind = kint), intent(inout) :: ierr
      character(len=kchara), intent(inout) :: errmsg
!
!
      call s_set_fixed_time_step_params                                 &
     &   (tctl, t_param%init_d, t_param%finish_d,                       &
     &    t_param%rst_step, t_param%ucd_step, ierr, errmsg)
!
      end subroutine set_fixed_time_step_params
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_initialize_time_step(init_d, time_d)
!
      type(time_data), intent(in) :: init_d
      type(time_data), intent(inout) :: time_d
!
!
      time_d%i_time_step = init_d%i_time_step - 1
!
      end subroutine s_initialize_time_step
!
! -----------------------------------------------------------------------
!
      subroutine s_set_fixed_time_step_params(tctl, init_d, finish_d,   &
     &          rst_step, ucd_step, ierr, errmsg)
!
      use t_ctl_data_4_time_steps
      use m_error_IDs
!
      type(time_data_control), intent(in) :: tctl
!
      type(time_data), intent(inout) :: init_d
      type(finish_data), intent(inout) :: finish_d
      type(IO_step_param), intent(inout) :: rst_step, ucd_step
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
!
      call set_output_step_4_fixed_step(ione, init_d%dt,                &
     &    tctl%i_step_rst_ctl, tctl%delta_t_rst_ctl, rst_step)
!
      call set_output_step_4_fixed_step(ione, init_d%dt,                &
     &    tctl%i_step_ucd_ctl, tctl%delta_t_field_ctl, ucd_step)
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
      end module t_step_parameter
