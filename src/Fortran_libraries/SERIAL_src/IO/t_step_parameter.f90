!>@file   t_step_parameter.f90
!!@brief  module t_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings with restart and field file IO
!
!!@verbatim
!!      subroutine set_fixed_time_step_params                           &
!!     &         (tctl, t_param, ierr, errmsg)
!!        type(time_data_control), intent(in) :: tctl
!!        type(time_step_param), intent(inout) :: t_param
!!        integer(kind = kint), intent(inout) :: ierr
!!        character(len=kchara), intent(inout) :: errmsg
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
      use set_time_step_params
!
      type(time_data_control), intent(in) :: tctl
!
      type(time_step_param), intent(inout) :: t_param
      integer(kind = kint), intent(inout) :: ierr
      character(len=kchara), intent(inout) :: errmsg
!
!
      call s_set_fixed_time_step_params                                 &
     &   (tctl, t_param%init_d, t_param%finish_d, ierr, errmsg)
!
      call output_step_4_fixed_step_ctl(ione, t_param%init_d%dt,        &
     &   tctl%i_step_rst_ctl, tctl%delta_t_rst_ctl, t_param%rst_step)
      call output_step_4_fixed_step_ctl(ione, t_param%init_d%dt,        &
     &   tctl%i_step_ucd_ctl, tctl%delta_t_field_ctl, t_param%ucd_step)
!
      end subroutine set_fixed_time_step_params
!
! -----------------------------------------------------------------------
!
      end module t_step_parameter
