!>@file   t_time_data.f90
!!@brief  module t_time_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  time and time step data for data IO
!!
!!@verbatim
!!      subroutine reset_time_data(time_d)
!!      subroutine evolve_time_data(time_d)
!!        type(time_data), intent(inout) :: time_d
!!      subroutine copy_time_steps_from_restart(time_IO, init_d)
!!        type(time_data), intent(in) :: time_IO
!!        type(time_data), intent(inout) :: init_d
!!      subroutine copy_time_step_size_data(time_org, time_new)
!!      subroutine copy_delta_t(time_org, time_new)
!!        type(time_data), intent(in) ::    time_org
!!        type(time_data), intent(inout) :: time_new
!!      subroutine add_one_step(time_d)
!!@endverbatim
!!
!!@n @param  id_file   file ID for data IO
!
      module t_time_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!
!>      Structure for time data
      type time_data
!>        Time step
        integer(kind = kint) :: i_time_step
!>        Time                  @f$ t @f$
        real(kind = kreal) :: time
!>        Length of time step   @f$ \Delta t @f$
        real(kind = kreal) :: dt
      end type time_data
!
!>      Structure for finish time data
      type finish_data
!>        End time steo
        integer(kind=kint) :: i_end_step
!
!>        Elapsed time to terminate simulation
        real(kind=kreal)   :: elapsed_time
!
!>        Start time to caliculate elapsed time
        real(kind=kreal) :: started_time
!>        Elapsed time for each process
        real(kind=kreal) :: elapsed_local
!>        Maximum elapsed time
        real(kind=kreal) :: elapsed_max
      end type finish_data
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine reset_time_data(time_d)
!
      type(time_data), intent(inout) :: time_d
!
!
      time_d%i_time_step = izero
      time_d%time =        zero
      time_d%dt =          zero
!
      end subroutine reset_time_data
!
! -------------------------------------------------------------------
!
      subroutine evolve_time_data(time_d)
!
      type(time_data), intent(inout) :: time_d
!
!
      time_d%time = time_d%time + time_d%dt
      time_d%i_time_step = time_d%i_time_step + 1
!
      end subroutine evolve_time_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_time_steps_from_restart(time_IO, init_d)
!
      type(time_data), intent(in) :: time_IO
      type(time_data), intent(inout) :: init_d
!
!
      call copy_time_step_data(time_IO, init_d)
!
      if(time_IO%dt .gt. zero) init_d%dt = time_IO%dt
!
      end subroutine copy_time_steps_from_restart
!
!  ---------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_time_step_size_data(time_org, time_new)
!
      type(time_data), intent(in) ::    time_org
      type(time_data), intent(inout) :: time_new
!
!
      time_new%i_time_step = time_org%i_time_step
      time_new%time =        time_org%time
      time_new%dt =          time_org%dt
!
      end subroutine copy_time_step_size_data
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_step_data(time_org, time_new)
!
      type(time_data), intent(in) ::    time_org
      type(time_data), intent(inout) :: time_new
!
!
      time_new%i_time_step = time_org%i_time_step
      time_new%time =        time_org%time
!
      end subroutine copy_time_step_data
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_data(time_org, time_new)
!
      type(time_data), intent(in) ::    time_org
      type(time_data), intent(inout) :: time_new
!
!
      time_new%time =        time_org%time
!
      end subroutine copy_time_data
!
!  ---------------------------------------------------------------------
!
      subroutine copy_delta_t(time_org, time_new)
!
      type(time_data), intent(in) ::    time_org
      type(time_data), intent(inout) :: time_new
!
!
      time_new%dt =        time_org%dt
!
      end subroutine copy_delta_t
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_one_step(time_d)
!
      type(time_data), intent(inout) :: time_d
!
!
      time_d%i_time_step = time_d%i_time_step + 1
!
      end subroutine add_one_step
!
!  ---------------------------------------------------------------------
!
      end module t_time_data
