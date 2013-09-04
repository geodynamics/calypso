!
!------- module copy_time_steps_4_restart ---------------------
!
!        programmed by H.Matsui on Nov., 2009
!
!      subroutine copy_init_time_from_restart
!      subroutine copy_time_steps_from_restart
!      subroutine copy_time_steps_to_restart
!
      module copy_time_steps_4_restart
!
      use m_precision
!
      use m_constants
      use m_t_step_parameter
      use m_t_int_parameter
      use m_time_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_init_time_from_restart
!
!
      time_init =   time_IO
      i_step_init = i_time_step_IO
!
      end subroutine copy_init_time_from_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_steps_from_restart
!
      use cal_num_digits
!
!
      call copy_init_time_from_restart
!
      if(delta_t_IO .gt. zero) then
        dt = delta_t_IO
        ddt= one / dt
        call cal_num_digit_real(dt, dt_fact, idt_digit)
      end if
!
      end subroutine copy_time_steps_from_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_steps_to_restart
!
!
      i_time_step_IO = i_step_MHD
      time_IO =        time
      delta_t_IO =     dt
!
      end subroutine copy_time_steps_to_restart
!
!  ---------------------------------------------------------------------
!
      end module copy_time_steps_4_restart
