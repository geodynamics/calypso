!m_flexible_time_step.f90
!     module m_flexible_time_step
!
!      Written by H. Matsui on Nov., 2009
!
      module m_flexible_time_step
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
!
      use t_flex_delta_t_data
!
      implicit  none
!
      type(flexible_stepping_parameter), save :: flex_p1
!
      type(flexible_stepping_data), save :: flex_data1
!
!
      end module m_flexible_time_step
