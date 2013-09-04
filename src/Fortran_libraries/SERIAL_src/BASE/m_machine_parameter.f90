!> @file m_machine_parameter.f90
!!      module m_machine_parameter
!!
!!
!! @author H. Matsui
!! @date Written on May, 2003
!!
!!> @brief  Parameters for computer
!
      module m_machine_parameter
!
      use m_precision
!
      implicit none
!
!
!>      number of SMP threads
      integer(kind=kint) :: np_smp = 1
!
!>      integer flag for minimum message
      integer(kind = kint), parameter :: iflag_minimum_msg =  0
!>      integer flag for routine name output
      integer(kind = kint), parameter :: iflag_routine_msg =  1
!>      integer flag for full debug output
      integer(kind = kint), parameter :: iflag_full_msg =     2
!
!>      debug flag for all processes
      integer(kind = kint) :: i_debug =     iflag_minimum_msg
!>      debug flag for master process
      integer(kind = kint) :: iflag_debug = iflag_minimum_msg
!
!>      character array for error message
      character(len=256) :: e_message
!
      end module m_machine_parameter
