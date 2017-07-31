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
!>      integer flag for keep endian
      integer(kind = kint), parameter :: iendian_KEEP =       0
!>      integer flag for flip endian
      integer(kind = kint), parameter :: iendian_FLIP =       1
!
!>      Endian check integer
      integer(kind = kint), parameter :: i_UNIX = ichar('U') * 256**3   &
     &                                           +ichar('N') * 256**2   &
     &                                           +ichar('I') * 256      &
     &                                           +ichar('X')
!>      Endian check integer (reversed)
      integer(kind = kint), parameter :: i_XINU = ichar('X') * 256**3   &
     &                                           +ichar('I') * 256**2   &
     &                                           +ichar('N') * 256      &
     &                                           +ichar('U')
!
!>      debug flag for all processes
      integer(kind = kint) :: i_debug =     iflag_minimum_msg
!>      debug flag for master process
      integer(kind = kint) :: iflag_debug = iflag_minimum_msg
!
!>      Integer flag of endian
!!      (the same endian: i_UNIX, opposit endian: i_XINU)
      integer(kind = kint) :: iflag_endian
!
!
!>      character array for error message
      character(len=256) :: e_message
!
      end module m_machine_parameter
