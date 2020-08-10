!>@file  m_time_labels.f90
!!       module m_time_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!! !!!!!  physical values!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!    time:     time
!!    t_step:   t_step
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_time_labels
!
      use m_precision
!
      implicit none
!
!>        Field label for time step
      character(len=kchara), parameter :: fhd_t_step = 't_step'
!>        Field label for time
!!         @f$ t @f$
      character(len=kchara), parameter :: fhd_time =   'time'
!
      end module m_time_labels
