!>@file   m_elapsed_labels_4_VIZ.f90
!!@brief  module m_elapsed_labels_4_VIZ
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!@endverbatim
!
      module m_elapsed_labels_4_VIZ
!
      use m_precision
      use m_work_time
!
      use t_elapsed_labels_4_VIZ
!
      implicit none
!
      logical, parameter :: flag_detailed1 = .TRUE.
      type(elapsed_labels_4_VIZ), save :: elps_viz1
!
      end module  m_elapsed_labels_4_VIZ
