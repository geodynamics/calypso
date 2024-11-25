!>@file   elapsed_labels_4_FLINE.f90
!!@brief  module elapsed_labels_4_FLINE
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine elpsed_label_4_FLINE(elps_fline, elps)
!!        type(elapsed_lables), intent(inout) :: elps_fline
!!        type(elapsed_time_data), intent(inout) :: elps
!!      subroutine elpsed_label_4_TRACER(elps_tracer, elps)
!!        type(elapsed_lables), intent(inout) :: elps_tracer
!!        type(elapsed_time_data), intent(inout) :: elps
!!@endverbatim
!
      module elapsed_labels_4_FLINE
!
      use m_precision
      use m_work_time
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_FLINE(elps_fline, elps)
!
      type(elapsed_lables), intent(inout) :: elps_fline
      type(elapsed_time_data), intent(inout) :: elps
!
      integer(kind = kint), parameter :: num_append = 4
!
!
      call append_elapsed_timer(num_append, elps_fline%ist_elapsed,     &
     &                          elps_fline%ied_elapsed, elps)
!
      elps%labels(elps_fline%ist_elapsed+1)                             &
     &                    = 'Set Seed points   '
      elps%labels(elps_fline%ist_elapsed+2)                             &
     &                    = 'Trace field line   '
      elps%labels(elps_fline%ist_elapsed+3)                             &
     &                    = 'Communication for field line   '
      elps%labels(elps_fline%ist_elapsed+4)                             &
     &                    = 'Output field line file   '
!
      elps_fline%flag_elapsed = .TRUE.
!
      end subroutine elpsed_label_4_FLINE
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_TRACER(elps_tracer, elps)
!
      type(elapsed_lables), intent(inout) :: elps_tracer
      type(elapsed_time_data), intent(inout) :: elps
      integer(kind = kint), parameter :: num_append = 3
!
!
      call append_elapsed_timer(num_append, elps_tracer%ist_elapsed,    &
     &                          elps_tracer%ied_elapsed, elps)
!
      elps%labels(elps_tracer%ist_elapsed+1)                            &
     &                    = 'Trace in elements   '
      elps%labels(elps_tracer%ist_elapsed+2)                            &
     &                    = 'Communication for tracers  '
      elps%labels(elps_tracer%ist_elapsed+3)                            &
     &                    = 'Output tracer file   '
!
!
      elps_tracer%flag_elapsed = .TRUE.
!
      end subroutine elpsed_label_4_TRACER
!
!-----------------------------------------------------------------------
!
      end module  elapsed_labels_4_FLINE
