!>@file   elapsed_labels_4_PSF.f90
!!@brief  module elapsed_labels_4_PSF
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine elpsed_label_4_PSF(elps_PSF, elps)
!!      subroutine reset_elapse_after_init_PSF(elps_PSF, elps)
!!        type(elapsed_lables), intent(inout) :: elps_PSF
!!        type(elapsed_time_data), intent(inout) :: elps
!!
!!      subroutine elpsed_label_4_ISO(elps_ISO, elps)
!!      subroutine reset_elapse_after_init_ISO(elps_ISO, elps)
!!        type(elapsed_lables), intent(inout) :: elps_ISO
!!        type(elapsed_time_data), intent(inout) :: elps
!!@endverbatim
!
      module elapsed_labels_4_PSF
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
      subroutine elpsed_label_4_PSF(elps_PSF, elps)
!
      type(elapsed_lables), intent(inout) :: elps_PSF
      type(elapsed_time_data), intent(inout) :: elps
      integer(kind = kint), parameter :: num_append = 3
!
!
      call append_elapsed_timer(num_append, elps_PSF%ist_elapsed,       &
     &                          elps_PSF%ied_elapsed, elps)
!
      elps%labels(elps_PSF%ist_elapsed+1)                               &
     &                    = 'Find Section patch   '
      elps%labels(elps_PSF%ist_elapsed+2)                               &
     &                    = 'Interpolate data on Section   '
      elps%labels(elps_PSF%ist_elapsed+3)                               &
     &                    = 'Output Sectioning data   '
!
      elps_PSF%flag_elapsed = .TRUE.
!
      end subroutine elpsed_label_4_PSF
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_PSF(elps_PSF, elps)
!
      type(elapsed_lables), intent(in) :: elps_PSF
      type(elapsed_time_data), intent(inout) :: elps
!
      if(elps_PSF%flag_elapsed .eqv. .FALSE.) return
      call reset_elapsed_timer(elps_PSF%ist_elapsed+2,                  &
     &                         elps_PSF%ist_elapsed+2, elps)
!
      end subroutine reset_elapse_after_init_PSF
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_ISO(elps_ISO, elps)
!
      type(elapsed_lables), intent(inout) :: elps_ISO
      type(elapsed_time_data), intent(inout) :: elps
!
      integer(kind = kint), parameter :: num_append = 3
!
!
      call append_elapsed_timer(num_append, elps_ISO%ist_elapsed,       &
     &                          elps_ISO%ied_elapsed, elps)
!
      elps%labels(elps_ISO%ist_elapsed+1)                               &
     &                    = 'Find Isosurface patch   '
      elps%labels(elps_ISO%ist_elapsed+2)                               &
     &                    = 'Interpolate data on isosurface   '
      elps%labels(elps_ISO%ist_elapsed+3)                               &
     &                    = 'Output Isosurface data   '
!
      elps_ISO%flag_elapsed = .TRUE.
!
      end subroutine elpsed_label_4_ISO
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_ISO(elps_ISO, elps)
!
      type(elapsed_lables), intent(in) :: elps_ISO
      type(elapsed_time_data), intent(inout) :: elps
!
      if(elps_ISO%flag_elapsed .eqv. .FALSE.) return
      call reset_elapsed_timer(elps_ISO%ist_elapsed+2,                  &
     &                         elps_ISO%ist_elapsed+2, elps)
!
      end subroutine reset_elapse_after_init_ISO
!
!-----------------------------------------------------------------------
!
      end module  elapsed_labels_4_PSF
