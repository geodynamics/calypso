!>@file  m_work_time.f90
!!       module m_work_time
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in 2001
!
!> @brief routines to count elapsed time
!!
!!@verbatim
!!      subroutine allocate_elapsed_times
!!      subroutine append_elapsed_times(num_append, iend_org, iend_new)
!!      subroutine deallocate_elapsed_times
!!
!!      subroutine start_elapsed_time(iflag_elps)
!!      subroutine end_elapsed_time(iflag_elps)
!!      subroutine reset_elapsed_times(istart, iend)
!!
!!      subroutine init_elapse_time_by_TOTAL
!!
!!      subroutine output_elapsed_times
!!@endverbatim
!!
!!@params  timer ID
!
      module m_work_time
!
      use m_precision
      use m_constants
      use t_work_time
!
      implicit  none
!
!
      integer(kind = kint) :: iflag_time_4_each_pe = 0
!
      integer(kind = kint), parameter :: id_timer_file = 13
      character(len=kchara), parameter                                  &
     &                   :: time_file_prefix = 'time_total'
!
      type(elapsed_time_data), save :: elps1
!
      logical, save :: iflag_TOT_time = .FALSE.
      integer(kind = kint), save, private :: ist_total_elapsed = 0
      integer(kind = kint), save :: ied_total_elapsed = 0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_elapsed_times
!
!
      call alloc_elapsed_timer(elps1)
!
      end subroutine allocate_elapsed_times
!
! ----------------------------------------------------------------------
!
      subroutine append_elapsed_times(num_append, iend_org, iend_new)
!
      integer(kind = kint), intent(in) :: num_append
      integer(kind = kint), intent(inout) :: iend_org, iend_new
!
!
      call append_elapsed_timer                                         &
     &         (num_append, iend_org, iend_new, elps1)
!
      end subroutine append_elapsed_times
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_elapsed_times
!
!
      call dealloc_elapsed_timer(elps1)
!
      end subroutine deallocate_elapsed_times
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine start_elapsed_time(iflag_elps)
!
      integer(kind = kint), intent(in) :: iflag_elps
!
!
      call start_elapsed_timer(iflag_elps, elps1)
!
      end subroutine start_elapsed_time
!
! ----------------------------------------------------------------------
!
      subroutine end_elapsed_time(iflag_elps)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: iflag_elps
!
!
      call end_elapsed_timer(iflag_elps, elps1)
!
      end subroutine end_elapsed_time
!
! ----------------------------------------------------------------------
!
      subroutine reset_elapsed_times(istart, iend)
!
      integer(kind = kint), intent(in) :: istart, iend
!
!
      call reset_elapsed_timer(istart, iend, elps1)
!
      end subroutine reset_elapsed_times
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_elapse_time_by_TOTAL
!
!
      elps1%num_elapsed = 1
      ist_total_elapsed = 0
      ied_total_elapsed = 1
      call allocate_elapsed_times
!
      elps1%labels(1) = 'Total time   '
      iflag_TOT_time = .TRUE.
!
      end subroutine init_elapse_time_by_TOTAL
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine output_elapsed_times
!
!
      call output_elapsed_log                                           &
     &   (iflag_time_4_each_pe, time_file_prefix, elps1)
!
      end subroutine output_elapsed_times
!
! ----------------------------------------------------------------------
!
      end module m_work_time
