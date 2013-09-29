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
!!      subroutine deallocate_elapsed_times
!!
!!      subroutine start_eleps_time(iflag_elps)
!!      subroutine end_eleps_time(iflag_elps)
!!      subroutine copy_COMM_TIME_to_eleps(iflag_elps)
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
!
      implicit  none
!
      real (kind=kreal)  ::  total_time, total_start
!
      integer(kind = kint) :: num_elapsed
      real (kind=kreal), allocatable :: elapsed(:)
!
      real (kind=kreal), allocatable :: elapsed_total(:)
      real (kind=kreal), allocatable :: elapsed_min(:)
      real (kind=kreal), allocatable :: elapsed_max(:)
!
      real (kind=kreal), allocatable :: start_times(:)
      character (len=kchara), allocatable :: elapse_labels(:)
!
      private :: start_times!, elapsed
      private :: elapsed_total, elapsed_min, elapsed_max
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
      allocate(elapsed(num_elapsed))
      allocate(elapsed_total(num_elapsed))
      allocate(elapsed_min(num_elapsed))
      allocate(elapsed_max(num_elapsed))
      allocate(start_times(num_elapsed))
      allocate(elapse_labels(num_elapsed))
!
      if(num_elapsed .gt. 0) then
        start_times =   zero
        elapsed =       zero
        elapsed_total = zero
        elapsed_min = zero
        elapsed_max = zero
      end if
!
      end subroutine allocate_elapsed_times
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_elapsed_times
!
!
      deallocate(elapsed, elapsed_total)
      deallocate(elapsed_min, elapsed_max)
      deallocate(start_times)
      deallocate(elapse_labels)
!
      end subroutine deallocate_elapsed_times
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine start_eleps_time(iflag_elps)
!
      use calypso_mpi
      use m_parallel_var_dof
!
      integer(kind = kint), intent(in) :: iflag_elps
!
!
      start_times(iflag_elps) = MPI_WTIME()
!
      end subroutine start_eleps_time
!
! ----------------------------------------------------------------------
!
      subroutine end_eleps_time(iflag_elps)
!
      use calypso_mpi
      use m_parallel_var_dof
!
      integer(kind = kint), intent(in) :: iflag_elps
!
!
      end_time = MPI_WTIME()
      elapsed(iflag_elps) = elapsed(iflag_elps)                         &
     &                           + end_time - start_times(iflag_elps)
!
      end subroutine end_eleps_time
!
! ----------------------------------------------------------------------
!
      subroutine copy_COMM_TIME_to_eleps(iflag_elps)
!
      use m_parallel_var_dof
!
      integer(kind = kint), intent(in) :: iflag_elps
!
!
      elapsed(iflag_elps) = COMMtime
!
      end subroutine copy_COMM_TIME_to_eleps
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine output_elapsed_times
!
      use calypso_mpi
      use m_parallel_var_dof
!
      integer(kind = kint) :: i
!
!
      call MPI_allREDUCE(elapsed, elapsed_total, num_elapsed,           &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE(elapsed, elapsed_min, num_elapsed,             &
     &    CALYPSO_REAL, MPI_MIN, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE(elapsed, elapsed_max, num_elapsed,             &
     &    CALYPSO_REAL, MPI_MAX, CALYPSO_COMM, ierr_MPI)
!
      if (my_rank.eq.0) then
!
        do i = 1, num_elapsed
          elapsed(i) = elapsed_total(i) / dble(nprocs)
        end do
!
        open(13,file='time_total.dat')
        write(13,*) 'Average elapsed time'
        do i = 1, num_elapsed
          if(elapsed(i) .gt. zero) then
            write(13,*) trim(elapse_labels(i)), ': ', elapsed(i)
          end if
        end do
!
        write(13,*) ''
        write(13,*) 'Minimum and maximum elapsed time'
        do i = 1, num_elapsed
          if(elapsed(i) .gt. zero) then
            write(13,*) trim(elapse_labels(i)), ': ',                   &
     &                  elapsed_min(i), elapsed_max(i)
           end if
        end do
        close(13)
      end if
!
      end subroutine output_elapsed_times
!
! ----------------------------------------------------------------------
!
      end module m_work_time
