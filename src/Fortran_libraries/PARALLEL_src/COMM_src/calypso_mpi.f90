!>@file   calypso_mpi.f90
!!@brief  module calypso_mpi
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!
!> @brief MPI wrapper for Calypso
!!
!!@verbatim
!!      subroutine calypso_MPI_init(ierr)
!!      subroutine calypso_MPI_finalize(ierr)
!!      subroutine calypso_MPI_abort(code, message, ierr)
!!
!!      subroutine calypso_MPI_barrier(ierr)
!!@endverbatim
!!
!!@n @param  ierr       error flag
!!@n @param  code       error code
!!@n @param  message    message to output
!
      module calypso_mpi
!
!      use mpi
      use m_precision
!
      implicit none
!
      include 'mpif.h'
!
!>     MPI communicator
      integer :: CALYPSO_COMM
!
!>     integer size for MPI
      integer :: CALYPSO_INTEGER
!>     real size for MPI
      integer :: CALYPSO_REAL
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine calypso_MPI_init(ierr)
!
      use m_machine_parameter
!
      integer(kind=kint), intent(inout) :: ierr
!
!
      call  MPI_INIT(ierr)
      call  MPI_COMM_DUP (MPI_COMM_WORLD, CALYPSO_COMM, ierr)
!
      if(kint .eq. 4) then
        CALYPSO_INTEGER = MPI_INTEGER
      else if(kint .eq. 8) then
        CALYPSO_INTEGER = MPI_INTEGER8
      else if(kint .eq. 2) then
        CALYPSO_INTEGER = MPI_INTEGER2
      else if(kint .eq. 1) then
        CALYPSO_INTEGER = MPI_INTEGER1
      else
        CALYPSO_INTEGER = MPI_INTEGER
      end if
!
      if(kreal .eq. 8) then
        CALYPSO_REAL = MPI_DOUBLE_PRECISION
      else if(kint .eq. 4) then
        CALYPSO_REAL = MPI_REAL
      else if(kint .eq. 16) then
        CALYPSO_REAL = MPI_REAL16
      else
        CALYPSO_REAL = MPI_DOUBLE_PRECISION
      end if
!
      end subroutine calypso_MPI_init
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_finalize(ierr)
!
      integer(kind=kint), intent(inout) :: ierr
!
!
      call  MPI_FINALIZE(ierr)
!
      end subroutine calypso_MPI_finalize
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_abort(code, message, ierr)
!
      integer,       intent(in)  ::  code
      character*(*), intent(in)  ::  message
      integer(kind=kint), intent(inout) :: ierr
!
!
      write(*,*) ' ///// abnormal termination ///// ', code,            &
     &                                            ' ', message
!
      call  MPI_ABORT(CALYPSO_COMM, ierr)
!
      stop
      end subroutine  calypso_MPI_abort
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_barrier(ierr)
!
      integer(kind=kint), intent(inout) :: ierr
!
!
      call MPI_BARRIER(CALYPSO_COMM, ierr)
!
      end subroutine  calypso_MPI_barrier
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi
