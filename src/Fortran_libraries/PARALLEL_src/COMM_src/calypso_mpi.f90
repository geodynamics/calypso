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
!!      subroutine calypso_MPI_init
!!      subroutine calypso_MPI_finalize
!!      subroutine calypso_MPI_abort(icode, message)
!!
!!      subroutine calypso_MPI_barrier
!!
!!      subroutine calypso_mpi_bcast_real(buffer, count, root)
!!      subroutine calypso_mpi_bcast_int(buffer, count, root)
!!      subroutine calypso_mpi_bcast_int8(buffer, count, root)
!!      subroutine calypso_mpi_bcast_character(buffer, count, root)
!!@endverbatim
!!
!!@n @param  icode       error code
!!@n @param  message    message to output
!
      module calypso_mpi
!
!      use mpi
      use m_precision
      use m_constants
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
!>     character size for MPI
      integer :: CALYPSO_CHARACTER
!
!>     integer size for MPI
      integer :: CALYPSO_GLOBAL_INT
!>     4-byte integer size for MPI
      integer :: CALYPSO_FOUR_INT
!
!>      process ID (start from 0)
      integer :: my_rank
!>      total number of processes
      integer :: nprocs
!
!>      error flag for MPI
      integer :: ierr_MPI
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine calypso_MPI_init
!
!
      call  MPI_INIT(ierr_MPI)
      call  MPI_COMM_DUP (MPI_COMM_WORLD, CALYPSO_COMM, ierr_MPI)
      call  MPI_COMM_SIZE(CALYPSO_COMM, nprocs, ierr_MPI)
      call  MPI_COMM_RANK(CALYPSO_COMM, my_rank, ierr_MPI)
!
      CALYPSO_CHARACTER = MPI_CHARACTER
      CALYPSO_FOUR_INT =  MPI_INTEGER 
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
      if(kint_gl .eq. 4) then
        CALYPSO_GLOBAL_INT = MPI_INTEGER
      else if(kint_gl .eq. 8) then
        CALYPSO_GLOBAL_INT = MPI_INTEGER8
      else
        CALYPSO_GLOBAL_INT = MPI_INTEGER
      end if
!
      end subroutine calypso_MPI_init
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_finalize
!
!
      call  MPI_FINALIZE(ierr_MPI)
!
      end subroutine calypso_MPI_finalize
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_abort(icode, message)
!
      integer(kind = kint), intent(in)  ::  icode
      character(len=*), intent(in)  ::  message
!
!
      write(*,*) ' ///// abnormal termination ///// ', icode,           &
     &                                            ' ', message
!
      call  MPI_ABORT(CALYPSO_COMM, 999, ierr_MPI)
!
      stop
      end subroutine calypso_MPI_abort
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_barrier
!
!
      call MPI_BARRIER(CALYPSO_COMM, ierr_MPI)
!
      end subroutine  calypso_MPI_barrier
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_bcast_real(buffer, count, root)
!
      integer, intent(in) :: root
      integer(kind = kint_gl), intent(in) :: count
      real(kind = kreal), intent(inout) :: buffer(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_BCAST(buffer(ist+1), ilen_in, CALYPSO_REAL,            &
     &      root, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_bcast_real
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_bcast_int(buffer, count, root)
!
      integer, intent(in) :: root
      integer(kind = kint_gl), intent(in) :: count
      integer(kind = kint), intent(inout) :: buffer(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_BCAST(buffer(ist+1), ilen_in, CALYPSO_INTEGER,         &
     &      root, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_bcast_int
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_bcast_int8(buffer, count, root)
!
      integer, intent(in) :: root
      integer(kind = kint_gl), intent(in) :: count
      integer(kind = kint_gl), intent(inout) :: buffer(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_BCAST(buffer(ist+1), ilen_in, CALYPSO_GLOBAL_INT,      &
     &      root, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_bcast_int8
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_bcast_character(buffer, count, root)
!
      integer, intent(in) :: root
      integer(kind = kint_gl), intent(in) :: count
      character(len = 1), intent(inout) :: buffer(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_BCAST(buffer(ist+1), ilen_in, CALYPSO_CHARACTER,       &
     &      root, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_bcast_character
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_reduce_real                                &
     &         (r_local, r_global, count, operation, destination)
!
      integer, intent(in) :: operation, destination
      integer(kind = kint_gl), intent(in) :: count
      real(kind = kreal), intent(in) ::    r_local(count)
      real(kind = kreal), intent(inout) :: r_global(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_Reduce(r_local(ist+1), r_global(ist+1), ilen_in,       &
     &      CALYPSO_REAL, operation, destination,                       &
     &      CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_reduce_real
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allreduce_real                             &
     &         (r_local, r_global, count, operation)
!
      integer, intent(in) :: operation
      integer(kind = kint_gl), intent(in) :: count
      real(kind = kreal), intent(in) ::    r_local(count)
      real(kind = kreal), intent(inout) :: r_global(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_allREDUCE(r_local(ist+1), r_global(ist+1), ilen_in,    &
     &      CALYPSO_REAL, operation, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_allreduce_real
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allreduce_int                              &
     &         (r_local, r_global, count, operation)
!
      integer, intent(in) :: operation
      integer(kind = kint_gl), intent(in) :: count
      integer(kind = kint), intent(in) ::    r_local(count)
      integer(kind = kint), intent(inout) :: r_global(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_allREDUCE(r_local(ist+1), r_global(ist+1), ilen_in,    &
     &      CALYPSO_INTEGER, operation, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_allreduce_int
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allreduce_int8                             &
     &         (r_local, r_global, count, operation)
!
      integer, intent(in) :: operation
      integer(kind = kint_gl), intent(in) :: count
      integer(kind = kint_gl), intent(in) ::    r_local(count)
      integer(kind = kint_gl), intent(inout) :: r_global(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_allREDUCE(r_local(ist+1), r_global(ist+1), ilen_in,    &
     &      CALYPSO_GLOBAL_INT, operation, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_allreduce_int8
!
!  ---------------------------------------------------------------------
!
!
      end module calypso_mpi
