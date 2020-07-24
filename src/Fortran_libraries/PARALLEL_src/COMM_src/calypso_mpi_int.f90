!>@file   calypso_mpi_int.f90
!!@brief  module calypso_mpi_int
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!> @brief MPI communication routines for integer in Calypso
!!
!!@verbatim
!!@endverbatim
!!
!!@n @param  icode       error code
!!      subroutine calypso_mpi_bcast_one_int(buffer, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint), intent(inout) :: buffer
!!      subroutine calypso_mpi_bcast_int(buffer, count, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint_gl), intent(in) :: count
!!        integer(kind = kint), intent(inout) :: buffer(count)
!!      subroutine calypso_mpi_allreduce_int                            &
!!     &         (r_local, r_global, count, operation)
!!        integer, intent(in) :: operation
!!        integer(kind = kint_gl), intent(in) :: count
!!        integer(kind = kint), intent(in) ::    r_local(count)
!!        integer(kind = kint), intent(inout) :: r_global(count)
!!      subroutine calypso_mpi_allgather_int                            &
!!     &         (isendbuf, n_send, irecvbuf, n_recv)
!!        integer(kind = kint), intent(in) :: n_send, n_recv
!!        integer(kind = kint), intent(in) ::    isendbuf(n_send)
!!        integer(kind = kint), intent(inout) :: irecvbuf(nprocs*n_recv)
!!@n @param  message    message to output
!
      module calypso_mpi_int
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine calypso_mpi_bcast_one_int(buffer, root)
!
      integer, intent(in) :: root
      integer(kind = kint), intent(inout) :: buffer
!
      integer(kind = kint) :: itmp(1)
!
!
      itmp(1) = buffer
      call MPI_BCAST(itmp, 1, CALYPSO_REAL,                             &
     &               root, CALYPSO_COMM, ierr_MPI)
      buffer = itmp(1)
!
      end subroutine calypso_mpi_bcast_one_int
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
      subroutine calypso_mpi_allgather_int                              &
     &         (isendbuf, n_send, irecvbuf, n_recv)
!
      integer(kind = kint), intent(in) :: n_send, n_recv
      integer(kind = kint), intent(in) ::    isendbuf(n_send)
      integer(kind = kint), intent(inout) :: irecvbuf(nprocs*n_recv)
!
!
      call MPI_AllGather(isendbuf, int(n_send), CALYPSO_INTEGER,        &
     &    irecvbuf, int(n_recv), CALYPSO_INTEGER, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      end subroutine calypso_mpi_allgather_int
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi_int
