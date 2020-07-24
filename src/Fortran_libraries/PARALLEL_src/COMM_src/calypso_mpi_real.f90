!>@file   calypso_mpi_real.f90
!!@brief  module calypso_mpi_real
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!
!> @brief MPI communication routines for real in Calypso
!!
!!@verbatim
!!      subroutine calypso_mpi_bcast_one_real(buffer, root)
!!        integer, intent(in) :: root
!!        real(kind = kreal), intent(inout) :: buffer
!!      subroutine calypso_mpi_bcast_real(buffer, count, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint_gl), intent(in) :: count
!!        real(kind = kreal), intent(inout) :: buffer(count)
!!      subroutine calypso_mpi_reduce_real                              &
!!     &         (r_local, r_global, count, operation, destination)
!!        integer, intent(in) :: operation, destination
!!        integer(kind = kint_gl), intent(in) :: count
!!        real(kind = kreal), intent(in) ::    r_local(count)
!!        real(kind = kreal), intent(inout) :: r_global(count)
!!      subroutine calypso_mpi_allreduce_real                           &
!!     &         (r_local, r_global, count, operation)
!!       integer, intent(in) :: operation
!!        integer(kind = kint_gl), intent(in) :: count
!!        real(kind = kreal), intent(in) ::    r_local(count)
!!        real(kind = kreal), intent(inout) :: r_global(count)
!!      subroutine calypso_mpi_allgather_real                           &
!!     &         (sendbuf, n_send, recvbuf, n_recv)
!!        integer(kind =kint), intent(in) :: n_send, n_recv
!!        real(kind = kreal), intent(in) ::    sendbuf(n_send)
!!        real(kind = kreal), intent(inout) :: recvbuf(nprocs*n_recv)
!!@endverbatim
!!
!!@n @param  icode       error code
!!@n @param  message    message to output
!
      module calypso_mpi_real
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
      subroutine calypso_mpi_bcast_one_real(buffer, root)
!
      integer, intent(in) :: root
      real(kind = kreal), intent(inout) :: buffer
!
      real(kind = kreal) :: rtmp(1)
!
!
      rtmp(1) = buffer
      call MPI_BCAST(rtmp, 1, CALYPSO_REAL,                             &
     &               root, CALYPSO_COMM, ierr_MPI)
      buffer = rtmp(1)
!
      end subroutine calypso_mpi_bcast_one_real
!
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
      subroutine calypso_mpi_allgather_real                             &
     &         (sendbuf, n_send, recvbuf, n_recv)
!
      integer(kind =kint), intent(in) :: n_send, n_recv
      real(kind = kreal), intent(in) ::    sendbuf(n_send)
      real(kind = kreal), intent(inout) :: recvbuf(nprocs*n_recv)
!
!
      call MPI_AllGather(sendbuf, int(n_send), CALYPSO_REAL,            &
     &    recvbuf, int(n_recv), CALYPSO_REAL, CALYPSO_COMM, ierr_MPI)
!
      end subroutine calypso_mpi_allgather_real
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi_real
