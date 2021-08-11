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
!!
!!      subroutine calypso_mpi_allreduce_one_real                       &
!!     &         (r_local, r_global, operation)
!!        integer, intent(in) :: operation
!!        real(kind = kreal), intent(in) ::    r_local
!!        real(kind = kreal), intent(inout) :: r_global
!!      subroutine calypso_mpi_allreduce_real                           &
!!     &         (r_local, r_global, count, operation)
!!        integer, intent(in) :: operation
!!        integer(kind = kint_gl), intent(in) :: count
!!        real(kind = kreal), intent(in) ::    r_local(count)
!!        real(kind = kreal), intent(inout) :: r_global(count)
!!
!!      subroutine calypso_mpi_gather_one_real(sendbuf, recvbuf, root)
!!        integer, intent(in) :: root
!!        real(kind = kreal), intent(in) ::    sendbuf
!!        real(kind = kreal), intent(inout) :: recvbuf(nprocs)
!!
!!      subroutine calypso_mpi_allgather_one_real(sendbuf, recvbuf)
!!        real(kind = kreal), intent(in) ::    sendbuf
!!        real(kind = kreal), intent(inout) :: recvbuf(nprocs)
!!      subroutine calypso_mpi_allgather_real                           &
!!     &         (sendbuf, n_send, recvbuf, n_recv)
!!        integer(kind =kint), intent(in) :: n_send, n_recv
!!        real(kind = kreal), intent(in) ::    sendbuf(n_send)
!!        real(kind = kreal), intent(inout) :: recvbuf(nprocs*n_recv)
!!
!!      subroutine calypso_mpi_seek_write_real                          &
!!     &         (id_mpi_file, ioffset, num, vector, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        integer(kind = kint_gl), intent(in) :: num
!!        real(kind = kreal), intent(in) :: vector(num)
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!      subroutine calypso_mpi_seek_read_real                           &
!!     &          (id_mpi_file, ioffset, num, vector, sta_IO)
!!        integer, intent(in) :: id_mpi_file
!!        integer(kind = kint_gl), intent(in) :: num
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        real(kind = kreal), intent(inout) :: vector(num)
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
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
      subroutine calypso_mpi_allreduce_one_real                         &
     &         (r_local, r_global, operation)
!
      integer, intent(in) :: operation
      real(kind = kreal), intent(in) ::    r_local
      real(kind = kreal), intent(inout) :: r_global
!
      real(kind = kreal) :: r_lc(1), r_gl(1)
!
!
      r_lc(1) = r_local
      call MPI_allREDUCE(r_lc, r_gl, 1, CALYPSO_REAL,                   &
     &   operation, CALYPSO_COMM, ierr_MPI)
      r_global = r_gl(1)
!
      end subroutine calypso_mpi_allreduce_one_real
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
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_gather_one_real(sendbuf, recvbuf, root)
!
      integer, intent(in) :: root
      real(kind = kreal), intent(in) ::    sendbuf
      real(kind = kreal), intent(inout) :: recvbuf(nprocs)
!
      real(kind = kreal) :: r_lc(1)
!
!
      r_lc(1) = sendbuf
      call MPI_Gather(r_lc, 1, CALYPSO_REAL, recvbuf, 1, CALYPSO_REAL,  &
     &                root, CALYPSO_COMM, ierr_MPI)
!
      end subroutine calypso_mpi_gather_one_real
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allgather_one_real(sendbuf, recvbuf)
!
      real(kind = kreal), intent(in) ::    sendbuf
      real(kind = kreal), intent(inout) :: recvbuf(nprocs)
!
      real(kind = kreal) :: r_lc(1)
!
!
      r_lc(1) = sendbuf
      call MPI_AllGather(r_lc, 1, CALYPSO_REAL,                         &
     &    recvbuf, 1, CALYPSO_REAL, CALYPSO_COMM, ierr_MPI)
!
      end subroutine calypso_mpi_allgather_one_real
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allgather_real                             &
     &         (sendbuf, n_send, recvbuf, n_recv)
!
      integer(kind = kint), intent(in) :: n_send, n_recv
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
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_real                            &
     &         (id_mpi_file, ioffset, num, vector, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: vector(num)
!
      integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!
      integer :: ilen_in
      integer(kind = kint_gl) :: l8_byte, ist
!
!
      ist = 0
      l8_byte = ioffset
      do
        ilen_in = int(min(num-ist, huge_20))
        call MPI_FILE_SEEK                                              &
     &     (id_mpi_file, l8_byte, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_mpi_file, vector(ist+1), ilen_in,        &
     &      CALYPSO_REAL, sta_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*kreal
        if(ist .ge. num) exit
      end do
!
      end subroutine calypso_mpi_seek_write_real
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_real                             &
     &          (id_mpi_file, ioffset, num, vector, sta_IO)
!
      integer, intent(in) :: id_mpi_file
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!
      real(kind = kreal), intent(inout) :: vector(num)
      integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!
      integer(kind = kint) :: ilen_in
      integer(kind = kint_gl) :: l8_byte, ist
!
!
      ist = 0
      l8_byte = ioffset
      do
        ilen_in = int(min(num-ist, huge_20))
        call MPI_FILE_SEEK                                              &
     &     (id_mpi_file, l8_byte, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_mpi_file, vector(ist+1), ilen_in,         &
     &      CALYPSO_REAL, sta_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*kreal
        if(ist .ge. num) exit
      end do
!
      end subroutine calypso_mpi_seek_read_real
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi_real
