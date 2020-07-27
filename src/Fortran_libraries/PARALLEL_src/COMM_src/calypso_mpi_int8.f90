!>@file   calypso_mpi_int8.f90
!!@brief  module calypso_mpi_int8
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!
!> @brief MPI communication routines for 8-byte integer in Calypso
!!
!!@verbatim
!!      subroutine calypso_mpi_bcast_one_int8(buffer, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint_gl), intent(inout) :: buffer
!!      subroutine calypso_mpi_bcast_int8(buffer, count, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint_gl), intent(in) :: count
!!        integer(kind = kint_gl), intent(inout) :: buffer(count)
!!
!!      subroutine calypso_mpi_reduce_one_int8                          &
!!     &         (i8_local, i8_global, operation, destination)
!!        integer, intent(in) :: operation, destination
!!        integer(kind = kint_gl), intent(in) ::    i8_local
!!        integer(kind = kint_gl), intent(inout) :: i8_global
!!      subroutine calypso_mpi_reduce_int8                              &
!!     &         (i8_local, i8_global, count, operation, destination)
!!        integer, intent(in) :: operation, destination
!!        integer(kind = kint_gl), intent(in) :: count
!!        integer(kind = kint_gl), intent(in) ::    i8_local(count)
!!        integer(kind = kint_gl), intent(inout) :: i8_global(count)
!!
!!      subroutine calypso_mpi_allreduce_one_int8                       &
!!     &         (i8_local, i8_global, operation)
!!        integer, intent(in) :: operation
!!        integer(kind = kint_gl), intent(in) ::    i8_local
!!        integer(kind = kint_gl), intent(inout) :: i8_global
!!      subroutine calypso_mpi_allreduce_int8                           &
!!     &         (i8_local, i8_global, count, operation)
!!        integer, intent(in) :: operation
!!        integer(kind = kint_gl), intent(in) :: count
!!        integer(kind = kint_gl), intent(in) ::    i8_local(count)
!!        integer(kind = kint_gl), intent(inout) :: i8_global(count)
!!
!!      subroutine calypso_mpi_allgather_one_int8(i8sendbuf, i8recvbuf)
!!        integer(kind = kint_gl), intent(in) :: i8sendbuf
!!        integer(kind = kint_gl), intent(inout) :: i8recvbuf(nprocs)
!!      subroutine calypso_mpi_allgather_int8                           &
!!     &         (i8sendbuf, n_send, i8recvbuf, n_recv)
!!        integer(kind = kint), intent(in) :: n_send, n_recv
!!        integer(kind = kint_gl), intent(in) ::    i8sendbuf(n_send)
!!        integer(kind = kint_gl), intent(inout)                        &
!!     &                        :: i8recvbuf(nprocs*n_recv)
!!
!!      subroutine calypso_mpi_seek_write_int8                          &
!!     &         (id_mpi_file, ioffset, num, i8_vector, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        integer(kind = kint_gl), intent(in) :: num
!!        integer(kind = kint_gl), intent(in) :: i8_vector(num)
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!      subroutine calypso_mpi_seek_read_int8                           &
!!     &         (id_mpi_file, ioffset, num, i8_vector, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        integer(kind = kint_gl), intent(in) :: num
!!        integer(kind = kint_gl), intent(inout) :: i8_vector(num)
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!@endverbatim
!
      module calypso_mpi_int8
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
      subroutine calypso_mpi_bcast_one_int8(buffer, root)
!
      integer, intent(in) :: root
      integer(kind = kint_gl), intent(inout) :: buffer
!
      integer(kind = kint_gl) :: itmp8(1)
!
!
      itmp8(1) = buffer
      call MPI_BCAST(itmp8, 1, CALYPSO_GLOBAL_INT,                      &
     &               root, CALYPSO_COMM, ierr_MPI)
      buffer = itmp8(1)
!
      end subroutine calypso_mpi_bcast_one_int8
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
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_reduce_one_int8                            &
     &         (i8_local, i8_global, operation, destination)
!
      integer, intent(in) :: operation, destination
      integer(kind = kint_gl), intent(in) ::    i8_local
      integer(kind = kint_gl), intent(inout) :: i8_global
!
      integer(kind = kint_gl) :: itmp_lc(1), itmp_gl(1)
!
!
      itmp_lc(1) = i8_local
      call MPI_Reduce(itmp_lc, itmp_gl, 1,                            &
     &    CALYPSO_GLOBAL_INT, operation, destination,                 &
     &    CALYPSO_COMM, ierr_MPI)
      i8_global = itmp_gl(1)
!
      end subroutine calypso_mpi_reduce_one_int8
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_reduce_int8                                &
     &         (i8_local, i8_global, count, operation, destination)
!
      integer, intent(in) :: operation, destination
      integer(kind = kint_gl), intent(in) :: count
      integer(kind = kint_gl), intent(in) ::    i8_local(count)
      integer(kind = kint_gl), intent(inout) :: i8_global(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_Reduce(i8_local(ist+1), i8_global(ist+1), ilen_in,     &
     &      CALYPSO_GLOBAL_INT, operation, destination,                 &
     &      CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_reduce_int8
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allreduce_one_int8                         &
     &         (i8_local, i8_global, operation)
!
      integer, intent(in) :: operation
      integer(kind = kint_gl), intent(in) ::    i8_local
      integer(kind = kint_gl), intent(inout) :: i8_global
!
      integer(kind = kint_gl) :: i8_lc(1), i8_gl(1)
!
!
      i8_lc(1) = i8_local
      call MPI_allREDUCE(i8_lc, i8_gl, 1, CALYPSO_GLOBAL_INT,           &
     &                   operation, CALYPSO_COMM, ierr_MPI)
      i8_global = i8_gl(1)
!
      end subroutine calypso_mpi_allreduce_one_int8
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allreduce_int8                             &
     &         (i8_local, i8_global, count, operation)
!
      integer, intent(in) :: operation
      integer(kind = kint_gl), intent(in) :: count
      integer(kind = kint_gl), intent(in) ::    i8_local(count)
      integer(kind = kint_gl), intent(inout) :: i8_global(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_allREDUCE(i8_local(ist+1), i8_global(ist+1), ilen_in,  &
     &      CALYPSO_GLOBAL_INT, operation, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_allreduce_int8
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allgather_one_int8(i8sendbuf, i8recvbuf)
!
      integer(kind = kint_gl), intent(in) :: i8sendbuf
      integer(kind = kint_gl), intent(inout) :: i8recvbuf(nprocs)
!
      integer(kind = kint_gl) :: i8_lc(1)
!
!
      i8_lc(1) = i8sendbuf
      call MPI_AllGather(i8_lc,     1, CALYPSO_GLOBAL_INT,              &
     &                   i8recvbuf, 1, CALYPSO_GLOBAL_INT,              &
     &                   CALYPSO_COMM, ierr_MPI)
!
      end subroutine calypso_mpi_allgather_one_int8
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allgather_int8                             &
     &         (i8sendbuf, n_send, i8recvbuf, n_recv)
!
      integer(kind = kint), intent(in) :: n_send, n_recv
      integer(kind = kint_gl), intent(in) ::    i8sendbuf(n_send)
      integer(kind = kint_gl), intent(inout)                            &
     &                        :: i8recvbuf(nprocs*n_recv)
!
!
      call MPI_AllGather(i8sendbuf, int(n_send), CALYPSO_GLOBAL_INT,    &
     &                   i8recvbuf, int(n_recv), CALYPSO_GLOBAL_INT,    &
     &                   CALYPSO_COMM, ierr_MPI)
!
      end subroutine calypso_mpi_allgather_int8
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_int8                            &
     &         (id_mpi_file, ioffset, num, i8_vector, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: i8_vector(num)
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
        call MPI_FILE_WRITE(id_mpi_file, i8_vector(ist+1), ilen_in,     &
     &      CALYPSO_GLOBAL_INT, sta_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*kint_gl
        if(ist .ge. num) exit
      end do
!
      end subroutine calypso_mpi_seek_write_int8
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_int8                             &
     &         (id_mpi_file, ioffset, num, i8_vector, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: i8_vector(num)
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
        call MPI_FILE_READ(id_mpi_file, i8_vector(ist+1), ilen_in,      &
     &      CALYPSO_GLOBAL_INT, sta_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*kint_gl
        if(ist .ge. num) exit
      end do
!
      end subroutine calypso_mpi_seek_read_int8
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi_int8
