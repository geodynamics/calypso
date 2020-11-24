!>@file   calypso_mpi_int.f90
!!@brief  module calypso_mpi_int
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!> @brief MPI communication routines for integer in Calypso
!!
!!@verbatim
!!      subroutine calypso_mpi_bcast_one_int(buffer, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint), intent(inout) :: buffer
!!      subroutine calypso_mpi_bcast_int(buffer, count, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint_gl), intent(in) :: count
!!        integer(kind = kint), intent(inout) :: buffer(count)
!!
!!      subroutine calypso_mpi_reduce_int                               &
!!     &         (i_local, i_global, count, operation, destination)
!!        integer, intent(in) :: destination
!!        integer, intent(in) :: operation
!!        integer(kind = kint_gl), intent(in) :: count
!!        integer(kind = kint), intent(in) ::    i_local(count)
!!        integer(kind = kint), intent(inout) :: i_global(count)
!!
!!      subroutine calypso_mpi_allreduce_one_int                        &
!!     &         (i_local, i_global, operation)
!!        integer, intent(in) :: operation
!!        integer(kind = kint), intent(in) ::    i_local
!!        integer(kind = kint), intent(inout) :: i_global
!!      subroutine calypso_mpi_allreduce_int                            &
!!     &         (i_local, i_global, count, operation)
!!        integer, intent(in) :: operation
!!        integer(kind = kint_gl), intent(in) :: count
!!        integer(kind = kint), intent(in) ::    i_local(count)
!!        integer(kind = kint), intent(inout) :: i_global(count)
!!
!!      subroutine calypso_mpi_scatter_one_int(isendbuf, irecvbuf, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint), intent(in) ::    isendbuf(nprocs)
!!        integer(kind = kint), intent(inout) :: irecvbuf
!!
!!      subroutine calypso_mpi_gather_one_int(isendbuf, irecvbuf, root)
!!        integer(kind = kint), intent(in) ::    isendbuf
!!        integer(kind = kint), intent(inout) :: irecvbuf(nprocs)
!!
!!      subroutine calypso_mpi_allgather_one_int(isendbuf, irecvbuf)
!!        integer(kind = kint), intent(in) ::    isendbuf
!!        integer(kind = kint), intent(inout) :: irecvbuf(nprocs)
!!      subroutine calypso_mpi_allgather_int                            &
!!     &         (isendbuf, n_send, irecvbuf, n_recv)
!!        integer(kind = kint), intent(in) :: n_send, n_recv
!!        integer(kind = kint), intent(in) ::    isendbuf(n_send)
!!        integer(kind = kint), intent(inout) :: irecvbuf(nprocs*n_recv)
!!
!!      subroutine calypso_mpi_alltoall_one_int(isendbuf, irecvbuf)
!!        integer(kind = kint), intent(in) ::    isendbuf(nprocs)
!!        integer(kind = kint), intent(inout) :: irecvbuf(nprocs)
!!
!!      subroutine calypso_mpi_seek_write_int                           &
!!     &         (id_mpi_file, ioffset, num, i_vector, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        integer(kind = kint_gl), intent(in) :: num
!!        integer(kind = kint), intent(in) :: i_vector(num)
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!      subroutine calypso_mpi_seek_read_int                            &
!!     &         (id_mpi_file, ioffset, num, i_vector, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        integer(kind = kint_gl), intent(in) :: num
!!        integer(kind = kint), intent(inout) :: i_vector(num)
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!@endverbatim
!!
!!@n @param  icode       error code
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
      call MPI_BCAST(itmp, 1, CALYPSO_INTEGER,                          &
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
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_reduce_int                                 &
     &         (i_local, i_global, count, operation, destination)
!
      integer, intent(in) :: destination
      integer, intent(in) :: operation
      integer(kind = kint_gl), intent(in) :: count
      integer(kind = kint), intent(in) ::    i_local(count)
      integer(kind = kint), intent(inout) :: i_global(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_REDUCE(i_local(ist+1), i_global(ist+1), ilen_in,       &
     &      CALYPSO_INTEGER, operation, destination,                    &
     &      CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_reduce_int
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allreduce_one_int                          &
     &         (i_local, i_global, operation)
!
      integer, intent(in) :: operation
      integer(kind = kint), intent(in) ::    i_local
      integer(kind = kint), intent(inout) :: i_global
!
      integer(kind = kint) :: i_lc(1), l_gl(1)
!
!
      i_lc(1) = i_local
      call MPI_allREDUCE(i_lc, l_gl, 1, CALYPSO_INTEGER,                &
     &                   operation, CALYPSO_COMM, ierr_MPI)
      i_global = l_gl(1)
!
      end subroutine calypso_mpi_allreduce_one_int
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allreduce_int                              &
     &         (i_local, i_global, count, operation)
!
      integer, intent(in) :: operation
      integer(kind = kint_gl), intent(in) :: count
      integer(kind = kint), intent(in) ::    i_local(count)
      integer(kind = kint), intent(inout) :: i_global(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_allREDUCE(i_local(ist+1), i_global(ist+1), ilen_in,    &
     &      CALYPSO_INTEGER, operation, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_allreduce_int
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_scatter_one_int(isendbuf, irecvbuf, root)
!
      integer, intent(in) :: root
      integer(kind = kint), intent(in) ::    isendbuf(nprocs)
      integer(kind = kint), intent(inout) :: irecvbuf
!
      integer(kind = kint) :: i_gl(1)
!
!
      call MPI_Scatter(isendbuf, 1, CALYPSO_INTEGER,                    &
     &                 i_gl, 1, CALYPSO_INTEGER,                        &
     &                 root, CALYPSO_COMM, ierr_MPI)
      irecvbuf = i_gl(1)
!
      end subroutine calypso_mpi_scatter_one_int
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_gather_one_int(isendbuf, irecvbuf, root)
!
      integer, intent(in) :: root
      integer(kind = kint), intent(in) ::    isendbuf
      integer(kind = kint), intent(inout) :: irecvbuf(nprocs)
!
      integer(kind = kint) :: i_lc(1)
!
!
      i_lc(1) = isendbuf
      call MPI_Gather(i_lc, 1, CALYPSO_INTEGER,                      &
     &                irecvbuf, 1, CALYPSO_INTEGER,                  &
     &                root, CALYPSO_COMM, ierr_MPI)
!
      end subroutine calypso_mpi_gather_one_int
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allgather_one_int(isendbuf, irecvbuf)
!
      integer(kind = kint), intent(in) ::    isendbuf
      integer(kind = kint), intent(inout) :: irecvbuf(nprocs)
!
      integer(kind = kint) :: i_lc(1)
!
!
      i_lc(1) = isendbuf
      call MPI_AllGather(i_lc, 1, CALYPSO_INTEGER,                      &
     &    irecvbuf, 1, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
!
      end subroutine calypso_mpi_allgather_one_int
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
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_alltoall_one_int(isendbuf, irecvbuf)
!
      integer(kind = kint), intent(in) ::    isendbuf(nprocs)
      integer(kind = kint), intent(inout) :: irecvbuf(nprocs)
!
!
      call MPI_AlltoAll(isendbuf, 1, CALYPSO_INTEGER,                   &
     &    irecvbuf, 1, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
!
      end subroutine calypso_mpi_alltoall_one_int
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_int                             &
     &         (id_mpi_file, ioffset, num, i_vector, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: i_vector(num)
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
        call MPI_FILE_WRITE(id_mpi_file, i_vector(ist+1), ilen_in,      &
     &      CALYPSO_INTEGER, sta_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*kint
        if(ist .ge. num) exit
      end do
!
      end subroutine calypso_mpi_seek_write_int
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_int                              &
     &         (id_mpi_file, ioffset, num, i_vector, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
!
      integer(kind = kint), intent(inout) :: i_vector(num)
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
        call MPI_FILE_READ(id_mpi_file, i_vector(ist+1), ilen_in,       &
     &      CALYPSO_INTEGER, sta_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*kint
        if(ist .ge. num) exit
      end do
!
      end subroutine calypso_mpi_seek_read_int
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi_int
