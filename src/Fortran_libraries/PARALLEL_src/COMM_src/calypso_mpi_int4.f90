!>@file   calypso_mpi_int4.f90
!!@brief  module calypso_mpi_int4
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!> @brief MPI communication routines for 4 byte integer in Calypso
!!
!!@verbatim
!!      subroutine calypso_mpi_bcast_one_int4(buffer, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint_4b), intent(inout) :: buffer
!!
!!      subroutine calypso_mpi_allreduce_one_int4                       &
!!     &         (i4_local, i4_global, operation)
!!        integer, intent(in) :: operation
!!        integer(kind = kint_4b), intent(in) ::    i4_local
!!        integer(kind = kint_4b), intent(inout) :: i4_global
!!
!!      subroutine calypso_mpi_seek_write_int4                          &
!!     &         (id_mpi_file, ioffset, num, i4_vector)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        integer(kind = kint_gl), intent(in) :: num
!!        integer(kind = kint_4b), intent(in) :: i4_vector(num)
!!       integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!      subroutine calypso_mpi_seek_write_one_int4                      &
!!     &         (id_mpi_file, ioffset, i4_dat, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = kint_gl), intent(in) :: ioffset
!!        integer(kind = kint_4b), intent(in) :: i4_dat
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!      subroutine calypso_mpi_seek_read_one_int4                       &
!!     &         (id_mpi_file, ioffset, i4_dat, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = kint_gl), intent(in) :: ioffset
!!        integer(kind = kint_4b), intent(inout) :: i4_dat
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!@endverbatim
!!
!!@n @param  icode       error code
!!@n @param  message    message to output
!
      module calypso_mpi_int4
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
      subroutine calypso_mpi_bcast_one_int4(buffer, root)
!
      integer, intent(in) :: root
      integer(kind = kint_4b), intent(inout) :: buffer
!
      integer(kind = kint_4b) :: i4_tmp(1)
!
!
      i4_tmp(1) = buffer
      call MPI_BCAST(i4_tmp, 1, CALYPSO_FOUR_INT,                       &
     &               root, CALYPSO_COMM, ierr_MPI)
      buffer = i4_tmp(1)
!
      end subroutine calypso_mpi_bcast_one_int4
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allreduce_one_int4                         &
     &         (i4_local, i4_global, operation)
!
      integer, intent(in) :: operation
      integer(kind = kint_4b), intent(in) ::    i4_local
      integer(kind = kint_4b), intent(inout) :: i4_global
!
      integer(kind = kint_4b) :: i4_lc(1), i4_gl(1)
!
!
      i4_lc(1) = i4_local
      call MPI_allREDUCE(i4_lc, i4_gl, 1, CALYPSO_FOUR_INT,           &
     &                   operation, CALYPSO_COMM, ierr_MPI)
      i4_global = i4_gl(1)
!
      end subroutine calypso_mpi_allreduce_one_int4
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_int4                            &
     &         (id_mpi_file, ioffset, num, i4_vector, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_4b), intent(in) :: i4_vector(num)
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
        call MPI_FILE_WRITE(id_mpi_file, i4_vector(ist+1), ilen_in,     &
     &      CALYPSO_FOUR_INT, sta_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*kint_4b
        if(ist .ge. num) exit
      end do
!
      end subroutine calypso_mpi_seek_write_int4
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_int4                             &
     &         (id_mpi_file, ioffset, num, i4_vector, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
!
      integer(kind = kint_4b), intent(inout) :: i4_vector(num)
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
        call MPI_FILE_READ(id_mpi_file, i4_vector(ist+1), ilen_in,      &
     &      CALYPSO_FOUR_INT, sta_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*kint_4b
        if(ist .ge. num) exit
      end do
!
      end subroutine calypso_mpi_seek_read_int4
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_one_int4                        &
     &         (id_mpi_file, ioffset, i4_dat, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(in) :: ioffset
      integer(kind = kint_4b), intent(in) :: i4_dat
      integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!
      integer(kind = kint_4b) :: i4_tmp(1)
!
!
      i4_tmp(1) = i4_dat
      call MPI_FILE_SEEK                                                &
     &   (id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, i4_tmp, 1,                       &
     &    CALYPSO_FOUR_INT, sta_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_write_one_int4
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_one_int4                         &
     &         (id_mpi_file, ioffset, i4_dat, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(in) :: ioffset
!
      integer(kind = kint_4b), intent(inout) :: i4_dat
      integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!
      integer(kind = kint_4b) :: i4_tmp(1)
!
!
      call MPI_FILE_SEEK                                                &
     &   (id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, i4_tmp, 1,                        &
     &    CALYPSO_FOUR_INT, sta_IO, ierr_MPI)
      i4_dat = i4_tmp(1)

      end subroutine calypso_mpi_seek_read_one_int4
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi_int4
