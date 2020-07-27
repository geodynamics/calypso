!>@file   calypso_mpi_char.f90
!!@brief  module calypso_mpi_char
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!
!> @brief MPI communication routines for characters in Calypso
!!
!!@verbatim
!!      subroutine calypso_mpi_bcast_character(buffer, count, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint_gl), intent(in) :: count
!!        character(len = 1), intent(inout) :: buffer(count)
!!
!!      subroutine calypso_mpi_seek_wrt_one_chara                       &
!!     &         (id_mpi_file, ioffset, ilength, textbuf, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer, intent(in) :: ilength
!!        character(len=ilength), intent(in) :: textbuf
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!      subroutine calypso_mpi_seek_wrt_mul_chara                       &
!!     &         (id_mpi_file, ioffset, ilength, nline, textbuf, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        integer, intent(in) :: ilength
!!        integer(kind = kint_gl), intent(in) :: nline
!!        character(len=ilength), intent(in) :: textbuf(nline)
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!
!!      subroutine calypso_mpi_seek_read_one_chara                      &
!!     &         (id_mpi_file, ioffset, ilength, charabuf, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        integer, intent(in) :: ilength
!!        character(len=ilength), intent(inout) :: charabuf
!!        integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!      subroutine calypso_mpi_seek_read_mul_chara                      &
!!     &         (id_mpi_file, ioffset, ilength, nline, textbuf, sta_IO)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!!        integer, intent(in) :: ilength
!!        integer(kind = kint_gl), intent(in) :: nline
!!        character(len=ilength), intent(inout) :: textbuf(nline)
!!       integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!!@endverbatim
!!
!!@n @param  icode       error code
!!@n @param  message    message to output
!
      module calypso_mpi_char
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
      subroutine calypso_mpi_seek_wrt_one_chara                         &
     &         (id_mpi_file, ioffset, ilength, textbuf, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer, intent(in) :: ilength
      character(len=ilength), intent(in) :: textbuf
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
!
      integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, textbuf, ilength,                &
     &    CALYPSO_CHARACTER, sta_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_wrt_one_chara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_wrt_mul_chara                         &
     &         (id_mpi_file, ioffset, ilength, nline, textbuf, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer, intent(in) :: ilength
      integer(kind = kint_gl), intent(in) :: nline
      character(len=ilength), intent(in) :: textbuf(nline)
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
        ilen_in = int(min((nline-ist), huge_20/ilength))
        call MPI_FILE_SEEK                                              &
     &     (id_mpi_file, l8_byte, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_WRITE(id_mpi_file, textbuf(ist+1),                &
     &      (ilen_in*ilength), CALYPSO_CHARACTER, sta_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*ilength
        if(ist .ge. nline) exit
      end do
!
      end subroutine calypso_mpi_seek_wrt_mul_chara
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_one_chara                        &
     &         (id_mpi_file, ioffset, ilength, charabuf, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer, intent(in) :: ilength
!
      character(len=ilength), intent(inout) :: charabuf
      integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, charabuf, ilength,                &
     &    CALYPSO_CHARACTER, sta_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_read_one_chara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_mul_chara                        &
     &         (id_mpi_file, ioffset, ilength, nline, textbuf, sta_IO)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer, intent(in) :: ilength
      integer(kind = kint_gl), intent(in) :: nline
!
      character(len=ilength), intent(inout) :: textbuf(nline)
      integer, intent(inout) :: sta_IO(MPI_STATUS_SIZE)
!
      integer(kind = kint) :: ilen_in
      integer(kind = kint_gl) :: l8_byte, ist
!
!
      ist = 0
      l8_byte = ioffset
      do
        ilen_in = int(min((nline-ist), huge_20/ilength))
        call MPI_FILE_SEEK                                              &
     &     (id_mpi_file, l8_byte, MPI_SEEK_SET, ierr_MPI)
        call MPI_FILE_READ(id_mpi_file, textbuf(ist+1),                 &
     &       (ilen_in*ilength), CALYPSO_CHARACTER, sta_IO, ierr_MPI)
        ist = ist + ilen_in
        l8_byte = l8_byte + ilen_in*ilength
        if(ist .ge. nline) exit
      end do
!
      end subroutine calypso_mpi_seek_read_mul_chara
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi_char
