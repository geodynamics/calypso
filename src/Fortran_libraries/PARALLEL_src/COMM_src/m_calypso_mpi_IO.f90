!>@file  m_calypso_mpi_IO.f90
!!       module m_calypso_mpi_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Base routines for MPI-IO
!!
!!@verbatim
!!      subroutine deallocate_mpi_IO_status
!!
!!      subroutine calypso_mpi_write_file_open                          &
!!     &         (file_name, nprocs_in, id_mpi_file)
!!      subroutine calypso_mpi_read_file_open(file_name, id_mpi_file)
!!      subroutine calypso_close_mpi_file(id_mpi_file)
!!
!!      subroutine calypso_mpi_seek_write_chara                         &
!!     &         (id_mpi_file, ioffset, ilength, textbuf)
!!      subroutine calypso_mpi_seek_wrt_mul_chara                       &
!!     &         (id_mpi_file, ioffset, ilength, nline, textbuf)
!!      subroutine calypso_mpi_seek_write_real                          &
!!     &         (id_mpi_file, ioffset, ilength, vector)
!!      subroutine calypso_mpi_seek_write_int                           &
!!     &         (id_mpi_file, ioffset, ilength, int_vector)
!!      subroutine calypso_mpi_seek_write_int8                          &
!!     &         (id_mpi_file, ioffset, ilength, i8_vector)
!!
!!      subroutine calypso_mpi_seek_write_head_c                        &
!!     &         (id_mpi_file, ioff_gl, textbuf)
!!      subroutine calypso_mpi_seek_write_head_r                        &
!!     &         (id_mpi_file, ioff_gl, textbuf)
!!      subroutine calypso_mpi_seek_write_head_i                        &
!!     &         (id_mpi_file, ioff_gl, textbuf)
!!      subroutine calypso_mpi_seek_write_head_i8                       &
!!     &         (id_mpi_file, ioff_gl, textbuf)
!!
!!      subroutine calypso_gz_mpi_seek_write(id_mpi_file,               &
!!     &          ioff_gl, ilen_gzipped, gzip_buf)
!!
!!      subroutine calypso_mpi_seek_read_lenchara                       &
!!     &         (id_mpi_file, ioffset, ilength, charabuf)
!!      subroutine calypso_mpi_seek_read_chara                          &
!!     &         (id_mpi_file, ioffset, ilength, c1buf)
!!      subroutine calypso_mpi_seek_read_real                           &
!!     &         (id_mpi_file, ioffset, ilength, vector)
!!      subroutine calypso_mpi_seek_read_int                            &
!!     &         (id_mpi_file, ioffset, ilength, int_vector)
!!      subroutine calypso_mpi_seek_read_int8                           &
!!     &         (id_mpi_file, ioffset, ilength, i8_vector)
!!
!!    calypso_gz_mpi_seek_write only work correctly when number of 
!!   subdomain is equal to number of threads
!!@endverbatim
!
      module m_calypso_mpi_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!>       status flag for sending
      integer, allocatable :: sta1_IO(:)
!
      private :: init_mpi_IO_status
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_mpi_IO_status
!
      if (allocated(sta1_IO)) return
      allocate(sta1_IO(MPI_STATUS_SIZE))
!
      end subroutine init_mpi_IO_status
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_mpi_IO_status
!
      if (allocated(sta1_IO)) deallocate(sta1_IO)
!
      end subroutine deallocate_mpi_IO_status
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_write_file_open                            &
     &         (file_name, nprocs_in, id_mpi_file)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in
      integer, intent(inout) ::  id_mpi_file
!
      integer(kind = MPI_OFFSET_KIND), parameter :: zerosize = 0
!
!
      call init_mpi_IO_status
      call MPI_FILE_OPEN(CALYPSO_COMM, file_name,                       &
     &    MPI_MODE_WRONLY+MPI_MODE_APPEND+MPI_MODE_CREATE,              &
     &    MPI_INFO_NULL, id_mpi_file, ierr_MPI)
!
     if(nprocs_in .le. nprocs) then
       call MPI_FILE_SET_SIZE(id_mpi_file, zerosize, ierr_MPI)
     end if
!
      end subroutine calypso_mpi_write_file_open
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_read_file_open(file_name, id_mpi_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(inout) ::  id_mpi_file
!
!
      call init_mpi_IO_status
      call calypso_mpi_barrier
      call MPI_FILE_OPEN                                                &
     &   (CALYPSO_COMM, file_name, MPI_MODE_RDONLY,                     &
     &    MPI_INFO_NULL, id_mpi_file, ierr_MPI)
!
      end subroutine calypso_mpi_read_file_open
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_close_mpi_file(id_mpi_file)
!
      integer, intent(in) ::  id_mpi_file
!
!
      call MPI_FILE_CLOSE(id_mpi_file, ierr_MPI)
      call calypso_mpi_barrier
      call deallocate_mpi_IO_status
!
      end subroutine calypso_close_mpi_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_chara                           &
     &         (id_mpi_file, ioffset, ilength, textbuf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength), intent(in) :: textbuf
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, textbuf, ilength,                &
     &      CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
        ioffset = ioffset + ilength
!
      end subroutine calypso_mpi_seek_write_chara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_wrt_mul_chara                         &
     &         (id_mpi_file, ioffset, ilength, nline, textbuf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = kint_gl), intent(in) :: nline
      character(len=ilength), intent(in) :: textbuf(nline)
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
      integer(kind = kint) :: ntot
!
!
      ntot = ilength * int(nline)
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, textbuf, ntot,                   &
     &      CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
      ioffset = ioffset + ntot
!
      end subroutine calypso_mpi_seek_wrt_mul_chara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_real                            &
     &         (id_mpi_file, ioffset, ilength, vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      real(kind = kreal), intent(in) :: vector(ilength)
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, vector, ilength,                 &
     &    CALYPSO_REAL, sta1_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_write_real
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_int                             &
     &         (id_mpi_file, ioffset, ilength, int_vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = kint), intent(in) :: int_vector(ilength)
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, int_vector, ilength,             &
     &    CALYPSO_INTEGER, sta1_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_write_int
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_int8                            &
     &         (id_mpi_file, ioffset, ilength, i8_vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = kint_gl), intent(in) :: i8_vector(ilength)
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_WRITE(id_mpi_file, i8_vector, ilength,              &
     &    CALYPSO_GLOBAL_INT, sta1_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_write_int8
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_head_c                          &
     &         (id_mpi_file, ioff_gl, textbuf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=*), intent(in) :: textbuf
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      ilength = len(textbuf)
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &     (id_mpi_file, ioffset, ilength, textbuf)
      end if
      ioff_gl = ioff_gl + ilength
!
      end subroutine calypso_mpi_seek_write_head_c
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_head_r                          &
     &         (id_mpi_file, ioff_gl, ilength, vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      real(kind = kreal), intent(in) :: vector(ilength)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_real                                &
     &         (id_mpi_file, ioffset, ilength, vector)
      end if
      ioff_gl = ioff_gl + ilength * kreal
!
      end subroutine calypso_mpi_seek_write_head_r
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_head_i                          &
     &         (id_mpi_file, ioff_gl, ilength, int_vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: int_vector(ilength)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_int                                 &
     &         (id_mpi_file, ioffset, ilength, int_vector)
      end if
      ioff_gl = ioff_gl + ilength * kint
!
      end subroutine calypso_mpi_seek_write_head_i
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_head_i8                         &
     &         (id_mpi_file, ioff_gl, ilength, i8_vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: i8_vector(ilength)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = ioff_gl
        call calypso_mpi_seek_write_int8                                &
     &         (id_mpi_file, ioffset, ilength, i8_vector)
      end if
      ioff_gl = ioff_gl + ilength * kint_gl
!
      end subroutine calypso_mpi_seek_write_head_i8
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_gz_mpi_seek_write(id_mpi_file,                 &
     &          ioff_gl, ilen_gzipped, gzip_buf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: ilen_gzipped
      character(len=1), intent(in) :: gzip_buf(ilen_gzipped)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint) :: ip
!
!
      call MPI_Allgather(ilen_gzipped, ione, CALYPSO_INTEGER,           &
     &    ilen_gzipped_gl, ione, CALYPSO_INTEGER, CALYPSO_COMM,         &
     &    ierr_MPI)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = ioff_gl
        do ip = 1, my_rank
          ioffset = ioffset + ilen_gzipped_gl(ip)
        end do
        call calypso_mpi_seek_write_chara                               &
     &    (id_mpi_file, ioffset, ilen_gzipped, gzip_buf(1))
      end if
!
      do ip = 1, nprocs
        ioff_gl = ioff_gl + ilen_gzipped_gl(ip)
      end do
!
      end subroutine calypso_gz_mpi_seek_write
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_lenchara                         &
     &         (id_mpi_file, ioffset, ilength, charabuf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer(kind = kint), intent(in) :: ilength
      character(len=ilength), intent(inout) :: charabuf
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, charabuf, ilength,                &
     &      CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
      ioffset = ioffset + ilength
!
      end subroutine calypso_mpi_seek_read_lenchara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_chara                            &
     &         (id_mpi_file, ioffset, ilength, c1buf)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer(kind = kint), intent(in) :: ilength
      character(len=1), intent(inout) :: c1buf(ilength)
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, c1buf(1), ilength,                &
     &      CALYPSO_CHARACTER, sta1_IO, ierr_MPI)
      ioffset = ioffset + ilength
!
      end subroutine calypso_mpi_seek_read_chara
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_real                             &
     &         (id_mpi_file, ioffset, ilength, vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      real(kind = kreal), intent(inout) :: vector(ilength)
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, vector, ilength,                  &
     &    CALYPSO_REAL, sta1_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_read_real
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_int                              &
     &         (id_mpi_file, ioffset, ilength, int_vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer(kind = kint), intent(inout) :: int_vector(ilength)
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, int_vector, ilength,              &
     &    CALYPSO_INTEGER, sta1_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_read_int
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_int8                             &
     &         (id_mpi_file, ioffset, ilength, i8_vector)
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: ilength
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer(kind = kint_gl), intent(inout) :: i8_vector(ilength)
!
!
      call MPI_FILE_SEEK(id_mpi_file, ioffset, MPI_SEEK_SET, ierr_MPI)
      call MPI_FILE_READ(id_mpi_file, i8_vector, ilength,              &
     &    CALYPSO_GLOBAL_INT, sta1_IO, ierr_MPI)
!
      end subroutine calypso_mpi_seek_read_int8
!
!  ---------------------------------------------------------------------
!
      end module m_calypso_mpi_IO
