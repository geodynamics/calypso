!>@file  gz_MPI_binary_datum_IO.f90
!!       module gz_MPI_binary_datum_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine open_write_gz_mpi_file_b                             &
!!     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!!        Substitution of open_wt_gzfile_b
!!      subroutine open_read_gz_mpi_file_b                              &
!!     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!!        Substitution of open_rd_gzfile_b
!!
!!      subroutine gz_mpi_write_one_inthead_b(IO_param, int_dat)
!!        Substitution of gz_write_one_integer_b
!!      subroutine gz_mpi_write_one_realhead_b(IO_param, real_dat)
!!        Substitution of gz_write_one_real_b
!!      subroutine gz_mpi_write_one_integer_b(IO_param, int_dat)
!!        Substitution of gz_write_one_integer_b
!!
!!      subroutine gz_mpi_read_endian_flag(IO_param)
!!        Substitution of gz_read_endian_flag
!!      subroutine gz_mpi_read_one_inthead_b(IO_param, int_dat)
!!        Substitution of gz_read_one_integer_b
!!      subroutine gz_mpi_read_one_realhead_b(IO_param, real_dat)
!!        Substitution of gz_read_one_real_b
!!      subroutine gz_mpi_read_one_integer_b(IO_param, int_dat)
!!        Substitution of gz_read_one_integer_b
!!@endverbatim
!
      module gz_MPI_binary_datum_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use MPI_ascii_data_IO
!
      implicit none
!
      character(len=1), allocatable, private :: gzip_buf(:)
!
      private :: gz_mpi_read_endian_flag
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_write_gz_mpi_file_b                               &
     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
      call gz_mpi_write_one_inthead_b(IO_param, i_UNIX)
!
      end subroutine open_write_gz_mpi_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine open_read_gz_mpi_file_b                                &
     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call open_read_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
      call gz_mpi_read_endian_flag(IO_param)
!
      end subroutine open_read_gz_mpi_file_b
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_one_inthead_b(IO_param, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: int_dat
!
      integer(kind = kint) :: itmp_IO(1)
!
!
      itmp_IO(1) = int_dat
      call gz_mpi_write_mul_inthead_b(IO_param, ione, itmp_IO)
!
      end subroutine gz_mpi_write_one_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_one_realhead_b(IO_param, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      real(kind = kreal), intent(in) :: real_dat
!
      real(kind = kreal) :: rtmp_IO(1)
!
!
      rtmp_IO(1) = real_dat
      call gz_mpi_write_mul_realhead_b(IO_param, ione, rtmp_IO)
!
      end subroutine gz_mpi_write_one_realhead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_one_integer_b(IO_param, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: int_dat
!
      integer(kind = kint) :: itmp_IO(1)
!
!
      itmp_IO(1) = int_dat
      call gz_mpi_write_int_vector_b(IO_param, ione, itmp_IO)
!
      end subroutine gz_mpi_write_one_integer_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_endian_flag(IO_param)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: int_dat(1)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        ilength = kint
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz                                   &
     &     (IO_param%id_file, ioffset, ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once                                          &
     &     (ilen_gz, gzip_buf(1), ilength, int_dat, ilen_gzipped)
        deallocate(gzip_buf)
!
        if(int_dat(1) .eq. i_UNIX) then
          write(*,*) 'binary data have correct endian!'
          iflag_endian = iendian_KEEP
        else if(int_dat(1) .eq. i_XINU) then
          write(*,*) 'binary data have opposite endian!'
          iflag_endian = iendian_FLIP
        else
          iflag_endian = -1
          call calypso_MPI_abort                                        &
     &       (ierr_fld,'Binary Data is someting wrong!')
        end if
      end if
!
      call MPI_BCAST(iflag_endian, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_read_endian_flag
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_one_inthead_b(IO_param, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint) :: itmp_IO(1)
!
!
      call gz_mpi_read_mul_inthead_b(IO_param, ione, itmp_IO)
      int_dat = itmp_IO(1)
!
      end subroutine gz_mpi_read_one_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_one_realhead_b(IO_param, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      real(kind = kreal), intent(inout) :: real_dat
!
      real(kind = kreal) ::   rtmp_IO(1)
!
!
      call gz_mpi_read_mul_realhead_b(IO_param, ione, rtmp_IO)
      real_dat = rtmp_IO(1)
!
      end subroutine gz_mpi_read_one_realhead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_one_integer_b(IO_param, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint) ::   itmp_IO(1)
!
!
      call gz_mpi_read_int_vector_b(IO_param, ione, itmp_IO(1))
      int_dat = itmp_IO(1)
!
      end subroutine gz_mpi_read_one_integer_b
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_binary_datum_IO
