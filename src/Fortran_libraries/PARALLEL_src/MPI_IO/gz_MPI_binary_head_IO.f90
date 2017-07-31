!>@file  gz_MPI_binary_head_IO.f90
!!       module gz_MPI_binary_head_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_mul_inthead_b(IO_param, num, int_dat)
!!        Substittion of gz_write_mul_integer_b
!!      subroutine gz_mpi_write_i8stack_head_b(IO_param, num, i8stack)
!!      subroutine gz_mpi_write_mul_int8head_b(IO_param, num, int8_dat)
!!        Substittion of gz_write_mul_int8_b
!!      subroutine gz_mpi_write_charahead(IO_param, ilength, chara_dat)
!!      subroutine gz_mpi_write_mul_charahead_b(IO_param, num, chara_dat)
!!       Substittion of gz_write_mul_character_b
!!      subroutine gz_mpi_write_mul_realhead_b(IO_param, num, real_dat)
!!
!!      subroutine gz_mpi_read_mul_inthead_b(IO_param, num, int_dat)
!!        Substittion of gz_read_mul_integer_b
!!      subroutine gz_mpi_read_i8stack_head_b(IO_param, num, i8stack)
!!      subroutine gz_mpi_read_mul_int8head_b(IO_param, num, int_dat)
!!        Substittion of gz_read_mul_int8_b
!!      subroutine gz_mpi_read_mul_charahead_b(IO_param, num, chara_dat)
!!        Substittion of gz_read_mul_character_b
!!      subroutine gz_mpi_read_mul_realhead_b(IO_param, num, real_dat)
!!@endverbatim
!
      module gz_MPI_binary_head_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
!
      implicit none
!
      character(len=1), allocatable, private :: gzip_buf(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_mul_inthead_b(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ilength = num * kint
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once(ilength, int_dat, ilen_gz,               &
     &      ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_write_mul_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_i8stack_head_b(IO_param, num, i8stack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: i8stack(0:num)
!
!
      call gz_mpi_write_mul_int8head_b(IO_param, num, i8stack(1))
!
      end subroutine gz_mpi_write_i8stack_head_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_mul_int8head_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ilength = num * kint_gl
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once (ilength, int8_dat(1), ilen_gz,          &
     &    ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_write_mul_int8head_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_mul_charahead_b(IO_param, num, chara_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ilength = num * kchara
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once(ilength, chara_dat(1), ilen_gz,          &
     &     ilen_gzipped, gzip_buf(1))
!
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_write_mul_charahead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_mul_realhead_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ilength = num * kreal
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once(ilength, real_dat(1), ilen_gz,           &
     &      ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_write_mul_realhead_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_mul_inthead_b(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: l8_byte
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        ilength = num * kint
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,        &
     &     ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once (ilen_gz, gzip_buf(1),                   &
     &     ilength, int_dat, ilen_gzipped)
        deallocate(gzip_buf)
!
        if(iflag_endian .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, int_dat(1))
        end if
      end if
!
      call MPI_BCAST(int_dat, num, CALYPSO_INTEGER, izero,              &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_read_mul_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_i8stack_head_b(IO_param, num, i8stack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: i8stack(0:num)
!
!
      i8stack(0) = 0
      call gz_mpi_read_mul_int8head_b(IO_param, num, i8stack(1))
!
      end subroutine gz_mpi_read_i8stack_head_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_mul_int8head_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: l8_byte
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        ilength = num * kint_gl
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,        &
     &     ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once(ilen_gz, gzip_buf(1),                    &
     &      ilength, int8_dat(1), ilen_gzipped)
        deallocate(gzip_buf)
!
        if(iflag_endian .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, int8_dat(1))
        end if
      end if
!
      call MPI_BCAST(int8_dat, num, CALYPSO_GLOBAL_INT, izero,          &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_read_mul_int8head_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_mul_charahead_b(IO_param, num, chara_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
!
      ilength = num * kchara
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,        &
     &      ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once(ilen_gz, gzip_buf(1),                    &
     &      ilength, chara_dat(1), ilen_gzipped)
        deallocate(gzip_buf)
      end if
!
      call MPI_BCAST(chara_dat, ilength, CALYPSO_CHARACTER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_read_mul_charahead_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_mul_realhead_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: l8_byte
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        ilength = num * kreal
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,        &
     &     ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once(ilen_gz, gzip_buf(1),                    &
     &     ilength, real_dat(1), ilen_gzipped)
        deallocate(gzip_buf)
!
        if(iflag_endian .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, real_dat(1))
        end if
      end if
!
      call MPI_BCAST(real_dat, num, CALYPSO_REAL, izero,                &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_gzipped
!
      end subroutine gz_mpi_read_mul_realhead_b
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_binary_head_IO
