!>@file  MPI_binary_head_IO.f90
!!       module MPI_binary_head_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine open_write_mpi_file_b                                &
!!     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!!      subroutine open_read_mpi_file_b                                 &
!!     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!!
!!      subroutine mpi_write_one_inthead_b(IO_param, int_dat)
!!      subroutine mpi_write_one_realhead_b(IO_param, real_dat)
!!      subroutine mpi_write_one_integer_b(IO_param, int_dat)
!!
!!      subroutine mpi_write_mul_inthead_b(IO_param, num, int_dat)
!!        Substitution of gz_write_mul_integer_b
!!      subroutine mpi_write_i8stack_head_b(IO_param, num, i8stack)
!!      subroutine mpi_write_mul_int8head_b(IO_param, num, int8_dat)
!!        Substitution of gz_write_mul_int8_b
!!      subroutine mpi_write_mul_charahead_b(IO_param, num, chara_dat)
!!        Substitution of gz_write_mul_character_b
!!      subroutine mpi_write_mul_realhead_b(IO_param, num, real_dat)
!! 
!!      subroutine mpi_read_one_inthead_b(IO_param, int_dat)
!!      subroutine mpi_read_one_realhead_b(IO_param, real_dat)
!!
!!      subroutine mpi_read_mul_inthead_b(IO_param, num, int_dat)
!!        Substitution of gz_read_mul_integer_b
!!      subroutine mpi_read_i8stack_head_b(IO_param, num, i8stack)
!!      subroutine mpi_read_mul_int8head_b(IO_param, num, int8_dat)
!!        Substitution of gz_read_mul_int8_b
!!      subroutine mpi_read_mul_charahead_b(IO_param, num, chara_dat)
!!        Substitution of gz_read_mul_character_b
!!      subroutine mpi_read_mul_realhead_b(IO_param, num, real_dat)
!!
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module MPI_binary_head_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_write_mpi_file_b                                  &
     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
      call calypso_mpi_seek_write_endian                                &
     &   (IO_param%id_file, IO_param%ioff_gl)
!
      end subroutine open_write_mpi_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine open_read_mpi_file_b                                   &
     &         (file_name, nprocs_in, my_rank_IO, IO_param)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call open_read_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
      call calypso_mpi_seek_read_endian                                 &
     &   (IO_param%id_file, IO_param%ioff_gl)
!
      end subroutine open_read_mpi_file_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_one_inthead_b(IO_param, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: int_dat
!
      integer(kind = kint) :: itmp_IO(1)
!
!
      itmp_IO(1) = int_dat
      call mpi_write_mul_inthead_b(IO_param, ione, itmp_IO)
!
      end subroutine mpi_write_one_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_one_realhead_b(IO_param, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      real(kind = kreal), intent(in) :: real_dat
!
      real(kind = kreal) :: rtmp_IO(1)
!
!
      rtmp_IO(1) = real_dat
      call mpi_write_mul_realhead_b(IO_param, ione, rtmp_IO)
!
      end subroutine mpi_write_one_realhead_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_mul_inthead_b(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_int                                 &
     &     (IO_param%id_file, ioffset, num, int_dat)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + num * kint
!
      end subroutine mpi_write_mul_inthead_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_i8stack_head_b(IO_param, num, i8stack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: i8stack(0:num)
!
!
      call mpi_write_mul_int8head_b(IO_param, num, i8stack(1))
!
      end subroutine mpi_write_i8stack_head_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_mul_int8head_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_int8                                &
     &     (IO_param%id_file, ioffset, num, int8_dat)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + num * kint_gl
!
      end subroutine mpi_write_mul_int8head_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_mul_charahead_b(IO_param, num, chara_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: num
      character(len=kchara), intent(in) :: chara_dat(num)
!
      integer(kind = kint) :: ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = num * kchara
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ilength, chara_dat(1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ilength
!
      end subroutine mpi_write_mul_charahead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_mul_realhead_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_real                                &
     &     (IO_param%id_file, ioffset, num, real_dat)
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + num * kreal
!
      end subroutine mpi_write_mul_realhead_b
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_one_inthead_b(IO_param, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint) :: itmp_IO(1)
!
!
      call mpi_read_mul_inthead_b(IO_param, ione, itmp_IO)
      int_dat = itmp_IO(1)
!
      end subroutine mpi_read_one_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_one_realhead_b(IO_param, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      real(kind = kreal), intent(inout) :: real_dat
!
      real(kind = kreal) ::   rtmp_IO(1)
!
!
      call mpi_read_mul_realhead_b(IO_param, ione, rtmp_IO)
      real_dat = rtmp_IO(1)
!
      end subroutine mpi_read_one_realhead_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_inthead_b(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind=kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_read_int                                  &
     &     (IO_param%id_file, ioffset, num, int_dat(1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + num*kint
!
      call MPI_BCAST(int_dat, num, CALYPSO_INTEGER, izero,              &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine mpi_read_mul_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_i8stack_head_b(IO_param, num, i8stack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: i8stack(0:num)
!
!
      i8stack(0) = 0
      call mpi_read_mul_int8head_b(IO_param, num, i8stack(1))
!
      end subroutine mpi_read_i8stack_head_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_int8head_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind=kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_read_int8                                 &
     &     (IO_param%id_file, ioffset, num, int8_dat(1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + num*kint_gl
!
      call MPI_BCAST(int8_dat, num, CALYPSO_GLOBAL_INT, izero,          &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine mpi_read_mul_int8head_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_charahead_b(IO_param, num, chara_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind=kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      ilength = num * kchara
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_read_gz                                   &
     &     (IO_param%id_file, ioffset, ilength, chara_dat(1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + ilength
!
      call MPI_BCAST(chara_dat, ilength, CALYPSO_CHARACTER, izero,      &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine mpi_read_mul_charahead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_realhead_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind=kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_read_real                                 &
     &     (IO_param%id_file, ioffset, num, real_dat(1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + num*kreal
!
      call MPI_BCAST(real_dat, num, CALYPSO_REAL, izero,                &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine mpi_read_mul_realhead_b
!
! -----------------------------------------------------------------------
!
      end module MPI_binary_head_IO
