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
!!     &         (file_name, num_pe, id_rank, IO_param)
!!      subroutine open_read_mpi_file_b                                 &
!!     &         (file_name, num_pe, id_rank, IO_param)
!!
!!      subroutine mpi_write_process_id_b(IO_param)
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
!!      subroutine mpi_read_process_id_b(IO_param)
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
!!      subroutine mpi_read_int4head_b(IO_param, int4_dat)
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
     &         (file_name, num_pe, id_rank, IO_param)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call open_write_mpi_file                                          &
     &   (file_name, num_pe, id_rank, IO_param)
      call calypso_mpi_seek_write_endian                                &
     &   (IO_param%id_file, IO_param%ioff_gl)
!
      end subroutine open_write_mpi_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine open_read_mpi_file_b                                   &
     &         (file_name, num_pe, id_rank, IO_param)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
      call calypso_mpi_seek_read_endian                                 &
     &   (IO_param%id_file, IO_param%iflag_bin_swap, IO_param%ioff_gl)
!
      end subroutine open_read_mpi_file_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_process_id_b(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: itmp_IO(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      itmp_IO(1) = int(IO_param%nprocs_in,KIND(itmp_IO(1)))
      call mpi_write_mul_inthead_b(IO_param, ione64, itmp_IO)
!
      end subroutine mpi_write_process_id_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_one_inthead_b(IO_param, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: int_dat
!
      integer(kind = kint) :: itmp_IO(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      itmp_IO(1) = int_dat
      call mpi_write_mul_inthead_b(IO_param, ione64, itmp_IO)
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
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      rtmp_IO(1) = real_dat
      call mpi_write_mul_realhead_b(IO_param, ione64, rtmp_IO)
!
      end subroutine mpi_write_one_realhead_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_mul_inthead_b(IO_param, num, int_dat)
!
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      type(tmp_i8_array)  :: tmp64
!
!
      if(num .le. 0) return
      call dup_from_short_array(num, int_dat, tmp64)
      call mpi_write_mul_int8head_b(IO_param, tmp64%n1, tmp64%id_a)
      call dealloc_1d_i8array(tmp64)
!
      end subroutine mpi_write_mul_inthead_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_i8stack_head_b(IO_param, num, i8stack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: i8stack(0:num)
!
!
      if(num .le. 0) return
      call mpi_write_mul_int8head_b(IO_param, num, i8stack(1))
!
      end subroutine mpi_write_i8stack_head_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_mul_int8head_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(num .le. 0) return
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
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind=kint_gl) :: num64
!
!
      if(num .le. 0) return
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        num64 = num
        call calypso_mpi_seek_wrt_mul_chara                             &
     &     (IO_param%id_file, ioffset, kchara, num64, chara_dat(1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + num * kchara
!
      end subroutine mpi_write_mul_charahead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_mul_realhead_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(num .le. 0) return
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
      subroutine mpi_read_process_id_b(IO_param)
!
      use m_error_IDs
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: itmp_IO(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call mpi_read_mul_inthead_b(IO_param, ione64, itmp_IO)
      if(int(itmp_IO(1)) .ne. IO_param%nprocs_in) then
        call calypso_mpi_abort(ierr_file, '#. of subdmain is wrong')
      end if
!
      end subroutine mpi_read_process_id_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_one_inthead_b(IO_param, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint) :: itmp_IO(1)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call mpi_read_mul_inthead_b(IO_param, ione64, itmp_IO)
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
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call mpi_read_mul_realhead_b(IO_param, ione64, rtmp_IO)
      real_dat = rtmp_IO(1)
!
      end subroutine mpi_read_one_realhead_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_inthead_b(IO_param, num, int_dat)
!
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      type(tmp_i8_array)  :: tmp64
!
!
      if(num .le. 0) return
      call alloc_1d_i8array(num, tmp64)
      call mpi_read_mul_int8head_b(IO_param, tmp64%n1, tmp64%id_a)
      call dup_to_short_array(tmp64, int_dat)
!
      end subroutine mpi_read_mul_inthead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_i8stack_head_b(IO_param, num, i8stack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: i8stack(0:num)
!
!
      i8stack(0) = 0
      if(num .le. 0) return
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
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(num .le. 0) return
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_read_int8                                 &
     &     (IO_param%id_file, IO_param%iflag_bin_swap,                  &
     &      ioffset, num, int8_dat(1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + num*kint_gl
!
      call calypso_mpi_bcast_int8(int8_dat, num, 0)
!
      end subroutine mpi_read_mul_int8head_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_charahead_b(IO_param, num, chara_dat)
!
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind=kint), intent(in) :: num
      character(len=kchara), intent(inout) :: chara_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: ilen_64
!
!
      if(num .le. 0) return
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_read_mul_chara(IO_param%id_file, ioffset, &
     &     kchara, cast_long(num), chara_dat(1))
      end if
      ilen_64 = num * kchara
      IO_param%ioff_gl = IO_param%ioff_gl + ilen_64
!
      call calypso_mpi_bcast_character(chara_dat, ilen_64, 0)
!
      end subroutine mpi_read_mul_charahead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_mul_realhead_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind=kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(num .le. 0) return
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_read_real                                 &
     &     (IO_param%id_file, IO_param%iflag_bin_swap,                  &
     &      ioffset, num, real_dat(1))
      end if
      IO_param%ioff_gl = IO_param%ioff_gl + num*kreal
!
      call calypso_mpi_bcast_real(real_dat, num, 0)
!
      end subroutine mpi_read_mul_realhead_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int4head_b(IO_param, int4_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer, intent(inout) :: int4_dat
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ioffset = IO_param%ioff_gl
      call calypso_mpi_seek_read_int4head                               &
     &   (IO_param%id_file, int4_dat, ioffset)
      IO_param%ioff_gl = ioffset
!
      end subroutine mpi_read_int4head_b
!
! -----------------------------------------------------------------------
!
      end module MPI_binary_head_IO
