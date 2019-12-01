!>@file  gz_MPI_binary_data_IO.f90
!!       module gz_MPI_binary_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief Output gzipped merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_integer_stack_b(IO_param, num, istack)
!!        Substitution of gz_write_integer_stack_b
!!      subroutine gz_mpi_write_int_vector_b(IO_param, num, int_dat)
!!        Substitutio of gz_write_mul_integer_b
!!      subroutine gz_mpi_write_int8_vector_b(IO_param, num, int8_dat)
!!        Substitutio of gz_write_mul_int8_b
!!      subroutine gz_mpi_write_1d_vector_b(IO_param, num, real_dat)
!!        Substitutio of gz_write_1d_vector_b
!!      subroutine gz_mpi_write_2d_vector_b(IO_param, n1, n2, real_dat)
!!        Substitutio of gz_write_2d_vector_b
!!
!!      subroutine gz_mpi_read_integer_stack_b                          &
!!     &         (IO_param, num, istack, ntot)
!!        Substittion of  gz_read_integer_stack_b
!!      subroutine gz_mpi_read_int_vector_b(IO_param, num, int_dat)
!!        Substitutio of gz_read_mul_integer_b
!!      subroutine gz_mpi_read_int8_vector_b(IO_param, num, int8_dat)
!!        Substitutio of gz_read_mul_int8_b
!!      subroutine gz_mpi_read_1d_vector_b(IO_param, num, real_dat)
!!        Substitutio of gz_read_1d_vector_b
!!      subroutine gz_mpi_read_2d_vector_b(IO_param, n1, n2, real_dat)
!!        Substitutio of gz_read_2d_vector_b
!!@endverbatim
!
      module gz_MPI_binary_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_buffer_4_gzip
      use t_calypso_mpi_IO_param
      use m_calypso_mpi_IO
      use gz_MPI_binary_head_IO
      use transfer_to_long_integers
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_integer_stack_b(IO_param, num, istack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: istack(0:num)
!
      integer(kind = kint_gl) :: ist
!
!
      ist = min(1,num)
      call gz_mpi_write_int_vector_b(IO_param, num, istack(ist))
!
      end subroutine gz_mpi_write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_vector_b(IO_param, num, int_dat)
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
      call dup_from_short_array(num, int_dat, tmp64)
      call gz_mpi_write_int8_vector_b(IO_param, tmp64%n1, tmp64%id_a)
      call dealloc_1d_i8array(tmp64)
!
      end subroutine gz_mpi_write_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int8_vector_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call defleate_int8_vector_b(num, int8_dat, zbuf)
!
      call istack64_4_parallel_data(zbuf%ilen_gzipped, IO_param)
!
      call gz_mpi_write_merged_stack_b                                  &
     &   (IO_param, nprocs, IO_param%istack_merged)
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl + IO_param%istack_merged(my_rank)
        call calypso_mpi_seek_write_gz(IO_param%id_file, ioffset, zbuf)
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(nprocs)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_mpi_write_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_1d_vector_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call defleate_1d_vector_b(num, real_dat, zbuf)
!
      call istack64_4_parallel_data(zbuf%ilen_gzipped, IO_param)
!
      call gz_mpi_write_merged_stack_b                                  &
     &   (IO_param, nprocs, IO_param%istack_merged)
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl + IO_param%istack_merged(my_rank)
        call calypso_mpi_seek_write_gz(IO_param%id_file, ioffset, zbuf)
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(nprocs)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_mpi_write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_2d_vector_b(IO_param, n1, n2, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
!
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = n1 * n2
      call gz_mpi_write_1d_vector_b(IO_param, num64, real_dat(1,1))
!
      end subroutine gz_mpi_write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_integer_stack_b                            &
     &         (IO_param, num, istack, ntot)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
!
      integer(kind = kint_gl) :: ist
!
      istack(0) = 0
      ist = min(1,num)
      call gz_mpi_read_int_vector_b(IO_param, num, istack(ist))
      ntot = istack(num)
!
      end subroutine gz_mpi_read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_vector_b(IO_param, num, int_dat)
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
      call alloc_1d_i8array(num, tmp64)
      call gz_mpi_read_int8_vector_b(IO_param, tmp64%n1, tmp64%id_a)
      call dup_to_short_array(tmp64, int_dat)
!
      end subroutine gz_mpi_read_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int8_vector_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer(kind = kint_gl) :: l8_byte
!
!
      call gz_mpi_read_merged_stack_b(IO_param,                         &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(num .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) then
        int8_dat(1:num) = 0
      else
        zbuf%ilen_gz = IO_param%istack_merged(IO_param%id_rank+1)       &
     &                - IO_param%istack_merged(IO_param%id_rank)
        call alloc_zip_buffer(zbuf)
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
        call infleate_int8_vector_b(num, int8_dat, zbuf)
!
        if(IO_param%iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = num * kint_gl
          call byte_swap_64bit_f(l8_byte, int8_dat(1))
        end if
      end if
!
      end subroutine gz_mpi_read_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_1d_vector_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer(kind = kint_gl) :: l8_byte
!
!
      call gz_mpi_read_merged_stack_b(IO_param,                         &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(num .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) then
        real_dat(1:num) = 0.0d0
      else
        zbuf%ilen_gz = IO_param%istack_merged(IO_param%id_rank+1)       &
     &                - IO_param%istack_merged(IO_param%id_rank)
        call alloc_zip_buffer(zbuf)
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
        call infleate_1d_vector_b(num, real_dat, zbuf)
!
        if(IO_param%iflag_bin_swap .eq. iendian_FLIP) then
          l8_byte = num * kreal
          call byte_swap_64bit_f(l8_byte, real_dat(1))
        end if
      end if
!
      end subroutine gz_mpi_read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_2d_vector_b(IO_param, n1, n2, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = n1 * n2
      call gz_mpi_read_1d_vector_b(IO_param, num64, real_dat(1,1))
!
      end subroutine gz_mpi_read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_binary_data_IO
