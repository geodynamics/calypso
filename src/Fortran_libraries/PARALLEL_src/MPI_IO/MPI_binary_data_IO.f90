!>@file  MPI_binary_data_IO.f90
!!       module MPI_binary_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief Output gzipped merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine mpi_write_one_integer_b(IO_param, int_dat)
!!      subroutine mpi_write_integer_stack_b(IO_param, num, istack)
!!        Substitution of gz_write_integer_stack_b
!!      subroutine mpi_write_int_vector_b(IO_param, num, int_dat)
!!        Substitution of gz_write_mul_integer_b
!!      subroutine mpi_write_int8_vector_b(IO_param, num, int8_dat)
!!        Substitution of gz_write_mul_int8_b
!!
!!      subroutine mpi_write_1d_vector_b(IO_param, num, real_dat)
!!        Substitution of gz_write_1d_vector_b
!!      subroutine mpi_write_2d_vector_b(IO_param, n1, n2, real_dat)
!!        Substitution of gz_write_2d_vector_b
!!
!!      subroutine mpi_read_one_integer_b(IO_param, int_dat)
!!      subroutine mpi_read_integer_stack_b(IO_param, num, istack, ntot)
!!        Substittion of  gz_read_integer_stack_b
!!      subroutine mpi_read_int_vector_b(IO_param, num, int_dat)
!!        Substitution of gz_read_mul_integer_b
!!      subroutine mpi_read_int8_vector_b(IO_param, num, int8_dat)
!!        Substitution of gz_read_mul_int8_b
!!
!!      subroutine mpi_read_1d_vector_b(IO_param, num, real_dat)
!!        Substitution of gz_read_1d_vector_b
!!      subroutine mpi_read_2d_vector_b(IO_param, n1, n2, real_dat)
!!        Substitution of gz_read_2d_vector_b
!!
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module MPI_binary_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_binary_head_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_one_integer_b(IO_param, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: int_dat
!
      integer(kind = kint_gl), parameter :: ione64 = 1
      integer(kind = kint) :: itmp_IO(1)
!
!
      itmp_IO(1) = int_dat
      call set_istack_4_fixed_num(ione, IO_param)
      call mpi_write_int_vector_b(IO_param, ione64, itmp_IO)
!
      end subroutine mpi_write_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_integer_stack_b(IO_param, num, istack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
!
      integer(kind = kint), intent(in) :: istack(0:num)
!
      integer(kind = kint_gl) :: ist
!
      ist = min(1,num)
      call mpi_write_int_vector_b(IO_param, num, istack(ist))
!
      end subroutine mpi_write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int_vector_b(IO_param, num, int_dat)
!
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      type(tmp_i8_array)  :: tmp64
!
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      call dup_from_short_array(num, int_dat, tmp64)
      call mpi_write_int8_vector_b(IO_param, tmp64%n1, tmp64%id_a)
      call dealloc_1d_i8array(tmp64)
!
      end subroutine mpi_write_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_int8_vector_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = kint_gl) :: istack_buffer(0:IO_param%nprocs_in)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = IO_param%nprocs_in
      istack_buffer(0:IO_param%nprocs_in)                               &
     &          = IO_param%istack_merged(0:IO_param%nprocs_in)*kint_gl
      call mpi_write_i8stack_head_b(IO_param, num64, istack_buffer)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + istack_buffer(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + istack_buffer(IO_param%nprocs_in)
!
      if(num .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      call calypso_mpi_seek_write_int8                                  &
     &    (IO_param%id_file, ioffset, num, int8_dat(1))
!
      end subroutine mpi_write_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_1d_vector_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: num
!
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = kint_gl) :: istack_buffer(0:IO_param%nprocs_in)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = IO_param%nprocs_in
      istack_buffer(0:IO_param%nprocs_in)                               &
     &          = IO_param%istack_merged(0:IO_param%nprocs_in) * kreal
      call mpi_write_i8stack_head_b(IO_param, num64, istack_buffer)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + istack_buffer(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + istack_buffer(IO_param%nprocs_in)
!
      if(num .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      call calypso_mpi_seek_write_real                                  &
     &    (IO_param%id_file, ioffset, num, real_dat(1))
!
      end subroutine mpi_write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_2d_vector_b(IO_param, n1, n2, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: n1
      integer(kind=kint), intent(in) :: n2
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = n1 * n2
      call mul_istack_4_parallell_vect(n2, IO_param)
      call mpi_write_1d_vector_b(IO_param, num64, real_dat(1,1))
!
      end subroutine mpi_write_2d_vector_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_read_one_integer_b(IO_param, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(inout) :: int_dat
!
      integer(kind = kint) :: itmp_IO(IO_param%nprocs_in)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call mpi_read_int_vector_b(IO_param, ione64, itmp_IO(1))
      int_dat = itmp_IO(1)
!
      end subroutine mpi_read_one_integer_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_integer_stack_b(IO_param, num, istack, ntot)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
!
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
!
      integer(kind = kint_gl) :: ist
!
      istack(0) = 0
      ist = min(1, num)
      call mpi_read_int_vector_b(IO_param, num, istack(ist))
      ntot = istack(num)
!
      end subroutine mpi_read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int_vector_b(IO_param, num, int_dat)
!
      use m_phys_constants
      use transfer_to_long_integers
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      type(tmp_i8_array)  :: tmp64
!
!
      call alloc_1d_i8array(num, tmp64)
      call mpi_read_int8_vector_b(IO_param, tmp64%n1, tmp64%id_a)
      call dup_to_short_array(tmp64, int_dat)
!
      end subroutine mpi_read_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_int8_vector_b(IO_param, num, int8_dat)
!
      use m_phys_constants
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: num
!
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = kint_gl) :: istack_buffer(0:IO_param%nprocs_in)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = IO_param%nprocs_in
      call mpi_read_i8stack_head_b(IO_param, num64, istack_buffer)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + istack_buffer(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + istack_buffer(IO_param%nprocs_in)
!
      if(num .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) then
        int8_dat(1:num) = 0
      else
        call calypso_mpi_seek_read_int8                                 &
     &     (IO_param%id_file, IO_param%iflag_bin_swap,                  &
     &      ioffset, num, int8_dat(1))
      end if
!
      end subroutine mpi_read_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_1d_vector_b(IO_param, num, real_dat)
!
      use m_phys_constants
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint_gl), intent(in) :: num
!
      integer(kind = kint_gl) :: istack_buffer(0:IO_param%nprocs_in)
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = IO_param%nprocs_in
      call mpi_read_i8stack_head_b(IO_param, num64, istack_buffer)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + istack_buffer(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + istack_buffer(IO_param%nprocs_in)
!
      if(num .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) then
        real_dat(1:num) = 0.0d0
      else
        call calypso_mpi_seek_read_real                                 &
     &     (IO_param%id_file, IO_param%iflag_bin_swap,                  &
     &      ioffset, num, real_dat(1))
      end if
!
      end subroutine mpi_read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_2d_vector_b(IO_param, n1, n2, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: n1
      integer(kind = kint), intent(in) :: n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = n1 * n2
      call mul_istack_4_parallell_vect(n2, IO_param)
      call mpi_read_1d_vector_b(IO_param, num64, real_dat(1,1))
!
      end subroutine mpi_read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module MPI_binary_data_IO
