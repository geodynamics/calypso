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
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use gz_MPI_binary_head_IO
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
      subroutine gz_mpi_write_integer_stack_b(IO_param, num, istack)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: istack(0:num)
!
!
      call gz_mpi_write_int_vector_b(IO_param, num, istack(1))
!
      end subroutine gz_mpi_write_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_vector_b(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength =  num * kint
      ilen_gz = int(real(ilength) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
      call gzip_defleat_once(ilength, int_dat(1), ilen_gz,              &
     &   ilen_gzipped, gzip_buf(1))
!
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!
      call gz_mpi_write_i8stack_head_b                                  &
     &   (IO_param, nprocs, IO_param%istack_merged)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl + IO_param%istack_merged(my_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(nprocs)
!
      end subroutine gz_mpi_write_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int8_vector_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength =  num * kint_gl
      ilen_gz = int(real(ilength) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
      call gzip_defleat_once(ilength, int8_dat(1), ilen_gz,             &
     &    ilen_gzipped, gzip_buf(1))
!
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!
      call gz_mpi_write_i8stack_head_b                                  &
     &   (IO_param, nprocs, IO_param%istack_merged)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl + IO_param%istack_merged(my_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(nprocs)
!
      end subroutine gz_mpi_write_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_1d_vector_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: real_dat(num)
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength =  num * kreal
      ilen_gz = int(real(ilength) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
      call gzip_defleat_once(ilength, real_dat(1), ilen_gz,             &
     &    ilen_gzipped, gzip_buf(1))
!
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!
      call gz_mpi_write_i8stack_head_b                                  &
     &   (IO_param, nprocs, IO_param%istack_merged)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl + IO_param%istack_merged(my_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(nprocs)
!
      end subroutine gz_mpi_write_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_2d_vector_b(IO_param, n1, n2, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: n1, n2
!
      real(kind = kreal), intent(in) :: real_dat(n1,n2)
!
      integer(kind = kint) :: num
!
!
      num = n1 * n2
      call gz_mpi_write_1d_vector_b(IO_param, num, real_dat(1,1))
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
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: ntot
      integer(kind = kint), intent(inout) :: istack(0:num)
!
!
      istack(0) = 0
      call gz_mpi_read_int_vector_b(IO_param, num, istack(1))
      ntot = istack(num)
!
      end subroutine gz_mpi_read_integer_stack_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_vector_b(IO_param, num, int_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: l8_byte
!
!
      call gz_mpi_read_i8stack_head_b                                   &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(num .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) then
        int_dat(1:num) = 0
      else
        ilength = num * kint
        ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)        &
     &            - IO_param%istack_merged(IO_param%id_rank))
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,        &
     &      ilen_gz, gzip_buf(1))
!
        call gzip_infleat_once(ilen_gz, gzip_buf(1),                    &
     &      ilength, int_dat(1), ilen_gzipped)
        deallocate(gzip_buf)
!
        if(iflag_endian .eq. iendian_FLIP) then
          l8_byte = ilength
          call byte_swap_f(l8_byte, int_dat(1))
        end if
      end if
!
      end subroutine gz_mpi_read_int_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int8_vector_b(IO_param, num, int8_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: l8_byte
!
!
      call gz_mpi_read_i8stack_head_b                                   &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(num .le. 0) return
      if(IO_param%id_rank .ge. IO_param%nprocs_in) then
        int8_dat(1:num) = 0
      else
        ilength = num * kint_gl
        ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)        &
     &            - IO_param%istack_merged(IO_param%id_rank))
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
      end subroutine gz_mpi_read_int8_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_1d_vector_b(IO_param, num, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_dat(num)
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
      integer(kind = kint_gl) :: l8_byte
!
!
      call gz_mpi_read_i8stack_head_b                                   &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
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
        ilength = num * kreal
        ilen_gz = int(IO_param%istack_merged(IO_param%id_rank+1)        &
     &            - IO_param%istack_merged(IO_param%id_rank))
!
        allocate(gzip_buf(ilen_gz))
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset,        &
     &      ilen_gz, gzip_buf(1))
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
      end subroutine gz_mpi_read_1d_vector_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_2d_vector_b(IO_param, n1, n2, real_dat)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint), intent(in) :: n1, n2
      real(kind = kreal), intent(inout) :: real_dat(n1,n2)
!
      integer(kind = kint) :: num
!
!
      num = n1 * n2
      call gz_mpi_read_1d_vector_b(IO_param, num, real_dat(1,1))
!
      end subroutine gz_mpi_read_2d_vector_b
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_binary_data_IO
