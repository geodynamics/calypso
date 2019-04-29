!>@file  gz_MPI_ascii_data_IO.f90
!!       module gz_MPI_ascii_data_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_mpi_write_charahead(IO_param, ilength, chara_dat)
!!      subroutine gz_mpi_write_num_of_data(IO_param, num)
!!      subroutine gz_mpi_write_stack_over_domain(IO_param, ilength)
!!      subroutine gz_mpi_write_characters(IO_param, ilength, chara_dat)
!!
!!      subroutine gz_mpi_read_num_of_data(IO_param, num)
!!      function gz_mpi_read_charahead(IO_param, ilength)
!!        character(len=ilength) :: gz_mpi_read_charahead
!!      function gz_mpi_read_characters(IO_param, ilength)
!!        character(len=ilength) :: gz_mpi_read_characters
!!
!!      subroutine gz_mpi_skip_header(IO_param, ilength)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module gz_MPI_ascii_data_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use t_buffer_4_gzip
      use data_IO_to_textline
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_charahead(IO_param, ilength, chara_dat)
!
      use zlib_convert_text
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer, intent(in) :: ilength
      character(len=ilength), intent(in) :: chara_dat
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        call defleate_characters(ilength, chara_dat, zbuf)
!
        ioffset = IO_param%ioff_gl
        call calypso_mpi_seek_write_gz(IO_param%id_file, ioffset, zbuf)
        call dealloc_zip_buffer(zbuf)
      end if
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT,          &
     &    0, CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_write_charahead
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_num_of_data(IO_param, num)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num
!
!
      call set_numbers_2_head_node(num, IO_param)
      call gz_mpi_write_charahead(IO_param,                             &
     &    len_byte_stack_textline(IO_param%nprocs_in),                  &
     &    byte_stack_textline(IO_param%nprocs_in,                       &
     &                        IO_param%istack_merged))
!
      end subroutine gz_mpi_write_num_of_data
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_stack_over_domain(IO_param, ilength)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in) :: ilength
!
!
      call istack64_4_parallel_data(ilength, IO_param)
      call gz_mpi_write_charahead(IO_param,                             &
     &    len_byte_stack_textline(IO_param%nprocs_in),                  &
     &    byte_stack_textline(IO_param%nprocs_in,                       &
     &                        IO_param%istack_merged))
!
      end subroutine gz_mpi_write_stack_over_domain
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_characters(IO_param, ilength, chara_dat)
!
      use data_IO_to_textline
      use zlib_convert_text
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer, intent(in) :: ilength
      character(len=ilength), intent(in) :: chara_dat
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      call defleate_characters(ilength, chara_dat, zbuf)
!
      call gz_mpi_write_stack_over_domain(IO_param, zbuf%ilen_gzipped)
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl + IO_param%istack_merged(my_rank)
        call calypso_mpi_seek_write_gz(IO_param%id_file, ioffset, zbuf)
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_mpi_write_characters
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_num_of_data(IO_param, num)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(inout) :: num
!
      integer :: ilength
!
!
      ilength = len_byte_stack_textline(IO_param%nprocs_in)
      call read_byte_stack_textline                                     &
         (gz_mpi_read_charahead(IO_param, ilength),                     &
     &    IO_param%nprocs_in, IO_param%istack_merged)
!
      if(IO_param%id_rank .lt. IO_param%nprocs_in) then
        num = int(IO_param%istack_merged(IO_param%id_rank+1),KIND(num))
      else
        num = 0
      end if
!
      end subroutine gz_mpi_read_num_of_data
!
! -----------------------------------------------------------------------
!
      function gz_mpi_read_charahead(IO_param, ilength)
!
      use zlib_convert_text
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer, intent(in) :: ilength
      character(len=ilength) :: gz_mpi_read_charahead
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        zbuf%ilen_gz = int(real(ilength) *1.1 + 24,KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
        call infleate_characters(ilength, gz_mpi_read_charahead, zbuf)
      end if
!
      call MPI_BCAST(gz_mpi_read_charahead, ilength,                    &
     &    CALYPSO_CHARACTER, 0,  CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT,          &
     &    0, CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end function gz_mpi_read_charahead
!
! -----------------------------------------------------------------------
!
      function gz_mpi_read_characters(IO_param, ilength)
!
      use data_IO_to_textline
      use zlib_convert_text
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer, intent(in) :: ilength
      character(len=ilength) :: gz_mpi_read_characters
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(ilength .le. 0) return
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      zbuf%ilen_gz = IO_param%istack_merged(IO_param%id_rank+1)         &
     &              - IO_param%istack_merged(IO_param%id_rank)
      call alloc_zip_buffer(zbuf)
      call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
      call infleate_characters(ilength, gz_mpi_read_characters, zbuf)
!
      end function gz_mpi_read_characters
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_skip_header(IO_param, ilength)
!
      use zlib_convert_text
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer, intent(in) :: ilength
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ioffset = IO_param%ioff_gl
        zbuf%ilen_gz = int(real(ilength) *1.1 + 24,KIND(zbuf%ilen_gz))
        call alloc_zip_buffer(zbuf)
        call calypso_mpi_seek_read_gz(IO_param%id_file, ioffset, zbuf)
!
        call infleate_skip_header(ilength, zbuf)
      end if
!
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT,          &
     &    0, CALYPSO_COMM, ierr_MPI)
      IO_param%ioff_gl = IO_param%ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_mpi_skip_header
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_ascii_data_IO
