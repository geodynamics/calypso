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
!!     &         (file_name, num_pe, id_mpi_file)
!!      subroutine calypso_mpi_append_file_open                         &
!!     &         (file_name, num_pe, id_mpi_file, ioffset)
!!      subroutine calypso_mpi_read_file_open(file_name, id_mpi_file)
!!      subroutine calypso_close_mpi_file(id_mpi_file)
!!
!!      subroutine mpi_write_endian_flag(id_mpi_file, ioffset)
!!      subroutine mpi_write_one_chara_b                                &
!!     &         (id_mpi_file, ioffset, ilength, textbuf)
!!      subroutine mpi_write_mul_chara_b                                &
!!     &         (id_mpi_file, ioffset, ilength, nline, textbuf)
!!      subroutine mpi_write_real_b                                     &
!!     &         (id_mpi_file, ioffset, num, vector)
!!      subroutine mpi_write_int_b                                      &
!!     &         (id_mpi_file, ioffset, num, i_vector)
!!      subroutine mpi_write_int8_b                                     &
!!     &         (id_mpi_file, ioffset, num, i8_vector)
!!      subroutine mpi_write_int4_b                                     &
!!     &         (id_mpi_file, ioffset, num, i4_vector)
!!
!!      subroutine calypso_mpi_seek_write_head_c                        &
!!     &         (id_mpi_file, ioffset, textbuf)
!!!
!!      subroutine mpi_read_set_endian_flag                             &
!!     &         (id_mpi_file, iflag_bin_swap, ioffset)
!!      subroutine mpi_read_one_chara_b                                 &
!!     &         (id_mpi_file, ioffset, ilength, charabuf)
!!      function calypso_mpi_seek_read_chara                            &
!!     &       (id_mpi_file, ioffset, ilength)
!!        character(len=ilength) :: calypso_mpi_seek_read_chara
!!      subroutine mpi_read_mul_chara_b                                 &
!!     &         (id_mpi_file, ioffset, ilength, nline, textbuf)
!!      subroutine mpi_read_real_b                                      &
!!     &         (id_mpi_file, iflag_bin_swap, ioffset, num, vector)
!!      subroutine mpi_read_int_b                                       &
!!     &         (id_mpi_file, iflag_bin_swap, ioffset, num, i_vector)
!!      subroutine mpi_read_int8_b                                      &
!!     &         (id_mpi_file, iflag_bin_swap, ioffset, num, i8_vector)
!!      subroutine mpi_read_int4_b                                      &
!!     &         (id_mpi_file, iflag_bin_swap, ioffset, num, i4_vector)
!!
!!      subroutine calypso_mpi_seek_write_gz(id_mpi_file, ioffset, zbuf)
!!      subroutine calypso_mpi_seek_read_gz(id_mpi_file, ioffset, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine calypso_gz_mpi_seek_write(id_mpi_file, ioff_gl, zbuf)
!!        type(buffer_4_gzip), intent(in) :: zbuf
!!    calypso_gz_mpi_seek_write only work correctly when number of 
!!   subdomain is equal to number of threads
!!@endverbatim
!
      module m_calypso_mpi_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
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
     &         (file_name, num_pe, id_mpi_file)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe
      integer, intent(inout) :: id_mpi_file
!
      integer(kind = MPI_OFFSET_KIND), parameter :: zerosize = 0
!
!
      call init_mpi_IO_status
      call MPI_FILE_OPEN(CALYPSO_COMM, file_name,                       &
     &    MPI_MODE_RDWR+MPI_MODE_APPEND+MPI_MODE_CREATE,                &
     &    MPI_INFO_NULL, id_mpi_file, ierr_MPI)
!
     if(num_pe .le. nprocs) then
       call MPI_FILE_SET_SIZE(id_mpi_file, zerosize, ierr_MPI)
     end if
!
      end subroutine calypso_mpi_write_file_open
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_append_file_open                           &
     &         (file_name, num_pe, id_mpi_file, ioffset)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe
      integer, intent(inout) :: id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioffset
!
!
      call init_mpi_IO_status
      call MPI_FILE_OPEN(CALYPSO_COMM, file_name,                       &
     &    MPI_MODE_RDWR+MPI_MODE_APPEND+MPI_MODE_CREATE,                &
     &    MPI_INFO_NULL, id_mpi_file, ierr_MPI)
!
      call MPI_FILE_GET_SIZE(id_mpi_file, ioffset, ierr_MPI)
!
      end subroutine calypso_mpi_append_file_open
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
      call deallocate_mpi_IO_status
!
      end subroutine calypso_close_mpi_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_one_chara_b                                  &
     &         (id_mpi_file, ioffset, ilength, textbuf)
!
      use calypso_mpi_char
!
      integer, intent(in) ::  id_mpi_file
      integer, intent(in) :: ilength
      character(len=ilength), intent(in) :: textbuf
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
!
!
      call calypso_mpi_seek_wrt_one_chara                               &
     &   (id_mpi_file, ioffset, ilength, textbuf, sta1_IO)
      ioffset = ioffset + ilength
!
      end subroutine mpi_write_one_chara_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_mul_chara_b                                  &
     &         (id_mpi_file, ioffset, ilength, nline, textbuf)
!
      use calypso_mpi_char
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer, intent(in) :: ilength
      integer(kind = kint_gl), intent(in) :: nline
      character(len=ilength), intent(in) :: textbuf(nline)
!
!
      call calypso_mpi_seek_wrt_mul_chara                               &
     &   (id_mpi_file, ioffset, ilength, nline, textbuf, sta1_IO)
!
      end subroutine mpi_write_mul_chara_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_real_b                                       &
     &         (id_mpi_file, ioffset, num, vector)
!
      use calypso_mpi_real
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: vector(num)
!
!
      call calypso_mpi_seek_write_real                                  &
     &   (id_mpi_file, ioffset, num, vector, sta1_IO)
!
      end subroutine mpi_write_real_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_int_b                                        &
     &         (id_mpi_file, ioffset, num, i_vector)
!
      use calypso_mpi_int
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(in) :: i_vector(num)
!
!
      call calypso_mpi_seek_write_int                                   &
     &    (id_mpi_file, ioffset, num, i_vector, sta1_IO)
!
      end subroutine mpi_write_int_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_int8_b                                       &
     &         (id_mpi_file, ioffset, num, i8_vector)
!
      use calypso_mpi_int8
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: i8_vector(num)
!
!
      call calypso_mpi_seek_write_int8                                  &
     &   (id_mpi_file, ioffset, num, i8_vector, sta1_IO)
!
      end subroutine mpi_write_int8_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_int4_b                                       &
     &         (id_mpi_file, ioffset, num, i4_vector)
!
      use calypso_mpi_int4
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer(kind = kint_gl), intent(in) :: num
      integer, intent(in) :: i4_vector(num)
!
!
      call calypso_mpi_seek_write_int4                                  &
     &        (id_mpi_file, ioffset, num, i4_vector, sta1_IO)
!
      end subroutine mpi_write_int4_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_head_c                          &
     &         (id_mpi_file, ioffset, textbuf)
!
      use calypso_mpi_char
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioffset
      character(len=*), intent(in) :: textbuf
!
      integer :: ilength
!
!
      ilength = len(textbuf)
      if(my_rank .eq. 0) then
        call calypso_mpi_seek_wrt_one_chara                             &
     &     (id_mpi_file, ioffset, ilength, textbuf, sta1_IO)
      end if
      ioffset = ioffset + ilength
!
      end subroutine calypso_mpi_seek_write_head_c
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_write_endian_flag(id_mpi_file, ioffset)
!
      use calypso_mpi_int4
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioffset
!
!
      if(my_rank .eq. 0) then
        call calypso_mpi_seek_write_one_int4                            &
     &          (id_mpi_file, ioffset, i_UNIX, sta1_IO)
      end if
      ioffset = ioffset + ifour
!
      end subroutine mpi_write_endian_flag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_set_endian_flag                               &
     &         (id_mpi_file, iflag_bin_swap, ioffset)
!
      use m_error_IDs
      use binary_IO
      use calypso_mpi_int4
!
      integer, intent(in) ::  id_mpi_file
      integer, intent(inout) :: iflag_bin_swap
      integer(kind = kint_gl), intent(inout) :: ioffset
!
      integer :: iflag_read
!
!
      if(my_rank .eq. 0) then
        call calypso_mpi_seek_read_one_int4                             &
&          (id_mpi_file, ioffset, iflag_read, sta1_IO)
        iflag_bin_swap = endian_check(my_rank, iflag_read)
      end if
      ioffset = ioffset + ifour
!
      call calypso_mpi_bcast_one_int4(iflag_bin_swap, 0)

      end subroutine mpi_read_set_endian_flag
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_one_chara_b                                   &
     &         (id_mpi_file, ioffset, ilength, charabuf)
!
      use calypso_mpi_char
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer, intent(in) :: ilength
      character(len=ilength), intent(inout) :: charabuf
!
!
      call calypso_mpi_seek_read_one_chara(id_mpi_file, ioffset,        &
     &    ilength, charabuf, sta1_IO)
      ioffset = ioffset + ilength
!
      end subroutine mpi_read_one_chara_b
!
!  ---------------------------------------------------------------------
!
      function calypso_mpi_seek_read_chara                              &
     &       (id_mpi_file, ioffset, ilength)
!
      use calypso_mpi_char
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      integer, intent(in) :: ilength
!
      character(len=ilength) :: calypso_mpi_seek_read_chara
!
!
      call calypso_mpi_seek_read_one_chara(id_mpi_file, ioffset,        &
     &    ilength, calypso_mpi_seek_read_chara, sta1_IO)
      ioffset = ioffset + ilength
!
      end function calypso_mpi_seek_read_chara
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mul_chara_b                                   &
     &         (id_mpi_file, ioffset, ilength, nline, textbuf)
!
      use calypso_mpi_char
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer, intent(in) :: ilength
      integer(kind = kint_gl), intent(in) :: nline
      character(len=ilength), intent(inout) :: textbuf(nline)
!
!
      call calypso_mpi_seek_read_mul_chara                              &
     &   (id_mpi_file, ioffset, ilength, nline, textbuf, sta1_IO)
!
      end subroutine mpi_read_mul_chara_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_real_b                                        &
     &         (id_mpi_file, iflag_bin_swap, ioffset, num, vector)
!
      use calypso_mpi_real
      use byte_swap_f
!
      integer, intent(in) :: id_mpi_file
      integer, intent(in) :: iflag_bin_swap
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      real(kind = kreal), intent(inout) :: vector(num)
!
!
      call calypso_mpi_seek_read_real                                   &
     &   (id_mpi_file, ioffset, num, vector, sta1_IO)
!
      if(iflag_bin_swap .eq. iendian_FLIP) then
        call byte_swap_real_f(num, vector(1))
      end if
!
      end subroutine mpi_read_real_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_int_b                                         &
     &         (id_mpi_file, iflag_bin_swap, ioffset, num, i_vector)
!
      use calypso_mpi_int
      use byte_swap_f
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer, intent(in) :: iflag_bin_swap
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint), intent(inout) :: i_vector(num)
!
!
      call calypso_mpi_seek_read_int                                    &
     &   (id_mpi_file, ioffset, num, i_vector, sta1_IO)
!
      if(iflag_bin_swap .eq. iendian_FLIP) then
        call byte_swap_int_f(num, i_vector(1))
      end if
!
      end subroutine mpi_read_int_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_int8_b                                        &
     &         (id_mpi_file, iflag_bin_swap, ioffset, num, i8_vector)
!
      use calypso_mpi_int8
      use byte_swap_f
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer, intent(in) :: iflag_bin_swap
      integer(kind = kint_gl), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: i8_vector(num)
!
!
      call calypso_mpi_seek_read_int8                                   &
     &   (id_mpi_file, ioffset, num, i8_vector, sta1_IO)
!
      if(iflag_bin_swap .eq. iendian_FLIP) then
        call byte_swap_int8_f(num, i8_vector(1))
      end if
!
      end subroutine mpi_read_int8_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_int4_b                                        &
     &         (id_mpi_file, iflag_bin_swap, ioffset, num, i4_vector)
!
      use calypso_mpi_int4
      use byte_swap_f
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(in) :: ioffset
      integer, intent(in) :: iflag_bin_swap
      integer(kind = kint_gl), intent(in) :: num
      integer, intent(inout) :: i4_vector(num)
!
!
      call calypso_mpi_seek_read_int4                                   &
     &   (id_mpi_file, ioffset, num, i4_vector, sta1_IO)
!
      if(iflag_bin_swap .eq. iendian_FLIP) then
        call byte_swap_int4_f(num, i4_vector(1))
      end if
!
      end subroutine mpi_read_int4_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_write_gz(id_mpi_file, ioffset, zbuf)
!
      use t_buffer_4_gzip
      use calypso_mpi_char
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      type(buffer_4_gzip), intent(in) :: zbuf
!
!
      call calypso_mpi_seek_wrt_mul_chara(id_mpi_file, ioffset,         &
     &    1, zbuf%ilen_gzipped, zbuf%gzip_buf, sta1_IO)
      ioffset = ioffset + zbuf%ilen_gzipped
!
      end subroutine calypso_mpi_seek_write_gz
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_seek_read_gz(id_mpi_file, ioffset, zbuf)
!
      use t_buffer_4_gzip
      use calypso_mpi_char
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND), intent(inout) :: ioffset
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call calypso_mpi_seek_read_mul_chara(id_mpi_file, ioffset,        &
     &    1, zbuf%ilen_gz, zbuf%gzip_buf, sta1_IO)
      ioffset = ioffset + zbuf%ilen_gz
!
      end subroutine calypso_mpi_seek_read_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_gz_mpi_seek_write(id_mpi_file, ioff_gl, zbuf)
!
      use calypso_mpi_int8
      use t_buffer_4_gzip
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      type(buffer_4_gzip), intent(in) :: zbuf
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: ilen_gzipped_gl(nprocs)
      integer(kind = kint) :: ip
!
!
      call calypso_mpi_allgather_one_int8                               &
     &   (zbuf%ilen_gzipped, ilen_gzipped_gl)
!
      if(zbuf%ilen_gzipped .gt. 0) then
        ioffset = ioff_gl
        do ip = 1, my_rank
          ioffset = ioffset + ilen_gzipped_gl(ip)
        end do
        call calypso_mpi_seek_write_gz(id_mpi_file, ioffset, zbuf)
      end if
!
      do ip = 1, nprocs
        ioff_gl = ioff_gl + ilen_gzipped_gl(ip)
      end do
!
      end subroutine calypso_gz_mpi_seek_write
!
!  ---------------------------------------------------------------------
!
      end module m_calypso_mpi_IO
